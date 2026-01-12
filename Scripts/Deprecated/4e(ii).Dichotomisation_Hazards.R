# ======================================= INPUT SPACE: LOSS SEVERITY-TWO STAGE============================
# Dichotomise probabilistic models into 0/1-decisions
# ------------------------------------------------------------------------------------------------------
# PROJECT TITLE: Loss Modelling (LGD) for Residential Mortgages
# SCRIPT AUTHOR(S): Marcel Muller (MM), Mohammed Gabru (MG)
# ------------------------------------------------------------------------------------------------------
# -- Script dependencies:
#   - 0.Setup.R
#   - 1.Data_Import.R
#   - 2a.Data_Prepare_Credit_Basic.R
#   - 2b.Data_Prepare_Credit_Advanced.R
#   - 2c.Data_Prepare_Credit_Advanced2.R
#   - 2d.Data_Enrich.R
#   - 2f.Data_Fusion1.R
#   - 2g.Data_Fusion2.R

# -- Inputs:
#   - datCredit_train_CDH | Prepared from script 2g
#   - datCredit_valid_CDH | Prepared from script 2g
#
# -- Outputs:
#   - thres_lst | Probability thresholds
# ------------------------------------------------------------------------------------------------------


# ------ 1. Preliminaries

# --- 1.1 Load and prepare data
# - Confirm prepared datasets are loaded into memory
if (!exists('datCredit_train_CDH')) unpack.ffdf(paste0(genPath,"creditdata_train_CDH"), tempPath);gc()
if (!exists('datCredit_valid_CDH')) unpack.ffdf(paste0(genPath,"creditdata_valid_CDH"), tempPath);gc()

# - Use only default spells
datCredit_train <- datCredit_train_CDH[!is.na(DefSpell_Key),]
datCredit_valid <- datCredit_valid_CDH[!is.na(DefSpell_Key),]

# - Remove previous objects from memory
rm(datCredit_train_CDH, datCredit_valid_CDH); gc()

# - Weigh write-off cases
datCredit_train[, Weight:=ifelse(DefSpell_Event==1,1,1)]
datCredit_valid[, Weight:=ifelse(DefSpell_Event==1,1,1)]

# - Create pointer to the appropriate data object 
datCredit <- rbind(data.table(datCredit_train, Sample="Train"), 
                   data.table(datCredit_valid, Sample="Validation"))


# --- 1.2 Load models
# - Basic discrete-time hazard model
modLR_Bas <- readRDS(paste0(genObjPath,"CoxDisc_Basic_Model.rds"))

# - Advanced discrete-time hazard model
modLR_Adv <- readRDS(paste0(genObjPath,"CoxDisc_Advanced_Model.rds"))

# - Classic logit model
modLR_Classic <- readRDS(paste0(genObjPath,"LR_Model.rds"))




# ------ 2. Constructing expected term-structures of write-off | un-dichotomised / raw

# --- 2.1 Handle left-truncated spells by adding a starting record 
### NOTE:  This is necessary for calculating certain survival quantities later

# - Create an additional record for each default spell
datAdd <- subset(datCredit, Counter == 1 & TimeInDefSpell > 1)
datAdd[, TimeInDefSpell:=TimeInDefSpell-1]
datAdd[, Counter:=0]

# - Add record to main dataset
datCredit <- rbind(datCredit, datAdd); setorder(datCredit, DefSpell_Key, TimeInDefSpell)


# --- 2.2 Calculate account-level survival quantities of interest
# - Score using classic model for each instance of [TimeInDefSpell] as [DefSpell_Age]
datCredit[, DefSpell_Age2:=DefSpell_Age]; datCredit[, DefSpell_Age:=TimeInDefSpell]

# - Predict hazard h(t) = P(T=t | T>= t) in discrete-time
datCredit[, Hazard_adv:=predict(modLR_Adv, newdata=datCredit, type = "response")]
datCredit[, Hazard_bas:=predict(modLR_Bas, newdata=datCredit, type = "response")]
datCredit[, Hazard_classic:=predict(modLR_Classic, newdata=.SD[], type="response")]

# - Derive survival probability S(t) = prod(1 - hazard)
datCredit[, Survival_adv:=cumprod(1-Hazard_adv), by=list(DefSpell_Key)]
datCredit[, Survival_bas:=cumprod(1-Hazard_bas), by=list(DefSpell_Key)]
datCredit[, Survival_classic:=cumprod(1-Hazard_classic), by=list(DefSpell_Key)]

# - Derive discrete density, or event probability f(t) = S(t-1) - S(t)
datCredit[, EventRate_adv:=data.table::shift(Survival_adv, type="lag", n=1, fill=1) - Survival_adv, by=list(DefSpell_Key)]
datCredit[, EventRate_bas:=data.table::shift(Survival_bas, type="lag", n=1, fill=1) - Survival_bas, by=list(DefSpell_Key)]
datCredit[, EventRate_classic:=data.table::shift(Survival_classic, type="lag", n=1, fill=1) - Survival_classic, by=list(DefSpell_Key)]

# - Remove added rows
datCredit <- subset(datCredit, Counter > 0)


# --- 2.3 Filtering
# - Identify where the loss rate is out of bounds and not feasible
datCredit[, OOB_Ind:=ifelse(LossRate_Real < 0 | LossRate_Real > 1,1,0)]

# - Subset to include only relevant data and recreate default spell event indicator (write-off) for classical LR-model
datCredit_train_classic <- subset(datCredit, !is.na(DefSpell_Key) & OOB_Ind==0 & Sample=="Train" & DefSpell_Counter==1)
datCredit_train_classic[, DefSpell_Event:=ifelse(DefSpellResol_Type_Hist!="WOFF",0,1)]




# ------ 3. Determining the thresholds for dichotomisation
# --- 3.1 Determine thresholds | Discrete time models
# - Create stripped-down version of required dataset or optimisation to minimise input/output run-time
datGiven <- datCredit[Sample=="Train",list(DefSpell_Event, Hazard_bas, Hazard_adv)]
datGiven_classic <- datCredit_train_classic[,list(DefSpell_Event, Hazard_classic)]

# - Calculate the cost multiple
# (q1 <- mean(datCredit_train$DefSpell_Event,na.rm=TRUE))
### RESULTS: Prevalence = 0.01111503
# (a <- (1-q1)/q1)
### RESULTS: Cost-multiple = 88.96827

# - Basic model
(thresh_dth_bas <- GenYoudenIndex(optimise_type="Pre-determined", Train_DataSet=datGiven,
                                  Target="DefSpell_Event",prob_vals_given="Hazard_bas", 
                                  a=38, replicate=48, numThreads=8, limits=c(0,0.025),
                                  replicateName="DtH-Basic"))
### RESULTS: Threshold at a=38: 0.02345153
### RESULTS: Threshold at a=1: 
### RESULTS: Threshold at a=(1-q1)/q1: 

# - Advanced model
(thresh_dth_adv <- GenYoudenIndex(optimise_type="Pre-determined", Train_DataSet=datGiven, 
                                  Target="DefSpell_Event", prob_vals_given="Hazard_adv", 
                                  a=1, replicate=8, numThreads=8, limits=c(0,0.3),
                                  replicateName="DtH-Advanced"))
### RESULTS: Threshold at a=1: 0.2933144
### RESULTS: Threshold at a=(1-q1)/q1: 


# --- 3.2 Determine thresholds | Classical models
# - Calculate the cost multiple
# (q1 <- mean(datCredit_train_classic$DefSpell_Event,na.rm=TRUE))
### RESULTS: Prevalence = 0.1824916
# (a <- (1-q1)/q1)
### RESULTS: Cost-multiple = 4.479705

# - Classical model
(thresh_lr_classic <- GenYoudenIndex(optimise_type="Pre-determined", Train_DataSet=datGiven_classic,
                                     Target="DefSpell_Event", prob_vals_given="Hazard_classic",
                                     a=12, replicate=48, numThreads=8, limits=c(0,0.4),
                                     replicateName="LR"))
### RESULTS: Threshold at a=12: 0.07077651
### RESULTS: Threshold at a=1: 
### RESULTS: Threshold at a=(1-q1)/q1: 


# --- 3.2 Save thresholds to disk
# - Combine thresholds into a single list
thres_lst <- list("Basic"=thresh_dth_bas$cutoff,
                  "Advanced"=thresh_dth_adv$cutoff,
                  "Classical"=thresh_lr_classic$cutoff)

# - Save list to disk
# saveRDS(thres_lst, file=paste0(genObjPath,"Classification_Thresholds.rds"))




# ------ 4. Assess dicotomised models' performance

# --- 4.1 Dichotomise model predictions
# - Dichotomise predictions
datCredit[,Youden_bas:=ifelse(Hazard_bas>thres_lst$Basic,1,0)]
datCredit[,Youden_adv:=ifelse(Hazard_adv>thres_lst$Advanced,1,0)]
datCredit[,Youden_classic:=ifelse(Hazard_classic>thres_lst$Classical,1,0)]


# --- 4.2 ROC analyses at time 44
# - Basic discrete-time model | B-series (dichotomised)
predictTime <- 44
ptm <- proc.time() #IGNORE: for computation time calculation
objROC44_CDH_CoxDisc_bas_B <- tROC.multi(datGiven=datCredit, modGiven=modLR_Bas, month_End=predictTime, sLambda=0.05, estMethod="NN-0/1", numDigits=4, 
                                         fld_ID="DefSpell_Key", fld_Event="DefSpell_Event", eventVal=1, fld_StartTime="Start", fld_EndTime="TimeInDefSpell",
                                         graphName="WOffSurvModel-ROC_CoxDisc_Advanced_TimeVar", genFigPathGiven=paste0(genFigPath, "tROC-Analyses/"), 
                                         caseStudyName=paste0("CoxDisc_CDH_", predictTime), numThreads=12, logPath=genPath, 
                                         predType="response", MarkerGiven="Hazard_bas", threshold=thres_lst$Basic)
proc.time() - ptm
objROC44_CDH_CoxDisc_bas_B$AUC; objROC44_CDH_CoxDisc_bas_B$ROC_graph
### RESULTS: AUC up to t: 48.79%, achieved in 123 secs

# - Advanced discrete-time model | B-series (dichotomised)
predictTime <- 44
ptm <- proc.time() #IGNORE: for computation time calculation
objROC44_CDH_CoxDisc_adv_B <- tROC.multi(datGiven=datCredit, modGiven=modLR_Adv, month_End=predictTime, sLambda=0.05, estMethod="NN-0/1", numDigits=4, 
                                         fld_ID="DefSpell_Key", fld_Event="DefSpell_Event", eventVal=1, fld_StartTime="Start", fld_EndTime="TimeInDefSpell",
                                         graphName="WOffSurvModel-ROC_CoxDisc_Advanced_TimeVar", genFigPathGiven=paste0(genFigPath, "tROC-Analyses/"), 
                                         caseStudyName=paste0("CoxDisc_CDH_", predictTime), numThreads=12, logPath=genPath, 
                                         predType="response", MarkerGiven="Hazard_adv", threshold=thres_lst$Advanced)
proc.time() - ptm
objROC44_CDH_CoxDisc_adv_B$AUC; objROC44_CDH_CoxDisc_adv_B$ROC_graph
### RESULTS: AUC up to t: 51.57%, achieved in 120 secs

# - Classical logistic regression model | B-series (dichotomised)
# Create an event indicator
datCredit[, DefSpell_Event2 := ifelse(DefSpellResol_Type_Hist=="WOFF", 1, 0)]
# Perform ROC analyses
ptm <- proc.time() #IGNORE: for computation time calculation
objROC_LR_B <- roc(response=datCredit[DefSpell_Counter==1, DefSpell_Event2], predictor=datCredit[DefSpell_Counter==1, Youden_classic])
proc.time() - ptm
objROC_LR_B$auc; plot(objROC_LR_B)
### RESULTS: AUC up to t: 75.3%, achieved in 1 secs

### CONCLUSION: ROC analyses results are very similar to the dichotomisation
###             performed on the event rates.


# --- 4.4 Term-structure | Actual
# - Compute Kaplan-Meier survival estimates (product-limit) for main-event | Spell-level with right-censoring & left-truncation
km_Default <- survfit(Surv(time=TimeInDefSpell-1, time2=TimeInDefSpell, event=DefSpell_Event==1, type="counting") ~ 1, 
                      id=DefSpell_Key, data=datCredit)

# - Create survival table
(datSurv_act <- surv_summary(km_Default))

# - Enrich data
datSurv_act <- datSurv_act %>%
  # Rename some fields
  rename(Time=time, AtRisk_n=`n.risk`, Event_n=`n.event`, Censored_n=`n.censor`, SurvivalProb_KM=`surv`) %>%
  # Calculate hazard rate
  mutate(Hazard_Actual=Event_n/AtRisk_n) %>% 
  # Calculate cumulative hazard rate
  mutate(CHaz=cumsum(Hazard_Actual)) %>%
  # Probability mass function f(t)=h(t)*S(t-1)
  mutate(EventRate=Hazard_Actual*data.table::shift(SurvivalProb_KM, n=1, fill=1)) %>%
  # Filter for observations with events and censoring
  filter(Event_n > 0 | Censored_n >0) %>% as.data.table()

# - Order dataset
setorder(datSurv_act, Time)

# - Calculate the at-risk population proportion
datSurv_act[,AtRisk_perc:=AtRisk_n/max(AtRisk_n, na.rm=T)]


# --- 4.3 Term-structure | Expected
# - Aggregate event rates to period-level
datSurv_exp <- datCredit[,.(# Basic model
  EventRate_bas=mean(EventRate_bas, na.rm=T),
  EventRate_bas_Youden=mean(Youden_bas, na.rm=T),
  # Advanced model
  EventRate_adv=mean(EventRate_adv, na.rm=T),
  EventRate_adv_Youden=mean(Youden_adv, na.rm=T),
  # Classical model
  EventRate_classic=mean(EventRate_classic, na.rm=T),
  EventRate_classic_Youden=mean(Youden_classic, na.rm=T),
  # Empirical estimates
  EventRate_Emp=sum(DefSpell_Event)/.N),
  by=list(TimeInDefSpell)]

# - Plotting event rates for actuals and all expecteds
plot(datSurv_act[Time<=120, EventRate], type="b")
lines(datSurv_exp[TimeInDefSpell<=120, EventRate_adv], type="b", col="red")
lines(datSurv_exp[TimeInDefSpell<=120, EventRate_adv_Youden], type="b", col="blue")
lines(datSurv_exp[TimeInDefSpell<=120, EventRate_bas], type="b", col="green")
lines(datSurv_exp[TimeInDefSpell<=120, EventRate_bas_Youden], type="b", col="purple")
lines(datSurv_exp[TimeInDefSpell<=120, EventRate_classic], type="b", col="cyan")
lines(datSurv_exp[TimeInDefSpell<=120, EventRate_classic_Youden], type="b", col="orange")

### CONCLUSION: - Advanced series B model is very similar (slightly worse than) the same model obtained by dichotomising the event rates
###             - Basic series B model is worse (under predicting) compare to the same model obtained by dichotomising the event rates
###             - Classical series B model is worse (over predicting) compare to the same model obtained by dichotomising the event rates


# --- 4.3 Clean-up
suppressWarnings(rm(datCredit_train, datCredit_valid, datCredit, datCredit_train_classic,
                    datSurv_act, datSurv_exp, km_Default, datAdd, datGiven,
                    datGiven_classic, modLR_Adv, modLR_Bas, modLR_Classic, thresh_dth_bas,
                    thres_lst, objROC44_CDH_CoxDisc_adv_B, objROC_LR_B))
gc()


