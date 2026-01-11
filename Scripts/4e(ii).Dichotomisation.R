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




# ------ 3. Constructing expected term-structures of write-off | un-dichotomised / raw

# --- 3.1 Handle left-truncated spells by adding a starting record 
### NOTE:  This is necessary for calculating certain survival quantities later

# - Create an additional record for each default spell
datAdd <- subset(datCredit, Counter == 1 & TimeInDefSpell > 1)
datAdd[, TimeInDefSpell:=TimeInDefSpell-1]
datAdd[, Counter:=0]

# - Add record to main dataset
datCredit <- rbind(datCredit, datAdd); setorder(datCredit, DefSpell_Key, TimeInDefSpell)


# --- 3.2 Calculate account-level survival quantities of interest
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
datCredit[, EventRate_adv:=shift(Survival_adv, type="lag", n=1, fill=1) - Survival_adv, by=list(DefSpell_Key)]
datCredit[, EventRate_bas:=shift(Survival_bas, type="lag", n=1, fill=1) - Survival_bas, by=list(DefSpell_Key)]
datCredit[, EventRate_classic:=shift(Survival_classic, type="lag", n=1, fill=1) - Survival_classic, by=list(DefSpell_Key)]

# - Remove added rows
datCredit <- subset(datCredit, Counter > 0)


# --- 3.3 Filtering
# - Identify where the loss rate is out of bounds and not feasible
datCredit[, OOB_Ind:=ifelse(LossRate_Real < 0 | LossRate_Real > 1,1,0)]

# - Subset to include only relevant data and recreate default spell event indicator (write-off) for classical LR-model
datCredit_train_classic <- subset(datCredit, !is.na(DefSpell_Key) & OOB_Ind==0 & Sample=="Train" & DefSpell_Counter==1)
datCredit_train_classic[, DefSpell_Event:=ifelse(DefSpellResol_Type_Hist!="WOFF",0,1)]




# ------ 3. Determining the thresholds for dichotomisation
# --- 3.1 Determine thresholds | Discrete time models
# - Create stripped-down version of required dataset or optimisation to minimise input/output run-time
datGiven <- datCredit[Sample=="Train",list(DefSpell_Event, EventRate_bas, EventRate_adv)]
datGiven_classic <- datCredit_train_classic[,list(DefSpell_Event, EventRate_classic)]

# - Calculate the cost multiple
# (q1 <- mean(datCredit_train$DefSpell_Event,na.rm=TRUE))
### RESULTS: Prevalence = 0.01111503
# (a <- (1-q1)/q1)
### RESULTS: Cost-multiple = 88.96827

# - Basic model
(thresh_dth_bas <- GenYoudenIndex(optimise_type="Pre-determined", Train_DataSet=datGiven,
                                  Target="DefSpell_Event",prob_vals_given="EventRate_bas", 
                                  a=38, replicate=48, numThreads=8, limits=c(0,0.025),
                                  replicateName="DtH-Basic"))
### RESULTS: Threshold at a=38: 0.01515866
### RESULTS: Threshold at a=1: 0.02498668
### RESULTS: Threshold at a=(1-q1)/q1: 0.009839146

# - Advanced model
(thresh_dth_adv <- GenYoudenIndex(optimise_type="Pre-determined", Train_DataSet=datGiven, 
                                  Target="DefSpell_Event", prob_vals_given="EventRate_adv", 
                                  a=1, replicate=8, numThreads=8, limits=c(0,0.3),
                                  replicateName="DtH-Advanced"))
### RESULTS: Threshold at a=1: 0.2950287
### RESULTS: Threshold at a=(1-q1)/q1: 0.005047943


# --- 3.2 Determine thresholds | Classical models
# - Calculate the cost multiple
# (q1 <- mean(datCredit_train_classic$DefSpell_Event,na.rm=TRUE))
### RESULTS: Prevalence = 0.1824916
# (a <- (1-q1)/q1)
### RESULTS: Cost-multiple = 4.479705

# - Classical model
(thresh_lr_classic <- GenYoudenIndex(optimise_type="Pre-determined", Train_DataSet=datGiven_classic,
                                     Target="DefSpell_Event", prob_vals_given="EventRate_classic",
                                     a=12, replicate=48, numThreads=8, limits=c(0,0.4),
                                     replicateName="LR"))
### RESULTS: Threshold at a=12: 0.0650852
### RESULTS: Threshold at a=1: 0.3997155
### RESULTS: Threshold at a=(1-q1)/q1: 0.2930735


# --- 3.2 Save thresholds to disk
# - Combine thresholds into a single list
thres_lst <- list("Basic"=thresh_dth_bas$cutoff,
                  "Advanced"=thresh_dth_adv$cutoff,
                  "Classical"=thresh_lr_classic$cutoff)

# - Save list to disk
saveRDS(thres_lst, file=paste0(genObjPath,"Classification_Thresholds.rds"))


# --- 3.3 Clean-up
suppressWarnings(rm(datCredit_train, datCredit_train_classic,
                    modLR_adv, modLR_bas, modLR_classic, thresh_dth_bas,
                    thresh_dth_adv, thresh_lr_classic, thres_lst))
gc()


