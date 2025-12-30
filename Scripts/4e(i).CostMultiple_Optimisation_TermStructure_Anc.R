# ======================================= INPUT SPACE: LOSS SEVERITY-TWO STAGE==========================
# Analyse cost multiple (a) in Generalised Youden Index by optimising it via
# minimising the MAE between the empirical term-structure of write-off risk and the resulting expected
# term-structure, given the 0/1-predictions from such a dichotomised model
# ------------------------------------------------------------------------------------------------------
# PROJECT TITLE: Loss Modelling (LGD) for Residential Mortgages
# SCRIPT AUTHOR(S): Dr Arno Botha (AB), Marcel Muller (MM)
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
#   - Input_Space
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

# - Create start and stop columns
datCredit_train[, Start:=TimeInDefSpell-1]
datCredit_valid[, Start:=TimeInDefSpell-1]

# - Weigh write-off cases
datCredit_train[, Weight:=ifelse(DefSpell_Event==1,1,1)]
datCredit_valid[, Weight:=ifelse(DefSpell_Event==1,1,1)]


# --- 1.2 Load models
# - Basic discrete-time hazard model
modLR_Bas <- readRDS(paste0(genObjPath,"CoxDisc_Basic_Model.rds"))

# - Advanced discrete-time hazard model
modLR_Adv <- readRDS(paste0(genObjPath,"CoxDisc_Advanced_Model.rds"))

# - Classic logit model
modLR_Classic <- readRDS(paste0(genObjPath,"LR_Model.rds"))


# --- 1.3 Other parameters
# Cost multiple (a) vector
vCostMultiples <- c(seq(from=0.1, to=1, by=0.1), 
                    seq(from=2, to=10, by=0.5),
                    seq(from=11,to=100, by=1))




# ------ 2. Constructing the empirical term-structure of write-off using a Kaplan-Meier estimator

# --- 2.1 Preliminaries
# - Create pointer to the appropriate data object 
datCredit <- rbind(data.table(datCredit_train, Sample="Train"), 
                   data.table(datCredit_valid, Sample="Validation"))


# --- 2.2 Estimate survival rate of the main event S(t) = P(T >=t) for time-to-event variable T
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
  mutate(EventRate=Hazard_Actual*shift(SurvivalProb_KM, n=1, fill=1)) %>%
  # Filter for observations with events and censoring
  filter(Event_n > 0 | Censored_n >0) %>% as.data.table()

# - Order dataset
setorder(datSurv_act, Time)

# - Calculate the at-risk population proportion
datSurv_act[,AtRisk_perc:=AtRisk_n/max(AtRisk_n, na.rm=T)]




# ------ 3. Constructing expected term-structures of write-off | un-dichotomised / raw

# --- 3.1 Handle left-truncated spells by adding a starting record 
### NOTE:  This is necessary for calculating certain survival quantities later

# - Create an additional record for each default spell
datAdd <- subset(datCredit, Counter == 1 & TimeInDefSpell > 1)
datAdd[, Start:=Start-1]
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
datCredit[, OOB_Ind:=ifelse(LossRate_Real<0 | LossRate_Real>1,1,0)]

# - Subset to include only relevant data and recreate default spell event indicator (write-off) for classical LR-model
datCredit_train_classic <- subset(datCredit, !is.na(DefSpell_Key) & OOB_Ind==0 & Sample=="Train" & DefSpell_Counter==1)
datCredit_train_classic[, DefSpell_Event:=ifelse(DefSpellResol_Type_Hist!="WOFF",0,1)]




# ------ 4. Iteration over a-vector (cost multiples) for Generalised Youden Index
# For a given cost multiple, we calculate the corresponding Generalised Youden Index towards
# finding an appropriate probability/marker cut-off that penalises false negatives by (a) times more than
# false positives. Then, impose this cut-off and aggregate the dichotomised event rates to the spell period level.
# Calculate the MAE between the resulting expected term-structure and its empirical counterpart.
# Repeat this process for each cost multiple and collate results.
# The 'best' cost multiple is then the one that minimises the MAE between the empirical and expected term-structures
### NOTE: Regarding replication within GenYoudenIndex, analysis has shown that the advanced DtH-model exhibits 
# far less variability in its results, hence the lower replication value compared to that of the basic DtH-model.

ptm <- proc.time() #IGNORE: for computation time calculation
for (i in 101:length(vCostMultiples)) {
  # - Testing conditions
  # a <- vCostMultiples[1]; i <- 1
  
  # -- Run subroutine for the Generalised Youden Index given a specific cost multiple (a)
  # - Basic DtH-model
  thresh_dth_bas <- GenYoudenIndex(optimise_type="Pre-determined", Train_DataSet=datCredit[Sample=="Train",],
                                   Target="DefSpell_Event",prob_vals_given="EventRate_bas", 
                                   a=vCostMultiples[i], replicate=150, numThreads=8)
  # - Advanced DtH-model
  thresh_dth_adv <- GenYoudenIndex(optimise_type="Pre-determined", Train_DataSet=datCredit[Sample=="Train",], 
                                   Target="DefSpell_Event", prob_vals_given="EventRate_adv", 
                                   a=vCostMultiples[i], replicate=3, numThreads=3)
  # - Classical model
  thresh_lr_classic <- GenYoudenIndex(optimise_type="Pre-determined", Train_DataSet=datCredit_train_classic,
                                      Target="DefSpell_Event", prob_vals_given="EventRate_classic",
                                      a=vCostMultiples[i])
  
  
  # -- Dichotomise probabilistic models
  datCredit[, Youden_bas := ifelse(EventRate_bas > thresh_dth_bas$cutoff,1,0)]
  datCredit[, Youden_adv := ifelse(EventRate_adv > thresh_dth_adv$cutoff,1,0)]
  datCredit[, Youden_classic := ifelse(EventRate_classic > thresh_lr_classic$cutoff,1,0)]
  
  # -- Aggregate event rates and calculate MAEs
  # - Aggregate event rates to period-level
  datSurv_exp <- datCredit[,.(
    EventRate_bas_Youden = mean(Youden_bas, na.rm=T),
    EventRate_adv_Youden = mean(Youden_adv, na.rm=T),
    EventRate_classic_Youden = mean(Youden_classic, na.rm=T)),
    by=list(TimeInDefSpell)]
  
  # - Plotting event rates for actuals and all expecteds
  #plot(datSurv_act[Time<=120, EventRate], type="b")
  #lines(datSurv_exp[TimeInDefSpell<=120, EventRate_adv_Youden], type="b", col="red")
  #lines(datSurv_exp[TimeInDefSpell<=120, EventRate_bas_Youden], type="b", col="blue")
  #lines(datSurv_exp[TimeInDefSpell<=120, EventRate_classic_Youden], type="b", col="orange")
  
  # - Calculate MAE between event rates
  # Basic model
  (MAE_eventProb_bas_Youden <- mean(abs(datSurv_act[Time<=sMaxSpellAge, EventRate] - 
                                          datSurv_exp[TimeInDefSpell<=sMaxSpellAge, EventRate_bas_Youden]), na.rm=T))
  # Advanced model
  (MAE_eventProb_adv_Youden <- mean(abs(datSurv_act[Time<=sMaxSpellAge, EventRate] - 
                                          datSurv_exp[TimeInDefSpell<=sMaxSpellAge, EventRate_adv_Youden]), na.rm=T))
  # Classical model
  (MAE_eventProb_classic_Youden <- mean(abs(datSurv_act[Time<=sMaxSpellAge, EventRate] - 
                                              datSurv_exp[TimeInDefSpell<=sMaxSpellAge, EventRate_classic_Youden]), na.rm=T))
  
  
  # - Compile results
  datResults_prep <- rbind(data.table(a=vCostMultiples[i], Type="DtH-Basic", Threshold=thresh_dth_bas$cutoff, 
                                      MAE=MAE_eventProb_bas_Youden),
                           data.table(a=vCostMultiples[i], Type="DtH-Advanced", Threshold=thresh_dth_adv$cutoff, 
                                      MAE=MAE_eventProb_adv_Youden),
                           data.table(a=vCostMultiples[i], Type="LR", Threshold=thresh_lr_classic$cutoff, 
                                      MAE=MAE_eventProb_classic_Youden))
  
  # - Save/append results
  if (i==1) datResults <- datResults_prep else datResults <- rbind(datResults, datResults_prep)
  cat(paste0("Iteration ", i, " of ",length(vCostMultiples), " done.\n"))
}
proc.time() - ptm

# - Save optimisation results to disk
pack.ffdf(paste0(genObjPath,"CostMultipleResults"), datResults)




# ------ 5. General analysis of results

# quick plots: MAE over a
plot(x=datResults[Type=="DtH-Basic",a], y=datResults[Type=="DtH-Basic",MAE],
     xlab="Cost multiple a", ylab="MAE",col="red", type="b")
plot(x=datResults[Type=="DtH-Advanced",a], y=datResults[Type=="DtH-Advanced",MAE],
     xlab="Cost multiple a", ylab="MAE",col="blue", type="b")
plot(x=datResults[Type=="LR",a], y=datResults[Type=="LR",MAE],
     xlab="Cost multiple a", ylab="MAE",col="orange", type="b")
plot(x=datResults[Type=="LR" & a < 21,a], y=datResults[Type=="LR" & a < 21,MAE],
     xlab="Cost multiple a", ylab="MAE",col="orange", type="b")

# Distributional analyses: event rates
describe(datCredit$EventRate_bas); hist(datCredit$EventRate_bas, breaks="FD")
### RESULTS: Bi-modal right-skewed distribution
describe(datCredit$EventRate_adv); hist(datCredit$EventRate_adv, breaks="FD")
### RESULTS: Right-skewed distribution with extreme outliers
describe(datCredit$EventRate_classic); hist(datCredit$EventRate_classic, breaks="FD")
### RESULTS: Right-skewed distribution with extreme outliers

# quick plots: Threshold over a
plot(x=datResults[Type=="DtH-Basic",a], y=datResults[Type=="DtH-Basic",Threshold],
     xlab="Cost multiple a", ylab="Threshold c",col="red", type="b")
plot(x=datResults[Type=="DtH-Advanced",a], y=datResults[Type=="DtH-Advanced",Threshold],
     xlab="Cost multiple a", ylab="Threshold c",col="blue", type="b")
plot(x=datResults[Type=="LR",a], y=datResults[Type=="LR",Threshold],
     xlab="Cost multiple a", ylab="Threshold c",col="orange", type="b")

# --- Investigate specific thresholds' impact on the expected term-structure
# just for inspection purposes
datTest <- datResults[Type=="DtH-Basic",list(Type, a, Threshold, MAE)]
datTest <- datResults[Type=="DtH-Advanced",list(Type, a, Threshold, MAE)]
datTest <- datResults[Type=="LR",list(Type, a, Threshold, MAE)]

# -- Assign cut-offs based on optimisation results
cutoff_dth_bas <- datResults[Type=="DtH-Basic" & a==40, Threshold]
#cutoff_dth_bas <- datResults[Type=="DtH-Basic" & a==82, Threshold]
cutoff_dtH_adv <- datResults[Type=="DtH-Advanced" & a==1, Threshold]
cutoff_dtH_adv <- 0.3894059 # found previously at a=1 with many more replications
cutoff_LR <- datResults[Type=="LR" & a==80, Threshold]

# -- Dichotomise probabilistic models
datCredit[, Youden_bas := ifelse(EventRate_bas > cutoff_dth_bas,1,0)]
datCredit[, Youden_adv := ifelse(EventRate_adv > cutoff_dtH_adv,1,0)]
datCredit[, Youden_classic := ifelse(EventRate_classic > cutoff_LR,1,0)]

# -- Aggregate event rates and calculate MAEs
# - Aggregate event rates to period-level
datSurv_exp <- datCredit[,.(
  EventRate_bas_Youden = mean(Youden_bas, na.rm=T),
  EventRate_adv_Youden = mean(Youden_adv, na.rm=T),
  EventRate_classic_Youden = mean(Youden_classic, na.rm=T)),
  by=list(TimeInDefSpell)]

# - Calculate MAE between event rates
# Basic model
(MAE_eventProb_bas_Youden <- mean(abs(datSurv_act[Time<=sMaxSpellAge, EventRate] - 
                                        datSurv_exp[TimeInDefSpell<=sMaxSpellAge, EventRate_bas_Youden]), na.rm=T))
# Advanced model
(MAE_eventProb_adv_Youden <- mean(abs(datSurv_act[Time<=sMaxSpellAge, EventRate] - 
                                        datSurv_exp[TimeInDefSpell<=sMaxSpellAge, EventRate_adv_Youden]), na.rm=T))
# Classical model
(MAE_eventProb_classic_Youden <- mean(abs(datSurv_act[Time<=sMaxSpellAge, EventRate] - 
                                            datSurv_exp[TimeInDefSpell<=sMaxSpellAge, EventRate_classic_Youden]), na.rm=T))


# - Plotting event rates for actuals and all expecteds
plot(datSurv_act[Time<=120, EventRate], type="b")
lines(datSurv_exp[TimeInDefSpell<=120, EventRate_bas_Youden], type="b", col="red")
lines(datSurv_exp[TimeInDefSpell<=120, EventRate_adv_Youden], type="b", col="blue")
lines(datSurv_exp[TimeInDefSpell<=120, EventRate_classic_Youden], type="b", col="orange")

# Specific plot for DtH-basic model
plot(datSurv_exp[TimeInDefSpell<=120, EventRate_bas_Youden], type="b", col="red")
lines(datSurv_act[Time<=120, EventRate], type="b")

# Specific plot for classic LR-model
plot(datSurv_exp[TimeInDefSpell<=120, EventRate_classic_Youden], type="b", col="orange")
lines(datSurv_act[Time<=120, EventRate], type="b")

### Conclucsion:
### -- DtH-Basic: The procedure yielded the lowest MAE across a \in [0,39]], though the resulting term-structure
### is a flat zero-valued construct, which is less desirable from a risk prudence point of view. At greater a-values,
### we took the a-value that produced the second lowest MAE, though which produced a much more credible term-structure
### at a=40: Threshold=0.01378371
### -- DtH-Advanced: Procedure succeeded in identifying the optimal cost multiple that achieved the lowest MAE
### at a=1: Threshold=0.3894059
### -- LR: The procedure's output (a=0.1) produced a flat zero-valued term-structure, which did achieve the low MAE,
### however is not desirable from a risk prudence point of view. At greater a-values, there was a local 
### minimum observed, which produced a more credible term-structure, albeit at the cost of a slightly higher MAE,
### at a=80: Threshold=0.158223



# ------ 6. Production-grade graph of results
# TBD

# ------ 7. Imposing limits on optimisation process's search space

# -- Run subroutine for the Generalised Youden Index given a specific cost multiple (a)
# - Basic DtH-model
thresh_dth_bas <- GenYoudenIndex(optimise_type="Pre-determined", Train_DataSet=datCredit[Sample=="Train",],
                                 Target="DefSpell_Event",prob_vals_given="EventRate_bas", 
                                 a=1, replicate=150, numThreads=8, limits=c(0,0.02))

# -- Dichotomise probabilistic models
datCredit[, Youden_bas := ifelse(EventRate_bas > thresh_dth_bas$cutoff,1,0)]

# -- Aggregate event rates and calculate MAEs
# - Aggregate event rates to period-level
datSurv_exp <- datCredit[,.(EventRate_bas_Youden = mean(Youden_bas, na.rm=T)),
                         by=list(TimeInDefSpell)]

# - Calculate MAE between event rates
# Basic model
(MAE_eventProb_bas_Youden <- mean(abs(datSurv_act[Time<=sMaxSpellAge, EventRate] - 
                                        datSurv_exp[TimeInDefSpell<=sMaxSpellAge, EventRate_bas_Youden]), na.rm=T))

# - Plotting event rates for actuals and all expecteds
plot(datSurv_act[Time<=120, EventRate], type="b")
lines(datSurv_exp[TimeInDefSpell<=120, EventRate_adv_Youden], type="b", col="red")
