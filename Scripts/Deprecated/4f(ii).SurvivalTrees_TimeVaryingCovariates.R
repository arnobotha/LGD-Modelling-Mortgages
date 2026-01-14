# ================================= SURVIVAL TREES ======================================
# Explore the use of survival trees with time-varying covariates using the LRTCtrees-pacakge
# with left-truncated and right-censored data (LTRC)
# Paper: https://academic.oup.com/biostatistics/article-abstract/18/2/352/2739324?redirectedFrom=fulltext
# Vignette: https://cran.r-project.org/web/packages/LTRCtrees/vignettes/LTRCtrees.html
# ---------------------------------------------------------------------------------------
# PROJECT TITLE: Loss Modelling (LGD) for Residential Mortgages
# SCRIPT AUTHOR(S): Dr Arno Botha (AB)
# ---------------------------------------------------------------------------------------
# -- Script dependencies:
#   - 0.Setup.R
#   - 2f.Data_Fusion1.R

# -- Inputs:
#   - datCredit_train_CDH | Prepared from script 2g
#   - datCredit_valid_CDH | Prepared from script 2g
#
# -- Outputs:
#   - <Insight>
# =======================================================================================



# ------ 1. Preliminaries

# --- Load package(s)
require(LTRCtrees) # uses rpart and party


# --- Load data
# - Confirm prepared datasets are loaded into memory
# Training set
if (!exists('datCredit_train_CDH')) unpack.ffdf(paste0(genPath,"creditdata_train_CDH"), tempPath)
# Validation set
if (!exists('datCredit_valid_CDH')) unpack.ffdf(paste0(genPath,"creditdata_valid_CDH"), tempPath)

# - Create copies of the training and validation data
datCredit_train <- copy(datCredit_train_CDH); datCredit_valid <- copy(datCredit_valid_CDH)
rm(datCredit_train_CDH, datCredit_valid_CDH); gc()


# --- Create a super-subsample to facilitate rapid model development
# - Parameters
smp_frac <- 2/3 # Set sub-sample size (% of full training set)
maxAge <- 240 # Set maximum spell age in filtering out outliers


# - Get unique account numbers (only select accounts that had a default event)
datKeys_train <- data.table(LoanID=unique(datCredit_train[!is.na(DefSpell_Key),LoanID]))
datKeys_valid <- data.table(LoanID=unique(datCredit_valid[!is.na(DefSpell_Key),LoanID]))

# - Subsample unique keys
set.seed(1234, kind="Mersenne-Twister")
datKeys_train_smp <- datKeys_train %>% slice_sample(prop=smp_frac) %>% as.data.table()
datKeys_valid_smp <- datKeys_valid %>% slice_sample(prop=smp_frac) %>% as.data.table()

# - Subset training data
datCredit_train_smp <- subset(datCredit_train, LoanID %in% datKeys_train_smp$LoanID)
datCredit_valid_smp <- subset(datCredit_valid, LoanID %in% datKeys_valid_smp$LoanID)

# - Subset only default spells
datCredit_train_smp <- subset(datCredit_train_smp, !is.na(DefSpell_Key))
datCredit_valid_smp <- subset(datCredit_valid_smp, !is.na(DefSpell_Key))


# --- Final data preperations
# - Add an event indicator
datCredit_train_smp[, DefSpell_Event:=ifelse(DefSpellResol_Type_Hist=="WOFF",1,0)]
datCredit_valid_smp[, DefSpell_Event:=ifelse(DefSpellResol_Type_Hist=="WOFF",1,0)]

# - Transform categorical variables to factor
# Payment method
datCredit_train_smp[,pmnt_method_grp_fac:=as.factor(pmnt_method_grp)]
datCredit_valid_smp[,pmnt_method_grp_fac:=as.factor(pmnt_method_grp)]
# Previous defaults
datCredit_train_smp[,PrevDefaults_fac:=as.factor(PrevDefaults)]
datCredit_valid_smp[,PrevDefaults_fac:=as.factor(PrevDefaults)]

# - Subset time in spell to preset max
if (!is.na(maxAge)){
  datCredit_train_smp <- subset(datCredit_train_smp, DefSpell_Age <= maxAge)
  datCredit_valid_smp <- subset(datCredit_valid_smp, DefSpell_Age <= maxAge)
}




# ------ 2 Survival tree-fitting procedure
# - Fit a conditional inference tree (partykit) based on log-rank tests. 
# Terminal nodes contain Kaplan-Meier survival curves
start_time <- proc.time()
SurvTree_LRTCIT  <- LTRCIT(Surv(time=TimeInDefSpell-1, time2=TimeInDefSpell, event=WOff_Ind==1) ~
                             # PrevDefaults_fac + DefSpell_Num_binned +
                             # ArrearsToBalance_1_Aggr_Prop + Balance_Real_1 +
                             # pmnt_method_grp_fac + InterestRate_Margin_Aggr_Med_2 + InterestRate_Nom +
                             # DefaultStatus1_Aggr_Prop_Lag_12 + g0_Delinq_Ave + M_DTI_Growth_6 +
                             # Principal_Real + M_RealGDP_Growth_12 + g0_Delinq_Num +
                             # M_Repo_Rate_2 + M_Inflation_Growth_3,
                             PrevDefaults_fac + DefSpell_Num_binned + Balance_Real_1 + Principal_Real + 
                             ArrearsToBalance_1_Aggr_Prop + g0_Delinq_Num +  +
                             pmnt_method_grp_fac + InterestRate_Margin_Aggr_Med_3 + InterestRate_Nom +
                             DefaultStatus1_Aggr_Prop_Lag_12 + g0_Delinq_Ave + g0_Delinq_Lag_1 + 
                             M_DTI_Growth_12 + M_RealIncome_Growth_9 + M_Repo_Rate_12 + M_Inflation_Growth_12,
                           data=datCredit_train_smp,
                           Control=partykit::ctree_control(mincriterion=0.99, # 1 - p-value threshold (default = 5% significance)
                                                 minsplit=1000, # minimum number of observations to attempt a split
                                                 minbucket=50, # minimum number in terminal node
                                                 maxdepth=5,
                                                 testtype = "Bonferroni")) # default – strongest multiple testing correction))
end_time <- proc.time()
(delta_time <- end_time - start_time) # 2351.00 seconds.

# - Visualise tree
plot(SurvTree_LRTCIT ) # Kaplan-Meier curves for the observations within each node




# ------ 3 Obtain survival probability predictions

# - Point to main credit data
datCredit <- rbind(data.table(datCredit_train_smp, Sample="Training"),
                   data.table(datCredit_valid_smp, Sample="Validation"))

# - Handle left-truncated spells by adding a starting record 
### NOTE:  This is necessary for calculating certain survival quantities later
# Create an additional record for each default spell
datAdd <- subset(datCredit, Counter==1 & TimeInDefSpell>1)
datAdd[, TimeInDefSpell:=TimeInDefSpell-1]
datAdd[, Counter:=0]
# Add record to main dataset
datCredit <- rbind(datCredit, datAdd); setorder(datCredit, DefSpell_Key, TimeInDefSpell)

# Predict survival curves (list of survfit objects): equal to number of rows in sample
survFits <- predict(SurvTree_LRTCIT , newdata = datCredit, type = "prob")



# ------ 4 Calculate survival-related quantities (hazard) from survival probability predictions

# Function for extracting survival probabilities within a mapply-setup (later)
extractSurv <- function(survFit, t, extrapolate="last", floor=NULL) {
  if (length(survFit$time) == 0) return(1)  # Edge case: No events in terminal node → everyone survives
  
  # retrieve survival probability at given t
  survFit_t <- summary(survFit, times = t)$surv
  
  # Most common & recommended logic gate; simply return retrieved result
  if (length(survFit_t) > 0) {
    if (!is.na(survFit_t)) return(survFit_t)
  }
  
  # Remaining cases need treatment. Extrapolation is now necessary since t > last observed time in survFit object
  last_surv <- tail(survFit$surv, 1) # obtain last survival probability
  
  if (extrapolate == "last") {
    result <- last_surv
  } else if (extrapolate == "zero") {
    result <- 0
  } else if (extrapolate == "floor" && !is.null(floor)) {
    result <- last_surv
  } else result <- last_surv  # fallback
  
  # Optional safety floor (common in credit scoring)
  if (!is.null(floor) && is.numeric(floor)) result <- max(result, floor)
  
  return(result)
  
  #if (length(survFit_t$surv) == 0) return(0)  # Edge case: no time found
  #if (is.na(survFit_t$surv)) return(survFit_t$surv[length(survFit_t$surv)])  # If t > max time, use last surv
  #return(survFit_t$surv)
}

# Obtain survival probabilities by feeding the list of KM-objects (survFits) together with [TimeInDefSpell]-vector
# into the extractSurv()-function across all rows
start_time <- proc.time()
datCredit[, SurvProb := mapply(function(p, times) {
  extractSurv(p, times)}, p = survFits, times = TimeInDefSpell)]
proc.time() - start_time # 482.27 seconds.


# - Calculate hazard rate
datCredit[, hazard := (data.table::shift(SurvProb,fill=1)-SurvProb)/
                           data.table::shift(SurvProb,fill=1), by=list(DefSpell_Key)]
datCredit[is.infinite(hazard) & hazard < 0, hazard := 0] # fail-safe

# Diagnostic plot for single loan
#datCredit_test <- subset(datCredit, LoanID == unique(datCredit[DefSpell_Age > 10, LoanID])[1])
#plot(datCredit_test$SurvProb); plot(datCredit_test$hazard)

# - Derive discrete density, or event probability f(t) = S(t-1) - S(t)
datCredit[, EventRate_SurvTree := data.table::shift(SurvProb, type="lag", n=1, fill=1) - SurvProb, by=list(DefSpell_Key)]

# - Remove added rows
datCredit <- subset(datCredit, Counter>0)



# ------ 5 Evaluate fitted model

# --- Classical ROC-analysis
# - Evaluate using validation data only
objROC <- roc(response=datCredit[Sample=="Validation", DefSpell_Event], 
              predictor=datCredit[Sample=="Validation", hazard])
objROC$auc; plot(objROC)
### RESULTS: AUC over various hyper-parametrisations
# -- Varying subsample size and maximum tree depth
# Max Tree-depth 2 (100% training subsample),   minsplit=1000 : AUC = 0.5493
# Max Tree-depth 4 (50% training subsample),    minsplit=1000 : AUC = 0.6541
# Max Tree-depth 4 (75% training subsample),    minsplit=1000 : AUC = 0.5322
# Max Tree-depth 5 (25% training subsample),    minsplit=1000 : AUC = 0.5279
# Max Tree-depth 5 (50% training subsample),    minsplit=1000 : AUC = 0.6520
# Max Tree-depth 5 (75% training subsample),    minsplit=1000 : AUC = 0.5301
# Max Tree-depth 6 (50% training subsample),    minsplit=1000 : AUC = 0.6361
# Max Tree-depth 6 (75% training subsample),    minsplit=1000 : AUC = 0.5425
# Max Tree-depth 7 (50% training subsample),    minsplit=1000 : AUC = 0.6080
# Max Tree-depth 7 (75% training subsample),    minsplit=1000 : AUC = 0.5435
# Max Tree-depth 10 (50% training subsample),   minsplit=1000 : AUC = 0.5298
# Max Tree-depth 10 (75% training subsample),   minsplit=1000 : AUC = 0.5330
# Max Tree-depth 10 (100% training subsample),  minsplit=1000 : AUC = 0.5376
# -- Varying subsample size & minbucket at a given max tree depth of 6
# Max Tree-depth 6 (75% training subsample),    minbucket=50 : AUC = 0.5425
# Max Tree-depth 6 (75% training subsample),    minbucket=250: AUC = 0.5425
# Max Tree-depth 6 (75% training subsample),    minbucket=500: AUC = 0.5424
# -- Restricting time in default to preset max (240), and varying by tree depth and subsample size
# Max Tree-depth 4 (50% training subsample),    minsplit=1000 : AUC = 0.6573
# Max Tree-depth 4 (75% training subsample),    minsplit=1000 : AUC = 0.5535
# Max Tree-depth 5 (50% training subsample),    minsplit=1000 : AUC = 0.6550
# Max Tree-depth 5 (75% training subsample),    minsplit=1000 : AUC = 0.5799
# -- Changing input space for a tree by swapping in the input space from the DtH-Advanced model
# NOTE: Time in default still restricted to a preset max (240)
# Max Tree-depth 4 (33% training subsample),    minsplit=1000 : AUC = 0.6572
# Max Tree-depth 4 (50% training subsample),    minsplit=1000 : AUC = 0.6636
# Max Tree-depth 4 (67% training subsample),    minsplit=1000 : AUC = 0.6694
# Max Tree-depth 4 (75% training subsample),    minsplit=1000 : AUC = 0.5556
# Max Tree-depth 5 (50% training subsample),    minsplit=1000 : AUC = 0.6491
# Max Tree-depth 5 (67% training subsample),    minsplit=1000 : AUC = 0.6663
# Max Tree-depth 5 (75% training subsample),    minsplit=1000 : AUC = 0.5538


### CONCLUSIONS:
# Greater subsampling fractions seem to increase AUC at a given parameter-set, which is sensible given that the 
# tree has more observations from which to learn. This is also in line with Fu2018's findings regarding the 
# interaction of censoring and sample size. Furthermore, varying minbucket did not seem to affect the AUC at all.
# Upon inspecting many of the resulting tree-structures via plot(), it seems the KM-curves are afflicted by a 
# lot of outliers, which may affect the overall tree fit.


# --- Constructing the empirical term-structure of write-off using a Kaplan-Meier estimator

# - Estimate survival rate of the main event S(t) = P(T >=t) for time-to-event variable T
# Compute Kaplan-Meier survival estimates (product-limit) for main-event | Spell-level with right-censoring & left-truncation
km_Default <- survfit(Surv(time=TimeInDefSpell-1, time2=TimeInDefSpell, event=DefSpell_Event==1, type="counting") ~ 1, 
                      id=DefSpell_Key, data=rbind(datCredit_train, datCredit_valid))

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

# -  Aggregate event rates to period-level
datSurv_exp <- datCredit[,.(EventRate_SurvTree = mean(EventRate_SurvTree, na.rm=T)),
  by=list(TimeInDefSpell)]


# --- Compare empirical vs expected term-structures, having aggregated the event probabilities from the survival tree
plot(datSurv_act[Time<=120, EventRate], type="b")
lines(datSurv_exp[TimeInDefSpell<=120, EventRate_SurvTree], type="b", col="red")
plot(datSurv_exp[TimeInDefSpell<=120, EventRate_SurvTree], type="b", col="red")
mean(abs(datSurv_act[Time<=120, EventRate] - datSurv_exp[TimeInDefSpell<=120, EventRate_SurvTree])) * 100
### RESULTS: Max Tree-depth 5 (67% training subsample): MAE = 1.6284%
