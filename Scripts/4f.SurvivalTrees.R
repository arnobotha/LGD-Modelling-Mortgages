# ================================= SURVIVAL TREES ======================================
# Fitting and testing survival trees in predicting the occurrence of write-off in defaulted
# loans at each time point during a default spell
# ---------------------------------------------------------------------------------------
# PROJECT TITLE: Loss Modelling (LGD) for Residential Mortgages
# SCRIPT AUTHOR(S): Marcel Muller (MM)
#
# DESCRIPTION:
# This script performs fits survival trees and has the following logic
#   1) 
# ---------------------------------------------------------------------------------------
# -- Script dependencies:
#   - 0.Setup.R
#   - 2f.Data_Fusion1.R

# -- Inputs:
#   - datCredit_train_CDH | Prepared from script 2g
#   - datCredit_valid_CDH | Prepared from script 2g
#
# -- Outputs:
#   - ... | ...
# ---------------------------------------------------------------------------------------
# =======================================================================================


# ------ 1. Preliminaries

# --- 1.1 Load data
# - Confirm prepared datasets are loaded into memory
# Training set
if (!exists('datCredit_train_CDH')) unpack.ffdf(paste0(genPath,"creditdata_train_CDH"), tempPath)
# Validation set
if (!exists('datCredit_valid_CDH')) unpack.ffdf(paste0(genPath,"creditdata_valid_CDH"), tempPath)

# - Create copies of the training and validation data
datCredit_train <- copy(datCredit_train_CDH); datCredit_valid <- copy(datCredit_valid_CDH)
rm(datCredit_train_CDH, datCredit_valid_CDH); gc()


# --- 1.2 Create a super-subsample to facilitate rapid model development
# - Set sub-sample size (% of full training set)
smp_frac <- 1

# - Get unique account numbers (only select accounts that had a default event)
datKeys_train <- data.table(unique(datCredit_train[!is.na(DefSpell_Key),LoanID]))
datKeys_valid <- data.table(unique(datCredit_valid[!is.na(DefSpell_Key),LoanID]))

# - Subsample unique keys
datKeys_train_smp <- datKeys_train %>% slice_sample(prop=smp_frac) %>% as.data.table()
datKeys_valid_smp <- datKeys_valid %>% slice_sample(prop=smp_frac) %>% as.data.table()

# - Subset training data
datCredit_train_smp <- subset(datCredit_train, LoanID %in% datKeys_train_smp$V1)
datCredit_valid_smp <- subset(datCredit_valid, LoanID %in% datKeys_valid_smp$V1)

# - Subset only default spells
datCredit_train_smp <- subset(datCredit_train_smp, !is.na(DefSpell_Key))
datCredit_valid_smp <- subset(datCredit_valid_smp, !is.na(DefSpell_Key))


# --- 1.3 Final data preparations
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

# - Create a cross-sectional dataset
datCredit_train_smp_cross <- subset(datCredit_train_smp, DefSpell_Counter==1)
datCredit_valid_smp_cross <- subset(datCredit_valid_smp, DefSpell_Counter==1)
### MM: Impose a limit of default spell age of 240


# --- 1.4 Additional parameters
# - Maximum tree-depth
max_depth <- 4

# - Maximum default spell age to model on (NA for no restriction)
max_DefSpell_Age <- NA

# - Filter datasets accordingly
if (!is.na(max_DefSpell_Age)){
  datCredit_train_smp <- subset(datCredit_train_smp, DefSpell_Age<=max_DefSpell_Age)
  datCredit_valid_smp <- subset(datCredit_valid_smp, DefSpell_Age<=max_DefSpell_Age)                              
  
  datCredit_train_smp_cross <- subset(datCredit_train_smp_cross, DefSpell_Age<=max_DefSpell_Age)
  datCredit_valid_smp_cross <- subset(datCredit_valid_smp_cross, DefSpell_Age<=max_DefSpell_Age)                              
}




# ------ 2. Fitting a survival tree and rendering predicitons

# --- 2.1 Fit tree
# - Curated input space
# PrevDefaults_fac+InterestRate_Nom+BalanceToPrincipal_1+pmnt_method_grp_fac+M_Repo_Rate_6+M_DTI_Growth_6

# - Advanced input space
# PrevDefaults_fac + DefSpell_Num_binned +
#   ArrearsToBalance_1_Aggr_Prop + Balance_Real_1 +
#   pmnt_method_grp_fac + InterestRate_Margin_Aggr_Med_2 + InterestRate_Nom +
#   DefaultStatus1_Aggr_Prop_Lag_12 + g0_Delinq_Ave + M_DTI_Growth_6 +
#   Principal_Real + M_RealGDP_Growth_12 + g0_Delinq_Num +
#   M_Repo_Rate_2 + M_Inflation_Growth_3

# - Fit tree
start_time <- proc.time()
SurvTree_PartyKit <- ctree(Surv(DefSpell_Age,DefSpell_Event)~
                             PrevDefaults_fac + DefSpell_Num_binned +
                             ArrearsToBalance_1_Aggr_Prop + Balance_Real_1 +
                             pmnt_method_grp_fac + InterestRate_Margin_Aggr_Med_2 + InterestRate_Nom +
                             DefaultStatus1_Aggr_Prop_Lag_12 + g0_Delinq_Ave + M_DTI_Growth_6 +
                             Principal_Real + M_RealGDP_Growth_12 + g0_Delinq_Num +
                             M_Repo_Rate_2 + M_Inflation_Growth_3,
                           data=datCredit_train_smp_cross,
                           control=ctree_control(mincriterion=0.99, # 1 - p-value threshold (default ≈ 5% significance)
                                                 minsplit=100, # minimum number of observations to attempt a split
                                                 minbucket=50, # minimum number in terminal node (common default)
                                                 testtype="Bonferroni", # most conservative → classical feeling
                                                 maxdepth=max_depth))
proc.time() - start_time
### Runtime = 1 sec

# - Visualise tree
plot(SurvTree_PartyKit)


# --- 2.2 Generate survival quantity predictions given the fitted tree
# - Point to main credit data
datCredit <- rbind(data.table(datCredit_train_smp, Sample="Training"),
                   data.table(datCredit_valid_smp, Sample="Validation"))

# - Get node from tree
datCredit[,Node:=predict(SurvTree_PartyKit, datCredit, type="node")]
datCredit_train_smp_cross[,Node:=predict(SurvTree_PartyKit, datCredit_train_smp_cross, type="node")]

# - Fit a survival object for each node | Using the training data only to align with how the tree was fitted
datHaz <- predSurv(survTree=SurvTree_PartyKit, datGiven=datCredit_train_smp_cross,
                   fld_DefSpell_Age="DefSpell_Age", fld_DefSpell_Event="DefSpell_Event",
                   max_DefSpell_Age=max_DefSpell_Age)

# - Join hazards back to main dataset
datCredit <- merge(datCredit,
                   datHaz,
                   by.x=c("Node","TimeInDefSpell"), by.y=c("Node","Time"),
                   all.x=T)

# - [SANITY CHECK] Inspect reasonableness of survival probabilities
ggplot(datHaz[Time<=120], aes(x=Time, y=Surv, group=Node)) + geom_line(aes(col=Node))
### RESULTS: Hazards are crossing in a few instances, but there seems to be good differentiation
check1.1 <- sum(is.nan(datCredit$Surv))/datCredit[,.N]
if (check1.1==0) {cat("SAFE: No undefined survival probabilities present.\n")} else {cat("WARNING:", percent(check1.1), "% of survival probabilities are undefined.\n")}
### RESULTS: Safe
check1.2 <- sum(is.na(datCredit$Surv))/datCredit[,.N]
if (check1.2==0) {cat("SAFE: No missing survival probabilities present.\n")} else {cat("WARNING:", round(check1.2*100,4), "% of survival probabilities are missing\n")}
### RESULTS: Safe

# - [SANITY CHECK] Inspect reasonableness of hazards
ggplot(datHaz[Time<=120], aes(x=Time, y=Hazard, group=Node)) + geom_line(aes(col=Node))
### RESULTS: Hazards are crossing in a few instances, but there seems to be good differentiation
check2.1 <- sum(is.nan(datCredit$Hazard))/datCredit[,.N]
if (check2.1==0) {cat("SAFE: No undefined hazards present.\n")} else {cat("WARNING:", percent(check2.1), "% of hazards are undefined.\n")}
### RESULTS: Safe
check2.2 <- sum(is.na(datCredit$Hazard))/datCredit[,.N]
if (check2.2==0) {cat("SAFE: No missing hazards present.\n")} else {cat("WARNING:", round(check2.2*100,4), "% of hazards are missing\n")}
### RESULTS: Safe

# - [SANITY CHECK] Inspect reasonableness of event rates
ggplot(datHaz[Time<=120], aes(x=Time, y=EventRate, group=Node)) + geom_line(aes(col=Node))
### RESULTS: Event rates are crossing in a few instances, but there seems to be good differentiation
check3.1 <- sum(is.nan(datCredit$EventRate))/datCredit[,.N]
if (check3.1==0) {cat("SAFE: No undefined event rates present.\n")} else {cat("WARNING:", round(check3.1*100,4), "% of event rates are undefined.\n")}
### RESULTS: Safe
check3.2 <- sum(is.na(datCredit$Hazard))/datCredit[,.N]
if (check3.2==0) {cat("SAFE: No missing event rates present.\n")} else {cat("WARNING:", round(check3.2*100,4), "% of event rates are missing\n")}
### RESULTS: Safe


# ------ 4. Evaluate tree accuracy

# --- 4.1 ROC analysis
# - Perform time-dependent ROC analysis
predictTime <- 44
ptm <- proc.time() #IGNORE: for computation time calculation
objROC44_ST <- tROC.multi(datGiven=datCredit[Sample=="Validation"], modGiven=NA, month_End=predictTime, sLambda=0.05, estMethod="NN-0/1", numDigits=4, 
                          fld_ID="DefSpell_Key", fld_Event="WOff_Ind", eventVal=1, fld_StartTime="Start", fld_EndTime="TimeInDefSpell",
                          caseStudyName=paste0("Cond_SurvTree_", predictTime), numThreads=12, logPath=genPath, 
                          predType="response", MarkerGiven="Hazard", Graph=T)
proc.time() - ptm
objROC44_ST$AUC ;objROC44_ST$ROC_graph

# -- Sampling fraction=0.25; No filtering on DefaultSpell_Age; Curated input space used; various maximum node depths
### RESULTS: AUC up to t: 82.71511%, achieved in 35 secs (maximum node depth=1)
### RESULTS: AUC up to t: 78.02695%, achieved in 45 secs (maximum node depth=2)
### RESULTS: AUC up to t: 77.66664%, achieved in 60 secs (maximum node depth=3)
### RESULTS: AUC up to t: 66.32666%, achieved in 65 secs (maximum node depth=4)
### RESULTS: AUC up to t: 74.58078%, achieved in 84 secs (maximum node depth=5)
### RESULTS: AUC up to t: 67.94278%, achieved in 100 secs (maximum node depth=6)
### RESULTS: AUC up to t: 69.11246%, achieved in 98 secs (maximum node depth=7)
### RESULTS: AUC up to t: 71.10731%, achieved in 98 secs (maximum node depth=10)
### CONCLUSION: Candidates are maximum node depths of 1, 2, and 3

# -- Sampling fraction=0.50; No filtering on DefaultSpell_Age; Curated input space used; various maximum node depths
### RESULTS: AUC up to t: 86.03937%, achieved in 82 secs (maximum node depth=1)
### RESULTS: AUC up to t: 74.99818%, achieved in 35 secs (maximum node depth=2)
### RESULTS: AUC up to t: 80.28313%, achieved in 147 secs (maximum node depth=3)
### RESULTS: AUC up to t: 71.49869%, achieved in 212 secs (maximum node depth=4)
### RESULTS: AUC up to t: 69.71464%, achieved in 255 secs (maximum node depth=5)
### RESULTS: AUC up to t: 66.5606%, achieved in 345 secs (maximum node depth=10)
### CONCLUSION: Candidates are maximum node depths of 1, 3, and 2

# -- Sampling fraction=0.70; No filtering on DefaultSpell_Age; Curated input space used; various maximum node depths
### RESULTS: AUC up to t: 85.82466%, achieved in 129 secs (maximum node depth=1)
### RESULTS: AUC up to t: 77.04039%, achieved in 176 secs (maximum node depth=2)
### RESULTS: AUC up to t: 79.35357%, achieved in 243 secs (maximum node depth=3)
### RESULTS: AUC up to t: 72.53476%, achieved in 318 secs (maximum node depth=4)
### RESULTS: AUC up to t: 71.05812%, achieved in 451 secs (maximum node depth=5)
### RESULTS: AUC up to t: 70.80433%, achieved in 605 secs (maximum node depth=10)
### CONCLUSION: Candidates are maximum node depths of 1, 3, and 2

# -- Sampling fraction=1; No filtering on DefaultSpell_Age; Curated input space used; various maximum node depths
### RESULTS: AUC up to t: 86.81077%, achieved in 202 secs (maximum node depth=1)
### RESULTS: AUC up to t: 78.87941%, achieved in 272 secs (maximum node depth=2)
### RESULTS: AUC up to t: 79.16705%, achieved in 381 secs (maximum node depth=3)
### RESULTS: AUC up to t: 73.05305%, achieved in 540 secs (maximum node depth=4)
### RESULTS: AUC up to t: 69.39938%, achieved in 746 secs (maximum node depth=5)
### RESULTS: AUC up to t: 64.36706%, achieved in 1187 secs (maximum node depth=10)
### CONCLUSION: Candidates are maximum node depths of 1, 3, and 2

# -- Sampling fraction=1; various filters on DefaultSpell_Age; Curated input space used; maximum node depth=3
### RESULTS: AUC up to t: 79.16705%, achieved in 381 secs (No filter on DefSpell_Age)
### RESULTS: AUC up to t: 79.16481%, achieved in 394 secs (DefSpell_Age<=420)
### RESULTS: AUC up to t: 79.00474%, achieved in 381 secs (DefSpell_Age<=360)
### RESULTS: AUC up to t: 79.46574%, achieved in 376 secs (DefSpell_Age<=240)
### RESULTS: AUC up to t: 77.57769%, achieved in 351 secs (DefSpell_Age<=180)
### RESULTS: AUC up to t: 71.12594%, achieved in 381 secs (DefSpell_Age<=120)
### RESULTS: AUC up to t: 69.94239%, achieved in 260 secs (DefSpell_Age<=100)
### RESULTS: AUC up to t: 72.16271%, achieved in 231 secs (DefSpell_Age<=80)
### CONCLUSION: Candidates are maximum node depths of 1, 2, and 3

# -- Sampling fraction=1; no filter on DefaultSpell_Age; Advanced DtH input space used; maximum node depth=3
### RESULTS: AUC up to t: 86.81077%, achieved in 202 secs (maximum node depth=1)
### RESULTS: AUC up to t: 78.87941%, achieved in 274 secs (maximum node depth=2)
### RESULTS: AUC up to t: 75.55762%, achieved in 327 secs (maximum node depth=3)
### RESULTS: AUC up to t: 76.03082%, achieved in 448 secs (maximum node depth=4)
### RESULTS: AUC up to t: 74.89666%, achieved in 622 secs (maximum node depth=5)
### RESULTS: AUC up to t: 72.71412%, achieved in 837 secs (maximum node depth=6)
### RESULTS: AUC up to t: 71.67736%, achieved in 1072 secs (maximum node depth=7)
### RESULTS: AUC up to t: 71.28697%, achieved in 1122 secs (maximum node depth=10)
### CONCLUSION: Candidates are maximum node depths of 1, 2, and 4


# --- 4.2 Constructing the empirical term-structure of write-off using a Kaplan-Meier estimator
# - Compute Kaplan-Meier survival estimates (product-limit) for main-event | Spell-level with right-censoring & left-truncation
km_Default <- survfit(Surv(time=TimeInDefSpell-1, time2=TimeInDefSpell, event=WOff_Ind==1, type="counting") ~ 1, 
                      id=DefSpell_Key, data=datCredit)

# - Create survival table
datSurv_act <- surv_summary(km_Default)

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


# --- 4.3 Estimate event rates | Expected
# - Aggregate account-specific event rates
datSurv_exp <- datCredit[,.(EventRate=mean(EventRate, na.rm=T)),by=list(TimeInDefSpell)]
datSurv_exp <- datSurv_exp[order(TimeInDefSpell)]


# --- 4.4 Compare event rates
plot(x=datSurv_exp[TimeInDefSpell<=120, TimeInDefSpell], y=datSurv_exp[TimeInDefSpell<=120, EventRate], type="l", col="green")
lines(datSurv_act[Time<=120, EventRate], type="l", col="red")
mean(abs(datSurv_exp[TimeInDefSpell<=120,EventRate] - datSurv_act[Time<=120,EventRate]), na.rm=T)

# -- Sampling fraction=0.25; No filtering on DefaultSpell_Age; Curated input space used; various maximum node depths
### RESULTS: MAE=0.002305036 (maximum tree-depth=1)
### RESULTS: MAE=0.002220901 (maximum tree-depth=2)
### RESULTS: MAE=0.002495713 (maximum tree-depth=3)
### RESULTS: MAE=0.002290141 (maximum tree-depth=4)
### RESULTS: MAE=0.002209491 (maximum tree-depth=5)
### RESULTS: MAE=0.002305725 (maximum tree-depth=6)
### RESULTS: MAE=0.002485983 (maximum tree-depth=7)
### RESULTS: MAE=0.001986202 (maximum tree-depth=10)
### CONCLUSION: Candidates are maximum node depths of 10, 2, and 5

# -- Sampling fraction=0.50; No filtering on DefaultSpell_Age; Curated input space used; various maximum node depths
### RESULTS: MAE=0.002267971 (maximum tree-depth=1)
### RESULTS: MAE=0.002057809 (maximum tree-depth=2)
### RESULTS: MAE=0.002081947 (maximum tree-depth=3)
### RESULTS: MAE=0.00181222 (maximum tree-depth=4)
### RESULTS: MAE=0.001976132 (maximum tree-depth=5)
### RESULTS: MAE=0.002155108 (maximum tree-depth=10)
### CONCLUSION: Candidates are maximum node depths of 2, 4, and 5

# -- Sampling fraction=0.70; No filtering on DefaultSpell_Age; Curated input space used; various maximum node depths
### RESULTS: MAE=0.002274059 (maximum tree-depth=1)
### RESULTS: MAE=0.002131272 (maximum tree-depth=2)
### RESULTS: MAE=0.001989732 (maximum tree-depth=3)
### RESULTS: MAE=0.001636337 (maximum tree-depth=4)
### RESULTS: MAE=0.001724127 (maximum tree-depth=5)
### RESULTS: MAE=0.002132982 (maximum tree-depth=10)
### CONCLUSION: Candidates are maximum node depths of 4, 5, and 3

# -- Sampling fraction=1; No filtering on DefaultSpell_Age; Curated input space used; various maximum node depths
### RESULTS: MAE=0.002269329 (maximum tree-depth=1)
### RESULTS: MAE=0.002029914 (maximum tree-depth=2)
### RESULTS: MAE=0.00178538 (maximum tree-depth=3)
### RESULTS: MAE=0.001530532 (maximum tree-depth=4)
### RESULTS: MAE=0.001570304 (maximum tree-depth=5)
### RESULTS: MAE=0.001977896 (maximum tree-depth=10)
### CONCLUSION: Candidates are maximum node depths of 4, 5, and 3

# -- Sampling fraction=1; various filters on DefaultSpell_Age; Curated input space used; maximum node depth=3
### RESULTS: MAE=0.00178538 (No filter on DefSpell_Age)
### RESULTS: MAE=0.001785742 (DefSpell_Age<=420)
### RESULTS: MAE=0.001789197 (DefSpell_Age<=360)
### RESULTS: MAE=0.001810198 (DefSpell_Age<=240)
### RESULTS: MAE=0.001831236 (DefSpell_Age<=180)
### RESULTS: MAE=0.001966916 (DefSpell_Age<=120)
### RESULTS: MAE=0.002067912 (DefSpell_Age<=100)
### RESULTS: MAE=0.001954588 (DefSpell_Age<=80)
### CONCLUSION: Candidates are maximum node depths of 1, 2, and 3

# -- Sampling fraction=1; various filters on DefaultSpell_Age; Advanced DtH input space used; maximum node depth=3
### RESULTS: MAE=0.002269329 (maximum tree-depth=1)
### RESULTS: MAE=0.002029914 (maximum tree-depth=2)
### RESULTS: MAE=0.001850487 (maximum tree-depth=3)
### RESULTS: MAE=0.001665903 (maximum tree-depth=4)
### RESULTS: MAE=0.001968791 (maximum tree-depth=5)
### RESULTS: MAE=0.002084007 (maximum tree-depth=6)
### RESULTS: MAE=0.002249192 (maximum tree-depth=7)
### RESULTS: MAE=0.002272194 (maximum tree-depth=10)
### CONCLUSION: Candidates are maximum node depths of 5, 4, and 3


# --- 4.5 Candidates
# - Best sampling fraction with various maximum node depth
# 1) Sampling fraction=0.25; No filtering on DefaultSpell_Age; Curated input space used; various maximum node depths
#   - Overlap of top three models in terms of AUC and MAE performance: 2 & 10
#   - Maximum depth tree of 2 selected due to simplicity (AUC=78% and MAE=0.2220901%)
# 2) Sampling fraction=0.50; No filtering on DefaultSpell_Age; Curated input space used; various maximum node depths
#   - Overlap of top three models in terms of AUC and MAE performance: 2
#   - Maximum depth tree of 2 selected (AUC=75% and MAE=0.2057809%)
# 3) Sampling fraction=0.75; No filtering on DefaultSpell_Age; Curated input space used; various maximum node depths
#   - Overlap of top three models in terms of AUC and MAE performance: 3
#   - Maximum depth tree of 3 selected (AUC=79% and MAE=0.1989732%)
# 4) Sampling fraction=1; No filtering on DefaultSpell_Age; Curated input space used; various maximum node depths
#   - Overlap of top three models in terms of AUC and MAE performance: 3
#   - Maximum depth tree of 3 selected (AUC=79% and MAE=0.178538%)
### CONCLUSION: Best candidate is the one with sampling fraction=1 and maximum depth=3
###             Continue with experimentation where sampling fraction=1 & maximum node-depth=3

# - Filtering of maximum spell age
#   - Sampling fraction=1; various filters on DefaultSpell_Age; Curated input space used; maximum node depth=3
#   - Overlap of top three models in terms of AUC and MAE performance: 1 (no filter), 2 (<=460), and 3 (<=360)
### CONCLUSION: Filtering by default spell age has no effect on observations prior to the maximum time imposed.
###             This exercise therefore serves to test the predictive power of the tree over various spell-horizons.
###             Will continue experiments without any filtering.

# - Advanced input space
#   - Sampling fraction=1; no filters on DefaultSpell_Age; Advanced DtH input space used; various maximum nodes
#   - Overlap of top three models in terms of AUC and MAE performance: 4
#   - Maximum depth tree of 4 selected (AUC=76% and MAE=0.1665903%)
### CONCLUSION: Use the advanced in put space with maximum node depth of 4

# - Final choice
### CONCLUSION: Use sampling fraction=1, no filters on DefSpell_Age, the Advanced DtH model's input space,
###             and a maximum node depth of 4


# --- 4.6 Save model
SurvTree_CTree <- list(datTrain=datCredit_train_smp_cross,survTree=SurvTree_PartyKit)
saveRDS(SurvTree_CTree, file=paste0(genObjPath,"SurvTree_CTree.rds"))
