# ================================= SURVIVAL TREES ======================================
# Fitting a multinomial survival tree in predicting the occurrence of both write-off and 
# cure events for defaulted loans, fitted at the spell-level.
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
#   - SurvTree_CTree | A list containing both the survival tree object and its
#                      associated training dataset
# =======================================================================================


# ------ 1. Preliminaries

# --- 1.1 Load data
# - Confirm prepared datasets are loaded into memory
# Training set
if (!exists('datCredit_train_CDH')) unpack.ffdf(paste0(genPath,"creditdata_train_CDH"), tempPath)
# Validation set
if (!exists('datCredit_valid_CDH')) unpack.ffdf(paste0(genPath,"creditdata_valid_CDH"), tempPath)

# Sampling window start for model fitting purposes
sDateStr <- as.Date("2008-01-31")
sDateStp <- as.Date("2019-12-31")

# - Filter data for default spells and date selections
datCredit_train <- subset(datCredit_train_CDH, !is.na(DefSpell_Key) & Date >= sDateStr & Date <= sDateStp)
datCredit_valid <- subset(datCredit_valid_CDH, !is.na(DefSpell_Key) & Date >= sDateStr & Date <= sDateStp)
rm(datCredit_train_CDH, datCredit_valid_CDH); gc()


# --- 1.2 Final data preparations

# - Transform categorical variables to factor
# Payment method
datCredit_train[,pmnt_method_grp_fac:=as.factor(pmnt_method_grp)]
datCredit_valid[,pmnt_method_grp_fac:=as.factor(pmnt_method_grp)]
# Previous defaults
datCredit_train[,PrevDefaults_fac:=as.factor(PrevDefaults)]
datCredit_valid[,PrevDefaults_fac:=as.factor(PrevDefaults)]
# Time binned
datCredit_train[,Time_Binned_fac:=as.factor(Time_Binned)]
datCredit_valid[,Time_Binned_fac:=as.factor(Time_Binned)]

# Event outcome
datCredit_train[, DefSpellResol_Fac:=factor(DefSpellResol_Type_Hist,
                                            c("Censored", "WOFF", "Cured"))]
datCredit_valid[, DefSpellResol_Fac:=factor(DefSpellResol_Type_Hist,
                                            c("Censored", "WOFF", "Cured"))]





# ------ 2. Fitting a survival tree and rendering predicitons

# --- 2.1 Fit tree
# - Input space of multinomial DtH-model
# Time_Binned, DefSpell_Num_binned, log(TimeInDefSpell):DefSpell_Num_binned,
# InterestRate_Nom, M_Repo_Rate_12, M_DTI_Growth_12, M_RealIncome_Growth_9,
# M_Inflation_Growth_12, NewLoans_Aggr_Prop
# - Input space of cross-sectional logistic regression model
# PrevDefaults_fac, Arrears, pmnt_method_grp_fac, g0_Delinq_Lag_1, slc_past_due_amt_imputed_med,
# InterestRate_Margin_imputed_mean, g0_Delinq_SD_6, Principal_Real, M_Repo_Rate_6,
# M_Inflation_Growth_6, NewLoans_Aggr_Prop, M_RealGDP_Growth_9
### NOTE: The following changes to the input space are made interactively using epxert judgment
# Added DefSpell_Num_binned

# - Fit tree
max_depth <- 4 # Maximum tree-depth
start_time <- proc.time()
modMultiSurvTree <- ctree(DefSpell_EventType ~ TimeInDefSpell + 
                             PrevDefaults_fac + DefSpell_Num_binned + pmnt_method_grp_fac + 
                            M_Inflation_Growth_6,
                             #Arrears + 
                             #g0_Delinq_Lag_1 + slc_past_due_amt_imputed_med + InterestRate_Margin_imputed_mean +
                             #g0_Delinq_SD_6 + Principal_Real + M_Repo_Rate_6 + 
                             #M_Inflation_Growth_6 + NewLoans_Aggr_Prop + M_RealGDP_Growth_9,
                           data=datCredit_train,
                           control=ctree_control(mincriterion=0.99, # 1 - p-value threshold (default ≈ 5% significance)
                                                 minsplit=1000, # minimum number of observations to attempt a split
                                                 minbucket=50, # minimum number in terminal node (common default)
                                                 testtype="Bonferroni", # most conservative → classical feeling
                                                 maxdepth=max_depth))
proc.time() - start_time
### Runtime = 1 sec

# - Visualise tree
plot(modMultiSurvTree)
### NOTE: Variables included in fitted model:


# --- 2.2 Obtain predictions from tree
# - Point to main credit data
datCredit <- rbind(data.table(datCredit_train, Sample="Training"),
                   data.table(datCredit_valid, Sample="Validation"))

# - Score data using fitted tree
datCredit[, Hazard_WriteOff_Exp := predict(modMultiSurvTree, newdata = datCredit, type="prob")[, "Write-off"]]
datCredit[, Hazard_Cured_Exp := predict(modMultiSurvTree, newdata = datCredit, type="prob")[, "Cured"]]




# - Fit Aalen-Johansen estimator for S(t) amidst competing risks
aj_All <- survfit(Surv(time=TimeInDefSpell-1, time2=TimeInDefSpell, event=DefSpell_EventType) ~ 1,
                  id=DefSpell_Key, data=datCredit_real[!is.na(DefSpell_Key),])
summary(aj_All)

# - Cast state probabilities over time into data object and merge with main object
### NOTE: These probabilities are equivalent to the cause-specific cumulative incidence functions (CIFs)
datSurv2 <- data.table(Time = aj_All$time, AtRisk_n = aj_All$n.risk[,"(s0)"],
                       Event_n_WriteOff = aj_All$n.event[,"Write-off"], Event_n_Cured = aj_All$n.event[,"Cured"],
                       Censored_n = aj_All$n.censor[,"(s0)"], SurvivalProb_AJ = aj_All$pstate[,"(s0)"], 
                       CIF_WriteOff = aj_All$pstate[,"Write-off"], # Write-off CIF
                       CIF_Cured = aj_All$pstate[,"Cured"]) # Cure CIF

# - Create event probabilities for cause g as f_g(t) = F_g(t) - F_g(t-1)
datSurv2[, EventRate_WriteOff := CIF_WriteOff - shift(CIF_WriteOff, n=1, fill=0) ]
datSurv2[, EventRate_Cured := CIF_Cured - shift(CIF_Cured, n=1, fill=0) ]

# - Create cause-specific sub-hazard functions h_1(t) and h_2(t)
datSurv2[, Hazard_WriteOff := Event_n_WriteOff / AtRisk_n]
datSurv2[, Hazard_Cured := Event_n_Cured / AtRisk_n]
datSurv2[, SurvivalProb_Overall := cumprod(1 - Hazard_WriteOff - Hazard_Cured)] # S(t) = 1 - h_1(t) - h_2(t)
