# ==================== INPUT SPACE: LOSS SEVERITY-TWO STAGE ====================
# Divide data into thematic groups and perform data analysis on them towards
# compiling an input space for the loss severity component.
# ------------------------------------------------------------------------------------------------------
# PROJECT TITLE: Loss Modelling (LGD) for Residential Mortgages
# SCRIPT AUTHOR(S): Mohammed Gabru (MG), Marcel Muller (MM)
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
#   - datCredit_train_CDH | Training dataset prepared in script 2g
#   - datCredit_valid_CDH | Validation dataset prepared in script 2g
#
# -- Outputs:
#   - modGLM_Severity_CPG | Final severity modeling object
# ------------------------------------------------------------------------------------------------------




# ------ 1. Preliminaries

# - Confirm prepared datasets are loaded into memory
if (!exists('datCredit_train_CDH')) unpack.ffdf(paste0(genPath,"creditdata_train_CDH"), tempPath);gc()
if (!exists('datCredit_valid_CDH')) unpack.ffdf(paste0(genPath,"creditdata_valid_CDH"), tempPath);gc()

# - Use only default spells 
datCredit_train <- datCredit_train_CDH[!is.na(DefSpell_Key) & DefSpellResol_Type_Hist=="WOFF",]
datCredit_valid <- datCredit_valid_CDH[!is.na(DefSpell_Key) & DefSpellResol_Type_Hist=="WOFF",]

# - Filter to maximum spell counter
datCredit_train <- datCredit_train[, .SD[which.max(DefSpell_Counter)], by = LoanID]
datCredit_valid <- datCredit_valid[, .SD[which.max(DefSpell_Counter)], by = LoanID]

# - Identify where the loss rate is out of bounds and not feasible
datCredit_train <- datCredit_train[, OOB_Ind := ifelse(LossRate_Real <=0 | LossRate_Real > 1, 1,0)]
datCredit_valid <- datCredit_valid[, OOB_Ind := ifelse(LossRate_Real <= 0 | LossRate_Real > 1, 1,0)]

# - Subset to include only relevant data
datCredit_train <- subset(datCredit_train, OOB_Ind == 0)
datCredit_valid <- subset(datCredit_valid, OOB_Ind == 0)

# - Remove previous objects from memory
rm(datCredit_train_CDH, datCredit_valid_CDH); gc()

# - Fit a base model for analyses
modGLM_base <- cpglm(LossRate_Real ~ 1, data=datCredit_train)




# ------ 2. Delinquency-themed variables

# --- 2.1 Selection of delinquency volatility variables
# - Initialize variables to be tested
vars <- c("g0_Delinq_SD_4", "g0_Delinq_SD_5", "g0_Delinq_SD_6", "g0_Delinq_SD_9", "g0_Delinq_SD_12")

# - Single-factor modelling results
table_Assess <- aicTable_LS(datCredit_train, vars, TimeDef=c("Cox_Discrete","LossRate_Real"), genPath=genObjPath, modelType="tweedie")
# Goodness-of-fit
table_Assess[order(AIC)]
### RESULTS: Best AIC-results: [g0_Delinq_SD_6]; [g0_Delinq_SD_5]; [g0_Delinq_SD_9]
# Discriminatory power (in-sample)
table_Assess[order(C, decreasing=T)]
### RESULTS: Best C-statistics: [g0_Delinq_SD_5]; [g0_Delinq_SD_6]; [g0_Delinq_SD_4]

### CONCLUSION: Select the top 3 variables (preference given to C-statistic rankings):
###               [g0_Delinq_SD_5]; [g0_Delinq_SD_6]; [g0_Delinq_SD_4]


# --- 2.2 Selection of portfolio-level variables indicating the prevalence of delinquent accounts
# - Initialize variables to be tested
vars <- c("g0_Delinq_Any_Aggr_Prop", "g0_Delinq_Any_Aggr_Prop_Lag_1", "g0_Delinq_Any_Aggr_Prop_Lag_2",
          "g0_Delinq_Any_Aggr_Prop_Lag_3", "g0_Delinq_Any_Aggr_Prop_Lag_4", "g0_Delinq_Any_Aggr_Prop_Lag_5",
          "g0_Delinq_Any_Aggr_Prop_Lag_6", "g0_Delinq_Any_Aggr_Prop_Lag_9", "g0_Delinq_Any_Aggr_Prop_Lag_12" )

# - Single-factor modelling results
table_Assess <- aicTable_LS(datCredit_train, vars, TimeDef=c("Cox_Discrete","LossRate_Real"), genPath=genObjPath, modelType="tweedie")
# Goodness-of-fit
table_Assess[order(AIC)]
### RESULTS: Best AIC-results: [g0_Delinq_Any_Aggr_Prop_Lag_3]; [g0_Delinq_Any_Aggr_Prop_Lag_2]; [g0_Delinq_Any_Aggr_Prop_Lag_1]
# Discriminatory power (in-sample)
table_Assess[order(C, decreasing=T)]
### RESULTS: Best C-statistics: [g0_Delinq_Any_Aggr_Prop_Lag_3]; [g0_Delinq_Any_Aggr_Prop_Lag_2]; [g0_Delinq_Any_Aggr_Prop_Lag_4]

### CONCLUSION: Select the top 3 variables (preference given to C-statistic rankings):
###               [g0_Delinq_Any_Aggr_Prop_Lag_3]; [g0_Delinq_Any_Aggr_Prop_Lag_2]; [g0_Delinq_Any_Aggr_Prop_Lag_4]


# --- 2.3 Selection of portfolio-level variables indicating the prevalence of defaulted accounts
# - Initialize variables to be tested
vars <- c("DefaultStatus1_Aggr_Prop", "DefaultStatus1_Aggr_Prop_Lag_1", "DefaultStatus1_Aggr_Prop_Lag_2",
          "DefaultStatus1_Aggr_Prop_Lag_3", "DefaultStatus1_Aggr_Prop_Lag_4", "DefaultStatus1_Aggr_Prop_Lag_5",
          "DefaultStatus1_Aggr_Prop_Lag_6", "DefaultStatus1_Aggr_Prop_Lag_9", "DefaultStatus1_Aggr_Prop_Lag_12" )

# - Single-factor modelling results
table_Assess <- aicTable_LS(datCredit_train, vars, TimeDef=c("Cox_Discrete","LossRate_Real"), genPath=genObjPath, modelType="tweedie")
# Goodness-of-fit
table_Assess[order(AIC)]
### RESULTS: Best AIC-results: [DefaultStatus1_Aggr_Prop_6]; [DefaultStatus1_Aggr_Prop_Lag_5]; [DefaultStatus1_Aggr_Prop_Lag_4]
# Discriminatory power (in-sample)
table_Assess[order(C, decreasing=T)]
### RESULTS: Best C-statistics: [DefaultStatus1_Aggr_Prop_12]; [DefaultStatus1_Aggr_Prop_Lag_6]; [DefaultStatus1_Aggr_Prop_Lag_9]

### CONCLUSION: Select the top 3 variables:
###               [DefaultStatus1_Aggr_Prop_12]; [DefaultStatus1_Aggr_Prop_Lag_6]; [DefaultStatus1_Aggr_Prop_Lag_9]


# --- 2.4 Selection of other portfolio-level delinquency-themed variables
# - Initialize variables to be tested
vars <- c("g0_Delinq_Ave", "ArrearsToBalance_Aggr_Prop", "CuringEvents_Aggr_Prop")

# - Single-factor modelling results
table_Assess <- aicTable_LS(datCredit_train, vars, TimeDef=c("Cox_Discrete","LossRate_Real"), genPath=genObjPath, modelType="tweedie")
# Goodness-of-fit
table_Assess[order(AIC)]
### RESULTS: Best AIC-results: [g0_Delinq_Ave]; [ArrearsToBalance_Aggr_Prop]; [CuringEvents_Aggr_Prop]
# Discriminatory power (in-sample)
table_Assess[order(C, decreasing=T)]
### RESULTS: Best C-statistics: [ArrearsToBalance_Aggr_Prop]; [g0_Delinq_Ave]; [CuringEvents_Aggr_Prop];

### CONCLUSION: Select the top 3 variables:
###               [ArrearsToBalance_Aggr_Prop]; [g0_Delinq_Ave]; [CuringEvents_Aggr_Prop];


# --- 2.5 Selection of account-level delinquency-themed variables
# - Initialize variables to be tested
vars <- c("g0_Delinq_Lag_1", "slc_acct_arr_dir_3_Change_Ind",
          "g0_Delinq_Num", "slc_acct_arr_dir_3", "slc_acct_roll_ever_24_imputed_mean",
          "Arrears", "PrevDefaults","TimeInDelinqState",
          "slc_past_due_amt_imputed_med", "slc_curing_ind","DefSpell_Age")

# - Single-factor modelling results
table_Assess <- aicTable_LS(datCredit_train, vars, TimeDef=c("Cox_Discrete","LossRate_Real"), genPath=genObjPath, modelType="tweedie")
# Goodness-of-fit
table_Assess[order(AIC)]
### RESULTS: Best AIC-results: [PrevDefaults]; [g0_Delinq_Num]; [g0_Delinq_Lag_1]; [slc_curing_ind]; [slc_acct_arr_dir_3]
# Discriminatory power (in-sample)
table_Assess[order(C, decreasing=T)]
### RESULTS: Best C-statistics: [PrevDefaults]; [slc_acct_arr_dir_3]; [slc_acct_arr_dir_3_Change_Ind]; [g0_Delinq_Num]; [slc_acct_roll_ever_24_imputed_mean]

### CONCLUSION: Select the top 5 variables (preference given to C-statistic rankings):
###               [PrevDefaults]; [slc_acct_arr_dir_3]; [slc_acct_arr_dir_3_Change_Ind]; [g0_Delinq_Num]; [slc_acct_roll_ever_24_imputed_mean]


# --- 2.6 Combining insights
# - Initialize variables to be tested
vars <- c("g0_Delinq_SD_6", "g0_Delinq_SD_5", "g0_Delinq_SD_4",
          "g0_Delinq_Any_Aggr_Prop_Lag_3", "g0_Delinq_Any_Aggr_Prop_Lag_2", "g0_Delinq_Any_Aggr_Prop_Lag_4",
          "DefaultStatus1_Aggr_Prop_Lag_12", "DefaultStatus1_Aggr_Prop_Lag_6", "DefaultStatus1_Aggr_Prop_Lag_9",
          "g0_Delinq_Ave", "ArrearsToBalance_Aggr_Prop", "CuringEvents_Aggr_Prop",
          "PrevDefaults", "slc_acct_arr_dir_3", "slc_acct_arr_dir_3_Change_Ind",
          "g0_Delinq_Num", "slc_acct_roll_ever_24_imputed_mean")

# - Full model
modGLM_full <- cpglm(as.formula(paste("LossRate_Real ~", paste(vars, collapse = " + "))),
                     data=datCredit_train)
summary(modGLM_full)
evalLS(modGLM_full, datCredit_train, targetFld="LossRate_Real", modGLM_base)
### RESULTS: AIC: 2 910; R^2: 3.85%; RMSE: 30.57%; MAE:  25.40%
### NOTE: Quasi-complete separation likely occurred, should be corrected in automated variable selection procedure

# - Stepwise forward selection using BIC
ptm <- proc.time() # for runtime calculations (ignore)
modGLM_step <- stepwise_cpglm_both(modGLM_base, modGLM_full,datCredit_train)
proc.time() - ptm # IGNORE: elapsed runtime; 15m

# - Evaluate model
summary(modGLM_step)
evalLS(modGLM_step,datCredit_train,targetFld="LossRate_Real",modGLM_base)
### RESULTS: AIC: 2922; Pseudo R^2: 3.81%; RMSE: 30.58%; MAE: 25.42%

### CONCLUSION: Select variables from automated selection procedure:
###               [g0_Delinq_Any_Aggr_Prop_Lag_3]; [g0_Delinq_SD_6];
###               [PrevDefaults]; [slc_acct_arr_dir_3]; [DefaultStatus1_Aggr_Prop_Lag_9]




# ------ 3. Other portfolio-level variables

# --- 3.1 Median portfolio-level interest rates
# - Initialize variables to be tested
vars <- c("InterestRate_Margin_Aggr_Med", "InterestRate_Margin_Aggr_Med_1", "InterestRate_Margin_Aggr_Med_2",
          "InterestRate_Margin_Aggr_Med_3", "InterestRate_Margin_Aggr_Med_9")

# - Single-factor modelling results
table_Assess <- aicTable_LS(datCredit_train, vars, TimeDef=c("Cox_Discrete","LossRate_Real"), genPath=genObjPath, modelType="tweedie")
# Goodness-of-fit
table_Assess[order(AIC)]
### RESULTS: Best AIC-results: [InterestRate_Margin_Aggr_Med_2]; [InterestRate_Margin_Aggr_Med_3]; [InterestRate_Margin_Aggr_Med_9]
# Discriminatory power (in-sample)
table_Assess[order(C, decreasing=T)]
### RESULTS: Best C-statistics: [InterestRate_Margin_Aggr_Med_9]; [InterestRate_Margin_Aggr_Med_3]; [InterestRate_Margin_Aggr_Med_2]

### CONCLUSION: Select the top 3 variables (preference given to C-statistic rankings):
###               [InterestRate_Margin_Aggr_Med_9]; [InterestRate_Margin_Aggr_Med_3]; [InterestRate_Margin_Aggr_Med_2]


# --- 3.2 Other portfolio-level (non-delinquency) variables
# - Initialize variables to be tested
vars <- c("InstalmentToBalance_Aggr_Prop", "AgeToTerm_Aggr_Mean", "DefSpell_Maturity_Aggr_Mean", "NewLoans_Aggr_Prop")

# - Single-factor modelling results
table_Assess <- aicTable_LS(datCredit_train, vars, TimeDef=c("Cox_Discrete","LossRate_Real"), genPath=genObjPath, modelType="tweedie")
# Goodness-of-fit
table_Assess[order(AIC)]
### RESULTS: Best AIC-results: [InstalmentToBalance_Aggr_Prop]; [AgeToTerm_Aggr_Mean]; [NewLoans_Aggr_Prop]
# Discriminatory power (in-sample)
table_Assess[order(C, decreasing=T)]
### RESULTS: Best C-statistics: [InstalmentToBalance_Aggr_Prop]; [AgeToTerm_Aggr_Mean]; [DefSpell_Maturity_Aggr_Mean]

### CONCLUSION: Select the top 3 variables (preference given to C-statistic rankings):
###               [InstalmentToBalance_Aggr_Prop]; [AgeToTerm_Aggr_Mean]; [DefSpell_Maturity_Aggr_Mean]


# --- 3.3 Combining insights
# - Initialize variables to be tested
vars <- c("InterestRate_Margin_Aggr_Med_9", "InterestRate_Margin_Aggr_Med_3", "InterestRate_Margin_Aggr_Med_2",
          "AgeToTerm_Aggr_Mean", "NewLoans_Aggr_Prop", "DefSpell_Maturity_Aggr_Mean")

# - Full model
modGLM_full <-cpglm( as.formula(paste("LossRate_Real ~", paste(vars, collapse = " + "))),
                     data=datCredit_train)
summary(modGLM_full)
evalLS(modGLM_full,datCredit_train,targetFld="LossRate_Real",modGLM_base)
### RESULTS: AIC: 3279; Pseudo R^2: 2.40%; RMSE: 30.80%; MAE: 25.94%

# - Stepwise forward selection using BIC
ptm <- proc.time() # for runtime calculations (ignore)
modGLM_step <- stepwise_cpglm_both(modGLM_base, modGLM_full,datCredit_train)
proc.time() - ptm # IGNORE: elapsed runtime; 6m

# - Evaluate model
summary(modGLM_step)
evalLS(modGLM_step,datCredit_train,targetFld="LossRate_Real",modGLM_base)
### RESULTS: AIC: 3284; Pseudo R^2: 2.32%; RMSE: 30.81%; MAE: 25.95%

### CONCLUSION: Select variables from automated variable selection procedure:
###             [InterestRate_Margin_Aggr_Med_2]; [NewLoans_Aggr_Prop]; [AgeToTerm_Aggr_Mean];
###             [DefSpell_Maturity_Aggr_Mean]




# ------ 4. Account-level variables

# --- 4.1 How do various non-delinquency account-level variables fare as single-factor models?
# - Initialize variables to be tested
vars <- c("Principal_Real", "Principal", "InterestRate_Margin_imputed_mean", 
          "Balance_Real_1", "Balance_1", "Instalment_Real", "InterestRate_Nom", "AgeToTerm",
          "BalanceToPrincipal_1", "slc_acct_pre_lim_perc_imputed_med")

# - Correlation analysis towards obtaining clusters of correlated variables
corrAnalysis(datCredit_train, vars, corrThresh = 0.6, method = 'spearman')
### RESULTS:
# Absolute correlations of  97%  found for  [Principal_Real] and [Principal ]
# Absolute correlations of  84%  found for  [Principal_Real] and [Balance_Real_1] 
# Absolute correlations of  80%  found for  [Principal]  and  [Balance_Real_1] 
# Absolute correlations of  82%  found for  [Principal_Real] and [Balance_1] 
# Absolute correlations of  82%  found for  [Principal] and [Balance_1]
# Absolute correlations of  98%  found for  [Balance_Real_1] and [Balance_1]
# Absolute correlations of  91%  found for  [Principal_Real] and [Instalment_Real] 
# Absolute correlations of  89%  found for  [Principal] and [Instalment_Real]
# Absolute correlations of  82%  found for  [Balance_Real_1] and [Instalment_Real]
# Absolute correlations of  81%  found for  [Balance_Real_1] and [Instalment_Real]
# Absolute correlations of  70%  found for  [Balance_Real_1] and [BalanceToPrincipal_1]
# Absolute correlations of  70%  found for  [Balance_1] and [BalanceToPrincipal_1]

# - Refine variables given insights from correlation analyses
### NOTE: - Inflation adjusted variables preferred over their non-adjusted counterparts
###       - Principal, balance, and instalment all kept regardless of their correlations, since each contains unique information
###       - Age-to-term and balance-to-principal are kept regardless of their high correlation, since each contains unique information
vars <- c("Principal_Real", "InterestRate_Margin_imputed_mean", "pmnt_method_grp",
          "Balance_Real_1", "Instalment_Real", "InterestRate_Nom", "AgeToTerm",
          "BalanceToPrincipal_1", "slc_acct_pre_lim_perc_imputed_med")

# - Single-factor modelling results
table_Assess <- aicTable_LS(datCredit_train, vars, TimeDef=c("Cox_Discrete","LossRate_Real"), genPath=genObjPath, modelType="tweedie")
# Goodness-of-fit
table_Assess[order(AIC)]
### RESULTS: Best AIC-results: [AgeToTerm]; [Principal_Real]; [Instalment_Real]; [pmnt_method_grp]; [Balance_Real_1]
# Discriminatory power (in-sample)
table_Assess[order(C, decreasing=T)]
### RESULTS: Best C-statistics: [Principal_Real]; [Instalment_Real]; [Balance_Real_1]; [AgeToTerm]; [pmnt_method_grp]

### CONCLUSION: Select the top 5 variables (preference given to C-statistic rankings):
###               [Principal_Real]; [Instalment_Real]; [Balance_Real_1];
###               [AgeToTerm]; [pmnt_method_grp]




# ------ 5. Macroeconomic variables

# --- 5.1 Which lag order is the best for: M_Repo_Rate
# - Initialize variables to be tested
vars <- c("M_Repo_Rate", "M_Repo_Rate_1 ", "M_Repo_Rate_2", "M_Repo_Rate_3", "M_Repo_Rate_6", "M_Repo_Rate_9", "M_Repo_Rate_12")

# - Single-factor modelling results
table_Assess <- aicTable_LS(datCredit_train, vars, TimeDef=c("Cox_Discrete","LossRate_Real"), genPath=genObjPath, modelType="tweedie")
# Goodness-of-fit
table_Assess[order(AIC)]
### RESULTS: Best AIC-results: [M_Repo_Rate_12]; [M_Repo_Rate_9]; [M_Repo_Rate]
# Discriminatory power (in-sample)
table_Assess[order(C, decreasing=T)]
### RESULTS: Best C-statistics: [M_Repo_Rate_1]; [M_Repo_Rate]; [M_Repo_Rate_12]

### CONCLUSION: Select the top 3 variables (preference given to C-statistic rankings):
###               [M_Repo_Rate_1]; [M_Repo_Rate]; [M_Repo_Rate_12]


# --- 5.2 Which lag order is the best for: [M_Inflation_Growth]
# - Initialize variables to be tested
vars <- c("M_Inflation_Growth", "M_Inflation_Growth_1 ", "M_Inflation_Growth_2", "M_Inflation_Growth_3", 
          "M_Inflation_Growth_6", "M_Inflation_Growth_9", "M_Inflation_Growth_12")

# - Single-factor modelling results
# Goodness-of-fit
table_Assess <- aicTable_LS(datCredit_train, vars, TimeDef=c("Cox_Discrete","LossRate_Real"), genPath=genObjPath, modelType="tweedie")
# Goodness-of-fit
table_Assess[order(AIC)]
### RESULTS: Best AIC-results: [M_Inflation_Growth_12]; [M_Inflation_Growth_9]; [M_Inflation_Growth_1]
# Discriminatory power (in-sample)
table_Assess[order(C, decreasing=T)]
### RESULTS: Best C-statistics: [M_Inflation_Growth_1]; [M_Inflation_Growth_2]; [M_Inflation_Growth_12]

### CONCLUSION: Select the top 3 variables (preference given to C-statistic rankings):
###               [M_Inflation_Growth_1]; [M_Inflation_Growth_2]; [M_Inflation_Growth_12]


# --- 5.3 Which lag order is the best for: [M_RealGDP_Growth]
# - Initialize variables to be tested
vars <- c("M_RealGDP_Growth", "M_RealGDP_Growth_1 ", "M_RealGDP_Growth_2", "M_RealGDP_Growth_3", 
          "M_RealGDP_Growth_6", "M_RealGDP_Growth_9", "M_RealGDP_Growth_12")

# - Single-factor modelling results
# Goodness-of-fit
table_Assess <- aicTable_LS(datCredit_train, vars, TimeDef=c("Cox_Discrete","LossRate_Real"), genPath=genObjPath, modelType="tweedie")
# Goodness-of-fit
table_Assess[order(AIC)]
### RESULTS: Best AIC-results: [M_RealGDP_Growth]; [M_RealGDP_Growth_1]; [M_RealGDP_Growth_2]
# Discriminatory power (in-sample)
table_Assess[order(C, decreasing=T)]
### RESULTS: Best C-statistics: [M_RealGDP_Growth]; [M_RealGDP_Growth_1]; [M_RealGDP_Growth_2]

### CONCLUSION: Select the top 3 variables:
###               [M_RealGDP_Growth]; [M_RealGDP_Growth_1]; [M_RealGDP_Growth_2]


# --- 5.4 Which lag order is the best for: [M_RealIncome_Growth]
# - Initialize variables to be tested
vars <- c("M_RealIncome_Growth", "M_RealIncome_Growth_1 ", "M_RealIncome_Growth_2", "M_RealIncome_Growth_3", 
          "M_RealIncome_Growth_6", "M_RealIncome_Growth_9", "M_RealIncome_Growth_12")

# - Single-factor modelling results
table_Assess <- aicTable_LS(datCredit_train, vars, TimeDef=c("Cox_Discrete","LossRate_Real"), genPath=genObjPath, modelType="tweedie")
# Goodness-of-fit
table_Assess[order(AIC)]
### RESULTS: Best AIC-results: [M_RealIncome_Growth_12]; [M_RealIncome_Growth_9]; [M_RealIncome_Growth]
# Discriminatory power (in-sample)
table_Assess[order(C, decreasing=T)]
### RESULTS: Best C-statistics: [M_RealIncome_Growth_12]; [M_RealIncome_Growth]; [M_RealIncome_Growth_9]

### CONCLUSION: Select the top 3 variables (preference given to C-statistic rankings):
###               [M_RealIncome_Growth_12]; [M_RealIncome_Growth]; [M_RealIncome_Growth_9]


# --- 5.5 Which lag order is the best for: [M_DTI_Growth]
# - Initialize variables to be tested
vars <- c("M_DTI_Growth", "M_DTI_Growth_1 ", "M_DTI_Growth_2", "M_DTI_Growth_3", 
          "M_DTI_Growth_6", "M_DTI_Growth_9", "M_DTI_Growth_12")

# - Single-factor modelling results
table_Assess <- aicTable_LS(datCredit_train, vars, TimeDef=c("Cox_Discrete","LossRate_Real"), genPath=genObjPath, modelType="tweedie")
# Goodness-of-fit
table_Assess[order(AIC)]
### RESULTS: Best AIC-results: [M_DTI_Growth_3]; [M_DTI_Growth_2]; [M_DTI_Growth_6]
# Discriminatory power (in-sample)
table_Assess[order(C, decreasing=T)]
### RESULTS: Best C-staistics: [M_DTI_Growth_3]; [M_DTI_Growth_6]; [M_DTI_Growth_2]

### CONCLUSION: Select the top 3 variables (preference given to C-statistic rankings):
###               [M_DTI_Growth_3]; [M_DTI_Growth_6]; [M_DTI_Growth_2] 


# --- 5.6 Which lag order is the best for: [M_Emp_Growth]
# - Initialize variables to be tested
vars <- c("M_Emp_Growth", "M_Emp_Growth_1 ", "M_Emp_Growth_2", "M_Emp_Growth_3", 
          "M_Emp_Growth_6", "M_Emp_Growth_9", "M_Emp_Growth_12")

# - Single-factor modelling results
table_Assess <- aicTable_LS(datCredit_train, vars, TimeDef=c("Cox_Discrete","LossRate_Real"), genPath=genObjPath, modelType="tweedie")
# Goodness-of-fit
table_Assess[order(AIC)]
### RESULTS: Best AIC-results: [M_Emp_Growth_12]; [M_Emp_Growth_9]; [M_Emp_Growth_6]
# Discriminatory power (in-sample)
table_Assess[order(C, decreasing=T)]
### RESULTS: Best C-statistics: [M_Emp_Growth_6]; [M_Emp_Growth_12]; [M_Emp_Growth]

### CONCLUSION: Select the top 3 variables (preference given to C-statistic rankings):
###               [M_Emp_Growth_6]; [M_Emp_Growth_12]; [M_Emp_Growth]


# --- 5.7 Combining insights: Macroeconomic variables
# - Initialize variables to be tested
vars <- c("M_Repo_Rate_1", "M_Repo_Rate", "M_Repo_Rate_12",
          "M_Inflation_Growth_1", "M_Inflation_Growth_2", "M_Inflation_Growth_12",
          "M_RealGDP_Growth", "M_RealGDP_Growth_1", "M_RealGDP_Growth_2",
          "M_RealIncome_Growth_12", "M_RealIncome_Growth", "M_RealIncome_Growth_9",
          "M_DTI_Growth_3", "M_DTI_Growth_6", "M_DTI_Growth_2",
          "M_Emp_Growth_6", "M_Emp_Growth_12", "M_Emp_Growth"
)

# - Full model | Stepwise forward selection procedure
modGLM_full <- cpglm( as.formula(paste("LossRate_Real ~", paste(vars, collapse = " + "))),
                      data=datCredit_train)
summary(modGLM_full)
evalLS(modGLM_full,datCredit_train,targetFld="LossRate_Real",modGLM_base)
### RESULTS: AIC: 3 239; R^2: 2.91%; RMSE: 30.72%; MAE: 25.84%

# - Stepwise forward selection using AIC
ptm <- proc.time() # for runtime calculations (ignore)
modGLM_step <- stepwise_cpglm_both(modGLM_base, modGLM_full,datCredit_train)
proc.time() - ptm # IGNORE: elapsed runtime; 24m

# - Evaluate model
summary(modGLM_step)
evalLS(modGLM_step,datCredit_train,targetFld="LossRate_Real",modGLM_base)
### RESULTS: AIC: 3 266; R^2: 2.52%; RMSE: 30.78%; MAE: 25.94%

### CONCLUSION: Select variables from automated variable selection procedure:
###             [M_DTI_Growth_3], [M_Repo_Rate_12], [M_RealIncome_Growth_12],
###             [M_Inflation_Growth_12]; [M_RealGDP_Growth]




# ------ 6. Combine variables from all themes

# --- 6.1 Run an automated selection procedure on the entire set
# - Initialise set of variables
vars <- c("g0_Delinq_Any_Aggr_Prop_Lag_3", "g0_Delinq_SD_6",
          "PrevDefaults", "slc_acct_arr_dir_3", "DefaultStatus1_Aggr_Prop_Lag_9",
          "InterestRate_Margin_Aggr_Med_2", "NewLoans_Aggr_Prop", "AgeToTerm_Aggr_Mean",
          "DefSpell_Maturity_Aggr_Mean",
          "Principal_Real", "Instalment_Real", "Balance_Real_1",
          "AgeToTerm", "pmnt_method_grp",
          "M_DTI_Growth_3", "M_Repo_Rate_12", "M_RealIncome_Growth_12",
          "M_Inflation_Growth_12", "M_RealGDP_Growth"
)

# - Full model 
modGLM_full <- cpglm( as.formula(paste("LossRate_Real ~", paste(vars, collapse = " + "))),
                      data=datCredit_train)
summary(modGLM_full)
evalLS(modGLM_full,datCredit_train,targetFld="LossRate_Real",modGLM_base)
### RESULTS: AIC: -636; R^2: -1.60; RMSE: 50.24%; MAE: 21.62%
### NOTE: Quasi-complete separation likely occurred, will be resolved within the automated variable selection procedure

# - Stepwise forward selection using AIC
ptm <- proc.time() # for runtime calculations (ignore)
modGLM_step <- stepwise_cpglm_both(modGLM_base, modGLM_full,datCredit_train)
proc.time() - ptm # IGNORE: elapsed runtime; 128m

# - Evaluate model
summary(modGLM_step)
evalLS(modGLM_step,datCredit_train,targetFld="LossRate_Real",modGLM_base)
### RESULTS: AIC: -612; R^2: -1.84; RMSE: 52.52%; MAE: 21.67%


# --- 6.2 Model refinement | Remove [AgeToTerm] following ad-hoc analyses
### NOTE: The following changes have been made:
### Removed: 1) [AgeToTerm] -> Results in negative R^2 values
### Added:   2) [DefSpell_Age]
# - Initialize variables to be tested
vars <- c("pmnt_method_grp", "Principal_Real",
          "Balance_Real_1", "g0_Delinq_SD_6", "AgeToTerm_Aggr_Mean",
          "slc_acct_arr_dir_3", "PrevDefaults",
          "M_RealIncome_Growth_12", "NewLoans_Aggr_Prop", "Instalment_Real",
          "M_DTI_Growth_3", "DefSpell_Age"
)

# - Fit model
modGLM <-cpglm(as.formula(paste("LossRate_Real ~", paste(vars, collapse = " + "))),
               data=datCredit_train)

# - Evaluate model
summary(modGLM)
evalLS(modGLM,datCredit_train,targetFld="LossRate_Real",modGLM_base)
### RESULTS: AIC: 3 266; R^2: 2.52%; RMSE: 30.78%; MAE: 25.94%

### CONCLUSION: Select variables from automated variable selection procedure:
###             [pmnt_method_grp]; [Principal_Real]
###             [Balance_Real_1]; [g0_Delinq_SD_6]; [AgeToTerm_Aggr_Mean];
###             [slc_acct_arr_dir_3]; [PrevDefaults];
###             [M_RealIncome_Growth_12]; [NewLoans_Aggr_Prop]; [Instalment_Real];
###             [M_DTI_Growth_3]




# ------ 9. Final model

# --- 9.1 Preliminaries
# - Confirm prepared datasets are loaded into memory
if (!exists('datCredit_train_CDH')) unpack.ffdf(paste0(genPath,"creditdata_train_CDH"), tempPath);gc()
if (!exists('datCredit_valid_CDH')) unpack.ffdf(paste0(genPath,"creditdata_valid_CDH"), tempPath);gc()

# - Use only default spells 
datCredit_train <- datCredit_train_CDH[!is.na(DefSpell_Key) & DefSpellResol_Type_Hist=="WOFF",]
datCredit_valid <- datCredit_valid_CDH[!is.na(DefSpell_Key) & DefSpellResol_Type_Hist=="WOFF",]

# - Filter to maximum spell counter
datCredit_train <- datCredit_train[, .SD[which.max(DefSpell_Counter)], by = LoanID]
datCredit_valid <- datCredit_valid[, .SD[which.max(DefSpell_Counter)], by = LoanID]

# - Identify where the loss rate is out of bounds and not feasible
datCredit_train <- datCredit_train[, OOB_Ind := ifelse(LossRate_Real <=0 | LossRate_Real > 1, 1,0)]
datCredit_valid <- datCredit_valid[, OOB_Ind := ifelse(LossRate_Real <= 0 | LossRate_Real > 1, 1,0)]

# - Subset to include only relevant data
datCredit_train <- subset(datCredit_train, OOB_Ind == 0)
datCredit_valid <- subset(datCredit_valid, OOB_Ind == 0)

# - Remove previous objects from memory
rm(datCredit_train_CDH, datCredit_valid_CDH); gc()

# - Fit a base model for analyses
modGLM_base <- cpglm(LossRate_Real ~ 1, data=datCredit_train)


# --- 9.2 Fit and evaluate final model
# - Initialize variables to be tested
vars <- c("pmnt_method_grp", "Principal_Real",
          "Balance_Real_1", "g0_Delinq_SD_6", "AgeToTerm_Aggr_Mean",
          "slc_acct_arr_dir_3", "PrevDefaults",
          "M_RealIncome_Growth_12", "NewLoans_Aggr_Prop", "Instalment_Real",
          "M_DTI_Growth_3"
)

# - Fit model
modGLM <-cpglm(as.formula(paste("LossRate_Real ~", paste(vars, collapse = " + "))),
               data=datCredit_train)

# - Evaluate model
summary(modGLM)
evalLS(modGLM,datCredit_train,targetFld="LossRate_Real",modGLM_base)
### RESULTS: AIC: 462; R^2: 21.80; RMSE: 27.57%; MAE: 22.43%


# --- 9.3 Save objects
modGLM_Severity_CPG <- copy(modGLM); rm(modGLM); gc()
saveRDS(modGLM_Severity_CPG, file=paste0(genObjPath,"Severity_CPH_Model.rds"))




