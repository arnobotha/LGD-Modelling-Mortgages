# ======================================= INPUT SPACE: ONE-STAGE-GAUSSIAN============================
# Divide data into thematic groups and perform data analysis on them to compile an input space for 
# a single stage model with a Gaussian link function.
# ------------------------------------------------------------------------------------------------------
# PROJECT TITLE: Loss Modelling (LGD) for FNB Mortgages
# SCRIPT AUTHOR(S): Mohammed Gabru (MG)
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

# - Confirm prepared datasets are loaded into memory
if (!exists('datCredit_train_CDH')) unpack.ffdf(paste0(genPath,"creditdata_train_CDH"), tempPath);gc()
if (!exists('datCredit_valid_CDH')) unpack.ffdf(paste0(genPath,"creditdata_valid_CDH"), tempPath);gc()

# - Filter data for default spells and first default observations
datCredit_train <- subset(datCredit_train_CDH, !is.na(DefSpell_Key) & DefSpell_Counter==1)
datCredit_valid <- subset(datCredit_valid_CDH, !is.na(DefSpell_Key) & DefSpell_Counter==1)

# - Identify where the loss rate is out of bounds and not feasible
datCredit_train <- datCredit_train[, OOB_Ind := ifelse(LossRate_Real < 0 | LossRate_Real > 1, 1,0)]
datCredit_valid <- datCredit_valid[, OOB_Ind := ifelse(LossRate_Real < 0 | LossRate_Real > 1, 1,0)]

# - Subset to exclude nonsensical loss-rates
datCredit_train <- subset(datCredit_train, OOB_Ind == 0)
datCredit_valid <- subset(datCredit_valid, OOB_Ind == 0)
### NOTE: The training- and validation datasets are reduced by ~14% due to these out-of-bound observations

# - Remove previous objects from memory
rm(datCredit_train_CDH, datCredit_valid_CDH); gc()

# - Fit an empty model to use for analyses
modGLM_base <- glm(LossRate_Real ~ 1, data=datCredit_train, family=gaussian(link="identity"))




# ------ 2. Delinquency-themed variables

# --- 2.1 Selection of delinquency volatility variables
# - Initialize variables to be tested
vars <- c("g0_Delinq_SD_4", "g0_Delinq_SD_5", "g0_Delinq_SD_6", "g0_Delinq_SD_9", "g0_Delinq_SD_12")

# - Single-factor modelling results
table_Assess <- aicTable_LS(datCredit_train, vars, TimeDef=c("Cox_Discrete","LossRate_Real"), genPath=genObjPath, modelType="gaussian")
# Goodness-of-fit
table_Assess[order(AIC)]
### RESULTS: Best AIC-results: [g0_Delinq_SD_5]; [g0_Delinq_SD_6]; [g0_Delinq_SD_4]
# Discriminatory power (in-sample)
table_Assess[order(C, decreasing=T)]
### RESULTS: Best C-staistics: [g0_Delinq_SD_6]; [g0_Delinq_SD_5]; [g0_Delinq_SD_4]

### CONCLUSION: Select the top 3 variables (preference given to C-statistic rankings):
###               [g0_Delinq_SD_6]; [g0_Delinq_SD_5]; [g0_Delinq_SD_4]


# --- 2.2 Selection of portfolio-level variables indicating the prevalence of delinquent accounts
# - Initialize variables to be tested
vars <- c("g0_Delinq_Any_Aggr_Prop", "g0_Delinq_Any_Aggr_Prop_Lag_1", "g0_Delinq_Any_Aggr_Prop_Lag_2",
          "g0_Delinq_Any_Aggr_Prop_Lag_3", "g0_Delinq_Any_Aggr_Prop_Lag_4", "g0_Delinq_Any_Aggr_Prop_Lag_5",
          "g0_Delinq_Any_Aggr_Prop_Lag_6", "g0_Delinq_Any_Aggr_Prop_Lag_9", "g0_Delinq_Any_Aggr_Prop_Lag_12" )

# - Single-factor modelling results
table_Assess <- aicTable_LS(datCredit_train, vars, TimeDef=c("Cox_Discrete","LossRate_Real"), genPath=genObjPath, modelType="gaussian")
# Goodness-of-fit
table_Assess[order(AIC)]
### RESULTS: Best AIC-results: [g0_Delinq_Any_Aggr_Prop_Lag_12]; [g0_Delinq_Any_Aggr_Prop_Lag_9]; [g0_Delinq_Any_Aggr_Prop_Lag_4]
# Discriminatory power (in-sample)
table_Assess[order(C, decreasing=T)]
### RESULTS: Best C-statistics: [g0_Delinq_Any_Aggr_Prop_Lag_12]; [g0_Delinq_Any_Aggr_Prop_Lag_9]; [g0_Delinq_Any_Aggr_Prop_Lag_5]

### CONCLUSION: Select the top 3 variables (preference given to C-statistic rankings):
###               [g0_Delinq_Any_Aggr_Prop_Lag_12]; [g0_Delinq_Any_Aggr_Prop_Lag_9]; [g0_Delinq_Any_Aggr_Prop_Lag_5]


# --- 2.3 Selection of portfolio-level variables indicating the prevalence of defaulted accounts
# - Initialize variables to be tested
vars <- c("DefaultStatus1_Aggr_Prop", "DefaultStatus1_Aggr_Prop_Lag_1", "DefaultStatus1_Aggr_Prop_Lag_2",
          "DefaultStatus1_Aggr_Prop_Lag_3", "DefaultStatus1_Aggr_Prop_Lag_4", "DefaultStatus1_Aggr_Prop_Lag_5",
          "DefaultStatus1_Aggr_Prop_Lag_6", "DefaultStatus1_Aggr_Prop_Lag_9", "DefaultStatus1_Aggr_Prop_Lag_12")

# - Single-factor modelling results
table_Assess <- aicTable_LS(datCredit_train, vars, TimeDef=c("Cox_Discrete","LossRate_Real"), genPath=genObjPath, modelType="gaussian")
# Goodness-of-fit
table_Assess[order(AIC)]
### RESULTS: Best AIC-results: [DefaultStatus1_Aggr_Prop]; [DefaultStatus1_Aggr_Prop_Lag_1]; [DefaultStatus1_Aggr_Prop_Lag_2]
# Discriminatory power (in-sample)
table_Assess[order(C, decreasing=T)]
### RESULTS: Best C-statistics: [DefaultStatus1_Aggr_Prop]; [DefaultStatus1_Aggr_Prop_Lag_1]; [DefaultStatus1_Aggr_Prop_Lag_2]

### CONCLUSION: Select the top 3 variables:
###               [DefaultStatus1_Aggr_Prop]; [DefaultStatus1_Aggr_Prop_Lag_1]; [DefaultStatus1_Aggr_Prop_Lag_2]


# --- 2.4 Selection of other portfolio-level delinquency-themed variables
# - Initialize variables to be tested
vars <- c("g0_Delinq_Ave", "ArrearsToBalance_Aggr_Prop", "CuringEvents_Aggr_Prop")

# - Single-factor modelling results
table_Assess <- aicTable_LS(datCredit_train, vars, TimeDef=c("Cox_Discrete","LossRate_Real"), genPath=genObjPath, modelType="gaussian")
# Goodness-of-fit
table_Assess[order(AIC)]
### RESULTS: Best AIC-results: [g0_Delinq_Ave]; [CuringEvents_Aggr_Prop]; [ArrearsToBalance_Aggr_Prop];
# Discriminatory power (in-sample)
table_Assess[order(C, decreasing=T)]
### RESULTS: Best C-statistics: [g0_Delinq_Ave]; [CuringEvents_Aggr_Prop]; [ArrearsToBalance_Aggr_Prop]

### CONCLUSION: Select the top 3 variables:
###               [g0_Delinq_Ave]; [CuringEvents_Aggr_Prop]; [ArrearsToBalance_Aggr_Prop]


# --- 2.5 Selection of account-level delinquency-themed variables
# - Initialize variables to be tested
vars <- c("g0_Delinq_Lag_1", "slc_acct_arr_dir_3_Change_Ind",
          "g0_Delinq_Num", "slc_acct_arr_dir_3", "slc_acct_roll_ever_24_imputed_mean",
          "Arrears", "PrevDefaults","TimeInDelinqState",
          "slc_past_due_amt_imputed_med", "slc_curing_ind","DefSpell_Age")

# - Single-factor modelling results
table_Assess <- aicTable_LS(datCredit_train, vars, TimeDef=c("Cox_Discrete","LossRate_Real"), genPath=genObjPath, modelType="gaussian")
# Goodness-of-fit
table_Assess[order(AIC)]
### RESULTS: Best AIC-results: [PrevDefaults]; [g0_Delinq_Num]; [slc_acct_arr_dir_3]; [g0_Delinq_Lag_1]; [slc_acct_arr_dir_3_Change_Ind]
# Discriminatory power (in-sample)
table_Assess[order(C, decreasing=T)]
### RESULTS: Best C-statistics: [PrevDefaults]; [DefSpell_Age]; [g0_Delinq_Num]; [Arrears]; [slc_past_due_amt_imputed_med]

### CONCLUSION: Select the top 5 variables (preference given to C-statistic rankings):
###               [PrevDefaults]; [DefSpell_Age]; [g0_Delinq_Num]; [Arrears]; [slc_past_due_amt_imputed_med]


# --- 2.6 Combining insights
# - Initialize variables to be tested
vars <- c("g0_Delinq_SD_6", "g0_Delinq_SD_5", "g0_Delinq_SD_4",
          "g0_Delinq_Any_Aggr_Prop_Lag_12", "g0_Delinq_Any_Aggr_Prop_Lag_9", "g0_Delinq_Any_Aggr_Prop_Lag_5",
          "DefaultStatus1_Aggr_Prop", "DefaultStatus1_Aggr_Prop_Lag_1", "DefaultStatus1_Aggr_Prop_Lag_2",
          "g0_Delinq_Ave", "CuringEvents_Aggr_Prop", "ArrearsToBalance_Aggr_Prop",
          "PrevDefaults", "DefSpell_Age", "g0_Delinq_Num", "Arrears", "slc_past_due_amt_imputed_med")

# - Full model
modGLM_full <- glm(as.formula(paste("LossRate_Real ~", paste(vars, collapse = " + "))),
                   data=datCredit_train,family = gaussian(link = "identity"))
summary(modGLM_full)
evalLS(modGLM_full, datCredit_train, targetFld="LossRate_Real", modGLM_base)
### RESULTS: AIC: -37 395; R^2: 13.18%; RMSE: 19.40%; MAE: 10.08%

# - Stepwise forward selection using BIC
ptm <- proc.time() # for runtime calculations (ignore)
modGLM_step <- stepAIC(modGLM_base, scope=list(lower=~ 1, 
                                               upper=as.formula(paste("~", paste(vars, collapse = " + ")))), 
                       direction="both", k=log(datCredit_train[,.N]), maxit=50)
proc.time() - ptm # IGNORE: elapsed runtime; 16s

# - Evaluate model
summary(modGLM_step)
evalLS(modGLM_step, datCredit_train,targetFld="LossRate_Real", modGLM_base)
### RESULTS: AIC: -37 380; R^2: 13.16%; RMSE: 19.41%; MAE: 10.08%

### CONCLUSION: Select variables from automated selection procedure:
###               [PrevDefaults]; [g0_Delinq_Any_Aggr_Prop_Lag_5];
###               [g0_Delinq_SD_6]; [DefSpell_Age];
###               [g0_Delinq_Any_Aggr_Prop_Lag_12]; [g0_Delinq_Num]; [g0_Delinq_Ave];
###               [ArrearsToBalance_Aggr_Prop]; [g0_Delinq_Any_Aggr_Prop_Lag_9];
###               [slc_past_due_amt_imputed_med]; [g0_Delinq_SD_5]; 
###               [DefaultStatus1_Aggr_Prop_Lag_2]; [DefaultStatus1_Aggr_Prop]




# ------ 3. Other portfolio-level variables

# --- 3.1 Median portfolio-level interest rates
# - Initialize variables to be tested
vars <- c("InterestRate_Margin_Aggr_Med", "InterestRate_Margin_Aggr_Med_1", "InterestRate_Margin_Aggr_Med_2",
          "InterestRate_Margin_Aggr_Med_3", "InterestRate_Margin_Aggr_Med_9")

# - Single-factor modelling results
table_Assess <- aicTable_LS(datCredit_train, vars, TimeDef=c("Cox_Discrete","LossRate_Real"), genPath=genObjPath, modelType="gaussian")
# Goodness-of-fit
table_Assess[order(AIC)]
### RESULTS: Best AIC-results: [InterestRate_Margin_Aggr_Med]; [InterestRate_Margin_Aggr_Med_1]; [InterestRate_Margin_Aggr_Med_2]
# Discriminatory power (in-sample)
table_Assess[order(C, decreasing=T)]
### RESULTS: Best C-statistics: [InterestRate_Margin_Aggr_Med]; [InterestRate_Margin_Aggr_Med_1]; [InterestRate_Margin_Aggr_Med_2]

### CONCLUSION: Select the top 3 variables (preference given to C-statistic rankings):
###               [InterestRate_Margin_Aggr_Med]; [InterestRate_Margin_Aggr_Med_1]; [InterestRate_Margin_Aggr_Med_2]


# --- 3.2 Other portfolio-level (non-delinquency) variables
# - Initialize variables to be tested
vars <- c("InstalmentToBalance_Aggr_Prop", "AgeToTerm_Aggr_Mean", "DefSpell_Maturity_Aggr_Mean", "NewLoans_Aggr_Prop")

# - Single-factor modelling results
table_Assess <- aicTable_LS(datCredit_train, vars, TimeDef=c("Cox_Discrete","LossRate_Real"), genPath=genObjPath, modelType="gaussian")
# Goodness-of-fit
table_Assess[order(AIC)]
### RESULTS: Best AIC-results: [AgeToTerm_Aggr_Mean]; [NewLoans_Aggr_Prop]; [DefSpell_Maturity_Aggr_Mean];
# Discriminatory power (in-sample)
table_Assess[order(C, decreasing=T)]
### RESULTS: Best C-statistics: [AgeToTerm_Aggr_Mean]; [NewLoans_Aggr_Prop]; [DefSpell_Maturity_Aggr_Mean]

### CONCLUSION: Select the top 3 variables (preference given to C-statistic rankings):
###               [AgeToTerm_Aggr_Mean]; [NewLoans_Aggr_Prop]; [DefSpell_Maturity_Aggr_Mean]


# --- 3.3 Combining insights
# - Initialize variables to be tested
vars <- c("InterestRate_Margin_Aggr_Med", "InterestRate_Margin_Aggr_Med_1", "InterestRate_Margin_Aggr_Med_2",
          "AgeToTerm_Aggr_Mean", "NewLoans_Aggr_Prop", "DefSpell_Maturity_Aggr_Mean")

# - Full model
modGLM_full <- glm(as.formula(paste("LossRate_Real ~", paste(vars, collapse = " + "))),
                   data=datCredit_train,family = gaussian(link = "identity"))
summary(modGLM_full)
evalLS(modGLM_full,datCredit_train,targetFld="LossRate_Real",modGLM_base)
### RESULTS: AIC: -27 240; Pseudo R^2: 2.11%; RMSE: 20.60%; MAE: 11.98%

# - Stepwise forward selection using BIC
ptm <- proc.time() # for runtime calculations (ignore)
modGLM_step <- stepAIC(modGLM_base, scope=list(lower=~ 1, 
                                               upper=as.formula(paste("~", paste(vars, collapse = " + ")))), 
                       direction="both", k=log(datCredit_train[,.N]), maxit=50)
proc.time() - ptm # IGNORE: elapsed runtime; 16s

# - Evaluate model
summary(modGLM_step)
evalLS(modGLM_step, datCredit_train,targetFld="LossRate_Real", modGLM_base)
### RESULTS: AIC: -27 237; Pseudo R^2: 2.10%; RMSE: 20.60%; MAE: 11.99%

### CONCLUSION: Select variables from automated selection procedure:
###               [InterestRate_Margin_Aggr_Med]; [DefSpell_Maturity_Aggr_Mean]; [AgeToTerm_Aggr_Mean]




# ------ 4. Account-level variables

# --- 4.1 How do various non-delinquency account-level variables fare as single-factor models?
# - Initialize variables to be tested
vars <- c("Principal_Real", "Principal", "InterestRate_Margin_imputed_mean", 
          "Balance_Real_1", "Balance_1", "Instalment_Real", "InterestRate_Nom", "AgeToTerm",
          "BalanceToPrincipal_1", "slc_acct_pre_lim_perc_imputed_med")

# - Correlation analysis towards obtaining clusters of correlated variables
corrAnalysis(datCredit_train, vars, corrThresh = 0.6, method = 'spearman')
### RESULTS:
# Absolute correlations of  96%  found for  [Principal_Real] and [Principal ]
# Absolute correlations of  92%  found for  [Principal_Real] and [Balance_Real_1] 
# Absolute correlations of  87%  found for  [Principal]  and  [Balance_Real_1] 
# Absolute correlations of  91%  found for  [Principal_Real] and [Balance_1] 
# Absolute correlations of  91%  found for  [Principal] and [Balance_1]
# Absolute correlations of  98%  found for  [Balance_Real_1] and [Balance_1]
# Absolute correlations of  91%  found for  [Principal_Real] and [Instalment_Real] 
# Absolute correlations of  86%  found for  [Principal] and [Instalment_Real]
# Absolute correlations of  96%  found for  [Balance_Real_1] and [Instalment_Real]
# Absolute correlations of  93%  found for  [Balance_Real_1] and [Instalment_Real]
# Absolute correlations of  75%  found for  [Balance_Real_1] and [BalanceToPrincipal_1]
# Absolute correlations of  70%  found for  [Balance_1] and [BalanceToPrincipal_1]
# Absolute correlations of  68%  found for  [Instalment_Real] and [BalanceToPrincipal_1] 
# Absolute correlations of  -62%  found for  [AgeToTerm]  and  [BalanceToPrincipal_1]

# - Refine variables given insights from correlation analyses
### NOTE: - Inflation adjusted variables preferred over their non-adjusted counterparts
###       - Principal, balance, and instalment all kept regardless of their correlations, each contains unique information
###       - Age-to-term and balance-to-principal are kept regardless of their high correlation, since each contains unique information
vars <- c("Principal_Real", "InterestRate_Margin_imputed_mean", "pmnt_method_grp",
          "Balance_Real_1", "Instalment_Real", "InterestRate_Nom", "AgeToTerm",
          "BalanceToPrincipal_1", "slc_acct_pre_lim_perc_imputed_med")

# - Single-factor modelling results
table_Assess <- aicTable_LS(datCredit_train, vars, TimeDef=c("Cox_Discrete","LossRate_Real"), genPath=genObjPath, modelType="gaussian")
# Goodness-of-fit
table_Assess[order(AIC)]
### RESULTS: Best AIC-results: [AgeToTerm]; [Balance_Real_1]; [Instalment_Real]; [Principal_Real]; [InterestRate_Nom]
# Discriminatory power (in-sample)
table_Assess[order(C, decreasing=T)]
### RESULTS: Best C-statistics: [AgeToTerm]; [Balance_Real_1]; [Instalment_Real]; [Principal_Real]; [InterestRate_Nom]

### CONCLUSION: Select the top 5 variables:
###               [AgeToTerm]; [Balance_Real_1]; [Instalment_Real];
###               [Principal_Real]; [InterestRate_Nom]




# ------ 5. Macroeconomic variables

# --- 5.1 Which lag order is the best for: M_Repo_Rate
# - Initialize variables to be tested
vars <- c("M_Repo_Rate", "M_Repo_Rate_1 ", "M_Repo_Rate_2", "M_Repo_Rate_3", "M_Repo_Rate_6", "M_Repo_Rate_9", "M_Repo_Rate_12")

# - Single-factor modelling results
table_Assess <- aicTable_LS(datCredit_train, vars, TimeDef=c("Cox_Discrete","LossRate_Real"), genPath=genObjPath, modelType="gaussian")
# Goodness-of-fit
table_Assess[order(AIC)]
### RESULTS: Best AIC-results: [M_Repo_Rate_6]; [M_Repo_Rate_9]; [M_Repo_Rate_3]
# Discriminatory power (in-sample)
table_Assess[order(C, decreasing=T)]
### RESULTS: Best C-statistics: [M_Repo_Rate_9]; [M_Repo_Rate_12]; [M_Repo_Rate_6]

### CONCLUSION: Select the top 3 variables (preference given to C-statistic rankings):
###               [M_Repo_Rate_9]; [M_Repo_Rate_12]; [M_Repo_Rate_6]


# --- 5.2 Which lag order is the best for: [M_Inflation_Growth]
# - Initialize variables to be tested
vars <- c("M_Inflation_Growth", "M_Inflation_Growth_1 ", "M_Inflation_Growth_2", "M_Inflation_Growth_3", 
          "M_Inflation_Growth_6", "M_Inflation_Growth_9", "M_Inflation_Growth_12")

# - Single-factor modelling results
# Goodness-of-fit
table_Assess <- aicTable_LS(datCredit_train, vars, TimeDef=c("Cox_Discrete","LossRate_Real"), genPath=genObjPath, modelType="gaussian")
# Goodness-of-fit
table_Assess[order(AIC)]
### RESULTS: Best AIC-results: [M_Inflation_Growth_3]; [M_Inflation_Growth_2]; [M_Inflation_Growth_1]
# Discriminatory power (in-sample)
table_Assess[order(C, decreasing=T)]
### RESULTS: Best C-statistics: [M_Inflation_Growth_9]; [M_Inflation_Growth_6]; [M_Inflation_Growth_3]

### CONCLUSION: Select the top 3 variables (preference given to C-statistic rankings):
###               [M_Inflation_Growth_9]; [M_Inflation_Growth_6]; [M_Inflation_Growth_3]


# --- 5.3 Which lag order is the best for: [M_RealGDP_Growth]
# - Initialize variables to be tested
vars <- c("M_RealGDP_Growth", "M_RealGDP_Growth_1 ", "M_RealGDP_Growth_2", "M_RealGDP_Growth_3", 
          "M_RealGDP_Growth_6", "M_RealGDP_Growth_9", "M_RealGDP_Growth_12")

# - Single-factor modelling results
# Goodness-of-fit
table_Assess <- aicTable_LS(datCredit_train, vars, TimeDef=c("Cox_Discrete","LossRate_Real"), genPath=genObjPath, modelType="gaussian")
# Goodness-of-fit
table_Assess[order(AIC)]
### RESULTS: Best AIC-results: [M_RealGDP_Growth_9]; [M_RealGDP_Growth_12]; [M_RealGDP_Growth_6]
# Discriminatory power (in-sample)
table_Assess[order(C, decreasing=T)]
### RESULTS: Best C-statistics: [M_RealGDP_Growth_9]; [M_RealGDP_Growth_12]; [M_RealGDP_Growth_6]

### CONCLUSION: Select the top 3 variables:
###               [M_RealGDP_Growth_12]; [M_RealGDP_Growth_9]; [M_RealGDP_Growth_6]


# --- 5.4 Which lag order is the best for: [M_RealIncome_Growth]
# - Initialize variables to be tested
vars <- c("M_RealIncome_Growth", "M_RealIncome_Growth_1 ", "M_RealIncome_Growth_2", "M_RealIncome_Growth_3", 
          "M_RealIncome_Growth_6", "M_RealIncome_Growth_9", "M_RealIncome_Growth_12")

# - Single-factor modelling results
table_Assess <- aicTable_LS(datCredit_train, vars, TimeDef=c("Cox_Discrete","LossRate_Real"), genPath=genObjPath, modelType="gaussian")
# Goodness-of-fit
table_Assess[order(AIC)]
### RESULTS: Best AIC-results: [M_RealIncome_Growth_12]; [M_RealIncome_Growth_9]; [M_RealIncome_Growth_6]
# Discriminatory power (in-sample)
table_Assess[order(C, decreasing=T)]
### RESULTS: Best C-statistics: [M_RealIncome_Growth_12]; [M_RealIncome_Growth_9]; [M_RealIncome_Growth_6]

### CONCLUSION: Select the top 3 variables (preference given to C-statistic rankings):
###               [M_RealIncome_Growth_12]; [M_RealIncome_Growth_9]; [M_RealIncome_Growth_6]


# --- 5.5 Which lag order is the best for: [M_DTI_Growth]
# - Initialize variables to be tested
vars <- c("M_DTI_Growth", "M_DTI_Growth_1 ", "M_DTI_Growth_2", "M_DTI_Growth_3", 
          "M_DTI_Growth_6", "M_DTI_Growth_9", "M_DTI_Growth_12")

# - Single-factor modelling results
table_Assess <- aicTable_LS(datCredit_train, vars, TimeDef=c("Cox_Discrete","LossRate_Real"), genPath=genObjPath, modelType="gaussian")
# Goodness-of-fit
table_Assess[order(AIC)]
### RESULTS: Best AIC-results: [M_DTI_Growth_12]; [M_DTI_Growth_9]; [M_DTI_Growth_6]
# Discriminatory power (in-sample)
table_Assess[order(C, decreasing=T)]
### RESULTS: Best C-staistics: [M_DTI_Growth_9]; [M_DTI_Growth_12]; [M_DTI_Growth_6]

### CONCLUSION: Select the top 3 variables (preference given to C-statistic rankings):
###               [M_DTI_Growth_9]; [M_DTI_Growth_12]; [M_DTI_Growth_6] 


# --- 5.6 Which lag order is the best for: [M_Emp_Growth]
# - Initialize variables to be tested
vars <- c("M_Emp_Growth", "M_Emp_Growth_1 ", "M_Emp_Growth_2", "M_Emp_Growth_3", 
          "M_Emp_Growth_6", "M_Emp_Growth_9", "M_Emp_Growth_12")

# - Single-factor modelling results
table_Assess <- aicTable_LS(datCredit_train, vars, TimeDef=c("Cox_Discrete","LossRate_Real"), genPath=genObjPath, modelType="gaussian")
# Goodness-of-fit
table_Assess[order(AIC)]
### RESULTS: Best AIC-results: [M_Emp_Growth_6]; [M_Emp_Growth_9]; [M_Emp_Growth_3]
# Discriminatory power (in-sample)
table_Assess[order(C, decreasing=T)]
### RESULTS: Best C-statistics: [M_Emp_Growth_6]; [M_Emp_Growth_12]; [M_Emp_Growth_9]

### CONCLUSION: Select the top 3 variables (preference given to C-statistic rankings):
###               [M_Emp_Growth_6]; [M_Emp_Growth_12]; [M_Emp_Growth_9]


# --- 5.7 Combining insights: Macroeconomic variables
# - Initialize variables to be tested
vars <- c("M_Repo_Rate_9", "M_Repo_Rate_12", "M_Repo_Rate_6",
          "M_Inflation_Growth_9", "M_Inflation_Growth_6", "M_Inflation_Growth_3",
          "M_RealGDP_Growth_12", "M_RealGDP_Growth_9", "M_RealGDP_Growth_6",
          "M_RealIncome_Growth_12", "M_RealIncome_Growth_9", "M_RealIncome_Growth_6",
          "M_DTI_Growth_9", "M_DTI_Growth_6", "M_DTI_Growth_12",
          "M_Emp_Growth_6", "M_Emp_Growth_12", "M_Emp_Growth_9"
)

# - Full model | Stepwise forward selection procedure
modGLM_full <- glm(as.formula(paste("LossRate_Real ~", paste(vars, collapse = " + "))),
                   data=datCredit_train,family = gaussian(link = "identity"))
summary(modGLM_full)
evalLS(modGLM_full,datCredit_train,targetFld="LossRate_Real",modGLM_base)
### RESULTS: AIC: -27 352; R^2: 2.26%; RMSE: 20.59%; MAE: 11.97%

# - Stepwise forward selection using AIC
ptm <- proc.time() # for runtime calculations (ignore)
modGLM_step <- stepAIC(modGLM_base, scope=list(lower=~ 1, 
                                               upper=as.formula(paste("~", paste(vars, collapse = " + ")))), 
                       direction="both", k=log(datCredit_train[,.N]), maxit=50)
proc.time() - ptm # IGNORE: elapsed runtime; 7s

# - Evaluate model
summary(modGLM_step)
evalLS(modGLM_step, datCredit_train,targetFld="LossRate_Real", modGLM_base)
### RESULTS: AIC: -27 308; R^2: 2.19%; RMSE: 20.60%; MAE: 11.98%

### CONCLUSION: Select variables from automated variable selection procedure:
###             [M_DTI_Growth_12], [M_Repo_Rate_6], [M_RealGDP_Growth_12],
###             [M_RealIncome_Growth_9]; [M_DTI_Growth_6]; [M_Repo_Rate_12]




# ------ 6. Combine variables from all themes

# --- 6.1 Run an automated selection procedure on the entire set
# - Initialise set of variables
vars <- c("PrevDefaults", "g0_Delinq_Any_Aggr_Prop_Lag_5",
          "g0_Delinq_SD_6", "DefSpell_Age",
          "g0_Delinq_Any_Aggr_Prop_Lag_12", "g0_Delinq_Num", "g0_Delinq_Ave",
          "ArrearsToBalance_Aggr_Prop", "g0_Delinq_Any_Aggr_Prop_Lag_9",
          "slc_past_due_amt_imputed_med", "g0_Delinq_SD_5",
          "DefaultStatus1_Aggr_Prop_Lag_2", "DefaultStatus1_Aggr_Prop",
          "InterestRate_Margin_Aggr_Med", "DefSpell_Maturity_Aggr_Mean", "AgeToTerm_Aggr_Mean",
          "AgeToTerm", "Balance_Real_1", "Instalment_Real",
          "Principal_Real", "InterestRate_Nom",
          "M_DTI_Growth_12", "M_Repo_Rate_6", "M_RealGDP_Growth_12",
          "M_RealIncome_Growth_9", "M_DTI_Growth_6", "M_Repo_Rate_12"
)

# - Full model 
modGLM_full <- glm(as.formula(paste("LossRate_Real ~", paste(vars, collapse=" + "))),
                   data=datCredit_train,family=gaussian(link="identity"))
summary(modGLM_full);
evalLS(modGLM_full,datCredit_train,targetFld="LossRate_Real",modGLM_base)
### RESULTS: AIC: -37 573; R^2: 13.38; RMSE: 19.38%; MAE: 10.78%

# - Stepwise forward selection using AIC
ptm <- proc.time() # for runtime calculations (ignore)
modGLM_step <- stepAIC(modGLM_base, scope=list(lower=~ 1, 
                                               upper=as.formula(paste("~", paste(vars, collapse = " + ")))), 
                       direction="both", k=log(datCredit_train[,.N]), maxit=50)
proc.time() - ptm # IGNORE: elapsed runtime; 39s

# - Evaluate model
summary(modGLM_step)
evalLS(modGLM_step,datCredit_train,targetFld="LossRate_Real",modGLM_base)
### RESULTS: AIC: -37 542; R^2: 13.33; RMSE: 19.39%; MAE: 10.78%

### CONCLUSION: Proceed to model refinements with variables from automated variable selection:
###             [PrevDefaults]; [DefSpell_Maturity_Aggr_Mean]; [M_Repo_Rate_6]
###             [DefSpell_Age]; [g0_Delinq_Any_Aggr_Prop_Lag_5]; [g0_Delinq_Num];
###             [g0_Delinq_SD_6]; [g0_Delinq_Any_Aggr_Prop_Lag_12]; [slc_past_due_amt_imputed_med];
###             [Balance_Real_1]; [InterestRate_Nom]; [DefaultStatus1_Aggr_Prop_Lag_2];
###             [DefaultStatus1_Aggr_Prop]; [g0_Delinq_Any_Aggr_Prop_Lag_9]; [g0_Delinq_Ave];
###             [AgeToTerm_Aggr_Mean]; [g0_Delinq_SD_5]


# --- 6.2 Model refinements | Enriching the input space based on expert judgement - Part I
# - Remove [g0_Delinq_SD_5] since [g0_Delinq_SD_6] is already present
vars <- c("PrevDefaults", "DefSpell_Maturity_Aggr_Mean", "M_Repo_Rate_6",
          "DefSpell_Age", "g0_Delinq_Any_Aggr_Prop_Lag_5", "g0_Delinq_Num",
          "g0_Delinq_SD_6", "g0_Delinq_Any_Aggr_Prop_Lag_12", "slc_past_due_amt_imputed_med",
          "Balance_Real_1", "InterestRate_Nom", "DefaultStatus1_Aggr_Prop_Lag_2",
          "DefaultStatus1_Aggr_Prop", "g0_Delinq_Any_Aggr_Prop_Lag_9", "g0_Delinq_Ave",
          "AgeToTerm_Aggr_Mean"
)

# - Fit model
modGLM_full <- glm(as.formula(paste("LossRate_Real ~", paste(vars, collapse = " + "))),
                   data=datCredit_train,family = gaussian(link = "identity"))

# - Evaluate model
summary(modGLM_full)
evalLS(modGLM_full, datCredit_train,targetFld="LossRate_Real", modGLM_base)
### RESULTS: AIC: -37 526; R^2: 13.31; RMSE: 19.39%; MAE: 10.78%


# --- 6.2 Model refinements | Enriching the input space based on expert judgement - Part II
# - Add a lagged inflation variable
vars <- c("PrevDefaults", "DefSpell_Maturity_Aggr_Mean", "M_Repo_Rate_6",
          "DefSpell_Age", "g0_Delinq_Any_Aggr_Prop_Lag_5", "g0_Delinq_Num",
          "g0_Delinq_SD_6", "g0_Delinq_Any_Aggr_Prop_Lag_12", "slc_past_due_amt_imputed_med",
          "Balance_Real_1", "InterestRate_Nom", "DefaultStatus1_Aggr_Prop_Lag_2",
          "DefaultStatus1_Aggr_Prop", "g0_Delinq_Any_Aggr_Prop_Lag_9", "g0_Delinq_Ave",
          "AgeToTerm_Aggr_Mean", "M_Inflation_Growth_12"
)

# - Fit model
modGLM_full <- glm(as.formula(paste("LossRate_Real ~", paste(vars, collapse = " + "))),
                   data=datCredit_train,family = gaussian(link = "identity"))

# - Evaluate model
summary(modGLM_full)
evalLS(modGLM_full, datCredit_train,targetFld="LossRate_Real", modGLM_base)
### RESULTS: AIC: -37 545; R^2: 13.33; RMSE: 19.39%; MAE: 10.78%

### CONCLUSION: Select variables from automated variable selection procedure:
###             [PrevDefaults]; [DefSpell_Maturity_Aggr_Mean]; [M_Repo_Rate_6]
###             [DefSpell_Age]; [g0_Delinq_Any_Aggr_Prop_Lag_5]; [g0_Delinq_Num];
###             [g0_Delinq_SD_6]; [g0_Delinq_Any_Aggr_Prop_Lag_12]; [slc_past_due_amt_imputed_med];
###             [Balance_Real_1]; [InterestRate_Nom]; [DefaultStatus1_Aggr_Prop_Lag_2];
###             [DefaultStatus1_Aggr_Prop]; [g0_Delinq_Any_Aggr_Prop_Lag_9]; [g0_Delinq_Ave];
###             [AgeToTerm_Aggr_Mean]; [M_Inflation_Growth_12]





# ------ 9. Final model

# --- 9.1 Preliminaries
# - Confirm prepared datasets are loaded into memory
if (!exists('datCredit_train_CDH')) unpack.ffdf(paste0(genPath,"creditdata_train_CDH"), tempPath);gc()
if (!exists('datCredit_valid_CDH')) unpack.ffdf(paste0(genPath,"creditdata_valid_CDH"), tempPath);gc()

# - Filter data for default spells and first default observations
datCredit_train <- subset(datCredit_train_CDH, !is.na(DefSpell_Key) & DefSpell_Counter==1)
datCredit_valid <- subset(datCredit_valid_CDH, !is.na(DefSpell_Key) & DefSpell_Counter==1)

# - Identify where the loss rate is out of bounds and not feasible
datCredit_train <- datCredit_train[, OOB_Ind := ifelse(LossRate_Real < 0 | LossRate_Real > 1, 1,0)]
datCredit_valid <- datCredit_valid[, OOB_Ind := ifelse(LossRate_Real < 0 | LossRate_Real > 1, 1,0)]

# - Subset to exclude nonsensical loss-rates
datCredit_train <- subset(datCredit_train, OOB_Ind == 0)
datCredit_valid <- subset(datCredit_valid, OOB_Ind == 0)
### NOTE: The training- and validation datasets are reduced by ~14% due to these out-of-bound observations

# - Remove previous objects from memory
rm(datCredit_train_CDH, datCredit_valid_CDH); gc()

# - Fit an empty model to use for analyses
modGLM_base <- glm(LossRate_Real~1, data=datCredit_train, family=gaussian(link="identity"))


# --- 9.2 Fit and evaluate final model
# - Initialize variables to be tested
vars <- c("PrevDefaults", "DefSpell_Maturity_Aggr_Mean", "M_Repo_Rate_6",
          "DefSpell_Age", "g0_Delinq_Any_Aggr_Prop_Lag_5", "g0_Delinq_Num",
          "g0_Delinq_SD_6", "g0_Delinq_Any_Aggr_Prop_Lag_12", "slc_past_due_amt_imputed_med",
          "Balance_Real_1", "InterestRate_Nom", "DefaultStatus1_Aggr_Prop_Lag_2",
          "DefaultStatus1_Aggr_Prop", "g0_Delinq_Any_Aggr_Prop_Lag_9", "g0_Delinq_Ave",
          "AgeToTerm_Aggr_Mean", "M_Inflation_Growth_12"
)

# - Fit model
modGLM <- glm(as.formula(paste("LossRate_Real ~", paste(vars, collapse = " + "))),
              data=datCredit_train,family = gaussian(link = "identity"))

# - Evaluate model
summary(modGLM)
evalLS(modGLM,datCredit_train,targetFld="LossRate_Real",modGLM_base)
### RESULTS: AIC: -37 545; R^2: 13.33; RMSE: 19.39%; MAE: 10.78%


# --- 9.3 Save objects
modGLM_OneStage_Gaus <- copy(modGLM); rm(modGLM); gc()
save(modGLM_OneStage_Gaus, file=paste0(genObjPath,"OneStage_Gaus_Model.rds"))













