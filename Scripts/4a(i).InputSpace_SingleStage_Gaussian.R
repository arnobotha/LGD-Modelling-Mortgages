# ========================= INPUT SPACE: ONE-STAGE-GAUSSIAN ====================
# Divide data into thematic groups and perform data analysis on them towards
# compiling an input space for a single stage model with a
# Gaussian link function.
# ------------------------------------------------------------------------------
# PROJECT TITLE: Loss Modelling (LGD) for Residential Mortgages
# SCRIPT AUTHOR(S): Mohammed Gabru (MG), Marcel Muller
# ------------------------------------------------------------------------------
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
#   - modGLM_OneStage_Gaus | Final single stage Gaussian modeling object
# ------------------------------------------------------------------------------------------------------




# ------ 1. Preliminaries

# - Confirm prepared datasets are loaded into memory
if (!exists('datCredit_train_CS')) unpack.ffdf(paste0(genPath,"creditdata_train_CS"), tempPath);gc()
if (!exists('datCredit_valid_CS')) unpack.ffdf(paste0(genPath,"creditdata_valid_CS"), tempPath);gc()

# - Identify where the loss rate is out of bounds and not feasible
datCredit_train_CS[, OOB_Ind := ifelse(LossRate_Real < 0 | LossRate_Real > 1, 1,0)]
datCredit_valid_CS[, OOB_Ind := ifelse(LossRate_Real < 0 | LossRate_Real > 1, 1,0)]

# - Subset to exclude nonsensical loss-rates
datCredit_train <- subset(datCredit_train_CS, OOB_Ind == 0)
datCredit_valid <- subset(datCredit_valid_CS, OOB_Ind == 0)
### NOTE: The training- and validation datasets are reduced by ~14% due to these out-of-bound observations

# - Remove previous objects from memory
rm(datCredit_train_CS, datCredit_valid_CS); gc()

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
### RESULTS: Best AIC-results: g0_Delinq_SD_6 ; g0_Delinq_SD_9 ; g0_Delinq_SD_5
# Discriminatory power (in-sample)
table_Assess[order(C, decreasing=T)]
### RESULTS: Best C-staistics: g0_Delinq_SD_6 ; g0_Delinq_SD_5 ; g0_Delinq_SD_4

### CONCLUSION: Select the top 3 variables (preference given to C-statistic rankings):
###               g0_Delinq_SD_6, g0_Delinq_SD_5, g0_Delinq_SD_4


# --- 2.2 Selection of portfolio-level variables indicating the prevalence of delinquent accounts
# - Initialize variables to be tested
vars <- c("g0_Delinq_Any_Aggr_Prop", "g0_Delinq_Any_Aggr_Prop_Lag_1", "g0_Delinq_Any_Aggr_Prop_Lag_2",
          "g0_Delinq_Any_Aggr_Prop_Lag_3", "g0_Delinq_Any_Aggr_Prop_Lag_4", "g0_Delinq_Any_Aggr_Prop_Lag_5",
          "g0_Delinq_Any_Aggr_Prop_Lag_6", "g0_Delinq_Any_Aggr_Prop_Lag_9", "g0_Delinq_Any_Aggr_Prop_Lag_12" )

# - Single-factor modelling results
table_Assess <- aicTable_LS(datCredit_train, vars, TimeDef=c("Cox_Discrete","LossRate_Real"), genPath=genObjPath, modelType="gaussian")
# Goodness-of-fit
table_Assess[order(AIC)]
### RESULTS: Best AIC-results: g0_Delinq_Any_Aggr_Prop_Lag_9 ; g0_Delinq_Any_Aggr_Prop_Lag_12 ; g0_Delinq_Any_Aggr_Prop_Lag_6 
# Discriminatory power (in-sample)
table_Assess[order(C, decreasing=T)]
### RESULTS: Best C-statistics: g0_Delinq_Any_Aggr_Prop_Lag_12 ; g0_Delinq_Any_Aggr_Prop_Lag_9 ; g0_Delinq_Any_Aggr_Prop_Lag_6 

### CONCLUSION: Select the top 3 variables (preference given to C-statistic rankings):
# g0_Delinq_Any_Aggr_Prop_Lag_12 ; g0_Delinq_Any_Aggr_Prop_Lag_9 ; g0_Delinq_Any_Aggr_Prop_Lag_6 


# --- 2.3 Selection of portfolio-level variables indicating the prevalence of defaulted accounts
# - Initialize variables to be tested
vars <- c("DefaultStatus1_Aggr_Prop", "DefaultStatus1_Aggr_Prop_Lag_1", "DefaultStatus1_Aggr_Prop_Lag_2",
          "DefaultStatus1_Aggr_Prop_Lag_3", "DefaultStatus1_Aggr_Prop_Lag_4", "DefaultStatus1_Aggr_Prop_Lag_5",
          "DefaultStatus1_Aggr_Prop_Lag_6", "DefaultStatus1_Aggr_Prop_Lag_9", "DefaultStatus1_Aggr_Prop_Lag_12")

# - Single-factor modelling results
table_Assess <- aicTable_LS(datCredit_train, vars, TimeDef=c("Cox_Discrete","LossRate_Real"), genPath=genObjPath, modelType="gaussian")
# Goodness-of-fit
table_Assess[order(AIC)]
### RESULTS: Best AIC-results: DefaultStatus1_Aggr_Prop ; DefaultStatus1_Aggr_Prop_Lag_1 ; DefaultStatus1_Aggr_Prop_Lag_2 
# Discriminatory power (in-sample)
table_Assess[order(C, decreasing=T)]
### RESULTS: Best C-statistics: DefaultStatus1_Aggr_Prop ; DefaultStatus1_Aggr_Prop_Lag_1 ; DefaultStatus1_Aggr_Prop_Lag_2 

### CONCLUSION: Select the top 3 variables (preference given to C-statistic rankings):
# DefaultStatus1_Aggr_Prop ; DefaultStatus1_Aggr_Prop_Lag_1 ; DefaultStatus1_Aggr_Prop_Lag_2 


# --- 2.4 Selection of other portfolio-level delinquency-themed variables
# - Initialize variables to be tested
vars <- c("g0_Delinq_Ave", "ArrearsToBalance_Aggr_Prop", "CuringEvents_Aggr_Prop")

# - Single-factor modelling results
table_Assess <- aicTable_LS(datCredit_train, vars, TimeDef=c("Cox_Discrete","LossRate_Real"), genPath=genObjPath, modelType="gaussian")
# Goodness-of-fit
table_Assess[order(AIC)]
### RESULTS: Best AIC-results: g0_Delinq_Ave ; ArrearsToBalance_Aggr_Prop ; CuringEvents_Aggr_Prop 
# Discriminatory power (in-sample)
table_Assess[order(C, decreasing=T)]
### RESULTS: Best C-statistics: g0_Delinq_Ave ; ArrearsToBalance_Aggr_Prop ; CuringEvents_Aggr_Prop 

### CONCLUSION: Select the top 3 variables (preference given to C-statistic rankings):
# g0_Delinq_Ave ; ArrearsToBalance_Aggr_Prop ; CuringEvents_Aggr_Prop 


# --- 2.5 Selection of account-level delinquency-themed variables
# - Initialize variables to be tested
vars <- c("g0_Delinq_Lag_1", "slc_acct_arr_dir_3_Change_Ind",
          "g0_Delinq_Num", "slc_acct_arr_dir_3", "slc_acct_roll_ever_24_imputed_mean",
          "Arrears", "PrevDefaults","TimeInDelinqState",
          "slc_past_due_amt_imputed_med", "slc_curing_ind","TimeInDefSpell")

# - Single-factor modelling results
table_Assess <- aicTable_LS(datCredit_train, vars, TimeDef=c("Cox_Discrete","LossRate_Real"), genPath=genObjPath, modelType="gaussian")
# Goodness-of-fit
table_Assess[order(AIC)]
### RESULTS: Best AIC-results: PrevDefaults ; slc_curing_ind ; g0_Delinq_Lag_1 ; slc_past_due_amt_imputed_med ; 
# Arrears ; slc_acct_arr_dir_3 ; TimeInDelinqState 
# Discriminatory power (in-sample)
table_Assess[order(C, decreasing=T)]
### RESULTS: Best C-statistics (>60%): PrevDefaults ; Arrears ; slc_past_due_amt_imputed_med ; TimeInDelinqState ; 
# g0_Delinq_Lag_1 ; slc_acct_arr_dir_3 ; slc_curing_ind ; slc_acct_arr_dir_3_Change_Ind ; g0_Delinq_Num 

### CONCLUSION: Select the top x variables (preference given to C-statistic rankings):
### PrevDefaults ; Arrears ; slc_past_due_amt_imputed_med ; TimeInDelinqState ; 
# g0_Delinq_Lag_1 ; slc_acct_arr_dir_3 ; slc_curing_ind ; slc_acct_arr_dir_3_Change_Ind ; g0_Delinq_Num 


# --- 2.6 Combining insights
# - Initialize variables to be tested
vars <- c("g0_Delinq_SD_6", "g0_Delinq_SD_5", "g0_Delinq_SD_4",
          "g0_Delinq_Any_Aggr_Prop_Lag_12", "g0_Delinq_Any_Aggr_Prop_Lag_9", "g0_Delinq_Any_Aggr_Prop_Lag_6", 
          "DefaultStatus1_Aggr_Prop", "DefaultStatus1_Aggr_Prop_Lag_1", "DefaultStatus1_Aggr_Prop_Lag_2",
          "PrevDefaults", "slc_past_due_amt_imputed_med", "TimeInDelinqState", "g0_Delinq_Lag_1", "slc_acct_arr_dir_3",
          "slc_acct_arr_dir_3_Change_Ind", "g0_Delinq_Num")

# - Full model
modGLM_full <- glm(as.formula(paste("LossRate_Real ~", paste(vars, collapse = " + "))),
                   data=datCredit_train,family = gaussian(link = "identity"))
summary(modGLM_full)
evalLS(modGLM_full, datCredit_train, targetFld="LossRate_Real", modGLM_base)
### RESULTS: AIC: -38842; R^2: 14.65%; RMSE: 19.24%; MAE: 10.65%

# - Stepwise forward selection using BIC
ptm <- proc.time() # for runtime calculations (ignore)
modGLM_step <- stepAIC(modGLM_base, scope=list(lower=~ 1, 
                                               upper=as.formula(paste("~", paste(vars, collapse = " + ")))), 
                       direction="both", k=log(datCredit_train[,.N]), maxit=50)
proc.time() - ptm # IGNORE: elapsed runtime; 16s

# - Evaluate model
summary(modGLM_step)
evalLS(modGLM_step, datCredit_train,targetFld="LossRate_Real", modGLM_base)
### RESULTS: AIC: -38841.73; R^2: 14.64%; RMSE: 19.24%; MAE: 10.65%

### CONCLUSION: Select variables from automated selection procedure:
# PrevDefaults, slc_acct_arr_dir_3, slc_past_due_amt_imputed_med, DefaultStatus1_Aggr_Prop_Lag_1,
# g0_Delinq_Lag_1, TimeInDelinqState, g0_Delinq_Num, g0_Delinq_SD_4, g0_Delinq_SD_6, g0_Delinq_Any_Aggr_Prop_Lag_9
### NOTE: Can remove one of g0_Delinq_SD_4 or g0_Delinq_SD_6 since they contain the same type of information



# ------ 3. Other portfolio-level variables

# --- 3.1 Median portfolio-level interest rates
# - Initialize variables to be tested
vars <- c("InterestRate_Margin_Aggr_Med", "InterestRate_Margin_Aggr_Med_1", "InterestRate_Margin_Aggr_Med_2",
          "InterestRate_Margin_Aggr_Med_3", "InterestRate_Margin_Aggr_Med_9")

# - Single-factor modelling results
table_Assess <- aicTable_LS(datCredit_train, vars, TimeDef=c("Cox_Discrete","LossRate_Real"), genPath=genObjPath, modelType="gaussian")
# Goodness-of-fit
table_Assess[order(AIC)]
### RESULTS: Best AIC-results: InterestRate_Margin_Aggr_Med_2 ; InterestRate_Margin_Aggr_Med_3 ; InterestRate_Margin_Aggr_Med_1 
# Discriminatory power (in-sample)
table_Assess[order(C, decreasing=T)]
### RESULTS: Best C-statistics: InterestRate_Margin_Aggr_Med_3 ; InterestRate_Margin_Aggr_Med_2 ; InterestRate_Margin_Aggr_Med_1 

### CONCLUSION: Select the top 3 variables (preference given to C-statistic rankings):
# InterestRate_Margin_Aggr_Med_3 ; InterestRate_Margin_Aggr_Med_2 ; InterestRate_Margin_Aggr_Med_1 


# --- 3.2 Other portfolio-level (non-delinquency) variables
# - Initialize variables to be tested
vars <- c("InstalmentToBalance_Aggr_Prop", "AgeToTerm_Aggr_Mean", "DefSpell_Maturity_Aggr_Mean", "NewLoans_Aggr_Prop")

# - Single-factor modelling results
table_Assess <- aicTable_LS(datCredit_train, vars, TimeDef=c("Cox_Discrete","LossRate_Real"), genPath=genObjPath, modelType="gaussian")
# Goodness-of-fit
table_Assess[order(AIC)]
### RESULTS: Best AIC-results: NewLoans_Aggr_Prop ; DefSpell_Maturity_Aggr_Mean ; InstalmentToBalance_Aggr_Prop ; AgeToTerm_Aggr_Mean 
# Discriminatory power (in-sample)
table_Assess[order(C, decreasing=T)]
### RESULTS: Best C-statistics: NewLoans_Aggr_Prop ; DefSpell_Maturity_Aggr_Mean ; AgeToTerm_Aggr_Mean ; InstalmentToBalance_Aggr_Prop 

### CONCLUSION: Select the top x variables (preference given to C-statistic rankings):
# NewLoans_Aggr_Prop ; DefSpell_Maturity_Aggr_Mean ; AgeToTerm_Aggr_Mean ; InstalmentToBalance_Aggr_Prop 


# --- 3.3 Combining insights
# - Initialize variables to be tested
vars <- c("InterestRate_Margin_Aggr_Med_3", "InterestRate_Margin_Aggr_Med_2", "InterestRate_Margin_Aggr_Med_1", 
          "NewLoans_Aggr_Prop", "DefSpell_Maturity_Aggr_Mean", "AgeToTerm_Aggr_Mean", "InstalmentToBalance_Aggr_Prop")

# - Full model
modGLM_full <- glm(as.formula(paste("LossRate_Real ~", paste(vars, collapse = " + "))),
                   data=datCredit_train,family = gaussian(link = "identity"))
summary(modGLM_full)
evalLS(modGLM_full,datCredit_train,targetFld="LossRate_Real",modGLM_base)
### RESULTS: AIC: -26822.96; R^2: 1.6%; RMSE: 20.65%; MAE: 12.15%

# - Stepwise forward selection using BIC
ptm <- proc.time() # for runtime calculations (ignore)
modGLM_step <- stepAIC(modGLM_base, scope=list(lower=~ 1, 
                                               upper=as.formula(paste("~", paste(vars, collapse = " + ")))), 
                       direction="both", k=log(datCredit_train[,.N]), maxit=50)
proc.time() - ptm # IGNORE: elapsed runtime; 16s

# - Evaluate model
summary(modGLM_step)
evalLS(modGLM_step, datCredit_train,targetFld="LossRate_Real", modGLM_base)
### RESULTS: AIC: -26825.11; R^2: 1.6%; RMSE: 20.66%; MAE: 12.16%

### CONCLUSION: Select variables from automated selection procedure:
# NewLoans_Aggr_Prop, AgeToTerm_Aggr_Mean, DefSpell_Maturity_Aggr_Mean, InterestRate_Margin_Aggr_Med_3




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
# Absolute correlations of  90%  found for  [Principal_Real] and [Balance_Real_1] 
# Absolute correlations of  86%  found for  [Principal]  and  [Balance_Real_1] 
# Absolute correlations of  89%  found for  [Principal_Real] and [Balance_1] 
# Absolute correlations of  89%  found for  [Principal] and [Balance_1]
# Absolute correlations of  98%  found for  [Balance_Real_1] and [Balance_1]
# Absolute correlations of  86%  found for  [Principal_Real] and [Instalment_Real] 
# Absolute correlations of  83%  found for  [Principal] and [Instalment_Real]
# Absolute correlations of  91%  found for  [Balance_Real_1] and [Instalment_Real]
# Absolute correlations of  89%  found for  [Balance_Real_1] and [Instalment_Real]
# Absolute correlations of  75%  found for  [Balance_Real_1] and [BalanceToPrincipal_1]
# Absolute correlations of  71%  found for  [Balance_1] and [BalanceToPrincipal_1]
# Absolute correlations of  61%  found for  [Instalment_Real] and [BalanceToPrincipal_1] 
# Absolute correlations of  -61%  found for  [AgeToTerm]  and  [BalanceToPrincipal_1]

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
### RESULTS: Best AIC-results: pmnt_method_grp ; InterestRate_Margin_imputed_mean ; Balance_Real_1 ; Principal_Real 
# Discriminatory power (in-sample)
table_Assess[order(C, decreasing=T)]
### RESULTS: Best C-statistics (>55%): pmnt_method_grp ; Balance_Real_1 ; AgeToTerm ; Principal_Real ; Instalment_Real ; InterestRate_Margin_imputed_mean 

### CONCLUSION: Select the top 5 variables:
#  pmnt_method_grp ; Balance_Real_1 ; AgeToTerm ; Principal_Real ; Instalment_Real ; InterestRate_Margin_imputed_mean 




# ------ 5. Macroeconomic variables

# --- 5.1 Which lag order is the best for: M_Repo_Rate
# - Initialize variables to be tested
vars <- c("M_Repo_Rate", "M_Repo_Rate_1 ", "M_Repo_Rate_2", "M_Repo_Rate_3", "M_Repo_Rate_6", "M_Repo_Rate_9", "M_Repo_Rate_12")

# - Single-factor modelling results
table_Assess <- aicTable_LS(datCredit_train, vars, TimeDef=c("Cox_Discrete","LossRate_Real"), genPath=genObjPath, modelType="gaussian")
# Goodness-of-fit
table_Assess[order(AIC)]
### RESULTS: Best AIC-results: M_Repo_Rate_12 ; M_Repo_Rate_9 ; M_Repo_Rate_6 
# Discriminatory power (in-sample)
table_Assess[order(C, decreasing=T)]
### RESULTS: Best C-statistics: M_Repo_Rate_12 ; M_Repo_Rate_9 ; M_Repo_Rate_6 

### CONCLUSION: Select the top 3 variables (preference given to C-statistic rankings):
# M_Repo_Rate_12 ; M_Repo_Rate_9 ; M_Repo_Rate_6 


# --- 5.2 Which lag order is the best for: [M_Inflation_Growth]
# - Initialize variables to be tested
vars <- c("M_Inflation_Growth", "M_Inflation_Growth_1 ", "M_Inflation_Growth_2", "M_Inflation_Growth_3", 
          "M_Inflation_Growth_6", "M_Inflation_Growth_9", "M_Inflation_Growth_12")

# - Single-factor modelling results
# Goodness-of-fit
table_Assess <- aicTable_LS(datCredit_train, vars, TimeDef=c("Cox_Discrete","LossRate_Real"), genPath=genObjPath, modelType="gaussian")
# Goodness-of-fit
table_Assess[order(AIC)]
### RESULTS: Best AIC-results: M_Inflation_Growth_12 ; M_Inflation_Growth_9 ; M_Inflation_Growth_6 
# Discriminatory power (in-sample)
table_Assess[order(C, decreasing=T)]
### RESULTS: Best C-statistics: M_Inflation_Growth_12 ; M_Inflation_Growth_9 ; M_Inflation_Growth_6 

### CONCLUSION: Select the top 3 variables (preference given to C-statistic rankings):
# M_Inflation_Growth_12 ; M_Inflation_Growth_9 ; M_Inflation_Growth_6 


# --- 5.3 Which lag order is the best for: [M_RealGDP_Growth]
# - Initialize variables to be tested
vars <- c("M_RealGDP_Growth", "M_RealGDP_Growth_1 ", "M_RealGDP_Growth_2", "M_RealGDP_Growth_3", 
          "M_RealGDP_Growth_6", "M_RealGDP_Growth_9", "M_RealGDP_Growth_12")

# - Single-factor modelling results
# Goodness-of-fit
table_Assess <- aicTable_LS(datCredit_train, vars, TimeDef=c("Cox_Discrete","LossRate_Real"), genPath=genObjPath, modelType="gaussian")
# Goodness-of-fit
table_Assess[order(AIC)]
### RESULTS: Best AIC-results: M_RealGDP_Growth_12 ; M_RealGDP_Growth_9 ; M_RealGDP_Growth 
# Discriminatory power (in-sample)
table_Assess[order(C, decreasing=T)]
### RESULTS: Best C-statistics: M_RealGDP_Growth ; M_RealGDP_Growth_12 ; M_RealGDP_Growth_1  

### CONCLUSION: Select the top 3 variables (preference given to C-statistic rankings):
# M_RealGDP_Growth ; M_RealGDP_Growth_12 ; M_RealGDP_Growth_1  


# --- 5.4 Which lag order is the best for: [M_RealIncome_Growth]
# - Initialize variables to be tested
vars <- c("M_RealIncome_Growth", "M_RealIncome_Growth_1 ", "M_RealIncome_Growth_2", "M_RealIncome_Growth_3", 
          "M_RealIncome_Growth_6", "M_RealIncome_Growth_9", "M_RealIncome_Growth_12")

# - Single-factor modelling results
table_Assess <- aicTable_LS(datCredit_train, vars, TimeDef=c("Cox_Discrete","LossRate_Real"), genPath=genObjPath, modelType="gaussian")
# Goodness-of-fit
table_Assess[order(AIC)]
### RESULTS: Best AIC-results: M_RealIncome_Growth ; M_RealIncome_Growth_1 ; M_RealIncome_Growth_2 
# Discriminatory power (in-sample)
table_Assess[order(C, decreasing=T)]
### RESULTS: Best C-statistics: M_RealIncome_Growth ; M_RealIncome_Growth_1 ; M_RealIncome_Growth_2 

### CONCLUSION: Select the top 3 variables (preference given to C-statistic rankings):
# M_RealIncome_Growth ; M_RealIncome_Growth_1 ; M_RealIncome_Growth_2 


# --- 5.5 Which lag order is the best for: [M_DTI_Growth]
# - Initialize variables to be tested
vars <- c("M_DTI_Growth", "M_DTI_Growth_1 ", "M_DTI_Growth_2", "M_DTI_Growth_3", 
          "M_DTI_Growth_6", "M_DTI_Growth_9", "M_DTI_Growth_12")

# - Single-factor modelling results
table_Assess <- aicTable_LS(datCredit_train, vars, TimeDef=c("Cox_Discrete","LossRate_Real"), genPath=genObjPath, modelType="gaussian")
# Goodness-of-fit
table_Assess[order(AIC)]
### RESULTS: Best AIC-results: M_DTI_Growth_12 ; M_DTI_Growth_9 ; M_DTI_Growth_6 
# Discriminatory power (in-sample)
table_Assess[order(C, decreasing=T)]
### RESULTS: Best C-staistics: M_DTI_Growth_12 ; M_DTI_Growth_9 ; M_DTI_Growth_6 

### CONCLUSION: Select the top 3 variables (preference given to C-statistic rankings):
# M_DTI_Growth_12 ; M_DTI_Growth_9 ; M_DTI_Growth_6 


# --- 5.6 Which lag order is the best for: [M_Emp_Growth]
# - Initialize variables to be tested
vars <- c("M_Emp_Growth", "M_Emp_Growth_1 ", "M_Emp_Growth_2", "M_Emp_Growth_3", 
          "M_Emp_Growth_6", "M_Emp_Growth_9", "M_Emp_Growth_12")

# - Single-factor modelling results
table_Assess <- aicTable_LS(datCredit_train, vars, TimeDef=c("Cox_Discrete","LossRate_Real"), genPath=genObjPath, modelType="gaussian")
# Goodness-of-fit
table_Assess[order(AIC)]
### RESULTS: Best AIC-results: M_Emp_Growth ; M_Emp_Growth_1 ; M_Emp_Growth_2 
# Discriminatory power (in-sample)
table_Assess[order(C, decreasing=T)]
### RESULTS: Best C-statistics: M_Emp_Growth ; M_Emp_Growth_1 ; M_Emp_Growth_2 

### CONCLUSION: Select the top 3 variables (preference given to C-statistic rankings):
# M_Emp_Growth ; M_Emp_Growth_1 ; M_Emp_Growth_2 


# --- 5.7 Combining insights: Macroeconomic variables
# - Initialize variables to be tested
vars <- c("M_Repo_Rate_12", "M_Repo_Rate_9", "M_Repo_Rate_6",
          "M_Inflation_Growth_12", "M_Inflation_Growth_9", "M_Inflation_Growth_6", 
          "M_RealGDP_Growth", "M_RealGDP_Growth_12", "M_RealGDP_Growth_1",
          "M_RealIncome_Growth", "M_RealIncome_Growth_1", "M_RealIncome_Growth_2",
          "M_DTI_Growth_12", "M_DTI_Growth_9", "M_DTI_Growth_6",
          "M_Emp_Growth", "M_Emp_Growth_1", "M_Emp_Growth_2" 
)

# - Full model | Stepwise forward selection procedure
modGLM_full <- glm(as.formula(paste("LossRate_Real ~", paste(vars, collapse = " + "))),
                   data=datCredit_train,family = gaussian(link = "identity"))
summary(modGLM_full)
evalLS(modGLM_full,datCredit_train,targetFld="LossRate_Real",modGLM_base)
### RESULTS: AIC: -27165.17; R^2: 2.05%; RMSE: 20.61%; MAE: 12.02%

# - Stepwise forward selection using AIC
ptm <- proc.time() # for runtime calculations (ignore)
modGLM_step <- stepAIC(modGLM_base, scope=list(lower=~ 1, 
                                               upper=as.formula(paste("~", paste(vars, collapse = " + ")))), 
                       direction="both", k=log(datCredit_train[,.N]), maxit=50)
proc.time() - ptm # IGNORE: elapsed runtime; 7s

# - Evaluate model
summary(modGLM_step)
evalLS(modGLM_step, datCredit_train,targetFld="LossRate_Real", modGLM_base)
### RESULTS: AIC: -27155.92; R^2: 2.02%; RMSE: 20.61%; MAE: 12.03%

### CONCLUSION: Select variables from automated variable selection procedure:
# M_Repo_Rate_12, M_DTI_Growth_12, M_DTI_Growth_6, M_Repo_Rate_6, M_RealGDP_Growth_12,
# M_Emp_Growth, M_RealIncome_Growth, M_RealGDP_Growth, M_RealIncome_Growth_2
### NOTE: M_Repo_Rate_6 and M_DTI_Growth_6 and M_RealIncome_Growth_2 and M_RealGDP_Growth_12
# removed due to lower c-statistic.
### Final selection:
# M_Repo_Rate_12, M_DTI_Growth_12, M_Emp_Growth, M_RealIncome_Growth, M_RealGDP_Growth



# ------ 6. Combine variables from all themes

# --- 6.1 Run an automated selection procedure on the entire set
# - Initialise set of variables
vars <- c("PrevDefaults", "slc_acct_arr_dir_3", "slc_past_due_amt_imputed_med", 
          "DefaultStatus1_Aggr_Prop_Lag_1", "g0_Delinq_Lag_1", "TimeInDelinqState", "g0_Delinq_Num",
          "g0_Delinq_SD_6", "g0_Delinq_Any_Aggr_Prop_Lag_9",
          "NewLoans_Aggr_Prop", "AgeToTerm_Aggr_Mean", "DefSpell_Maturity_Aggr_Mean",
          "InterestRate_Margin_Aggr_Med_3", "g0_Delinq_Ave", "ArrearsToBalance_Aggr_Prop",
          # Judgmentally added for testing
          "AgeToTerm", "Balance_Real_1", "Instalment_Real",
          "Principal_Real", "InterestRate_Nom",
          # Macroeconomic variables
          "M_Repo_Rate_12", "M_DTI_Growth_12", "M_Emp_Growth", "M_RealIncome_Growth", "M_RealGDP_Growth"
)

# - Full model 
modGLM_full <- glm(as.formula(paste("LossRate_Real ~", paste(vars, collapse=" + "))),
                   data=datCredit_train,family=gaussian(link="identity"))
summary(modGLM_full);
evalLS(modGLM_full,datCredit_train,targetFld="LossRate_Real",modGLM_base)
### RESULTS: AIC: -39381.76; R^2: 15.21; RMSE: 19.18%; MAE: 10.72%

# - Stepwise forward selection using AIC
ptm <- proc.time() # for runtime calculations (ignore)
modGLM_step <- stepAIC(modGLM_base, scope=list(lower=~ 1, 
                                               upper=as.formula(paste("~", paste(vars, collapse = " + ")))), 
                       direction="both", k=log(datCredit_train[,.N]), maxit=50)
proc.time() - ptm # IGNORE: elapsed runtime; 39s

# - Evaluate model
summary(modGLM_step)
evalLS(modGLM_step,datCredit_train,targetFld="LossRate_Real",modGLM_base)
### RESULTS: AIC: -39370.83; R^2: 15.18; RMSE: 19.18%; MAE: 10.72%

### CONCLUSION: Proceed to model refinements with variables from automated variable selection:
# PrevDefaults, slc_acct_arr_dir_3, slc_past_due_amt_imputed_med, 
# g0_Delinq_Ave, g0_Delinq_Lag_1, g0_Delinq_Num, g0_Delinq_SD_6,
# TimeInDelinqState, InterestRate_Nom, DefSpell_Maturity_Aggr_Mean, Principal_Real, 
# AgeToTerm_Aggr_Mean, AgeToTerm,
# M_Repo_Rate_12, M_Emp_Growth, M_RealGDP_Growth


# --- 6.2 Model refinements | Adjusting the input space based on expert judgement
vars <- c("PrevDefaults", "slc_acct_arr_dir_3", "slc_past_due_amt_imputed_med",
          "g0_Delinq_Ave", "g0_Delinq_Lag_1", "g0_Delinq_Num", "g0_Delinq_SD_6",
          "TimeInDelinqState", "InterestRate_Nom", "DefSpell_Maturity_Aggr_Mean", "Principal_Real", 
          "AgeToTerm_Aggr_Mean", "AgeToTerm", "DefaultStatus1_Aggr_Prop",
          "M_Repo_Rate_12", "M_Emp_Growth", "M_RealGDP_Growth"
)

# - Fit model
modGLM_full <- glm(as.formula(paste("LossRate_Real ~", paste(vars, collapse = " + "))),
                   data=datCredit_train,family = gaussian(link = "identity"))

# - Evaluate model
summary(modGLM_full)
evalLS(modGLM_full, datCredit_train,targetFld="LossRate_Real", modGLM_base)
### RESULTS: AIC: -37 265; R^2: 13.03; RMSE: 19.42%; MAE: 10.71%





# ------ 9. Final model

# --- 9.1 Preliminaries
# - Confirm prepared datasets are loaded into memory
if (!exists('datCredit_train_CS')) unpack.ffdf(paste0(genPath,"creditdata_train_CS"), tempPath);gc()
if (!exists('datCredit_valid_CS')) unpack.ffdf(paste0(genPath,"creditdata_valid_CS"), tempPath);gc()

# Sampling window start for model fitting purposes
sDateStr <- as.Date("2008-01-31")
sDateStp <- as.Date("2019-12-31")

# - Filter data for default spells and first default observations
datCredit_train <- subset(datCredit_train_CS, Date >= sDateStr & Date <= sDateStp )
datCredit_valid <- subset(datCredit_valid_CS, Date >= sDateStr & Date <= sDateStp)

# - Identify where the loss rate is out of bounds and not feasible
datCredit_train[, OOB_Ind := ifelse(LossRate_Real < 0 | LossRate_Real > 1, 1,0)]
datCredit_valid[, OOB_Ind := ifelse(LossRate_Real < 0 | LossRate_Real > 1, 1,0)]

# - Subset to exclude nonsensical loss-rates
datCredit_train <- subset(datCredit_train, OOB_Ind == 0)
datCredit_valid <- subset(datCredit_valid, OOB_Ind == 0)
### NOTE: The training- and validation datasets are reduced by ~14% due to these out-of-bound observations

# - Remove previous objects from memory
rm(datCredit_train_CS, datCredit_valid_CS); gc()

# - Fit an empty model to use for analyses
modGLM_base <- glm(LossRate_Real~1, data=datCredit_train, family=gaussian(link="identity"))


# --- 9.2 Fit and evaluate final model
# - Initialize and curate final list variables
# Removed AgeToTerm_Aggr_Mean since it proved insignificant once restricted sampling window is imposed
# Removed M_RealGDP_Growth since it proved insignificant once restricted sampling window is imposed

### AB: Test following
# Added TimeInDefSpell
vars <- c("PrevDefaults", "slc_acct_arr_dir_3", "slc_past_due_amt_imputed_med",
          "g0_Delinq_Ave", "g0_Delinq_Lag_1", "g0_Delinq_Num", "g0_Delinq_SD_6",
          "TimeInDelinqState", "InterestRate_Nom", "DefSpell_Maturity_Aggr_Mean", "Principal_Real", 
          "AgeToTerm", "DefaultStatus1_Aggr_Prop",
          "M_Repo_Rate_12", "M_Emp_Growth"
)

# - Fit and evaluate model
modGLM <- glm(as.formula(paste("LossRate_Real ~", paste(vars, collapse = " + "))),
              data=datCredit_train,family = gaussian(link = "identity"))
summary(modGLM)
evalLS(modGLM,datCredit_train,targetFld="LossRate_Real",modGLM_base)
### RESULTS: AIC: -26910.01; R^2: 16.19; RMSE: 19.89%; MAE: 11.35%


# --- 9.3 Save objects
modGLM_OneStage_Gaus <- copy(modGLM); rm(modGLM); gc()
saveRDS(modGLM_OneStage_Gaus, file=paste0(genObjPath,"OneStage_Gaus_Model.rds"))

# - Cleanup
cleanEnviron()
