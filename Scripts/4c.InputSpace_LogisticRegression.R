# ========================= INPUT SPACE: LOGISTIC REGRESSION ===================
# Divide data into thematic groups and perform data analysis on them towards
# compiling an input space for an advanced logistic regression model.
# ------------------------------------------------------------------------------
# PROJECT TITLE: Loss Modelling (LGD) for FNB Mortgages
# SCRIPT AUTHOR(S): Mohammed Gabru (MG), Marcel Muller (MM), Dr Arno Botha (AB)
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
#   - modLR | Final logistic regression modeling object
# ------------------------------------------------------------------------------

# ------ 1. Preliminaries
# --- 1.1. Load data
# - Confirm prepared datasets are loaded into memory
if (!exists('datCredit_train_CDH')) unpack.ffdf(paste0(genPath,"creditdata_train_CDH"), tempPath);gc()
if (!exists('datCredit_valid_CDH')) unpack.ffdf(paste0(genPath,"creditdata_valid_CDH"), tempPath);gc()

# - Use only default spells and first default observations
datCredit_train <- datCredit_train_CDH[!is.na(DefSpell_Key) & DefSpell_Counter==1,]
datCredit_valid <- datCredit_valid_CDH[!is.na(DefSpell_Key) & DefSpell_Counter==1,]

# - Remove previous objects from memory
rm(datCredit_train_CDH, datCredit_valid_CDH); gc()


# --- 1.2 Prepare data
# - Create an indicator to identify a write-off
datCredit_train[, DefSpell_Event := ifelse(DefSpellResol_Type_Hist=="WOFF", 1, 0)]
datCredit_valid[, DefSpell_Event := ifelse(DefSpellResol_Type_Hist=="WOFF", 1, 0)]


# --- 1.3 Fit an empty model to use in analytics
modLR_base <- glm(DefSpell_Event ~ 1, data=datCredit_train, family="binomial")




# ------ 2. Delinquency-themed variables

# --- 2.1 Selection of delinquency volatility variables
# - Initialize variables to be tested
vars <- c("g0_Delinq_SD_4", "g0_Delinq_SD_5", "g0_Delinq_SD_6", "g0_Delinq_SD_9", "g0_Delinq_SD_12")

# - Single-factor modelling results
# Goodness-of-fit
aicTable(datCredit_train, vars, TimeDef=c("Cox_Discrete","DefSpell_Event"), genPath=genObjPath, modelType="Cox_Discrete")
### RESULTS: Best AIC-results: [g0_Delinq_SD_12]; [g0_Delinq_SD_9]; [g0_Delinq_SD_5]
# Discriminatory power (in-sample)
concTable(datCredit_train, datCredit_valid, vars, TimeDef=c("Cox_Discrete","DefSpell_Event"), genPath=genObjPath, modelType="Cox_Discrete")
### RESULTS: Best Harrell's C-statistics: [g0_Delinq_SD_12]; [g0_Delinq_SD_5]; [g0_Delinq_SD_6]

### Conclusion: Select top three variables (preference given to C-statistic rankings):
###               [g0_Delinq_SD_12]; [g0_Delinq_SD_5]; [g0_Delinq_SD_6]


# --- 2.2 Selection of portfolio-level variables indicating the prevalence of delinquent accounts
# - Set initial variables to be tested
vars <- c("g0_Delinq_Any_Aggr_Prop", "g0_Delinq_Any_Aggr_Prop_Lag_1", "g0_Delinq_Any_Aggr_Prop_Lag_2",
          "g0_Delinq_Any_Aggr_Prop_Lag_3", "g0_Delinq_Any_Aggr_Prop_Lag_4", "g0_Delinq_Any_Aggr_Prop_Lag_5",
          "g0_Delinq_Any_Aggr_Prop_Lag_6", "g0_Delinq_Any_Aggr_Prop_Lag_9", "g0_Delinq_Any_Aggr_Prop_Lag_12")

# - Single-factor modelling results
# Goodness-of-fit
aicTable(datCredit_train, vars, TimeDef=c("Cox_Discrete","DefSpell_Event"), genPath=genObjPath, modelType="Cox_Discrete")
### RESULTS: Best AIC-results: [g0_Delinq_Any_Aggr_Prop_12]; [g0_Delinq_Any_Aggr_Prop_Lag_9]; [g0_Delinq_Any_Aggr_Prop_Lag_6]
# Discriminatory power (in-sample)
concTable(datCredit_train, datCredit_valid, vars, TimeDef=c("Cox_Discrete","DefSpell_Event"), genPath=genObjPath, modelType="Cox_Discrete")
### RESULTS: Best C-statistics: [g0_Delinq_Any_Aggr_Prop_Lag_12]; [g0_Delinq_Any_Aggr_Prop_9]; [g0_Delinq_Any_Aggr_Prop_Lag_6]

### Conclusion: Select top three variabels:
###               [g0_Delinq_Any_Aggr_Prop_Lag_12]; [g0_Delinq_Any_Aggr_Prop_9]; [g0_Delinq_Any_Aggr_Prop_Lag_6]


# --- 2.3 Selection of portfolio-level variables indicating the prevalence of defaulted accounts
# - Set initial variables to be tested
vars <- c("DefaultStatus1_Aggr_Prop", "DefaultStatus1_Aggr_Prop_Lag_1", "DefaultStatus1_Aggr_Prop_Lag_2",
          "DefaultStatus1_Aggr_Prop_Lag_3", "DefaultStatus1_Aggr_Prop_Lag_4", "DefaultStatus1_Aggr_Prop_Lag_5",
          "DefaultStatus1_Aggr_Prop_Lag_6", "DefaultStatus1_Aggr_Prop_Lag_9", "DefaultStatus1_Aggr_Prop_Lag_12" )

# - Single-factor modelling results
# Goodness-of-fit
aicTable(datCredit_train, vars, TimeDef=c("Cox_Discrete","DefSpell_Event"), genPath=genObjPath, modelType="Cox_Discrete")
### RESULTS: Best AIC-results: [DefaultStatus1_Aggr_Prop]; [DefaultStatus1_Aggr_Prop_Lag_1]; [DefaultStatus1_Aggr_Prop_Lag_2]
# Discriminatory power (in-sample)
concTable(datCredit_train, datCredit_valid, vars, TimeDef=c("Cox_Discrete","DefSpell_Event"), genPath=genObjPath, modelType="Cox_Discrete")
### RESULTS: Best C-statistics: [DefaultStatus1_Aggr_Prop]; [DefaultStatus1_Aggr_Prop_Lag_1]; [DefaultStatus1_Aggr_Prop_Lag_12]

### CONCLUSION: Select top three variables (preference given to C-statistic rankings):
###               [DefaultStatus1_Aggr_Prop]; [DefaultStatus1_Aggr_Prop_Lag_1]; [DefaultStatus1_Aggr_Prop_Lag_12]


# --- 2.4 Selection of other portfolio-level delinquency-themed variables
# - Set initial variables to be tested
vars <- c("g0_Delinq_Ave", "ArrearsToBalance_1_Aggr_Prop", "CuringEvents_Aggr_Prop")

# - Single-factor modelling results
# Goodness-of-fit
aicTable(datCredit_train, vars, TimeDef=c("Cox_Discrete","DefSpell_Event"), genPath=genObjPath, modelType="Cox_Discrete")
### RESULTS: Best AIC-results: [CuringEvents_Aggr_Prop]; [g0_Delinq_Ave]; [ArrearsToBalance_1_Aggr_Prop]
# Discriminatory power (in-sample)
concTable(datCredit_train, datCredit_valid, vars, TimeDef=c("Cox_Discrete","DefSpell_Event"), genPath=genObjPath, modelType="Cox_Discrete")
### RESULTS: Best C-statistics: [CuringEvents_Aggr_Prop]; [g0_Delinq_Ave]; [ArrearsToBalance_1_Aggr_Prop]

### Conclusion: All are significant, therefore include all three


# --- 2.5 Selection of account-level delinquency-themed variables
# - Set initial variables to be tested
vars <- c("DefSpell_Age", "g0_Delinq_Lag_1", "slc_acct_arr_dir_3_Change_Ind",
          "g0_Delinq_Num", "slc_acct_arr_dir_3", "slc_acct_roll_ever_24_imputed_mean", "Arrears", "PrevDefaults","TimeInDelinqState",
          "slc_past_due_amt_imputed_med", "slc_curing_ind", "DefSpell_Num_binned")

# - Single-factor modelling results
# Goodness-of-fit
aicTable(datCredit_train, vars, TimeDef=c("Cox_Discrete","DefSpell_Event"), genPath=genObjPath, modelType="Cox_Discrete")
### RESULTS: Best AIC-results: [PrevDefaults]; [g0_Delinq_Num]; [DefSpell_Age]; [DefSpell_Num_binned]; [slc_past_due_amt_imputed_med]; [slc_acct_arr_dir_3]
# Discriminatory power (in-sample)
concTable(datCredit_train, datCredit_valid, vars, TimeDef=c("Cox_Discrete","DefSpell_Event"), genPath=genObjPath, modelType="Cox_Discrete")
### RESULTS: Best AIC-results: [PrevDefaults]; [DefSpell_Age]; [g0_Delinq_Num]; [Arrears]; [DefSpell_Num_binned]; [slc_acct_roll_ever_24_imputed_mean];
###                            [slc_past_due_amt_imputed_med]; [slc_acct_arr_dir_3]

### CONCLUSION: Differences are pronounced between the AICs and C-statistics; preference given to C-statistic in final choice
###             Select top 5 variables (preference given to C-statistic rankings):
###               [PrevDefaults]; [DefSpell_Age]; [g0_Delinq_Num]; [Arrears]; [DefSpell_Num_binned]


# --- 2.6 Combining insights: delinquency-themed variables
# - Set initial variables to be tested
vars <- c("g0_Delinq_SD_12", "g0_Delinq_SD_5", "g0_Delinq_SD_6",
          "DefaultStatus1_Aggr_Prop_Lag_12", "DefaultStatus1_Aggr_Prop_Lag_9" , "DefaultStatus1_Aggr_Prop_Lag_6",
          "CuringEvents_Aggr_Prop", "g0_Delinq_Ave", "ArrearsToBalance_1_Aggr_Prop",
          "PrevDefaults", "DefSpell_Age", "g0_Delinq_Num", "Arrears", "DefSpell_Num_binned")

# - Full model | Stepwise forward selection procedure
modLR_full <- glm(as.formula(paste("DefSpell_Event ~", paste(vars, collapse = " + "))),
                  data=datCredit_train, family="binomial")
summary(modLR_full);
evalLR(modLR_full, modLR_base, datCredit_train, targetFld="DefSpell_Event", predClass=1)
### RESULTS: AIC:   70 839; McFadden R^2:  29.21%; AUC:  85.75%

ptm <- proc.time() # for runtime calculations (ignore)
modLR_step <- stepAIC(modLR_base, scope=list(lower=~ 1, 
                                             upper=as.formula(paste("~", paste(vars, collapse=" + ")))), 
                      direction="both", k=log(datCredit_train[,.N]), maxit=50)
summary(modLR_step)
evalLR(modLR_step, modLR_base, datCredit_train, targetFld="DefSpell_Event", predClass=1)
proc.time() - ptm # IGNORE: elapsed runtime; 1m
### RESULTS: AIC:   70 840; McFadden R^2:  25.93%; AUC:  85.74%

# - Domain expertise
### CONCLUSION: Remove [g0_Delinq_Any_Aggr_Prop_Lag_9] since [g0_Delinq_Any_Aggr_Prop_Lag_12] is already present
### CONCLUSION: Remove [g0_Delinq_SD_6] since [g0_Delinq_SD_12] is already present

### CONCLUSION: Refine selection from automated selection with expert judgement

# - Final variables (expert judgement)
vars <- c("g0_Delinq_SD_12",
          "DefaultStatus1_Aggr_Prop_Lag_12",
          "CuringEvents_Aggr_Prop", "g0_Delinq_Ave", "ArrearsToBalance_1_Aggr_Prop",
          "PrevDefaults", "DefSpell_Age", "g0_Delinq_Num", "Arrears", "DefSpell_Num_binned")
  
modLR <- glm( as.formula(paste("DefSpell_Event ~", paste(vars, collapse = " + "))),
              data=datCredit_train, family="binomial")
summary(modLR);
evalLR(modLR, modLR_base, datCredit_train, targetFld="DefSpell_Event", predClass=1)
### RESULTS: AIC:   71 775; McFadden R^2:  25.21%; AUC:  85.44%.

### CONCLUSION: Select:
###               [g0_Delinq_SD_12]; [DefaultStatus1_Aggr_Prop_Lag_12];
###               [CuringEvents_Aggr_Prop]; [g0_Delinq_Ave]; [ArrearsToBalance_1_Aggr_Prop];
###               [PrevDefaults]; [DefSpell_Age]; [g0_Delinq_Num] [Arrears]; [DefSpell_Num_binned]




# ------ 3. Other portfolio-level variables
# --- 3.1 Selection of the portfolio median interest rate margin
# - Set initial variables to be tested
vars <- c("InterestRate_Margin_Aggr_Med", "InterestRate_Margin_Aggr_Med_1", "InterestRate_Margin_Aggr_Med_2",
          "InterestRate_Margin_Aggr_Med_3", "InterestRate_Margin_Aggr_Med_9")

# - Single-factor modelling results
# Goodness-of-fit
aicTable(datCredit_train, vars, TimeDef=c("Cox_Discrete","DefSpell_Event"), genPath=genObjPath, modelType="Cox_Discrete")
### RESULTS: Best AIC-results: [InterestRate_Margin_Aggr_Med]; [InterestRate_Margin_Aggr_Med_1]; [InterestRate_Margin_Aggr_Med_2]
# Discriminatory power (in-sample)
concTable(datCredit_train, datCredit_valid, vars, TimeDef=c("Cox_Discrete","DefSpell_Event"), genPath=genObjPath, modelType="Cox_Discrete")
### RESULTS: Best C-statistics: [InterestRate_Margin_Aggr_Med]; [InterestRate_Margin_Aggr_Med_1]; [InterestRate_Margin_Aggr_Med_2];

### Conclusion: Select top three variables:
###               [InterestRate_Margin_Aggr_Med];, [InterestRate_Margin_Aggr_Med_1]; [InterestRate_Margin_Aggr_Med_2]


# --- 3.2 Selection of other portfolio-level, non-delinquency themed, variables
# - Set initial variables to be tested
vars <- c("InstalmentToBalance_1_Aggr_Prop", "AgeToTerm_Aggr_Mean", "DefSpell_Maturity_Aggr_Mean", "NewLoans_Aggr_Prop")

# - Single-factor modelling results
# Goodness-of-fit
aicTable(datCredit_train, vars, TimeDef=c("Cox_Discrete","DefSpell_Event"), genPath=genObjPath, modelType="Cox_Discrete")
### RESULTS: Best AIC-results: [AgeToTerm_Aggr_Mean]; [InstalmentToBalance_1_Aggr_Prop]; [DefSpell_Maturity_Aggr_Mean]; [NewLoans_Aggr_Prop]
# Discriminatory power (in-sample)
concTable(datCredit_train, datCredit_valid, vars, TimeDef=c("Cox_Discrete","DefSpell_Event"), genPath=genObjPath, modelType="Cox_Discrete")
### RESULTS: Best AIC-results: [AgeToTerm_Aggr_Mean]; [InstalmentToBalance_1_Aggr_Prop]; [DefSpell_Maturity_Aggr_Mean]; [NewLoans_Aggr_Prop]

### CONCLUSION: Select top three variables:
###               [AgeToTerm_Aggr_Mean]; [InstalmentToBalance_1_Aggr_Prop]; [DefSpell_Maturity_Aggr_Mean]


# --- 3.3 Combining insights
# - Set initial variables to be tested
vars <- c("InterestRate_Margin_Aggr_Med", "InterestRate_Margin_Aggr_Med_1", "InterestRate_Margin_Aggr_Med_2",
          "AgeToTerm_Aggr_Mean", "InstalmentToBalance_1_Aggr_Prop", "DefSpell_Maturity_Aggr_Mean")

# - Full model
modLR_full <- glm(as.formula(paste("DefSpell_Event ~", paste(vars, collapse = " + "))),
                  data=datCredit_train, family="binomial")
summary(modLR_full)
evalLR(modLR_full, modLR_base, datCredit_train, targetFld="DefSpell_Event", predClass=1)
### RESULTS: AIC: 94 464; McFadden R^2: 5.63%; AUC: 66.64%.

# - Stepwise forward selection using BIC
ptm <- proc.time() # for runtime calculations (ignore)
modLR_step <- stepAIC(modLR_base, scope = list(lower = ~ 1, 
                                                upper = as.formula(paste("~", paste(vars, collapse = " + ")))), 
                      direction = "both", k=log(datCredit_train[,.N]), maxit=50)
proc.time() - ptm # IGNORE: elapsed runtime; 1m
summary(modLR_step)
evalLR(modLR_step, modLR_base, datCredit_train, targetFld="DefSpell_Event", predClass=1)
### RESULTS: AIC:   90 413; McFadden R^2: 5.62%; AUC: 66.64%

### CONCLUSION: Select variables from automated variable selection:
###             [InterestRate_Margin_Aggr_Med_2]; [InstalmentToBalance_1_Aggr_Prop]




# ------ 4. Account-level variables
# --- 4.1 Selection of non-delinquency related account-level variables
# - Set initial variables to be tested
vars <- c("Principal_Real", "Principal", "InterestRate_Margin_imputed_mean", 
          "Balance_Real_1", "Balance_1", "Instalment_Real", "InterestRate_Nom", "AgeToTerm",
          "BalanceToPrincipal_1", "slc_acct_pre_lim_perc_imputed_med")

# - Correlation analysis towards obtaining clusters of correlated variables
corrAnalysis(datCredit_train, vars, corrThresh = 0.6, method = 'spearman')
### RESULTS:
###   Absolute correlations of  96%  found for  [Principal_Real] and [Principal] 
###   Absolute correlations of  92%  found for  [Principal_Real] and [Balance_Real_1] 
###   Absolute correlations of  87%  found for  [Principal] and [Balance_Real_1]
###   Absolute correlations of  90%  found for  [Principal_Real] and [Balance_1] 
###   Absolute correlations of  91%  found for  [Principal] and [Balance_1] 
###   Absolute correlations of  97%  found for  [Balance_Real_1] and [Balance_1] 
###   Absolute correlations of  91%  found for  [Principal_Real] and [Instalment_Real] 
###   Absolute correlations of  86%  found for  [Principal] and [Instalment_Real] 
###   Absolute correlations of  96%  found for  [Balance_Real_1] and [Instalment_Real] 
###   Absolute correlations of  92%  found for  [Balance_1] and [Instalment_Real] 
###   Absolute correlations of  -60%  found for  [Balance_Real_1] and [AgeToTerm] 
###   Absolute correlations of  76%  found for  [Balance_Real_1] and [BalanceToPrincipal_1] 
###   Absolute correlations of  69%  found for  [Balance_1] and [BalanceToPrincipal_1] 
###   Absolute correlations of  67%  found for  [Instalment_Real] and [BalanceToPrincipal_1] 
###   Absolute correlations of  -65%  found for  [AgeToTerm] and [BalanceToPrincipal_1]

# - Initialize variables to be tested
### NOTE: - Inflation adjusted variables preferred over their non-adjusted counterparts
###       - Principal, balance, and instalment all kept regardless of their correlations, each contains unique information
vars <- c("Principal_Real", "InterestRate_Margin_imputed_mean", "pmnt_method_grp",
          "Balance_Real_1", "InterestRate_Nom",
          "BalanceToPrincipal_1", "slc_acct_pre_lim_perc_imputed_med")

# - Single-factor modelling results
# Goodness-of-fit
aicTable(datCredit_train, vars, TimeDef=c("Cox_Discrete","DefSpell_Event"), genPath=genObjPath, modelType="Cox_Discrete")
### RESULTS: Best AIC-results: [Balance_Real_1]; [InterestRate_Nom]; [Principal_Real]; [pmnt_method_grp]; [InterestRate_Margin_imputed_mean];
###                            [slc_acct_pre_lim_perc_imputed_med]; [BalanceToPrincipal_1]
# Discriminatory power (in-sample)
concTable(datCredit_train, datCredit_valid, vars, TimeDef=c("Cox_Discrete","DefSpell_Event"), genPath=genObjPath, modelType="Cox_Discrete")
### RESULTS: Best C-statistics: [Balance_Real_1]; [Principal_Real]; [InterestRate_Nom]; [BalanceToPrincipal_1]; [pmnt_method_grp];
###                            [InterestRate_Margin_imputed_mean]; [slc_acct_pre_lim_perc_imputed_med]

### CONCLUSION: Select top five variables (preference given to C-statistic rankings):
###               [Balance_Real_1]; [Principal_Real]; [InterestRate_Nom];
###               [BalanceToPrincipal_1]; [pmnt_method_grp]




# ------ 5. Macroeconomic variables
# --- 5.1 Which lag order is the best for: [M_Repo_Rate]
# - Set initial variables to be tested
vars <- c("M_Repo_Rate", "M_Repo_Rate_1 ", "M_Repo_Rate_2", "M_Repo_Rate_3", "M_Repo_Rate_6", "M_Repo_Rate_9", "M_Repo_Rate_12")

# - Single-factor modelling results
# Goodness-of-fit
aicTable(datCredit_train, vars, TimeDef=c("Cox_Discrete","DefSpell_Event"), genPath=genObjPath, modelType="Cox_Discrete")
### RESULTS: Best AIC-results: [M_Repo_Rate_3]; [M_Repo_Rate_2]; [M_Repo_Rate_1] 
# Discriminatory power (in-sample)
concTable(datCredit_train, datCredit_valid, vars, TimeDef=c("Cox_Discrete","DefSpell_Event"), genPath=genObjPath, modelType="Cox_Discrete")
### RESULTS: Best C-statistics: [M_Repo_Rate_3]; [M_Repo_Rate_6]; [M_Repo_Rate_2] 

### CONCLUSION: Select top three variables (preference given to C-statistic rankings):
###               [M_Repo_Rate_3]; [M_Repo_Rate_6]; [M_Repo_Rate_2] 


# --- 5.2 Which lag order is the best for: [M_Inflation_Growth]
# - Set initial variables to be tested
vars <- c("M_Inflation_Growth", "M_Inflation_Growth_1 ", "M_Inflation_Growth_2", "M_Inflation_Growth_3", 
          "M_Inflation_Growth_6", "M_Inflation_Growth_9", "M_Inflation_Growth_12")

# - Single-factor modelling results
# Goodness-of-fit
aicTable(datCredit_train, vars, TimeDef=c("Cox_Discrete","DefSpell_Event"), genPath=genObjPath, modelType="Cox_Discrete")
### RESULTS: Best AIC-results: [M_Inflation_Growth_3]; [M_Inflation_Growth_2]; [M_Inflation_Growth_1]
# Discriminatory power (in-sample)
concTable(datCredit_train, datCredit_valid, vars, TimeDef=c("Cox_Discrete","DefSpell_Event"), genPath=genObjPath, modelType="Cox_Discrete")
### results: Best C-statistics: [M_Inflation_Growth_6]; [M_Inflation_Growth_3]; [M_Inflation_Growth_1]

### CONCLUSION: Select top three variables (preference given to C-statistic rankings):
###               [M_Inflation_Growth_6]; [M_Inflation_Growth_3]; [M_Inflation_Growth_1]
### MM: I changed this selection from the previous one: [M_Inflation_Growth_2]; [M_Inflation_Growth_3]; [M_Inflation_Growth_1]


# --- 5.3 Which lag order is the best for: [M_RealGDP_Growth]
# - Set initial variables to be tested
vars <- c("M_RealGDP_Growth", "M_RealGDP_Growth_1 ", "M_RealGDP_Growth_2", "M_RealGDP_Growth_3", 
          "M_RealGDP_Growth_6", "M_RealGDP_Growth_9", "M_RealGDP_Growth_12")

# - Single-factor modelling results
# Goodness-of-fit
aicTable(datCredit_train, vars, TimeDef=c("Cox_Discrete","DefSpell_Event"), genPath=genObjPath, modelType="Cox_Discrete")
### RESULTS: Best AIC-results: [M_RealGDP_Growth_12]; [M_RealGDP_Growth_9]; [M_RealGDP_Growth_6]
# Discriminatory power (in-sample)
concTable(datCredit_train, datCredit_valid, vars, TimeDef=c("Cox_Discrete","DefSpell_Event"), genPath=genObjPath, modelType="Cox_Discrete")
### RESULTS: Best C-statistics: [M_RealGDP_Growth_12]; [M_RealGDP_Growth_9]; [M_RealGDP_Growth_6]

### CONCLUSION: Select top three variables:
###               [M_RealGDP_Growth_6]; [M_RealGDP_Growth_12]; [M_RealGDP_Growth_9]


# --- 5.4 Which lag order is the best for: [M_RealIncome_Growth]
# - Set initial variables to be tested
vars <- c("M_RealIncome_Growth", "M_RealIncome_Growth_1 ", "M_RealIncome_Growth_2", "M_RealIncome_Growth_3", 
          "M_RealIncome_Growth_6", "M_RealIncome_Growth_9", "M_RealIncome_Growth_12")

# - Single-factor modelling results
# Goodness-of-fit
aicTable(datCredit_train, vars, TimeDef=c("Cox_Discrete","DefSpell_Event"), genPath=genObjPath, modelType="Cox_Discrete")
### RESULTS: Best AIC-results: [M_RealIncome_Growth_12]; [M_RealIncome_Growth_9]; [M_RealIncome_Growth_6] 
# Discriminatory power (in-sample)
concTable(datCredit_train, datCredit_valid, vars, TimeDef=c("Cox_Discrete","DefSpell_Event"), genPath=genObjPath, modelType="Cox_Discrete")
### RESULTS: Best C-statistics: [M_RealIncome_Growth_12]; [M_RealIncome_Growth_9]; [M_RealIncome_Growth_6]

### CONCLUSION: Select top three variables (preference given C-statistic rankings):
###               [M_RealIncome_Growth_12]; [M_RealIncome_Growth_9]; [M_RealIncome_Growth_6]


# --- 5.5 Which lag order is the best for: [M_DTI_Growth]
# - Set initial variables to be tested
vars <- c("M_DTI_Growth", "M_DTI_Growth_1 ", "M_DTI_Growth_2", "M_DTI_Growth_3", 
          "M_DTI_Growth_6", "M_DTI_Growth_9", "M_DTI_Growth_12")

# - Single-factor modelling results
# Goodness-of-fit
aicTable(datCredit_train, vars, TimeDef=c("Cox_Discrete","DefSpell_Event"), genPath=genObjPath, modelType="Cox_Discrete")
### RESULTS: Best AIC-results: [M_DTI_Growth_3]; [M_DTI_Growth_6]; [M_DTI_Growth_2]
# Discriminatory power (in-sample)
concTable(datCredit_train, datCredit_valid, vars, TimeDef=c("Cox_Discrete","DefSpell_Event"), genPath=genObjPath, modelType="Cox_Discrete")
### RESULTS: Best C-statistics: [M_DTI_Growth_3]; [M_DTI_Growth_6]; [M_DTI_Growth_2]

### CONCLUSION: Select top three variables:
###               [M_DTI_Growth_3]; [M_DTI_Growth_6]; [M_DTI_Growth_2]


# --- 5.6 Which lag order is the best for: [M_Emp_Growth]
# - Set initial variables to be tested
vars <- c("M_Emp_Growth", "M_Emp_Growth_1 ", "M_Emp_Growth_2", "M_Emp_Growth_3", 
          "M_Emp_Growth_6", "M_Emp_Growth_9", "M_Emp_Growth_12")

# - Single-factor modelling results
# Goodness-of-fit
aicTable(datCredit_train, vars, TimeDef=c("Cox_Discrete","DefSpell_Event"), genPath=genObjPath, modelType="Cox_Discrete")
### RESULTS: Best AIC-results: [M_Emp_Growth_12]; [M_Emp_Growth_9]; [M_Emp_Growth_6]
# Discriminatory power (in-sample)
concTable(datCredit_train, datCredit_valid, vars, TimeDef=c("Cox_Discrete","DefSpell_Event"), genPath=genObjPath, modelType="Cox_Discrete")
### RESULTS: Best C-statistics: [M_Emp_Growth_12]; [M_Emp_Growth_9]; [M_Emp_Growth_6]

### CONCLUSION: Select top three variables:
###               [M_Emp_Growth_12]; [M_Emp_Growth_9]; [M_Emp_Growth_6]


# --- 7.7 Combining insights: Macroeconomic variables
# - Initialize variables to be tested
vars <- c("M_Repo_Rate_3", "M_Repo_Rate_6", "M_Repo_Rate_2", 
          "M_Inflation_Growth_6", "M_Inflation_Growth_3", "M_Inflation_Growth_1",
          "M_RealGDP_Growth_6", "M_RealGDP_Growth_12", "M_RealGDP_Growth_9",
          "M_RealIncome_Growth_12", "M_RealIncome_Growth_9", "M_RealIncome_Growth_2",
          "M_DTI_Growth_3", "M_DTI_Growth_6", "M_DTI_Growth_2",
          "M_Emp_Growth_12", "M_Emp_Growth_9", "M_Emp_Growth_6")

# - Full model | Stepwise forward selection procedure
modLR_full <- glm( as.formula(paste("DefSpell_Event ~", paste(vars, collapse = " + "))),
                   data=datCredit_train, family="binomial")
summary(modLR_full);
evalLR(modLR_full, modLR_base, datCredit_train, targetFld="DefSpell_Event", predClass=1)
### RESULTS: AIC:  93 775; McFadden R^2:  6.28%; AUC:  67.24%

# - Stepwise forward selection using BIC
ptm <- proc.time() # for runtime calculations (ignore)
modLR_step <- stepAIC(modLR_base, scope = list(lower = ~ 1, 
                                                upper = as.formula(paste("~", paste(vars, collapse = " + ")))), 
                      direction = "both", k=log(datCredit_train[,.N]), maxit=50)
proc.time() - ptm # IGNORE: elapsed runtime; 1m

# - Evaluate model
summary(modLR_step)
evalLR(modLR_step, modLR_base, datCredit_train, targetFld="DefSpell_Event", predClass=1)
### RESULTS: AIC:   93 983;   McFadden R^2:  6.06%; AUC:  66.98%

### CONCLUSION: Select variables from automated variable selection:
###               [M_Repo_Rate_2]; [M_Repo_Rate_6];
###               [M_Inflation_Growth_3]; [M_Inflation_Growth_6];
###               [M_RealGDP_Growth_9]; [M_RealGDP_Growth_12];
###               [M_RealIncome_Growth_9]; [M_RealIncome_Growth_12];
###               [M_DTI_Growth_2]; [M_DTI_Growth_3]; [M_DTI_Growth_6]
###               [M_Emp_Growth_6]; [M_Emp_Growth_9]




# ------ 6. Combine variables from all themes

# --- 6.1 Run an automated selection procedure on the entire set
# - Initialise set of variables
vars <- c("g0_Delinq_SD_12", "DefaultStatus1_Aggr_Prop_Lag_12",
          "CuringEvents_Aggr_Prop", "g0_Delinq_Ave", "ArrearsToBalance_1_Aggr_Prop",
          "PrevDefaults", "DefSpell_Age", "g0_Delinq_Num", "Arrears", "DefSpell_Num_binned",
          "InterestRate_Margin_Aggr_Med_2", "InstalmentToBalance_1_Aggr_Prop",
          "Balance_Real_1", "Principal_Real", "InterestRate_Nom",
          "BalanceToPrincipal_1", "pmnt_method_grp",
          "M_Repo_Rate_2", "M_Repo_Rate_6",
          "M_Inflation_Growth_3", "M_Inflation_Growth_6",
          "M_RealGDP_Growth_9", "M_RealGDP_Growth_12",
          "M_RealIncome_Growth_9", "M_RealIncome_Growth_12",
          "M_DTI_Growth_2", "M_DTI_Growth_3", "M_DTI_Growth_6",
          "M_Emp_Growth_6", "M_Emp_Growth_9")

# - Full model
modLR_full <- glm( as.formula(paste("DefSpell_Event ~", paste(vars, collapse = " + "))),
                   data=datCredit_train, family="binomial")
summary(modLR_full)
evalLR(modLR_full, modLR_base, datCredit_train, targetFld="DefSpell_Event", predClass=1)
### RESULTS: AIC: 67 526; McFadden R^2: 32.56%; AUC: 87.36%

# - Stepwise forward selection using BIC
ptm <- proc.time() # for runtime calculations (ignore)
modLR_step <- stepAIC(modLR_base, scope = list(lower = ~ 1, 
                                               upper = as.formula(paste("~", paste(vars, collapse = " + ")))), 
                      direction = "both", k=log(datCredit_train[,.N]), maxit=50)
proc.time() - ptm # IGNORE: elapsed runtime; 3m

# - Evaluate model
summary(modLR_step)
evalLR(modLR_step, modLR_base, datCredit_train, targetFld="DefSpell_Event", predClass=1)
### RESULTS: AIC: 67 678; McFadden R^2: 32.38%; AUC: 87.25%

### CONCLUSION: Select variables from automated variable selection:
###               [PrevDefaults]; [M_Repo_Rate_6]; [DefSpell_Num_binned];
###               [ArrearsToBalance_1_Aggr_Prop]; [Balance_Real_1]; [DefSpell_Age];
###               [pmnt_method_grp]; [InterestRate_Margin_Aggr_Med_2]; [InterestRate_Nom];
###               [DefaultStatus1_Aggr_Prop_Lag_12]; [g0_Delinq_Ave]; [M_DTI_Growth_6];
###               [Principal_Real]; [M_RealGDP_Growth_12]; [g0_Delinq_Num];
###               [M_Repo_Rate_2]; [M_Inflation_Growth_3]


# --- 6.2 Model refinements | Adjusting the input space based on expert judgement
### NOTE: The following changes have been made:
### Removed: 1) [M_Repo_Rate_6]
vars <- c("PrevDefaults", "DefSpell_Num_binned",
          "ArrearsToBalance_1_Aggr_Prop", "Balance_Real_1", 'DefSpell_Age',
          "pmnt_method_grp", "InterestRate_Margin_Aggr_Med_2", 'InterestRate_Nom',
          "DefaultStatus1_Aggr_Prop_Lag_12", "g0_Delinq_Ave", "M_DTI_Growth_6",
          "Principal_Real", "M_RealGDP_Growth_12", "g0_Delinq_Num",
          "M_Repo_Rate_2", "M_Inflation_Growth_3")

# - Fit model
modLR <- glm( as.formula(paste("DefSpell_Event ~", paste(vars, collapse = " + "))),
              data=datCredit_train, family="binomial")
summary(modLR)
evalLR(modLR, modLR_base, datCredit_train, targetFld="DefSpell_Event", predClass=1)
### RESULTS: AIC: 67 873; McFadden R^2: 32.18%; AUC: 87.11%




# ------ 7. Final 

# - Confirm prepared datasets are loaded into memory
if (!exists('datCredit_train_CDH')) unpack.ffdf(paste0(genPath,"creditdata_train_CDH"), tempPath);gc()
if (!exists('datCredit_valid_CDH')) unpack.ffdf(paste0(genPath,"creditdata_valid_CDH"), tempPath);gc()

# - Use only default spells
datCredit_train <- datCredit_train_CDH[!is.na(DefSpell_Key) & DefSpell_Counter==1,]
datCredit_valid <- datCredit_valid_CDH[!is.na(DefSpell_Key) & DefSpell_Counter==1,]

# - Add an event indicator
datCredit_train[, DefSpell_Event := ifelse(DefSpellResol_Type_Hist=="WOFF", 1, 0)]
datCredit_valid[, DefSpell_Event := ifelse(DefSpellResol_Type_Hist=="WOFF", 1, 0)]

# - Remove previous objects from memory
rm(datCredit_train_CDH, datCredit_valid_CDH); gc()

# - Weigh default cases heavier. as determined interactively based on calibration success (script 6e)
datCredit_train[, Weight := ifelse(DefSpell_Event==1,1,1)]
datCredit_valid[, Weight := ifelse(DefSpell_Event==1,1,1)]

# - Fit an "empty" model as a performance gain, used within some diagnostic functions
modLR_base <- glm(DefSpell_Event ~ 1, data=datCredit_train, family="binomial")

# - Final variables
vars <- c("PrevDefaults", "DefSpell_Num_binned",
          "ArrearsToBalance_1_Aggr_Prop", "Balance_Real_1", 'DefSpell_Age',
          "pmnt_method_grp", "InterestRate_Margin_Aggr_Med_2", 'InterestRate_Nom',
          "DefaultStatus1_Aggr_Prop_Lag_12", "g0_Delinq_Ave", "M_DTI_Growth_6",
          "Principal_Real", "M_RealGDP_Growth_12", "g0_Delinq_Num",
          "M_Repo_Rate_2", "M_Inflation_Growth_3")
  
# - Fit model
modLR <- glm( as.formula(paste("DefSpell_Event ~", paste(vars, collapse = " + "))),
              data=datCredit_train, family="binomial")
summary(modLR)

# - Test significance of variables
# Robust (sandwich) standard errors
robust_se <- vcovHC(modLR, type="HC0")
# Summary with robust SEs
coeftest(modLR, vcov.=robust_se)
### RESULTS: All variables are significant

# - Other diagnostics
evalLR(modLR, modLR_base, datCredit_train, targetFld="DefSpell_Event", predClass=1)
### RESULTS: AIC: 67 873; McFadden R^2: 32.18%; AUC: 87.11%

# - Test goodness-of-fit using AIC-measure over single-factor models
(aicTable_LR <- aicTable(datCredit_train, vars, TimeDef=c("Cox_Discrete","DefSpell_Event"), genPath=genObjPath, modelType="Cox_Discrete"))
### RESULTS: Top variables: [Prev_Defaults]; [M_Repo_Rate_2]; [M_Repo_Rate_6]; [M_DTI_Growth_6]; [InterestRate_Margin_Aggr_Med_2];
###                         [Balance_Real_1]; [M_RealGDP_Growth_12]

# - Test accuracy using c-statistic over single-factor models
(concTable_LR <- concTable(datCredit_train, datCredit_valid, vars, TimeDef=c("Cox_Discrete","DefSpell_Event"), genPath=genObjPath, modelType="Cox_Discrete"))
### RESULTS: Top variables: [Prev_Defaults]; [DefSpell_Age]; [M_Repo_Rate_6]; [M_Repo_Rate_2]; [M_DTI_Growth_6]
###                          [InterestRate_Margin_Aggr_Med_2]; [Balance_Real_1]

# - Combine results into a single object
Table_LR <- merge(concTable_LR[,1:2], aicTable_LR, by="Variable")

GoF_CoxSnell_KS(modLR, datCredit_train, GraphInd=TRUE, ,legPos=c(0.6,0.4), panelTitle="Logistic Regression",
                fileName = paste0(genFigPath, "KS_Test_CoxSnellResiduals_Exp_CDH_LR", ".png"), dpi=280,fldLstRowInd="DefSpell_Counter")
### RESULTS: KS-statistic = 43%; Harell's c = 87.25%; AIC = 67 678

# - Save objects
# Model analytics
pack.ffdf(paste0(genObjPath,"LR_fits"), Table_LR)
# Modeling object
saveRDS(modLR, file=paste0(genObjPath,"LR_Model.rds"))
