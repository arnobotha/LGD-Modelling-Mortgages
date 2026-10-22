# ========================= INPUT SPACE: LOGISTIC REGRESSION ===================
# Divide data into thematic groups and perform data analysis on them towards
# compiling an input space for an advanced logistic regression model.
# ------------------------------------------------------------------------------
# PROJECT TITLE: Loss Modelling (LGD) for Residential Mortgages
# SCRIPT AUTHOR(S): Mohammed Gabru (MG), Marcel Muller (MM)
# ------------------------------------------------------------------------------
# -- Script dependencies:
#   - 0.Setup.R
#   - 2g.Data_Fusion2.R

# -- Inputs:
#   - datCredit_train_CDH | Training dataset prepared in script 2g
#   - datCredit_valid_CDH | Validation dataset prepared in script 2g
#
# -- Outputs:
#   - modLR | Final logistic regression modeling object
# ------------------------------------------------------------------------------



# ============ 1. Preliminaries ============

# - Confirm prepared datasets are loaded into memory
if (!exists('datCredit_train_CS')) unpack.ffdf(paste0(genPath,"creditdata_train_CS"), tempPath); gc()
if (!exists('datCredit_valid_CS')) unpack.ffdf(paste0(genPath,"creditdata_valid_CS"), tempPath); gc()

# - Fit an empty model to use in analytics
modLR_base <- glm(DefSpell_Event~1, data=datCredit_train_CS, family="binomial")




# ============ 2. Delinquency-themed variables ============

# --- 2.1 Selection of delinquency volatility variables
# - Initialize variables to be tested
vars <- c("g0_Delinq_SD_4", "g0_Delinq_SD_5", "g0_Delinq_SD_6", "g0_Delinq_SD_9", "g0_Delinq_SD_12")

# - Single-factor modelling results
# Goodness-of-fit
aicTable(datCredit_train_CS, vars, TimeDef=c("Cox_Discrete","DefSpell_Event"), genPath=genObjPath, modelType="Cox_Discrete")
### RESULTS: Best AIC-results: [g0_Delinq_SD_4]; [g0_Delinq_SD_5]; [g0_Delinq_SD_6]
# Discriminatory power (in-sample)
concTable(datCredit_train_CS, datCredit_valid_CS, vars, TimeDef=c("Cox_Discrete","DefSpell_Event"), genPath=genObjPath, modelType="Cox_Discrete")
### RESULTS: Best Harrell's C-statistics: [g0_Delinq_SD_12]; [g0_Delinq_SD_4]; [g0_Delinq_SD_9]

### Conclusion: Select top three variables (preference given to C-statistic rankings):
###               [g0_Delinq_SD_12]; [g0_Delinq_SD_4]; [g0_Delinq_SD_9]


# --- 2.2 Selection of portfolio-level variables indicating the prevalence of delinquent accounts
# - Set initial variables to be tested
vars <- c("g0_Delinq_Any_Aggr_Prop", "g0_Delinq_Any_Aggr_Prop_Lag_1", "g0_Delinq_Any_Aggr_Prop_Lag_2",
          "g0_Delinq_Any_Aggr_Prop_Lag_3", "g0_Delinq_Any_Aggr_Prop_Lag_4", "g0_Delinq_Any_Aggr_Prop_Lag_5",
          "g0_Delinq_Any_Aggr_Prop_Lag_6", "g0_Delinq_Any_Aggr_Prop_Lag_9", "g0_Delinq_Any_Aggr_Prop_Lag_12")

# - Single-factor modelling results
# Goodness-of-fit
aicTable(datCredit_train_CS, vars, TimeDef=c("Cox_Discrete","DefSpell_Event"), genPath=genObjPath, modelType="Cox_Discrete")
### RESULTS: Best AIC-results: [g0_Delinq_Any_Aggr_Prop]; [g0_Delinq_Any_Aggr_Prop_Lag_6]; [g0_Delinq_Any_Aggr_Prop_Lag_12]
# Discriminatory power (in-sample)
concTable(datCredit_train_CS, datCredit_valid_CS, vars, TimeDef=c("Cox_Discrete","DefSpell_Event"), genPath=genObjPath, modelType="Cox_Discrete")
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
aicTable(datCredit_train_CS, vars, TimeDef=c("Cox_Discrete","DefSpell_Event"), genPath=genObjPath, modelType="Cox_Discrete")
### RESULTS: Best AIC-results: [DefaultStatus1_Aggr_Prop]_12; [DefaultStatus1_Aggr_Prop_Lag_9]; [DefaultStatus1_Aggr_Prop_Lag_6]
# Discriminatory power (in-sample)
concTable(datCredit_train_CS, datCredit_valid_CS, vars, TimeDef=c("Cox_Discrete","DefSpell_Event"), genPath=genObjPath, modelType="Cox_Discrete")
### RESULTS: Best C-statistics: [DefaultStatus1_Aggr_Prop]; [DefaultStatus1_Aggr_Prop_Lag_1]; [DefaultStatus1_Aggr_Prop_Lag_2]

### CONCLUSION: Select top three variables (preference given to C-statistic rankings):
###               [DefaultStatus1_Aggr_Prop]; [DefaultStatus1_Aggr_Prop_Lag_1]; [DefaultStatus1_Aggr_Prop_Lag_12]


# --- 2.4 Selection of other portfolio-level delinquency-themed variables
# - Set initial variables to be tested
vars <- c("g0_Delinq_Ave", "ArrearsToBalance_1_Aggr_Prop", "CuringEvents_Aggr_Prop")

# - Single-factor modelling results
# Goodness-of-fit
aicTable(datCredit_train_CS, vars, TimeDef=c("Cox_Discrete","DefSpell_Event"), genPath=genObjPath, modelType="Cox_Discrete")
### RESULTS: Best AIC-results: [g0_Delinq_Ave]; [ArrearsToBalance_1_Aggr_Prop]; [CuringEvents_Aggr_Prop]
# Discriminatory power (in-sample)
concTable(datCredit_train_CS, datCredit_valid_CS, vars, TimeDef=c("Cox_Discrete","DefSpell_Event"), genPath=genObjPath, modelType="Cox_Discrete")
### RESULTS: Best C-statistics: [g0_Delinq_Ave]; [CuringEvents_Aggr_Prop]; [ArrearsToBalance_1_Aggr_Prop]

### Conclusion: All are significant, therefore include all three


# --- 2.5 Selection of account-level delinquency-themed variables
# - Set initial variables to be tested
vars <- c("TimeInDefSpell", "g0_Delinq_Lag_1", "slc_acct_arr_dir_3_Change_Ind",
          "g0_Delinq_Num", "slc_acct_arr_dir_3", "slc_acct_roll_ever_24_imputed_mean", "Arrears", "PrevDefaults","TimeInDelinqState",
          "slc_past_due_amt_imputed_med", "slc_curing_ind", "DefSpell_Num_binned")

# - Single-factor modelling results
# Goodness-of-fit
aicTable(datCredit_train_CS, vars, TimeDef=c("Cox_Discrete","DefSpell_Event"), genPath=genObjPath, modelType="Cox_Discrete")
### RESULTS: Best AIC-results: [Arrears]; [g0_Delinq_Num]; [slc_past_due_amt_imputed_med]; [TimeInDelinqState]; [slc_curing_ind]; [slc_acct_arr_dir_3]
# Discriminatory power (in-sample)
concTable(datCredit_train_CS, datCredit_valid_CS, vars, TimeDef=c("Cox_Discrete","DefSpell_Event"), genPath=genObjPath, modelType="Cox_Discrete")
### RESULTS: Best AIC-results: [PrevDefaults]; [g0_Delinq_Lag_1]; [g0_Delinq_Num]; [TimeInDefSpell]; [DefSpell_Num_binned]; [slc_acct_roll_ever_24_imputed_mean];
###                            [slc_acct_arr_dir_3]; [slc_acct_arr_dir_3_Change_Ind]

### CONCLUSION: Differences are pronounced between the AICs and C-statistics; preference given to C-statistic in final choice
###             Select top 5 variables (preference given to C-statistic rankings):
###               [PrevDefaults]; [g0_Delinq_Lag_1]; [g0_Delinq_Num]; [TimeInDefSpell]; [DefSpell_Num_binned]


# --- 2.6 Combining insights: delinquency-themed variables
# - Set initial variables to be tested
vars <- c("g0_Delinq_SD_12", "g0_Delinq_SD_4", "g0_Delinq_SD_9",
          "DefaultStatus1_Aggr_Prop", "DefaultStatus1_Aggr_Prop_Lag_1" , "DefaultStatus1_Aggr_Prop_Lag_12",
          "CuringEvents_Aggr_Prop", "g0_Delinq_Ave", "ArrearsToBalance_1_Aggr_Prop",
          "PrevDefaults", "g0_Delinq_Lag_1", "g0_Delinq_Num", "TimeInDefSpell", "DefSpell_Num_binned")

# - Full model | Stepwise forward selection procedure
modLR_full <- glm(as.formula(paste("DefSpell_Event ~", paste(vars, collapse = " + "))),
                  data=datCredit_train_CS, family="binomial")
summary(modLR_full);
evalLR(modLR_full, modLR_base, datCredit_train_CS, targetFld="DefSpell_Event", predClass=1)
### RESULTS: AIC:   8 959; McFadden R^2:  46.95%; AUC:  95.74%

ptm <- proc.time() # for runtime calculations (ignore)
modLR_step <- stepAIC(modLR_base, scope=list(lower=~ 1, 
                                             upper=as.formula(paste("~", paste(vars, collapse=" + ")))), 
                      direction="both", k=log(datCredit_train_CS[,.N]), maxit=50)
summary(modLR_step)
evalLR(modLR_step, modLR_base, datCredit_train_CS, targetFld="DefSpell_Event", predClass=1)
proc.time() - ptm # IGNORE: elapsed runtime; 1m
### RESULTS: AIC:   8 960; McFadden R^2:  46.91%; AUC:  95.73%

# - Domain expertise
### CONCLUSION: Remove [DefaultStatus1_Aggr_Prop] since [g0_Delinq_Any_Aggr_Prop_Lag_12] is already present
### CONCLUSION: Remove [g0_Delinq_SD_9] since [g0_Delinq_SD_4] is already present

### CONCLUSION: Refine selection from automated selection with expert judgement

# - Final variables (expert judgement)
vars <- colnames(modLR_step$model[,-1])
vars <- vars[!(vars %in% c("DefaultStatus1_Aggr_Prop", "g0_Delinq_SD_9"))]
modLR <- glm( as.formula(paste("DefSpell_Event ~", paste(vars, collapse = " + "))),
              data=datCredit_train_CS, family="binomial")
summary(modLR);
evalLR(modLR, modLR_base, datCredit_train_CS, targetFld="DefSpell_Event", predClass=1)
### RESULTS: AIC:   9 026; McFadden R^2:  46.50%; AUC:  95.61%.

### CONCLUSION: Select:
###               [g0_Delinq_SD_4]; [g0_Delinq_Lag_1];
###               [PrevDefaults]; [DefaultStatus1_Aggr_Prop_Lag_12]; [DefSpell_Num_binned];
###               [TimeInDefSpell]; [g0_Delinq_Ave]; [ArrearsToBalance_1_Aggr_Prop] [g0_Delinq_Num]




# ============ 3. Other portfolio-level variables ============

# --- 3.1 Selection of the portfolio median interest rate margin
# - Set initial variables to be tested
vars <- c("InterestRate_Margin_Aggr_Med", "InterestRate_Margin_Aggr_Med_1", "InterestRate_Margin_Aggr_Med_2",
          "InterestRate_Margin_Aggr_Med_3", "InterestRate_Margin_Aggr_Med_9")

# - Single-factor modelling results
# Goodness-of-fit
aicTable(datCredit_train_CS, vars, TimeDef=c("Cox_Discrete","DefSpell_Event"), genPath=genObjPath, modelType="Cox_Discrete")
### RESULTS: Best AIC-results: [InterestRate_Margin_Aggr_Med_9]; [InterestRate_Margin_Aggr_Med_3]; [InterestRate_Margin_Aggr_Med_2]
# Discriminatory power (in-sample)
concTable(datCredit_train_CS, datCredit_valid_CS, vars, TimeDef=c("Cox_Discrete","DefSpell_Event"), genPath=genObjPath, modelType="Cox_Discrete")
### RESULTS: Best C-statistics: [InterestRate_Margin_Aggr_Med_3]; [InterestRate_Margin_Aggr_Med_9]; [InterestRate_Margin_Aggr_Med_2];

### Conclusion: Select top three variables:
###               [InterestRate_Margin_Aggr_Med_2];, [InterestRate_Margin_Aggr_Med_3]; [InterestRate_Margin_Aggr_Med_9]


# --- 3.2 Selection of other portfolio-level, non-delinquency themed, variables
# - Set initial variables to be tested
vars <- c("InstalmentToBalance_1_Aggr_Prop", "AgeToTerm_Aggr_Mean", "DefSpell_Maturity_Aggr_Mean", "NewLoans_Aggr_Prop")

# - Single-factor modelling results
# Goodness-of-fit
aicTable(datCredit_train_CS, vars, TimeDef=c("Cox_Discrete","DefSpell_Event"), genPath=genObjPath, modelType="Cox_Discrete")
### RESULTS: Best AIC-results: [InstalmentToBalance_1_Aggr_Prop]; [NewLoans_Aggr_Prop]; [AgeToTerm_Aggr_Mean]; [DefSpell_Maturity_Aggr_Mean]
# Discriminatory power (in-sample)
concTable(datCredit_train_CS, datCredit_valid_CS, vars, TimeDef=c("Cox_Discrete","DefSpell_Event"), genPath=genObjPath, modelType="Cox_Discrete")
### RESULTS: Best AIC-results: [NewLoans_Aggr_Prop]; [AgeToTerm_Aggr_Mean]; [InstalmentToBalance_1_Aggr_Prop]; [DefSpell_Maturity_Aggr_Mean]

### CONCLUSION: Select top three variables:
###               [NewLoans_Aggr_Prop]; [AgeToTerm_Aggr_Mean]; [InstalmentToBalance_1_Aggr_Prop]


# --- 3.3 Combining insights
# - Set initial variables to be tested
vars <- c("InterestRate_Margin_Aggr_Med_2", "InterestRate_Margin_Aggr_Med_3", "InterestRate_Margin_Aggr_Med_9",
          "NewLoans_Aggr_Prop", "AgeToTerm_Aggr_Mean", "InstalmentToBalance_1_Aggr_Prop")

# - Full model
modLR_full <- glm(as.formula(paste("DefSpell_Event ~", paste(vars, collapse = " + "))),
                  data=datCredit_train_CS, family="binomial")
summary(modLR_full)
evalLR(modLR_full, modLR_base, datCredit_train_CS, targetFld="DefSpell_Event", predClass=1)
### RESULTS: AIC: 16 622; McFadden R^2: 1.33%; AUC: 60.79%.

# - Stepwise forward selection using BIC
ptm <- proc.time() # for runtime calculations (ignore)
modLR_step <- stepAIC(modLR_base, scope = list(lower = ~ 1, 
                                                upper = as.formula(paste("~", paste(vars, collapse = " + ")))), 
                      direction = "both", k=log(datCredit_train_CS[,.N]), maxit=50)
proc.time() - ptm # IGNORE: elapsed runtime; 1m
summary(modLR_step)
evalLR(modLR_step, modLR_base, datCredit_train_CS, targetFld="DefSpell_Event", predClass=1)
### RESULTS: AIC:   16 633; McFadden R^2: 1.22%; AUC: 60.50%

### CONCLUSION: Select variables from automated variable selection:
###             [InterestRate_Margin_Aggr_Med_9]; [InstalmentToBalance_1_Aggr_Prop]




# ============ 4. Account-level variables ============

# --- 4.1 Selection of non-delinquency related account-level variables
# - Set initial variables to be tested
vars <- c("Principal_Real", "Principal", "InterestRate_Margin_imputed_mean", 
          "Balance_Real_1", "Balance_1", "Instalment_Real", "InterestRate_Nom", "AgeToTerm",
          "BalanceToPrincipal_1", "slc_acct_pre_lim_perc_imputed_med")

# - Correlation analysis towards obtaining clusters of correlated variables
corrAnalysis(datCredit_train_CS, vars, corrThresh = 0.6, method = 'spearman')
### RESULTS:
###   Absolute correlations of  96%  found for  [Principal_Real] and [Principal] 
###   Absolute correlations of  90%  found for  [Principal_Real] and [Balance_Real_1] 
###   Absolute correlations of  85%  found for  [Principal] and [Balance_Real_1]
###   Absolute correlations of  89%  found for  [Principal_Real] and [Balance_1] 
###   Absolute correlations of  89%  found for  [Principal] and [Balance_1] 
###   Absolute correlations of  98%  found for  [Balance_Real_1] and [Balance_1] 
###   Absolute correlations of  87%  found for  [Principal_Real] and [Instalment_Real] 
###   Absolute correlations of  83%  found for  [Principal] and [Instalment_Real] 
###   Absolute correlations of  91%  found for  [Balance_Real_1] and [Instalment_Real] 
###   Absolute correlations of  89%  found for  [Balance_1] and [Instalment_Real] 
###   Absolute correlations of  -60%  found for  [Balance_Real_1] and [AgeToTerm] 
###   Absolute correlations of  76%  found for  [Balance_Real_1] and [BalanceToPrincipal_1] 
###   Absolute correlations of  70%  found for  [Balance_1] and [BalanceToPrincipal_1] 
###   Absolute correlations of  61%  found for  [Instalment_Real] and [BalanceToPrincipal_1] 
###   Absolute correlations of  -62%  found for  [AgeToTerm] and [BalanceToPrincipal_1]

# - Initialize variables to be tested
### NOTE: - Inflation adjusted variables preferred over their non-adjusted counterparts
###       - Principal, balance, and instalment all kept regardless of their correlations, each contains unique information
vars <- c("Principal_Real", "InterestRate_Margin_imputed_mean", "pmnt_method_grp",
          "Balance_Real_1", "InterestRate_Nom",
          "BalanceToPrincipal_1", "slc_acct_pre_lim_perc_imputed_med")

# - Single-factor modelling results
# Goodness-of-fit
aicTable(datCredit_train_CS, vars, TimeDef=c("Cox_Discrete","DefSpell_Event"), genPath=genObjPath, modelType="Cox_Discrete")
### RESULTS: Best AIC-results: [InterestRate_Nom]; [InterestRate_Margin_imputed_mean]; [pmnt_method_grp]; [Principal_Real];
###                            [Balance_Real_1]; [BalanceToPrincipal_1]; [slc_acct_pre_lim_perc_imputed_med];
# Discriminatory power (in-sample)
concTable(datCredit_train_CS, datCredit_valid_CS, vars, TimeDef=c("Cox_Discrete","DefSpell_Event"), genPath=genObjPath, modelType="Cox_Discrete")
### RESULTS: Best C-statistics: [Balance_Real_1]; [Principal_Real]; [BalanceToPrincipal_1]; [pmnt_method_grp];
###                            [InterestRate_Margin_imputed_mean]; [slc_acct_pre_lim_perc_imputed_med]; [InterestRate_Nom]

### CONCLUSION: Select top five variables (preference given to C-statistic rankings):
###               [Balance_Real_1]; [Principal_Real]; [BalanceToPrincipal_1]; [pmnt_method_grp];
###               [InterestRate_Margin_imputed_mean]




# ============ 5. Macroeconomic variables ============

# --- 5.1 Which lag order is the best for: [M_Repo_Rate]
# - Set initial variables to be tested
vars <- c("M_Repo_Rate", "M_Repo_Rate_1 ", "M_Repo_Rate_2", "M_Repo_Rate_3", "M_Repo_Rate_6", "M_Repo_Rate_9", "M_Repo_Rate_12")

# - Single-factor modelling results
# Goodness-of-fit
aicTable(datCredit_train_CS, vars, TimeDef=c("Cox_Discrete","DefSpell_Event"), genPath=genObjPath, modelType="Cox_Discrete")
### RESULTS: Best AIC-results: [M_Repo_Rate_12]; [M_Repo_Rate_9]; [M_Repo_Rate_6] 
# Discriminatory power (in-sample)
concTable(datCredit_train_CS, datCredit_valid_CS, vars, TimeDef=c("Cox_Discrete","DefSpell_Event"), genPath=genObjPath, modelType="Cox_Discrete")
### RESULTS: Best C-statistics: [M_Repo_Rate_12]; [M_Repo_Rate_9]; [M_Repo_Rate_6] 

### CONCLUSION: Select top three variables (preference given to C-statistic rankings):
###               [M_Repo_Rate_12]; [M_Repo_Rate_9]; [M_Repo_Rate_6] 


# --- 5.2 Which lag order is the best for: [M_Inflation_Growth]
# - Set initial variables to be tested
vars <- c("M_Inflation_Growth", "M_Inflation_Growth_1 ", "M_Inflation_Growth_2", "M_Inflation_Growth_3", 
          "M_Inflation_Growth_6", "M_Inflation_Growth_9", "M_Inflation_Growth_12")

# - Single-factor modelling results
# Goodness-of-fit
aicTable(datCredit_train_CS, vars, TimeDef=c("Cox_Discrete","DefSpell_Event"), genPath=genObjPath, modelType="Cox_Discrete")
### RESULTS: Best AIC-results: [M_Inflation_Growth_12]; [M_Inflation_Growth_9]; [M_Inflation_Growth]
# Discriminatory power (in-sample)
concTable(datCredit_train_CS, datCredit_valid_CS, vars, TimeDef=c("Cox_Discrete","DefSpell_Event"), genPath=genObjPath, modelType="Cox_Discrete")
### results: Best C-statistics: [M_Inflation_Growth_12]; [M_Inflation_Growth_9]; [M_Inflation_Growth_6]

### CONCLUSION: Select top three variables (preference given to C-statistic rankings):
###               [M_Inflation_Growth_12]; [M_Inflation_Growth_9]; [M_Inflation_Growth_6]


# --- 5.3 Which lag order is the best for: [M_RealGDP_Growth]
# - Set initial variables to be tested
vars <- c("M_RealGDP_Growth", "M_RealGDP_Growth_1 ", "M_RealGDP_Growth_2", "M_RealGDP_Growth_3", 
          "M_RealGDP_Growth_6", "M_RealGDP_Growth_9", "M_RealGDP_Growth_12")

# - Single-factor modelling results
# Goodness-of-fit
aicTable(datCredit_train_CS, vars, TimeDef=c("Cox_Discrete","DefSpell_Event"), genPath=genObjPath, modelType="Cox_Discrete")
### RESULTS: Best AIC-results: [M_RealGDP_Growth_12]; [M_RealGDP_Growth]; [M_RealGDP_Growth_1]
# Discriminatory power (in-sample)
concTable(datCredit_train_CS, datCredit_valid_CS, vars, TimeDef=c("Cox_Discrete","DefSpell_Event"), genPath=genObjPath, modelType="Cox_Discrete")
### RESULTS: Best C-statistics: [M_RealGDP_Growth_12]; [M_RealGDP_Growth_9]; [M_RealGDP_Growth_6]

### CONCLUSION: Select top three variables:
###               [M_RealGDP_Growth_12]; [M_RealGDP_Growth_9]; [M_RealGDP_Growth_6]


# --- 5.4 Which lag order is the best for: [M_RealIncome_Growth]
# - Set initial variables to be tested
vars <- c("M_RealIncome_Growth", "M_RealIncome_Growth_1 ", "M_RealIncome_Growth_2", "M_RealIncome_Growth_3", 
          "M_RealIncome_Growth_6", "M_RealIncome_Growth_9", "M_RealIncome_Growth_12")

# - Single-factor modelling results
# Goodness-of-fit
aicTable(datCredit_train_CS, vars, TimeDef=c("Cox_Discrete","DefSpell_Event"), genPath=genObjPath, modelType="Cox_Discrete")
### RESULTS: Best AIC-results: [M_RealIncome_Growth_9]; [M_RealIncome_Growth_6]; [M_RealIncome_Growth_12] 
# Discriminatory power (in-sample)
concTable(datCredit_train_CS, datCredit_valid_CS, vars, TimeDef=c("Cox_Discrete","DefSpell_Event"), genPath=genObjPath, modelType="Cox_Discrete")
### RESULTS: Best C-statistics: [M_RealIncome_Growth_1]; [M_RealIncome_Growth_2]; [M_RealIncome_Growth_3]

### CONCLUSION: Select top three variables (preference given C-statistic rankings):
###               [M_RealIncome_Growth_1]; [M_RealIncome_Growth_2]; [M_RealIncome_Growth_3]


# --- 5.5 Which lag order is the best for: [M_DTI_Growth]
# - Set initial variables to be tested
vars <- c("M_DTI_Growth", "M_DTI_Growth_1 ", "M_DTI_Growth_2", "M_DTI_Growth_3", 
          "M_DTI_Growth_6", "M_DTI_Growth_9", "M_DTI_Growth_12")

# - Single-factor modelling results
# Goodness-of-fit
aicTable(datCredit_train_CS, vars, TimeDef=c("Cox_Discrete","DefSpell_Event"), genPath=genObjPath, modelType="Cox_Discrete")
### RESULTS: Best AIC-results: [M_DTI_Growth_12]; [M_DTI_Growth_9]; [M_DTI_Growth_6]
# Discriminatory power (in-sample)
concTable(datCredit_train_CS, datCredit_valid_CS, vars, TimeDef=c("Cox_Discrete","DefSpell_Event"), genPath=genObjPath, modelType="Cox_Discrete")
### RESULTS: Best C-statistics: [M_DTI_Growth_12]; [M_DTI_Growth_9]; [M_DTI_Growth_6]

### CONCLUSION: Select top three variables:
###               [M_DTI_Growth_12]; [M_DTI_Growth_9]; [M_DTI_Growth_6]


# --- 5.6 Which lag order is the best for: [M_Emp_Growth]
# - Set initial variables to be tested
vars <- c("M_Emp_Growth", "M_Emp_Growth_1 ", "M_Emp_Growth_2", "M_Emp_Growth_3", 
          "M_Emp_Growth_6", "M_Emp_Growth_9", "M_Emp_Growth_12")

# - Single-factor modelling results
# Goodness-of-fit
aicTable(datCredit_train_CS, vars, TimeDef=c("Cox_Discrete","DefSpell_Event"), genPath=genObjPath, modelType="Cox_Discrete")
### RESULTS: Best AIC-results: [M_Emp_Growth_1]; [M_Emp_Growth_2]; [M_Emp_Growth_6]
# Discriminatory power (in-sample)
concTable(datCredit_train_CS, datCredit_valid_CS, vars, TimeDef=c("Cox_Discrete","DefSpell_Event"), genPath=genObjPath, modelType="Cox_Discrete")
### RESULTS: Best C-statistics: [M_Emp_Growth_3]; [M_Emp_Growth_2]; [M_Emp_Growth_1]

### CONCLUSION: Select top three variables:
###               [M_Emp_Growth_3]; [M_Emp_Growth_2]; [M_Emp_Growth_1]


# --- 5.7 Combining insights: Macroeconomic variables
# - Initialize variables to be tested
vars <- c("M_Repo_Rate_12", "M_Repo_Rate_9", "M_Repo_Rate_6", 
          "M_Inflation_Growth_12", "M_Inflation_Growth_9", "M_Inflation_Growth_6",
          "M_RealGDP_Growth_12", "M_RealGDP_Growth_9", "M_RealGDP_Growth_6",
          "M_RealIncome_Growth_1", "M_RealIncome_Growth_2", "M_RealIncome_Growth_3",
          "M_DTI_Growth_12", "M_DTI_Growth_9", "M_DTI_Growth_6",
          "M_Emp_Growth_3", "M_Emp_Growth_2", "M_Emp_Growth_1")

# - Full model | Stepwise forward selection procedure
modLR_full <- glm( as.formula(paste("DefSpell_Event ~", paste(vars, collapse = " + "))),
                   data=datCredit_train_CS, family="binomial")
summary(modLR_full);
evalLR(modLR_full, modLR_base, datCredit_train_CS, targetFld="DefSpell_Event", predClass=1)
### RESULTS: AIC:  16 614; McFadden R^2:  1.52%; AUC:  60.94%

# - Stepwise forward selection using BIC
ptm <- proc.time() # for runtime calculations (ignore)
modLR_step <- stepAIC(modLR_base, scope = list(lower = ~ 1, 
                                                upper = as.formula(paste("~", paste(vars, collapse = " + ")))), 
                      direction = "both", k=log(datCredit_train_CS[,.N]), maxit=50)
proc.time() - ptm # IGNORE: elapsed runtime; 1m

# - Evaluate model
summary(modLR_step)
evalLR(modLR_step, modLR_base, datCredit_train_CS, targetFld="DefSpell_Event", predClass=1)
### RESULTS: AIC:   93 983;   McFadden R^2:  6.06%; AUC:  66.98%

### CONCLUSION: Select variables from automated variable selection:
###               [M_Repo_Rate_6]; [M_Repo_Rate_12];
###               [M_RealIncome_Growth_2]; [M_Emp_Growth_1]




# ============ 6. Combine variables from all themes ============

# --- 6.1 Run an automated selection procedure on the entire set
# - Initialise set of variables
vars <- c("g0_Delinq_SD_4", "g0_Delinq_Lag_1",
          "PrevDefaults", "DefaultStatus1_Aggr_Prop_Lag_12", "DefSpell_Num_binned",
          "TimeInDefSpell", "g0_Delinq_Ave", "ArrearsToBalance_1_Aggr_Prop", "g0_Delinq_Num",
          "InterestRate_Margin_Aggr_Med_9", "InstalmentToBalance_1_Aggr_Prop",
          "Balance_Real_1", "Principal_Real", "BalanceToPrincipal_1", "pmnt_method_grp",
          "InterestRate_Margin_imputed_mean",
          "M_Repo_Rate_6", "M_Repo_Rate_12", "M_RealIncome_Growth_2", "M_Emp_Growth_1")

# - Full model
modLR_full <- glm( as.formula(paste("DefSpell_Event ~", paste(vars, collapse = " + "))),
                   data=datCredit_train_CS, family="binomial")
summary(modLR_full)
evalLR(modLR_full, modLR_base, datCredit_train_CS, targetFld="DefSpell_Event", predClass=1)
### RESULTS: AIC: 6 164; McFadden R^2: 63.65%; AUC: 98.35%

# - Stepwise forward selection using BIC
ptm <- proc.time() # for runtime calculations (ignore)
modLR_step <- stepAIC(modLR_base, scope = list(lower = ~ 1, 
                                               upper = as.formula(paste("~", paste(vars, collapse = " + ")))), 
                      direction = "both", k=log(datCredit_train_CS[,.N]), maxit=50)
proc.time() - ptm # IGNORE: elapsed runtime; 3m

# - Evaluate model
summary(modLR_step)
evalLR(modLR_step, modLR_base, datCredit_train_CS, targetFld="DefSpell_Event", predClass=1)
### RESULTS: AIC: 6 205; McFadden R^2: 63.30%; AUC: 98.28%

### CONCLUSION: Select variables from automated variable selection:
###               [InterestRate_Margin_imputed_mean]; [InstalmentToBalance_1_Aggr_Prop]; [g0_Delinq_SD_4];
###               [g0_Delinq_Lag_1]; [InterestRate_Margin_Aggr_Med_9]; [pmnt_method_grp];
###               [TimeInDefSpell]; [DefaultStatus1_Aggr_Prop_Lag_12]; [Principal_Real];
###               [Balance_Real_1]; [g0_Delinq_Ave]


# --- 6.2 Model refinements | Adjusting the input space based on expert judgement
### NOTE: The following changes have been made to ensure a macroeconomic variable is included:
### Add: 1) [M_Repo_Rate_6]
### Remove 1) [InstalmentToBalance_1_Aggr_Prop] 
vars <- colnames(modLR_step$model[,-1])
vars <- c(vars, c("M_Repo_Rate_6")); vars <- vars[!(vars %in% c("InstalmentToBalance_1_Aggr_Prop"))]

# - Fit model
modLR <- glm( as.formula(paste("DefSpell_Event ~", paste(vars, collapse = " + "))),
              data=datCredit_train_CS, family="binomial")
summary(modLR)
evalLR(modLR, modLR_base, datCredit_train_CS, targetFld="DefSpell_Event", predClass=1)
### RESULTS: AIC: 6 363; McFadden R^2: 62.37%; AUC: 98.09%




# ============ 7. Final ============

# - Confirm prepared datasets are loaded into memory
if (!exists('datCredit_train_CS')) unpack.ffdf(paste0(genPath,"creditdata_train_CS"), tempPath);gc()
if (!exists('datCredit_valid_CS')) unpack.ffdf(paste0(genPath,"creditdata_valid_CS"), tempPath);gc()

# - Fit an "empty" model as a performance gain, used within some diagnostic functions
modLR_base <- glm(DefSpell_Event ~ 1, data=datCredit_train_CS, family="binomial")

# - Final variables
vars <- c("InterestRate_Margin_imputed_mean", "g0_Delinq_SD_4",
          "g0_Delinq_Lag_1", "InterestRate_Margin_Aggr_Med_9", "pmnt_method_grp",
          "TimeInDefSpell", "DefaultStatus1_Aggr_Prop_Lag_12", "Principal_Real",
          "Balance_Real_1", "g0_Delinq_Ave", "M_Repo_Rate_6")
  
# - Fit model
modLR <- glm( as.formula(paste("DefSpell_Event ~", paste(vars, collapse = " + "))),
              data=datCredit_train_CS, family="binomial")
summary(modLR)

# - Test significance of variables
# Robust (sandwich) standard errors
robust_se <- vcovHC(modLR, type="HC0")
# Summary with robust SEs
coeftest(modLR, vcov.=robust_se)
### RESULTS: All variables are significant

# - Other diagnostics
evalLR(modLR, modLR_base, datCredit_train_CS, targetFld="DefSpell_Event", predClass=1)
### RESULTS: AIC: 6 363; McFadden R^2: 62.37%; AUC: 98.09%

# - Test goodness-of-fit using AIC-measure over single-factor models
(aicTable_LR <- aicTable(datCredit_train_CS, vars, TimeDef=c("Cox_Discrete","DefSpell_Event"), genPath=genObjPath, modelType="Cox_Discrete"))
### RESULTS: Top variables: [InterestRate_Margin_imputed_mean]; [g0_Delinq_SD_4]; [g0_Delinq_Lag_1]; [InterestRate_Margin_Aggr_Med_9]
###                         [DefaultStatus1_Aggr_Prop_Lag_12]; [g0_Delinq_Ave]; [pmnt_method_grp]

# - Test accuracy using c-statistic over single-factor models
(concTable_LR <- concTable(datCredit_train_CS, datCredit_valid_CS, vars, TimeDef=c("Cox_Discrete","DefSpell_Event"), genPath=genObjPath, modelType="Cox_Discrete"))
### RESULTS: Top variables: [g0_Delinq_Lag_1]; [InterestRate_Margin_Aggr_Med_9]; [Balance_Real_1]; [Principal_Real]; [TimeInDefSpell]
###                         [M_Repo_Rate_6]; [g0_Delinq_Ave]

# - Combine results into a single object
Table_LR <- merge(concTable_LR[,1:2], aicTable_LR, by="Variable")

GoF_CoxSnell_KS(modLR, datCredit_train_CS, GraphInd=TRUE, ,legPos=c(0.6,0.4), panelTitle="Logistic Regression",
                fileName = paste0(genFigPath, "KS_Test_CoxSnellResiduals_Exp_CDH_LR", ".png"), dpi=280,fldLstRowInd="DefSpell_Counter")
### RESULTS: KS-statistic=92%; Harell's c=98.090%; AIC=6 363

# - Save objects
# Model analytics
pack.ffdf(paste0(genObjPath,"LR_fits"), Table_LR)
# Modeling object
saveRDS(modLR, file=paste0(genObjPath,"LR_Model.rds"))




# ============ 8. Housekeeping ============

cleanEnviron()



