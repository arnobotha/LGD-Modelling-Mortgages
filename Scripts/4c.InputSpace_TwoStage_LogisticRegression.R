# ========================= INPUT SPACE: LOGISTIC REGRESSION ===================
# Divide data into thematic groups and perform data analysis on them towards
# compiling an input space for an advanced logistic regression model.
# ------------------------------------------------------------------------------
# PROJECT TITLE: Loss Modelling (LGD) for Residential Mortgages
# SCRIPT AUTHOR(S): Mohammed Gabru (MG), Marcel Muller (MM), Dr Arno Botha (AB)
# ------------------------------------------------------------------------------
# -- Script dependencies:
#   - 0.Setup.R
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



# ============ 1. Preliminaries ============

# - Confirm prepared datasets are loaded into memory
if (!exists('datCredit_train_CS')) unpack.ffdf(paste0(genPath,"creditdata_train_CS"), tempPath); gc()
if (!exists('datCredit_valid_CS')) unpack.ffdf(paste0(genPath,"creditdata_valid_CS"), tempPath); gc()
if (!exists('datCredit_train_CS_sub')) unpack.ffdf(paste0(genPath,"creditdata_train_CS_sub"), tempPath); gc()
if (!exists('datCredit_valid_CS_sub')) unpack.ffdf(paste0(genPath,"creditdata_valid_CS_sub"), tempPath); gc()
if (!exists('datCredit_OOT_CS')) unpack.ffdf(paste0(genPath,"creditdata_OOT_CS"), tempPath); gc()

# - Fit an empty model to use in analytics
modLR_base <- glm(DefSpell_Event~1, data=datCredit_train_CS, family="binomial")




# ============ 2. Delinquency-themed variables ============

# --- 2.1 Selection of delinquency volatility variables
# - Initialize variables to be tested
vars <- c("g0_Delinq_SD_4", "g0_Delinq_SD_5", "g0_Delinq_SD_6", "g0_Delinq_SD_9", "g0_Delinq_SD_12")

# - Single-factor modelling results
# Goodness-of-fit
aicTable(datCredit_train_CS, vars, TimeDef=c("Cox_Discrete","DefSpell_Event"), genPath=genObjPath, modelType="Cox_Discrete")
### RESULTS: Best AIC-results: [g0_Delinq_SD_6]; [g0_Delinq_SD_9]; [g0_Delinq_SD_5]
# Discriminatory power (in-sample)
concTable(datCredit_train_CS, datCredit_valid_CS, vars, TimeDef=c("Cox_Discrete","DefSpell_Event"), genPath=genObjPath, modelType="Cox_Discrete")
### RESULTS: Best Harrell's C-statistics: [g0_Delinq_SD_6]; [g0_Delinq_SD_5]; [g0_Delinq_SD_9]

### Conclusion: Select top three variables (preference given to C-statistic rankings):
###               [g0_Delinq_SD_5]; [g0_Delinq_SD_6]; [g0_Delinq_SD_9]


# --- 2.2 Selection of portfolio-level variables indicating the prevalence of delinquent accounts
# - Set initial variables to be tested
vars <- c("g0_Delinq_Any_Aggr_Prop", "g0_Delinq_Any_Aggr_Prop_Lag_1", "g0_Delinq_Any_Aggr_Prop_Lag_2",
          "g0_Delinq_Any_Aggr_Prop_Lag_3", "g0_Delinq_Any_Aggr_Prop_Lag_4", "g0_Delinq_Any_Aggr_Prop_Lag_5",
          "g0_Delinq_Any_Aggr_Prop_Lag_6", "g0_Delinq_Any_Aggr_Prop_Lag_9", "g0_Delinq_Any_Aggr_Prop_Lag_12")

# - Single-factor modelling results
# Goodness-of-fit
aicTable(datCredit_train_CS, vars, TimeDef=c("Cox_Discrete","DefSpell_Event"), genPath=genObjPath, modelType="Cox_Discrete")
### RESULTS: Best AIC-results: [g0_Delinq_Any_Aggr_Prop_12]; [g0_Delinq_Any_Aggr_Prop_Lag_9]; [g0_Delinq_Any_Aggr_Prop_Lag_6]
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
### RESULTS: Best AIC-results: [DefaultStatus1_Aggr_Prop]; [DefaultStatus1_Aggr_Prop_Lag_1]; [DefaultStatus1_Aggr_Prop_Lag_2]
# Discriminatory power (in-sample)
concTable(datCredit_train_CS, datCredit_valid_CS, vars, TimeDef=c("Cox_Discrete","DefSpell_Event"), genPath=genObjPath, modelType="Cox_Discrete")
### RESULTS: Best C-statistics: [DefaultStatus1_Aggr_Prop]; [DefaultStatus1_Aggr_Prop_Lag_1]; [DefaultStatus1_Aggr_Prop_Lag_2]

### CONCLUSION: Select top three variables (preference given to C-statistic rankings):
###               [DefaultStatus1_Aggr_Prop]; [DefaultStatus1_Aggr_Prop_Lag_1]; [DefaultStatus1_Aggr_Prop_Lag_2]


# --- 2.4 Selection of other portfolio-level delinquency-themed variables
# - Set initial variables to be tested
vars <- c("g0_Delinq_Ave", "ArrearsToBalance_1_Aggr_Prop", "CuringEvents_Aggr_Prop")

# - Single-factor modelling results
# Goodness-of-fit
aicTable(datCredit_train_CS, vars, TimeDef=c("Cox_Discrete","DefSpell_Event"), genPath=genObjPath, modelType="Cox_Discrete")
### RESULTS: Best AIC-results: [g0_Delinq_Ave]; [CuringEvents_Aggr_Prop]; [ArrearsToBalance_1_Aggr_Prop]
# Discriminatory power (in-sample)
concTable(datCredit_train_CS, datCredit_valid_CS, vars, TimeDef=c("Cox_Discrete","DefSpell_Event"), genPath=genObjPath, modelType="Cox_Discrete")
### RESULTS: Best C-statistics: [g0_Delinq_Ave]; [CuringEvents_Aggr_Prop]; [ArrearsToBalance_1_Aggr_Prop]

### Conclusion: All are significant, therefore include all three


# --- 2.5 Selection of account-level delinquency-themed variables
# - Set initial variables to be tested
vars <- c("TimeInDefSpell", "g0_Delinq_Lag_1", "slc_acct_arr_dir_3_Change_Ind",
          "g0_Delinq_Num", "slc_acct_arr_dir_3", "slc_acct_roll_ever_24_imputed_mean",
          "Arrears", "PrevDefaults","TimeInDelinqState",
          "slc_past_due_amt_imputed_med", "slc_curing_ind", "DefSpell_Num_binned")

# - Single-factor modelling results
# Goodness-of-fit
aicTable(datCredit_train_CS, vars, TimeDef=c("Cox_Discrete","DefSpell_Event"), genPath=genObjPath, modelType="Cox_Discrete")
### RESULTS: Best AIC-results: [PrevDefaults]; [Arrears]; [slc_past_due_amt_imputed_med]; [g0_Delinq_Lag_1]; [slc_curing_ind];
###                            [TimeInDelinqState]; [slc_acct_arr_dir_3]
# Discriminatory power (in-sample)
concTable(datCredit_train_CS, datCredit_valid_CS, vars, TimeDef=c("Cox_Discrete","DefSpell_Event"), genPath=genObjPath, modelType="Cox_Discrete")
### RESULTS: Best AIC-results: [PrevDefaults]; [Arrears]; [TimeInDelinqState]; [slc_past_due_amt_imputed_med]; [g0_Delinq_Lag_1]; [slc_acct_arr_dir_3];
###                            [slc_curing_ind]; [slc_acct_arr_dir_3_Change_Ind]

### CONCLUSION: Differences are pronounced between the AICs and C-statistics; preference given to C-statistic in final choice
###             Select top 5 variables (preference given to C-statistic rankings):
###               [PrevDefaults]; [Arrears]; [TimeInDelinqState]; [slc_past_due_amt_imputed_med]; [g0_Delinq_Lag_1]



# --- 2.6 Combining insights: delinquency-themed variables
# - Set initial variables to be tested
vars <- c("g0_Delinq_SD_6", "g0_Delinq_SD_9", "g0_Delinq_SD_6",
          "DefaultStatus1_Aggr_Prop", "DefaultStatus1_Aggr_Prop_Lag_1" , "DefaultStatus1_Aggr_Prop_Lag_2",
          "CuringEvents_Aggr_Prop", "g0_Delinq_Ave", "ArrearsToBalance_1_Aggr_Prop",
          "PrevDefaults", "Arrears", "TimeInDelinqState", "slc_past_due_amt_imputed_med", "g0_Delinq_Lag_1")

# - Full model | Stepwise forward selection procedure
modLR_full <- glm(as.formula(paste("DefSpell_Event ~", paste(vars, collapse = " + "))),
                  data=datCredit_train_CS, family="binomial")
summary(modLR_full);
evalLR(modLR_full, modLR_base, datCredit_train_CS, targetFld="DefSpell_Event", predClass=1)
### RESULTS: AIC:   64 592; McFadden R^2:  32.95%; AUC:  87.04%

ptm <- proc.time() # for runtime calculations (ignore)
modLR_step <- stepAIC(modLR_base, scope=list(lower=~ 1, 
                                             upper=as.formula(paste("~", paste(vars, collapse=" + ")))), 
                      direction="both", k=log(datCredit_train_CS[,.N]), maxit=50)
summary(modLR_step)
evalLR(modLR_step, modLR_base, datCredit_train_CS, targetFld="DefSpell_Event", predClass=1)
proc.time() - ptm # IGNORE: elapsed runtime; 1m
### RESULTS: AIC:   64 596; McFadden R^2:  32.94%; AUC:  87.04%

# - Domain expertise
### CONCLUSION: - Remove [g0_Delinq_SD_9] since [g0_Delinq_SD_6] is already present
###             - Remove [DefaultStatus1_Aggr_Prop] since [DefaultStatus1_Aggr_Prop_Lag_2] is already present
###             - Refine selection from automated selection with expert judgement

# - Final variables (expert judgement)
vars <- colnames(modLR_step$model[,-1])
vars <- vars[!(vars %in% c("g0_Delinq_SD_9", "DefaultStatus1_Aggr_Prop"))]
modLR <- glm( as.formula(paste("DefSpell_Event ~", paste(vars, collapse = " + "))),
              data=datCredit_train_CS, family="binomial")
summary(modLR);
evalLR(modLR, modLR_base, datCredit_train_CS, targetFld="DefSpell_Event", predClass=1)
### RESULTS: AIC:   64 738; McFadden R^2:  32.79%; AUC:  86.96%

### CONCLUSION: Select:
###               [PrevDefaults]; [Arrears]; [g0_Delinq_Lag_1]; [g0_Delinq_Ave];
###               [TimeInDelinqState]; [g0_Delinq_SD_6]; [ArrearsToBalance_1_Aggr_Prop]
###               [DefaultStatus1_Aggr_Prop_Lag_2]; [slc_past_due_amt_imputed_med]




# ============ 3. Other portfolio-level variables ============

# --- 3.1 Selection of the portfolio median interest rate margin
# - Set initial variables to be tested
vars <- c("InterestRate_Margin_Aggr_Med", "InterestRate_Margin_Aggr_Med_1", "InterestRate_Margin_Aggr_Med_2",
          "InterestRate_Margin_Aggr_Med_3", "InterestRate_Margin_Aggr_Med_9")

# - Single-factor modelling results
# Goodness-of-fit
aicTable(datCredit_train_CS, vars, TimeDef=c("Cox_Discrete","DefSpell_Event"), genPath=genObjPath, modelType="Cox_Discrete")
### RESULTS: Best AIC-results: [InterestRate_Margin_Aggr_Med]; [InterestRate_Margin_Aggr_Med_1; [InterestRate_Margin_Aggr_Med_2]
# Discriminatory power (in-sample)
concTable(datCredit_train_CS, datCredit_valid_CS, vars, TimeDef=c("Cox_Discrete","DefSpell_Event"), genPath=genObjPath, modelType="Cox_Discrete")
### RESULTS: Best C-statistics: [InterestRate_Margin_Aggr_Med_1]; [InterestRate_Margin_Aggr_Med_3]; [InterestRate_Margin_Aggr_Med_2]

### Conclusion: Select top three variables:
###               [InterestRate_Margin_Aggr_Med_1];, [InterestRate_Margin_Aggr_Med_2]; [InterestRate_Margin_Aggr_Med_3]


# --- 3.2 Selection of other portfolio-level, non-delinquency themed, variables
# - Set initial variables to be tested
vars <- c("InstalmentToBalance_1_Aggr_Prop", "AgeToTerm_Aggr_Mean", "DefSpell_Maturity_Aggr_Mean", "NewLoans_Aggr_Prop")

# - Single-factor modelling results
# Goodness-of-fit
aicTable(datCredit_train_CS, vars, TimeDef=c("Cox_Discrete","DefSpell_Event"), genPath=genObjPath, modelType="Cox_Discrete")
### RESULTS: Best AIC-results: [AgeToTerm_Aggr_Mean]; [NewLoans_Aggr_Prop]; [InstalmentToBalance_1_Aggr_Prop]; [DefSpell_Maturity_Aggr_Mean]
# Discriminatory power (in-sample)
concTable(datCredit_train_CS, datCredit_valid_CS, vars, TimeDef=c("Cox_Discrete","DefSpell_Event"), genPath=genObjPath, modelType="Cox_Discrete")
### RESULTS: Best AIC-results: [NewLoans_Aggr_Prop]; [AgeToTerm_Aggr_Mean]; [DefSpell_Maturity_Aggr_Mean]; [InstalmentToBalance_1_Aggr_Prop]

### CONCLUSION: Select top three variables:
###               [NewLoans_Aggr_Prop]; [AgeToTerm_Aggr_Mean]; [DefSpell_Maturity_Aggr_Mean]


# --- 3.3 Combining insights
# - Set initial variables to be tested
vars <- c("InterestRate_Margin_Aggr_Med_1", "InterestRate_Margin_Aggr_Med_2", "InterestRate_Margin_Aggr_Med_3",
          "NewLoans_Aggr_Prop", "AgeToTerm_Aggr_Mean", "DefSpell_Maturity_Aggr_Mean")

# - Full model
modLR_full <- glm(as.formula(paste("DefSpell_Event ~", paste(vars, collapse = " + "))),
                  data=datCredit_train_CS, family="binomial")
summary(modLR_full)
evalLR(modLR_full, modLR_base, datCredit_train_CS, targetFld="DefSpell_Event", predClass=1)
### RESULTS: AIC: 93 879; McFadden R^2: 2.52%; AUC: 61.00%.

# - Stepwise forward selection using BIC
ptm <- proc.time() # for runtime calculations (ignore)
modLR_step <- stepAIC(modLR_base, scope = list(lower = ~ 1, 
                                                upper = as.formula(paste("~", paste(vars, collapse = " + ")))), 
                      direction = "both", k=log(datCredit_train_CS[,.N]), maxit=50)
proc.time() - ptm # IGNORE: elapsed runtime; 1m
summary(modLR_step)
evalLR(modLR_step, modLR_base, datCredit_train_CS, targetFld="DefSpell_Event", predClass=1)
### RESULTS: AIC:   93 881; McFadden R^2: 2.51%; AUC: 60.96%

### CONCLUSION: Select variables from automated variable selection:
###             [InterestRate_Margin_Aggr_Med_1]; [DefSpell_Maturity_Aggr_Mean]; [AgeToTerm_Aggr_Mean]; [NewLoans_Aggr_Prop]




# ============ 4. Account-level variables ============

# --- 4.1 Selection of non-delinquency related account-level variables
# - Set initial variables to be tested
vars <- c("Principal_Real", "Principal", "InterestRate_Margin_imputed_mean", 
          "Balance_Real_1", "Balance_1", "Instalment_Real", "InterestRate_Nom", "AgeToTerm",
          "BalanceToPrincipal_1", "slc_acct_pre_lim_perc_imputed_med")

# - Correlation analysis towards obtaining clusters of correlated variables
corrAnalysis(datCredit_train_CS, vars, corrThresh = 0.6, method = 'spearman')
### RESULTS:
###   Absolute correlations of  97%  found for  [Principal_Real] and [Principal] 
###   Absolute correlations of  90%  found for  [Principal_Real] and [Balance_Real_1] 
###   Absolute correlations of  85%  found for  [Principal] and [Balance_Real_1]
###   Absolute correlations of  89%  found for  [Principal_Real] and [Balance_1] 
###   Absolute correlations of  89%  found for  [Principal] and [Balance_1] 
###   Absolute correlations of  98%  found for  [Balance_Real_1] and [Balance_1] 
###   Absolute correlations of  86%  found for  [Principal_Real] and [Instalment_Real] 
###   Absolute correlations of  83%  found for  [Principal] and [Instalment_Real] 
###   Absolute correlations of  91%  found for  [Balance_Real_1] and [Instalment_Real] 
###   Absolute correlations of  89%  found for  [Balance_1] and [Instalment_Real] 
###   Absolute correlations of  76%  found for  [Balance_Real_1] and [BalanceToPrincipal_1] 
###   Absolute correlations of  71%  found for  [Balance_1] and [BalanceToPrincipal_1] 
###   Absolute correlations of  62%  found for  [Instalment_Real] and [BalanceToPrincipal_1] 
###   Absolute correlations of  -60%  found for  [AgeToTerm] and [BalanceToPrincipal_1]

# - Initialize variables to be tested
### NOTE: - Inflation adjusted variables preferred over their non-adjusted counterparts
###       - Principal, balance, and instalment all kept regardless of their correlations, each contains unique information
vars <- c("Principal_Real", "InterestRate_Margin_imputed_mean", "pmnt_method_grp",
          "Balance_Real_1", "InterestRate_Nom",
          "BalanceToPrincipal_1", "slc_acct_pre_lim_perc_imputed_med")

# - Single-factor modelling results
# Goodness-of-fit
aicTable(datCredit_train_CS, vars, TimeDef=c("Cox_Discrete","DefSpell_Event"), genPath=genObjPath, modelType="Cox_Discrete")
### RESULTS: Best AIC-results: [pmnt_method_grp]; [Balance_Real_1]; [Principal_Real]; [BalanceToPrincipal_1];
###                            [InterestRate_Margin_imputed_mean]; [slc_acct_pre_lim_perc_imputed_med]; [InterestRate_Nom];
# Discriminatory power (in-sample)
concTable(datCredit_train_CS, datCredit_valid_CS, vars, TimeDef=c("Cox_Discrete","DefSpell_Event"), genPath=genObjPath, modelType="Cox_Discrete")
### RESULTS: Best C-statistics: [Balance_Real_1]; [pmnt_method_grp]; [Principal_Real]; [BalanceToPrincipal_1];
###                            [InterestRate_Margin_imputed_mean]; [slc_acct_pre_lim_perc_imputed_med]; [InterestRate_Nom]

### CONCLUSION: Select top five variables (preference given to C-statistic rankings):
###               [Balance_Real_1]; [pmnt_method_grp]; [Principal_Real]; [BalanceToPrincipal_1];
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
### RESULTS: Best AIC-results: [M_Inflation_Growth_12]; [M_Inflation_Growth_9]; [M_Inflation_Growth_6]
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
### RESULTS: Best AIC-results: [M_RealGDP_Growth_12]; [M_RealGDP_Growth_9]; [M_RealGDP_Growth_6]
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
### RESULTS: Best AIC-results: [M_RealIncome_Growth_12]; [M_RealIncome_Growth]; [M_RealIncome_Growth] 
# Discriminatory power (in-sample)
concTable(datCredit_train_CS, datCredit_valid_CS, vars, TimeDef=c("Cox_Discrete","DefSpell_Event"), genPath=genObjPath, modelType="Cox_Discrete")
### RESULTS: Best C-statistics: [M_RealIncome_Growth_12]; [M_RealIncome_Growth]; [M_RealIncome_Growth_9]

### CONCLUSION: Select top three variables (preference given C-statistic rankings):
###               [M_RealIncome_Growth]; [M_RealIncome_Growth_9]; [M_RealIncome_Growth_12]


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
### RESULTS: Best AIC-results: [M_Emp_Growth_12]; [M_Emp_Growth_9]; [M_Emp_Growth]
# Discriminatory power (in-sample)
concTable(datCredit_train_CS, datCredit_valid_CS, vars, TimeDef=c("Cox_Discrete","DefSpell_Event"), genPath=genObjPath, modelType="Cox_Discrete")
### RESULTS: Best C-statistics: [M_Emp_Growth_12]; [M_Emp_Growth_9]; [M_Emp_Growth_6]

### CONCLUSION: Select top three variables:
###               [M_Emp_Growth_12]; [M_Emp_Growth_9]; [M_Emp_Growth_6]


# --- 5.7 Combining insights: Macroeconomic variables
# - Initialize variables to be tested
vars <- c("M_Repo_Rate_12", "M_Repo_Rate_9", "M_Repo_Rate_6", 
          "M_Inflation_Growth_12", "M_Inflation_Growth_9", "M_Inflation_Growth_6",
          "M_RealGDP_Growth_12", "M_RealGDP_Growth_9", "M_RealGDP_Growth_6",
          "M_RealIncome_Growth", "M_RealIncome_Growth_9", "M_RealIncome_Growth_12",
          "M_DTI_Growth_12", "M_DTI_Growth_9", "M_DTI_Growth_6",
          "M_Emp_Growth_12", "M_Emp_Growth_9", "M_Emp_Growth_6")

# - Full model | Stepwise forward selection procedure
modLR_full <- glm( as.formula(paste("DefSpell_Event ~", paste(vars, collapse = " + "))),
                   data=datCredit_train_CS, family="binomial")
summary(modLR_full);
evalLR(modLR_full, modLR_base, datCredit_train_CS, targetFld="DefSpell_Event", predClass=1)
### RESULTS: AIC:  93 480; McFadden R^2:  2.9%; AUC:  61.92%

# - Stepwise forward selection using BIC
ptm <- proc.time() # for runtime calculations (ignore)
modLR_step <- stepAIC(modLR_base, scope = list(lower = ~ 1, 
                                                upper = as.formula(paste("~", paste(vars, collapse = " + ")))), 
                      direction = "both", k=log(datCredit_train_CS[,.N]), maxit=50)
proc.time() - ptm # IGNORE: elapsed runtime; 1m

# - Evaluate model
summary(modLR_step)
evalLR(modLR_step, modLR_base, datCredit_train_CS, targetFld="DefSpell_Event", predClass=1)
### RESULTS: AIC:   93 525;   McFadden R^2:  2.89%; AUC:  61.78%

### CONCLUSION: Select variables from automated variable selection:
###               [M_Repo_Rate_12]; [M_DTI_Growth_12]; [M_Inflation_Growth_6]; [M_DTI_Growth_6];
###               [M_Inflation_Growth_12]; [M_RealGDP_Growth_6]; [M_Repo_Rate_9]; [M_Emp_Growth_12]




# ============ 6. Combine variables from all themes ============

# --- 6.1 Run an automated selection procedure on the entire set
# - Initialise set of variables
vars <- c("PrevDefaults", "Arrears", "g0_Delinq_Lag_1", "g0_Delinq_Ave",
          "TimeInDelinqState", "g0_Delinq_SD_6", "ArrearsToBalance_1_Aggr_Prop",
          "DefaultStatus1_Aggr_Prop_Lag_2", "slc_past_due_amt_imputed_med",
          "InterestRate_Margin_Aggr_Med_1", "DefSpell_Maturity_Aggr_Mean", "AgeToTerm_Aggr_Mean",
          "NewLoans_Aggr_Prop",
          "Balance_Real_1", "pmnt_method_grp", "Principal_Real", "BalanceToPrincipal_1",
          "InterestRate_Margin_imputed_mean",
          "M_Repo_Rate_12", "M_DTI_Growth_12", "M_Inflation_Growth_6", "M_DTI_Growth_6",
          "M_Inflation_Growth_12", "M_RealGDP_Growth_6", "M_Repo_Rate_9", "M_Emp_Growth_12")

# - Full model
modLR_full <- glm( as.formula(paste("DefSpell_Event ~", paste(vars, collapse = " + "))),
                   data=datCredit_train_CS, family="binomial")
summary(modLR_full)
evalLR(modLR_full, modLR_base, datCredit_train_CS, targetFld="DefSpell_Event", predClass=1)
### RESULTS: AIC: 63 280 McFadden R^2: 34.34%; AUC: 87.43%

# - Stepwise forward selection using BIC
ptm <- proc.time() # for runtime calculations (ignore)
modLR_step <- stepAIC(modLR_base, scope = list(lower = ~ 1, 
                                               upper = as.formula(paste("~", paste(vars, collapse = " + ")))), 
                      direction = "both", k=log(datCredit_train_CS[,.N]), maxit=50)
proc.time() - ptm # IGNORE: elapsed runtime; 3m

# - Evaluate model
summary(modLR_step)
evalLR(modLR_step, modLR_base, datCredit_train_CS, targetFld="DefSpell_Event", predClass=1)
### RESULTS: AIC: 63 279; McFadden R^2: 34.33%; AUC: 87.43%

### CONCLUSION: Select variables from automated variable selection:
###               [PrevDefaults]; [Arrears]; [pmnt_method_grp]; [g0_Delinq_Lag_1]; [TimeInDelinqState];
###               [M_Repo_Rate_12]; [InterestRate_Margin_imputed_mean]; [g0_Delinq_SD_6]; [Balance_Real_1];
###               [slc_past_due_amt_imputed_med]; [DefaultStatus1_Aggr_Prop_Lag_2]; [M_DTI_Growth_6];
###               [M_RealGDP_Growth_6]; [M_Inflation_Growth_12]; [NewLoans_Aggr_Prop]; [ArrearsToBalance_1_Aggr_Prop];
###               [InterestRate_Margin_Aggr_Med_1]; [AgeToTerm_Aggr_Mean]; [M_Emp_Growth_12]


# --- 6.2 Model refinements | Adjusting the input space based on expert judgement
### NOTE: 1) We replace [TimeInDelinqState] with [TimeInDefSpell] to ensure easier/more intuitive inference:
###           - Added: [TimeInDefSpell]
###           - Removed: [TimeInDelinqState]
###       2) The following variables are removed to enhance model parsimony:
###           - Removed: [M_Emp_Growth_12]; [DefaultStatus1_Aggr_Prop_Lag_2]
###       3) The following changes are made due to relatively large coefficients & standard errors:
###           - Removed: [InterestRate_Margin_Aggr_Med_1]
###       4) The following residual adjustment are made due to non-signifigance when applying the above changes:
###           - Removed: [NewLoans_Aggr_Prop]; [M_DTI_Growth_6]
vars <- colnames(modLR_step$model[,-1])
vars <- c(vars, c("TimeInDefSpell")); vars <- vars[!(vars %in% c("TimeInDelinqState", "M_Emp_Growth_12",
                                                                 "InterestRate_Margin_Aggr_Med_1", "DefaultStatus1_Aggr_Prop_Lag_2",
                                                                 "NewLoans_Aggr_Prop", "M_DTI_Growth_6"))]

# - Fit model
modLR <- glm( as.formula(paste("DefSpell_Event ~", paste(vars, collapse = " + "))),
              data=datCredit_train_CS, family="binomial")
summary(modLR)
evalLR(modLR, modLR_base, datCredit_train_CS, targetFld="DefSpell_Event", predClass=1)
### RESULTS: AIC: 63 799; McFadden R^2: 33.78%; AUC: 87.21%




# ============ 7. Adjusting the model training period ============
### NOTE: We change the training period to 2008-01-01 -> 2019-12-31, which
###       ensures an out-of-time and out-of-sample back-testing/assessment.

# --- 7.1 Initial fit
# - Fit an "empty" model as a performance gain, used within some diagnostic functions
modLR_base <- glm(DefSpell_Event ~ 1, data=datCredit_train, family="binomial")

# - Final variables
vars <- colnames(modLR$model[,-1])

# - Fit model
modLR <- glm( as.formula(paste("DefSpell_Event ~", paste(vars, collapse = " + "))),
              data=datCredit_train_CS_sub, family="binomial")
summary(modLR)
evalLR(modLR, modLR_base, datCredit_train_CS_sub, targetFld="DefSpell_Event", predClass=1)
### RESULTS: - AIC: 48 996; McFadden R^2: 49.15%; AUC: 88.54%
###          - Insignificant variables: [AgeToTerm_Aggr_Mean]

### CONCLUSION: Refit model with refined input space


# --- 7.2 Model refinements | Adjusting the input space based on expert judgement
### NOTE: 1) The following variables are removed due to insignificance:
###             [AgeToTerm_Aggr_Mean], slc_past_due_amt_imputed_med     
vars <- colnames(modLR$model[,-1])
vars <- vars[!(vars %in% c("AgeToTerm_Aggr_Mean", "slc_past_due_amt_imputed_med"))]

# - Fit model
modLR <- glm(as.formula(paste("DefSpell_Event ~", paste(vars, collapse = " + "))),
             data=datCredit_train_CS_sub, family="binomial")
summary(modLR)
evalLR(modLR, modLR_base, datCredit_train_CS_sub, targetFld="DefSpell_Event", predClass=1)
### RESULTS: AIC: 48 995; McFadden R^2: 37.05%; AUC: 88.54%

### CONCLUSION: Final model confirmed as:
###               [PrevDefaults]; [Arrears]; [g0_Delinq_Lag_1]; [M_Repo_Rate_12];
###               [InterestRate_Margin_imputed_mean]; [g0_Delinq_SD_6]; [Balance_Real_1];
###               [M_RealGDP_Growth_6]; [M_Inflation_Growth_12];
###               [ArrearsToBalance_1_Aggr_Prop]; [TimeInDefSpell]




# ============ 8. Final ============

# --- 8.1 Preliminaries
# - Confirm prepared datasets are loaded into memory
if (!exists('datCredit_train_CS_sub')) unpack.ffdf(paste0(genPath,"creditdata_train_CS_sub"), tempPath); gc()
if (!exists('datCredit_valid_CS_sub')) unpack.ffdf(paste0(genPath,"creditdata_valid_CS_sub"), tempPath); gc()

# - Fit an "empty" model as a performance gain, used within some diagnostic functions
modLR_base <- glm(DefSpell_Event ~ 1, data=datCredit_train_CS_sub, family="binomial")


# --- 8.2 Fit model
# - Final variables
### NOTE: [slc_past_due_amt_imputed_med] removed due to statistical insignifance on the rextricted training window
vars <- c("PrevDefaults", "Arrears", "g0_Delinq_Lag_1", "M_Repo_Rate_12",
         "InterestRate_Margin_imputed_mean", "g0_Delinq_SD_6", "Balance_Real_1",
          "M_RealGDP_Growth_6", "M_Inflation_Growth_12",
         "ArrearsToBalance_1_Aggr_Prop", "TimeInDefSpell")
  
# - Fit model
modLR <- glm(as.formula(paste("DefSpell_Event ~", paste(vars, collapse = " + "))),
             data=datCredit_train_CS_sub, family="binomial")
summary(modLR)
evalLR(modLR, modLR_base, datCredit_train_CS_sub, targetFld="DefSpell_Event", predClass=1)
### RESULTS: AIC: 49 785; McFadden R^2: 36.02%; AUC: 88.35%


# --- 8.3 Assess model
# - Test significance of variables
# Robust (sandwich) standard errors
robust_se <- vcovHC(modLR, type="HC0")
# Summary with robust SEs
coeftest(modLR, vcov.=robust_se)
### RESULTS: All variables are significant

# - Test goodness-of-fit using AIC-measure over single-factor models
(aicTable_LR <- aicTable(datCredit_train_CS_sub, vars, TimeDef=c("Cox_Discrete","DefSpell_Event"), genPath=genObjPath, modelType="Cox_Discrete"))
### RESULTS: Top variables: [PrevDefaults]; [Arrears]; [slc_past_due_amt_imputed_med]; [g0_Delinq_SD_6]; [Balance_Real_1];
###                         [M_Repo_Rate_1]; [M_Inflation_Growth_12]

# - Test accuracy using c-statistic over single-factor models
(concTable_LR <- concTable(datCredit_train_CS_sub, datCredit_valid_CS, vars, TimeDef=c("Cox_Discrete","DefSpell_Event"), genPath=genObjPath, modelType="Cox_Discrete"))
### RESULTS: Top variables: [PrevDefaults]; [Arrears]; [slc_past_due_amt_imputed_med]; [g0_Delinq_SD_6]; [Balance_Real_1]
###                         [TimeInDefSpell]; [M_Repo_Rate_12]

# - Combine results into a single object
Table_LR <- merge(concTable_LR[,1:2], aicTable_LR, by="Variable")


# --- 8.4 Housekeeping
# - Save objects
# Model analytics
pack.ffdf(paste0(genObjPath,"LR_fits"), Table_LR)
# Modeling object
saveRDS(modLR, file=paste0(genObjPath,"LR_Model.rds"))

# - Clean-up
cleanEnviron()



