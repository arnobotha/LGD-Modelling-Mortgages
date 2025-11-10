# ======================================= INPUT SPACE: LOSS SEVERITY============================
# Divide data into thematic groups and perform data analysis on them to compile an input space for 
# the loss severity component.
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

# - Use only default spells and first counter where write-off are present
datCredit_train <- datCredit_train_CDH[!is.na(DefSpell_Key)&DefSpell_Counter==1&DefSpellResol_Type_Hist=="WOFF",]
datCredit_valid <- datCredit_valid_CDH[!is.na(DefSpell_Key)&DefSpell_Counter==1&DefSpellResol_Type_Hist=="WOFF",]

# Identify where the loss rate is out of bounds and not feasible
datCredit_train <- datCredit_train[, OOB_Ind := ifelse(LossRate_Real < 0 | LossRate_Real > 1, 1,0)]
datCredit_valid <- datCredit_valid[, OOB_Ind := ifelse(LossRate_Real < 0 | LossRate_Real > 1, 1,0)]

# Subset to include only relevant data
datCredit_train <- subset(datCredit_train, OOB_Ind == 0)
datCredit_valid <- subset(datCredit_valid, OOB_Ind == 0)


# remove previous objects from memory
rm(datCredit_train_CDH, datCredit_valid_CDH); gc()

modLR_base <- cpglm(LossRate_Real ~ 1, data=datCredit_train)


# - Initialize variables to be tested
vars <- c("g0_Delinq_Any_Aggr_Prop", "g0_Delinq_Any_Aggr_Prop_Lag_1", "g0_Delinq_Any_Aggr_Prop_Lag_2",
          "g0_Delinq_Any_Aggr_Prop_Lag_3", "g0_Delinq_Any_Aggr_Prop_Lag_4", "g0_Delinq_Any_Aggr_Prop_Lag_5",
          "g0_Delinq_Any_Aggr_Prop_Lag_6", "g0_Delinq_Any_Aggr_Prop_Lag_9", "g0_Delinq_Any_Aggr_Prop_Lag_12" )

# - Single-factor modelling results
# Goodness-of-fit
aicTable_LS(datCredit_train, vars, TimeDef=c("Cox_Discrete","LossRate_Real"), genPath=genObjPath, modelType="tweedie")


### RESULTS: Best AIC-results: g0_Delinq_Any_Aggr_Prop_12, g0_Delinq_Any_Aggr_Prop_Lag_9, g0_Delinq_Any_Aggr_Prop_Lag_6, g0_Delinq_Any_Aggr_Prop
# , g0_Delinq_Any_Aggr_Prop_4
# Best Deviance : g0_Delinq_Any_Aggr_Prop_12, g0_Delinq_Any_Aggr_Prop_Lag_9, g0_Delinq_Any_Aggr_Prop_Lag_6, g0_Delinq_Any_Aggr_Prop
# , g0_Delinq_Any_Aggr_Prop_4
# Conclusion: The differences in AIC and deviance are minor
# Choose the overall top 3 performing variables g0_Delinq_Any_Aggr_Prop_12 g0_Delinq_Any_Aggr_Prop_Lag_9, g0_Delinq_Any_Aggr_Prop_Lag_6
# All are above 50% 


# ------ 4.3 Which lag order is the best in calculating the portfolio-level fraction of defaulted accounts during the default spell?
vars <- c("DefaultStatus1_Aggr_Prop", "DefaultStatus1_Aggr_Prop_Lag_1", "DefaultStatus1_Aggr_Prop_Lag_2",
          "DefaultStatus1_Aggr_Prop_Lag_3", "DefaultStatus1_Aggr_Prop_Lag_4", "DefaultStatus1_Aggr_Prop_Lag_5",
          "DefaultStatus1_Aggr_Prop_Lag_6", "DefaultStatus1_Aggr_Prop_Lag_9", "DefaultStatus1_Aggr_Prop_Lag_12" )

# - Single-factor modelling results
# Goodness-of-fit
aicTable_LS(datCredit_train, vars, TimeDef=c("Cox_Discrete","LossRate_Real"), genPath=genObjPath, modelType="tweedie")
### RESULTS: Best AIC-results: DefaultStatus1_Aggr_Prop_Lag_12, DefaultStatus1_Aggr_Prop_Lag_9, DefaultStatus1_Aggr_Prop_Lag_6, 
# This decreases from the highest lag to lowest lag in decreasing order
# Differences are not that major
# Best Deviance: DefaultStatus1_Aggr_Prop_Lag_12, DefaultStatus1_Aggr_Prop_Lag_9, DefaultStatus1_Aggr_Prop_Lag_6, 
# Similar trend as to AIC


### CONCLUSION: Later is better, though the AIC-differences were minuscule.
# Top 3 varibales: DefaultStatus1_Aggr_Prop_Lag_12, DefaultStatus1_Aggr_Prop_Lag_9, DefaultStatus1_Aggr_Prop_Lag_6


# ------ 4.4 How do other portfolio-level delinquency-themed variables fare as single-factor models?

# - Initialize variables to be tested
vars <- c("g0_Delinq_Ave", "ArrearsToBalance_Aggr_Prop", "CuringEvents_Aggr_Prop" )

# - Single-factor modelling results
# Goodness-of-fit
aicTable_LS(datCredit_train, vars, TimeDef=c("Cox_Discrete","LossRate_Real"), genPath=genObjPath, modelType="tweedie")

### RESULTS: Best AIC-results:  CuringEvents_Aggr_Prop, g0_Delinq_Ave,ArrearsToBalance_Aggr_Prop
# However they aren't that big with their being a e00 difference between the lowest and highest
# Best Deviance: CuringEvents_Aggr_Prop, g0_Delinq_Ave,ArrearsToBalance_Aggr_Prop


### Conclusion: All are significant include all 3 of them, curing event is relevant


# ------ 4.5 How do account-level delinquency-themed variables fare as single-factor models?

# - Initialize variables to be tested
vars <- c("TimeInDefSpell", "g0_Delinq_fac", "g0_Delinq", "g0_Delinq_Lag_1", "slc_acct_arr_dir_3_Change_Ind",
          "g0_Delinq_Num", "slc_acct_arr_dir_3", "slc_acct_roll_ever_24_imputed_mean", "Arrears", "PrevDefaults","TimeInDelinqState"
          ,"slc_past_due_amt_imputed_med", "slc_curing_ind","DefSpell_Age")

# - Single-factor modelling results
# Goodness-of-fit
aicTable_LS(datCredit_train, vars, TimeDef=c("Cox_Discrete","LossRate_Real"), genPath=genObjPath, modelType="tweedie")
# Discriminatory power (in-sample)
### RESULTS: Best AIC-results: TimeInDefSpell, Arrears, g0_Delinq_Lag_1, DefSpell_Age, slc_Acct_Arr_dir_3, slc_past_due_amt_imputed_med
# Best Deviance: TimeInDefSpell, Arrears, g0_Delinq_Lag_1, DefSpell_Age, slc_Acct_Arr_dir_3, slc_past_due_amt_imputed_med
### CONCLUSION
# Choose: TimeInDefSpell, Arrears, g0_Delinq_Lag_1, DefSpell_Age, slc_Acct_Arr_dir_3, slc_past_due_amt_imputed_med
# These variables performed significantly better than the rest with th lowest AIC 

# ------ 4.6 Combining insights: delinquency-themed variables
# - Initialize variables to be tested
vars <- c("TimeInDefSpell", "Arrears", "g0_Delinq_Lag_1", "DefSpell_Age", "slc_acct_arr_dir_3", "slc_past_due_amt_imputed_med",
          "g0_Delinq_Ave", "ArrearsToBalance_Aggr_Prop", "CuringEvents_Aggr_Prop",
          "DefaultStatus1_Aggr_Prop_Lag_6", "DefaultStatus1_Aggr_Prop_Lag_9", "DefaultStatus1_Aggr_Prop_Lag_12",
          "g0_Delinq_Any_Aggr_Prop_Lag_6", "g0_Delinq_Any_Aggr_Prop_Lag_9", "g0_Delinq_Any_Aggr_Prop_Lag_12")
# - Full model | Stepwise forward selection procedure
modLR_full <- cpglm( as.formula(paste("LossRate_Real ~", paste(vars, collapse = " + "))),
                   data=datCredit_train)
summary(modLR_full);
evalLS(modLR_full,datCredit_train,targetFld="LossRate_Real",modLR_base)

### RESULTS: AIC:   616; Pseudo R^2:  37.87%; RMSE:  27.83%; MAE:  22.65%

summary(modLR_step)
evalLR(modLR_step, modLR_base, datCredit_train, targetFld="DefSpell_Event", predClass=1)
proc.time() - ptm # IGNORE: elapsed runtime; 70m
### RESULTS: AIC:   124,655; McFadden R^2:  43.80%; AUC:  96.75%.
# - Domain expertise
# Remove g0_Delinq_SD_5 and g0_Delinq_SD_6 since g0_Delinq_SD_4 is already present

# - Final variables (expert judgement)
# Arrears removed and TimeInDelinqState as it caused the model to not converge

vars <- c("log(TimeInDefSpell)*DefSpell_Num_binned", "g0_Delinq", 
          "slc_curing_ind", "DefaultStatus1_Aggr_Prop_Lag_12","g0_Delinq_Ave", 
          "g0_Delinq_Any_Aggr_Prop_Lag_1")
modLR <- glm( as.formula(paste("DefSpell_Event ~", paste(vars, collapse = " + "))),
              data=datCredit_train, family="binomial")
summary(modLR);
evalLR(modLR, modLR_base, datCredit_train, targetFld="DefSpell_Event", predClass=1)
### RESULTS: AIC:   118,524; McFadden R^2:  46.56%; AUC:  97.28%.
# Didn't in include slc_acct_arr_dir_3 due to own judgement


# - Final variables 
vars <- c("log(TimeInDefSpell)*DefSpell_Num_binned", "g0_Delinq", 
          "slc_curing_ind", "DefaultStatus1_Aggr_Prop_Lag_12","g0_Delinq_Ave", 
          "g0_Delinq_Any_Aggr_Prop_Lag_1")
modLR <- glm( as.formula(paste("DefSpell_Event ~", paste(vars, collapse = " + "))),
              data=datCredit_train, family="binomial")
summary(modLR);
evalLR(modLR, modLR_base, datCredit_train, targetFld="DefSpell_Event", predClass=1)
### RESULTS: AIC:   118,524; McFadden R^2:  46.56%; AUC:  97.28%.




# ------ 5. Other portfolio-level variables


# ------ 5.1 Which lag order is the best in calculating the median interest rate of the portfolio?

# - Initialize variables to be tested
vars <- c("InterestRate_Margin_Aggr_Med", "InterestRate_Margin_Aggr_Med_1", "InterestRate_Margin_Aggr_Med_2",
          "InterestRate_Margin_Aggr_Med_3", "InterestRate_Margin_Aggr_Med_9")

# - Single-factor modelling results
# Goodness-of-fit
aicTable_LS(datCredit_train, vars, TimeDef=c("Cox_Discrete","LossRate_Real"), genPath=genObjPath, modelType="tweedie")
# Discriminatory power (in-sample)
concTable(datCredit_train, datCredit_valid, vars, TimeDef=c("Cox_Discrete","DefSpell_Event"), genPath=genObjPath, modelType="Cox_Discrete")
### RESULTS: Best AIC-results: InterestRate_Margin_Aggr_Med_3 , InterestRate_Margin_Aggr_Med_9, InterestRate_Margin_Aggr_Med_2,
# InterestRate_Margin_Aggr_Med_1. InterestRate_Margin_Aggr_Med
# Best Deviance: nterestRate_Margin_Aggr_Med_3 , InterestRate_Margin_Aggr_Med_9, InterestRate_Margin_Aggr_Med_2,
# InterestRate_Margin_Aggr_Med_1. InterestRate_Margin_Aggr_Med

### Conclusion
#  Lag 3 performed the best over the two-metrics with lag 9 performing 2nd best
# However the difference in AIC is minor


# ------ 5.2 How do other portfolio-level (non-delinquency) variables fare as single-factor models?

# - Initialize variables to be tested
vars <- c("InstalmentToBalance_Aggr_Prop", "AgeToTerm_Aggr_Mean", "DefSpell_Maturity_Aggr_Mean", "NewLoans_Aggr_Prop")

# - Single-factor modelling results
# Goodness-of-fit
aicTable_LS(datCredit_train, vars, TimeDef=c("Cox_Discrete","LossRate_Real"), genPath=genObjPath, modelType="tweedie")
# Discriminatory power (in-sample)
concTable(datCredit_train, datCredit_valid, vars, TimeDef=c("Cox_Discrete","DefSpell_Event"), genPath=genObjPath, modelType="Cox_Discrete")
### RESULTS: Best AIC-results: NewLoans_Aggr_Prop DefSpell_Maturity_Aggr_Mean,AgeToTerm_Aggr_Mean, InstalmentToBalance_Aggr_Prop
# Best Deviance: NewLoans_Aggr_Prop DefSpell_Maturity_Aggr_Mean,AgeToTerm_Aggr_Mean, InstalmentToBalance_Aggr_Prop

### CONCLUSION: NewLoans_Aggr_Prop DefSpell_Maturity_Aggr_Mean,AgeToTerm_Aggr_Mean


# ------ 5.3 Combining insights: Delinquency-themed and portfolio-level variables

# - Initialize variables to be tested
vars <- c("log(TimeInDefSpell)*DefSpell_Num_binned", "g0_Delinq", 
          "slc_curing_ind", "DefaultStatus1_Aggr_Prop_Lag_12","g0_Delinq_Ave", 
          "g0_Delinq_Any_Aggr_Prop_Lag_1","InterestRate_Margin_Aggr_Med_2", 
          "InterestRate_Margin_Aggr_Med_3", "InterestRate_Margin_Aggr_Med_9",
          "InstalmentToBalance_Aggr_Prop_adj_WOff","AgeToTerm_Aggr_Mean", "NewLoans_Aggr_Prop")
# - Full model | Stepwise forward selection procedure
modLR_full <- glm( as.formula(paste("DefSpell_Event ~", paste(vars, collapse = " + "))),
                   data=datCredit_train, family="binomial")
summary(modLR_full);
evalLR(modLR_full, modLR_base, datCredit_train, targetFld="DefSpell_Event", predClass=1)
### RESULTS: AIC:   118,480; McFadden R^2:  46.59%; AUC:  97.29%.

# - Stepwise forward selection using BIC
ptm <- proc.time() # for runtime calculations (ignore)
modLR_step <- stepAIC(modLR_base2, scope = list(lower = ~ log(TimeInDefSpell)*DefSpell_Num_binned, 
                                                upper = as.formula(paste("~", paste(vars, collapse = " + ")))), 
                      direction = "both", k=log(datCredit_train[,.N]), maxit=50)
summary(modLR_step)
evalLR(modLR_step, modLR_base, datCredit_train, targetFld="DefSpell_Event", predClass=1)
proc.time() - ptm # IGNORE: elapsed runtime; 36m
### RESULTS: AIC:   118,492; McFadden R^2:  46.58%; AUC:  97.28%.

# Final variables(Expert judgement)
# Included InterestRate_Margin_Med_9
vars <- c("log(TimeInDefSpell)*DefSpell_Num_binned", "g0_Delinq", 
          "slc_curing_ind", "DefaultStatus1_Aggr_Prop_Lag_12","g0_Delinq_Ave", 
          "InterestRate_Margin_Aggr_Med_9","NewLoans_Aggr_Prop","g0_Delinq_Any_Aggr_Prop_Lag_1")
modLR <- glm( as.formula(paste("DefSpell_Event ~", paste(vars, collapse = " + "))),
              data=datCredit_train, family="binomial")
summary(modLR);
evalLR(modLR, modLR_base, datCredit_train, targetFld="DefSpell_Event", predClass=1)
### RESULTS: AIC:   118,878; McFadden R^2:  46,40%; AUC:  97.27%.



# ------ 6. Account-level variables

# ------ 6.1 How do various non-delinquency account-level variables fare as single-factor models?

vars <- c("Principal_Real", "Principal", "InterestRate_Margin", 
          "Balance_Real_1", "Balance_1", "Instalment_Real", "InterestRate_Nom", "AgeToTerm",
          "BalanceToPrincipal_1", "slc_acct_pre_lim_perc_imputed_med")

# - Correlation analysis towards obtaining clusters of correlated variables
corrAnalysis(datCredit_train, vars, corrThresh = 0.6, method = 'spearman')
### RESULTS:
# Absolute correlations of  97%  found for  Principal_Real  and  Principal 
# Absolute correlations of  91%  found for  Principal_Real  and  Balance_Real_1 
# Absolute correlations of  87%  found for  Principal  and  Balance_Real_1 
# Absolute correlations of  90%  found for  Principal_Real  and  Balance_1 
# Absolute correlations of  90%  found for  Principal  and  Balance_1 
# Absolute correlations of  98%  found for  Balance_Real_1  and  Balance_1 
# Absolute correlations of  89%  found for  Principal_Real  and  Instalment_Real 
# Absolute correlations of  86%  found for  Principal  and  Instalment_Real 
# Absolute correlations of  93%  found for  Balance_Real_1  and  Instalment_Real 
# Absolute correlations of  92%  found for  Balance_1  and  Instalment_Real 
# Absolute correlations of  75%  found for  Balance_Real_1  and  BalanceToPrincipal_1 
# Absolute correlations of  72%  found for  Balance_1  and  BalanceToPrincipal_1 
# Absolute correlations of  63%  found for  Instalment_Real  and  BalanceToPrincipal_1

# - Initialize variables to be tested
vars <- c("Principal_Real", "Principal", "InterestRate_Margin_imputed_mean", "pmnt_method_grp",
          "Balance_Real_1", "Balance_1", "Instalment_Real", "InterestRate_Nom", "AgeToTerm",
          "BalanceToPrincipal_1", "slc_acct_pre_lim_perc_imputed_med")

# - Single-factor modelling results
# Goodness-of-fit
aicTable(datCredit_train, vars, TimeDef=c("Cox_Discrete","DefSpell_Event"), genPath=genObjPath, modelType="Cox_Discrete")
# Discriminatory power (in-sample)
concTable(datCredit_train, datCredit_valid, vars, TimeDef=c("Cox_Discrete","DefSpell_Event"), genPath=genObjPath, modelType="Cox_Discrete")
### RESULTS: Best AIC-results: InterestRate_Nom, InterestRate_Margin_imputed_mean, Balance_adj_WOff, etc from there all very similar
# Best C-statistics: InterestRate_Nom, InterestRate_Margin_imputed_mean, Balance_adj_WOff, from there similar
# ALL of the have very low AIC and good Harell's c values>0 
# Choose these 5


# ------ 6.2 Combining insights: Delinquency-themed, portfolio-level, and account-level variables

# - Initialize variables to be tested
vars <- c("log(TimeInDefSpell)*DefSpell_Num_binned", "g0_Delinq_Lag_1", 
          "DefaultStatus1_Aggr_Prop_Lag_12","g0_Delinq_Ave", 
          "InterestRate_Margin_Aggr_Med_9","NewLoans_Aggr_Prop","InterestRate_Nom",
          "Balance_1","Principal","InterestRate_Margin_imputed_mean","pmnt_method_grp")

### RESULTS:

# - Full model | Stepwise forward selection procedure
modLR_full <- glm( as.formula(paste("DefSpell_Event ~", paste(vars, collapse = " + "))),
                   data=datCredit_train, family="binomial")
summary(modLR_full);
evalLR(modLR_full, modLR_base, datCredit_train, targetFld="DefSpell_Event", predClass=1)
### RESULTS: AIC:   49,781; McFadden R^2:  77.57%; AUC:  99.39%.


# - Stepwise forward selection using BIC
ptm <- proc.time() # for runtime calculations (83m) 
modLR_step <- stepAIC(modLR_base2, scope = list(lower = ~ log(TimeInDefSpell)*DefSpell_Num_binned, 
                                                upper = as.formula(paste("~", paste(vars, collapse = " + ")))), 
                      direction = "both", k=log(datCredit_train[,.N]), maxit=50)
summary(modLR_step)
evalLR(modLR_step, modLR_base, datCredit_train, targetFld="DefSpell_Event", predClass=1)
proc.time() - ptm
### RESULTS: AIC:   49,780; McFadden R^2:  77.57%; AUC:  99.39%.

# - Final variables
vars <- c("log(TimeInDefSpell)*DefSpell_Num_binned", "g0_Delinq_Lag_1", 
          "DefaultStatus1_Aggr_Prop_Lag_12","g0_Delinq_Ave", 
          "InterestRate_Margin_Aggr_Med_9","NewLoans_Aggr_Prop","InterestRate_Nom",
          "Balance_1","Principal","pmnt_method_grp")
modLR <- glm( as.formula(paste("DefSpell_Event ~", paste(vars, collapse = " + "))),
              data=datCredit_train, family="binomial")
summary(modLR);
evalLR(modLR, modLR_base, datCredit_train, targetFld="DefSpell_Event", predClass=1)
### RESULTS: AIC:   49,780; McFadden R^2:  77.57%; AUC:  99.39%.




# ------ 7. Macroeconomic variables

# ------ 7.1 Which lag order is the best for: M_Repo_Rate

# - Initialize variables to be tested
vars <- c("M_Repo_Rate", "M_Repo_Rate_1 ", "M_Repo_Rate_2", "M_Repo_Rate_3", "M_Repo_Rate_6", "M_Repo_Rate_9", "M_Repo_Rate_12")

# - Single-factor modelling results
# Goodness-of-fit
aicTable(datCredit_train, vars, TimeDef=c("Cox_Discrete","DefSpell_Event"), genPath=genObjPath, modelType="Cox_Discrete")
# Discriminatory power (in-sample)
concTable(datCredit_train, datCredit_valid, vars, TimeDef=c("Cox_Discrete","DefSpell_Event"), genPath=genObjPath, modelType="Cox_Discrete")
### RESULTS: Best AIC-results: M_Repo_Rate_12, M_Repo_Rate_9, M_Repo_Rate, M_Repo_Rate_1, 
# M_Repo_Rate_2, M_Repo_Rate_6, M_Repo_Rate_3.
# Best C-statistics: M_Repo_Rate_12, M_Repo_Rate_9, M_Repo_Rate_6, M_Repo_Rate, 
# M_Repo_Rate_1, M_Repo_Rate_2, M_Repo_Rate_3.

### CONCLUSION
# There are minor difference between AIC value which suggest the main repo rate
# and lags 1 and 0 are best. C-differences are small toe (ranging from 50%-52%).
# Choose: M_Repo_Rate_12, M_Repo_Rate_9, M_Repo_Rate


# ------ 7.2 Which lag order is the best for: M_Inflation_Growth

# - Initialize variables to be tested
vars <- c("M_Inflation_Growth", "M_Inflation_Growth_1 ", "M_Inflation_Growth_2", "M_Inflation_Growth_3", 
          "M_Inflation_Growth_6", "M_Inflation_Growth_9", "M_Inflation_Growth_12")

# - Single-factor modelling results
# Goodness-of-fit
aicTable(datCredit_train, vars, TimeDef=c("Cox_Discrete","DefSpell_Event"), genPath=genObjPath, modelType="Cox_Discrete")
# Discriminatory power (in-sample)
concTable(datCredit_train, datCredit_valid, vars, TimeDef=c("Cox_Discrete","DefSpell_Event"), genPath=genObjPath, modelType="Cox_Discrete")
### RESULTS: Best AIC-results: M_Inflation_Growth_12, M_Inflation_Growth, M_Inflation_Growth_1, M_Inflation_Growth_2, 
# M_Inflation_Growth_3,M_Inflation_Growth_9, M_Inflation_Growth_6 .
# Best C-statistics: MM_Inflation_Growth, M_Inflation_Growth_1, M_Inflation_Growth_2, M_Inflation_Growth_12, 
# M_Inflation_Growth_3,M_Inflation_Growth_9, M_Inflation_Growth_6 .

### CONCLUSION: Small AIC and Harell's c-statsitics differences
# Best variables: M_Inflation_Growth_12, M_Inflation_Growth, M_Inflation_Growth_1


# ------ 7.3 Which lag order is the best for: M_RealGDP_Growth

# - Initialize variables to be tested
vars <- c("M_RealGDP_Growth", "M_RealGDP_Growth_1 ", "M_RealGDP_Growth_2", "M_RealGDP_Growth_3", 
          "M_RealGDP_Growth_6", "M_RealGDP_Growth_9", "M_RealGDP_Growth_12")

# - Single-factor modelling results
# Goodness-of-fit
aicTable(datCredit_train, vars, TimeDef=c("Cox_Discrete","DefSpell_Event"), genPath=genObjPath, modelType="Cox_Discrete")
# Discriminatory power (in-sample)
concTable(datCredit_train, datCredit_valid, vars, TimeDef=c("Cox_Discrete","DefSpell_Event"), genPath=genObjPath, modelType="Cox_Discrete")
### RESULTS: Best AIC-results: M_RealGDP_Growth_6, M_RealGDP_Growth_3, M_RealGDP_Growth_2, M_RealGDP_Growth_1, 
# M_RealGDP_Growth, M_RealGDP_Growth_9, M_RealGDP_Growth_12.
# Best C-statistics: M_RealGDP_Growth_3, M_RealGDP_Growth_6, M_RealGDP_Growth_2, M_RealGDP_Growth_1, 
# M_RealGDP_Growth, M_RealGDP_Growth_12, M_RealGDP_Growth_9.

### CONCLUSION: Middle lags seem better, based on very small AIC-differences and c-differences (57-54%)
# Choose: M_RealGDP_Growth_3, M_RealGDP_Growth_2, M_RealGDP_Growth_6


# ------ 7.4 Which lag order is the best for: M_RealIncome_Growth

# - Initialize variables to be tested
vars <- c("M_RealIncome_Growth", "M_RealIncome_Growth_1 ", "M_RealIncome_Growth_2", "M_RealIncome_Growth_3", 
          "M_RealIncome_Growth_6", "M_RealIncome_Growth_9", "M_RealIncome_Growth_12")

# - Single-factor modelling results
# Goodness-of-fit
aicTable(datCredit_train, vars, TimeDef=c("Cox_Discrete","DefSpell_Event"), genPath=genObjPath, modelType="Cox_Discrete")
# Discriminatory power (in-sample)
concTable(datCredit_train, datCredit_valid, vars, TimeDef=c("Cox_Discrete","DefSpell_Event"), genPath=genObjPath, modelType="Cox_Discrete")
### RESULTS: Best AIC-results: M_RealIncome_Growth_6, M_RealIncome_Growth_3, M_RealIncome_Growth_9, M_RealIncome_Growth_2, 
# M_RealIncome_Growth_12, M_RealIncome_Growth_1, M_RealIncome_Growth.
# Best C-statistics: M_RealIncome_Growth_6, M_RealIncome_Growth_3, M_RealIncome_Growth_9, M_RealIncome_Growth_12, 
# M_RealIncome_Growth_2, M_RealIncome_Growth_1, M_RealIncome_Growth.

### CONCLUSION: Middle lags seem better, based on very small AIC-differences and c-differences (50%-51%)
# # Choose:  M_RealIncome_Growth_6, M_RealIncome_Growth_3, M_RealIncome_Growth_9


# ------ 7.5 Which lag order is the best for: M_DTI_Growth

# - Initialize variables to be tested
vars <- c("M_DTI_Growth", "M_DTI_Growth_1 ", "M_DTI_Growth_2", "M_DTI_Growth_3", 
          "M_DTI_Growth_6", "M_DTI_Growth_9", "M_DTI_Growth_12")

# - Single-factor modelling results
# Goodness-of-fit
aicTable(datCredit_train, vars, TimeDef=c("Cox_Discrete","DefSpell_Event"), genPath=genObjPath, modelType="Cox_Discrete")
# Discriminatory power (in-sample)
concTable(datCredit_train, datCredit_valid, vars, TimeDef=c("Cox_Discrete","DefSpell_Event"), genPath=genObjPath, modelType="Cox_Discrete")
### RESULTS: Best AIC-results: M_DTI_Growth_12, M_DTI_Growth_9, M_DTI_Growth_6, M_DTI_Growth_3, M_DTI_Growth_3, 
# M_DTI_Growth_2,M_DTI_Growth .
# Best C-statistics: M_DTI_Growth_12, M_DTI_Growth_9, M_DTI_Growth_6, M_DTI_Growth_3, M_DTI_Growth_3, 
# M_DTI_Growth_2,M_DTI_Growth 

### CONCLUSION: Longer lags seem better, based on very small AIC-differences,
# trend exist across c-differences (54-56%)
# Choose: M_DTI_Growth_12, M_DTI_Growth_9, M_DTI_Growth_6


# ------ 7.6 Which lag order is the best for: M_Emp_Growth

# - Initialize variables to be tested
vars <- c("M_Emp_Growth", "M_Emp_Growth_1 ", "M_Emp_Growth_2", "M_Emp_Growth_3", 
          "M_Emp_Growth_6", "M_Emp_Growth_9", "M_Emp_Growth_12")

# - Single-factor modelling results
# Goodness-of-fit
aicTable(datCredit_train, vars, TimeDef=c("Cox_Discrete","DefSpell_Event"), genPath=genObjPath, modelType="Cox_Discrete")
# Discriminatory power (in-sample)
concTable(datCredit_train, datCredit_valid, vars, TimeDef=c("Cox_Discrete","DefSpell_Event"), genPath=genObjPath, modelType="Cox_Discrete")
### RESULTS: Best AIC-results: M_Emp_Growth_6, M_Emp_Growth_9, M_Emp_Growth_12, M_Emp_Growth_3
# M_Emp_Growth_2, M_Emp_Growth_1, M_Emp_Growth
# Best C-statistics: M_Emp_Growth_6, M_Emp_Growth_12, M_Emp_Growth_9, M_Emp_Growth_3
# M_Emp_Growth_2, M_Emp_Growth_1, M_Emp_Growth

### CONCLUSION: Middle to late lags seem better, based on very small AIC-differences, affirmed by the c-differences (50-51%)
# Choose: M_Emp_Growth_6, M_Emp_Growth_9, M_Emp_Growth_12 

# ------ 7.7 Combining insights: Macroeconomic variables


# - Initialize variables to be tested
vars <- c("log(TimeInDefSpell)*DefSpell_Num_binned",
          "M_Repo_Rate", "M_Repo_Rate_12", "M_Repo_Rate_9", 
          "M_Inflation_Growth_12", "M_Inflation_Growth", "M_Inflation_Growth_1",
          "M_RealGDP_Growth_2", "M_RealGDP_Growth_3", "M_RealGDP_Growth_6",
          "M_RealIncome_Growth_9", "M_RealIncome_Growth_3", "M_RealIncome_Growth_6",
          "M_DTI_Growth_9", "M_DTI_Growth_12", "M_DTI_Growth_6", 
          "M_Emp_Growth_12", "M_Emp_Growth_9", "M_Emp_Growth_6")

# - Full model | Stepwise forward selection procedure
modLR_full <- glm( as.formula(paste("DefSpell_Event ~", paste(vars, collapse = " + "))),
                   data=datCredit_train, family="binomial")
summary(modLR_full);
evalLR(modLR_full, modLR_base, datCredit_train, targetFld="DefSpell_Event", predClass=1)
### RESULTS: AIC:  213,158; McFadden R^2:  3.9%; AUC:  68.81%.

# - Stepwise forward selection using BIC
ptm <- proc.time() # for runtime calculations (ignore)
modLR_step <- stepAIC(modLR_base2, scope = list(lower = ~ log(TimeInDefSpell)*DefSpell_Num_binned, 
                                                upper = as.formula(paste("~", paste(vars, collapse = " + ")))), 
                      direction = "both", k=log(datCredit_train[,.N]), maxit=50)
summary(modLR_step)
evalLR(modLR_step, modLR_base, datCredit_train, targetFld="DefSpell_Event", predClass=1)
proc.time() - ptm # IGNORE: elapsed runtime; 140m
### RESULTS: AIC:   213,183;   McFadden R^2:  3.88%; AUC:  68.76%.


# - Final variables (Expert Judgement)
vars <- c("log(TimeInDefSpell)*DefSpell_Num_binned",
          "M_RealIncome_Growth_9", "M_Inflation_Growth_12","M_DTI_Growth_12","M_Repo_Rate_12")
modLR <- glm( as.formula(paste("DefSpell_Event ~", paste(vars, collapse = " + "))),
              data=datCredit_train, family="binomial")
summary(modLR);
evalLR(modLR, modLR_base, datCredit_train, targetFld="DefSpell_Event", predClass=1)
### RESULTS: AIC:   214,631;   McFadden R^2:  3.22%; AUC:  67.61%.
# Included the Repo rate as it is vital. GDP growth and emp growth have a negative effect
# They increase the AIC and R^2 values


# ------ 7.8 Combining insights: Delinquency-themed, portfolio-level, account-level, and macroeconomic variables

# - Initialize variables to be tested
vars <- c("log(TimeInDefSpell)*DefSpell_Num_binned", "g0_Delinq_Lag_1", 
          "DefaultStatus1_Aggr_Prop_Lag_12","g0_Delinq_Ave", "g0_Delinq_Any_Aggr_Prop_Lag_1",
          "InterestRate_Margin_Aggr_Med_9","NewLoans_Aggr_Prop","InterestRate_Nom",
          "Balance_1","Principal","pmnt_method_grp",
          "M_RealIncome_Growth_9", "M_Inflation_Growth_12","M_DTI_Growth_12","M_Repo_Rate_12")


# - Full model | Stepwise forward selection procedure
modLR_full <- glm( as.formula(paste("DefSpell_Event ~", paste(vars, collapse = " + "))),
                   data=datCredit_train, family="binomial")
summary(modLR_full);
evalLR(modLR_full, modLR_base, datCredit_train, targetFld="DefSpell_Event", predClass=1)
### RESULTS: AIC:   80,968;   McFadden R^2:  63.51%; AUC:  99.40%.

# - Stepwise forward selection using BIC
ptm <- proc.time() # for runtime calculations (ignore)
modLR_step <- stepAIC(modLR_base2, scope = list(lower = ~ log(TimeInDefSpell)*DefSpell_Num_binned, 
                                                upper = as.formula(paste("~", paste(vars, collapse = " + ")))), 
                      direction = "both", k=log(datCredit_train[,.N]), maxit=50)
summary(modLR_step)
evalLR(modLR_step, modLR_base, datCredit_train, targetFld="DefSpell_Event", predClass=1)
proc.time() - ptm # IGNORE: elapsed runtime; 117m
### RESULTS: AIC:  26,008;  McFadden R^2:  85.41%; AUC:  99.91%.

# - Domain expertise
# Include log(TimeInDefSpell) with Time_Binned for final modelling iteration












calc_AIC_LS <- function(formula, data_train, variables="", it=NA, logPath="", 
                        fldSpellID="DefSpell_Key", modelType="tweedie") {
  # - Testing conditions
  # j <- 1; formula=TimeDef_Form(TimeDef,variables[j], strataVar=strataVar); 
  
  tryCatch({
    if (modelType=="tweedie") {
      model <- cpglm(formula,data= data_train)# Fit tweedie model 
    } else if (modelType=="Cox_Discrete") {
      model <- glm(formula,data = data_train, family="binomial") # Fit discrete-time Cox model 
    } else stop("Unknown model type in calc_AIC().")
    
    if (!is.na(it)) {# Output the number of models built, where the log is stored in a text file afterwards.
      cat(paste0("\n\t ", it,") Single-factor model built. "),
          file=paste0(logPath,"AIC_log_LS.txt"), append=T)
    }
    
    AIC <- AIC(model) # Calculate AIC of the model.
    Dev <- deviance(model)
    c_ind <- rcorr.cens(data_train$score, data_train$LossRate_Real)[["C Index"]]
    # Return results as a data.table
    if (modelType=="tweedie") {
      return(data.table(Variable = variables, AIC = AIC, Deviance= Dev, C=c_ind,pValue=summary(model)$coefficients[1,4]))
    } else if (modelType=="Cox_Discrete") {
      return(data.table(Variable = variables, AIC = AIC,Deviance= Dev,,C=c_ind, pValue=summary(model)$coefficients[1,4]))
    }
    
    
  }, error=function(e) {
    AIC <- Inf
    Dev <- Inf
    c_ind <- Inf
    if (!is.na(it)) {
      cat(paste0("\n\t ", it,") Single-factor model failed. "),
          file=paste0(logPath,"AIC_log_LS.txt"), append=T)
    }
    return(data.table(Variable = variables, AIC = AIC,Deviance= Dev, pValue=NA,C=c_ind)) 
  })
}


# --- Function to extract the Akaike Information Criterion (AIC) from single-factor models
# Input:  [data_train]: Training data; [data_valid]: [variables]: List of variables used to build single-factor models;
#         [fldSpellID]: Field name of spell-level ID; [TimeDef]: Time definition incorporated.
#         [numThreads]: Number of threads used; [genPath]: Optional path for log file. 
# Output: [matResults]: Result matrix.
aicTable_LS <- function(data_train, variables, fldSpellID="DefSpell_Key",
                        TimeDef, numThreads=6, genPath, strataVar="", modelType="tweedie") {
  # - Testing conditions
  # data_train <- datCredit; TimeDef=Cox_Discrete","PerfSpell_Event; numThreads=6
  # fldSpellID<-" DefSpell_Key"; variables<-"g0_Delinq_SD_4"; strataVar="DefSpell_Num"
  
  # - Iterate across loan space using a multi-threaded setup
  ptm <- proc.time() #IGNORE: for computation time calculation
  cl.port <- makeCluster(round(numThreads)); registerDoParallel(cl.port) # multi-threading setup
  cat("New Job: Estimating AIC for each variable as a single-factor model ..",
      file=paste0(genPath,"AIC_log_LS.txt"), append=F)
  
  results <- foreach(j=1:length(variables), .combine='rbind', .verbose=F, .inorder=T,
                     .packages=c('data.table', 'survival','cplm','Hmisc'), .export=c('calc_AIC_LS', 'TimeDef_Form')) %dopar%
    { # ----------------- Start of Inner Loop -----------------
      # - Testing conditions
      # j <- 1
      calc_AIC_LS(formula=TimeDef_Form(TimeDef,variables[j], strataVar=strataVar), variables=variables[j],
                  data_train=data_train, it=j, logPath=genPath,  fldSpellID=fldSpellID, modelType=modelType)
    } # ----------------- End of Inner Loop -----------------
  stopCluster(cl.port); proc.time() - ptm  
  
  # Sort by concordance in ascending order.
  setorder(results, AIC)
  
  # Return resulting table.
  return(results)
}


# Evaluate Tweedie cpglm performance (no weights)
#), MAE, RMSE
# Optional: supply model_base or let the function fit an intercept-only baseline.
# Optional: pass newdata for OOS MAE/RMSE.
evalLS <- function(model_full,dat_train, targetFld,model_base = NULL) {
  
  
  y_tr  <- dat_train[[targetFld]]
  mu_tr <- predict(model_full, newdata = dat_train, type = "response")
  rmse_tr <- sqrt(mean((mu_tr - y_tr)^2))
  mae_tr  <- mean(abs(mu_tr - y_tr))
  
  aic_full <- AIC(model_full)
  aic_base <- AIC(model_base)
  dev_full <- deviance(model_full)        # residual deviance of full model
  dev_base <- deviance(model_base)        # residual deviance of baseline
  
  # Pseudo R^2 (deviance explained): 1 - D_full / D_base
  pseudo_r2 <- 1 - (dev_full / dev_base)
  data.table(AIC_full = aic_full,AIC_base = aic_base, dAIC = aic_full - aic_base,
             Deviance_full = dev_full,
             Deviance_base = dev_base,
             PseudoR2_Deviance = pseudo_r2,
             Train_RMSE = rmse_tr,
             Train_MAE  = mae_tr
  )
}



