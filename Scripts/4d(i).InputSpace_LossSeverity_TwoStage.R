# ======================================= INPUT SPACE: LOSS SEVERITY-TWO STAGE============================
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

# - Use only default spells 
datCredit_train <- datCredit_train_CDH[!is.na(DefSpell_Key)&DefSpellResol_Type_Hist=="WOFF",]
datCredit_valid <- datCredit_valid_CDH[!is.na(DefSpell_Key)&DefSpellResol_Type_Hist=="WOFF",]

# - filter to maximum spell counter
datCredit_train <- datCredit_train[, .SD[which.max(DefSpell_Counter)], by = LoanID]
datCredit_valid <- datCredit_valid[, .SD[which.max(DefSpell_Counter)], by = LoanID]

# Identify where the loss rate is out of bounds and not feasible
datCredit_train <- datCredit_train[, OOB_Ind := ifelse(LossRate_Real <=0 | LossRate_Real > 1, 1,0)]
datCredit_valid <- datCredit_valid[, OOB_Ind := ifelse(LossRate_Real <= 0 | LossRate_Real > 1, 1,0)]

# Subset to include only relevant data
datCredit_train <- subset(datCredit_train, OOB_Ind == 0)
datCredit_valid <- subset(datCredit_valid, OOB_Ind == 0)

# remove previous objects from memory
rm(datCredit_train_CDH, datCredit_valid_CDH); gc()

modLR_base <- cpglm(LossRate_Real ~ 1, data=datCredit_train)



# ------ 2. Delinquency-themed variables
#----- 2.1 Which lag order is the best in calculating the portfolio-level fraction of the defaulted proportion with any delinquency?
# indicates the lag order of past data that should be included  when assessing an account
# - Initialize variables to be tested
vars <- c("g0_Delinq_Any_Aggr_Prop", "g0_Delinq_Any_Aggr_Prop_Lag_1", "g0_Delinq_Any_Aggr_Prop_Lag_2",
          "g0_Delinq_Any_Aggr_Prop_Lag_3", "g0_Delinq_Any_Aggr_Prop_Lag_4", "g0_Delinq_Any_Aggr_Prop_Lag_5",
          "g0_Delinq_Any_Aggr_Prop_Lag_6", "g0_Delinq_Any_Aggr_Prop_Lag_9", "g0_Delinq_Any_Aggr_Prop_Lag_12" )

# - Single-factor modelling results
# Goodness-of-fit
aicTable_LS(datCredit_train, vars, TimeDef=c("Cox_Discrete","LossRate_Real"), genPath=genObjPath, modelType="tweedie")


### RESULTS: Best AIC-results: g0_Delinq_Any_Aggr_Prop_Lag_3, g0_Delinq_Any_Aggr_Prop_Lag_2, g0_Delinq_Any_Aggr_Prop_Lag_1, 
# , g0_Delinq_Any_Aggr_Prop_4
# Best Deviance : g0_Delinq_Any_Aggr_Prop_Lag_3, g0_Delinq_Any_Aggr_Prop_Lag_2, g0_Delinq_Any_Aggr_Prop_Lag_1, 
# , g0_Delinq_Any_Aggr_Prop_4
# Conclusion: The differences in AIC and deviance are minor
# Choose the overall top 3 performing variables g0_Delinq_Any_Aggr_Prop_3 g0_Delinq_Any_Aggr_Prop_Lag_2, g0_Delinq_Any_Aggr_Prop_Lag_1
# All are above 50% 


# ------ 2.2 Which lag order is the best in calculating the portfolio-level fraction of defaulted accounts during the default spell?
vars <- c("DefaultStatus1_Aggr_Prop", "DefaultStatus1_Aggr_Prop_Lag_1", "DefaultStatus1_Aggr_Prop_Lag_2",
          "DefaultStatus1_Aggr_Prop_Lag_3", "DefaultStatus1_Aggr_Prop_Lag_4", "DefaultStatus1_Aggr_Prop_Lag_5",
          "DefaultStatus1_Aggr_Prop_Lag_6", "DefaultStatus1_Aggr_Prop_Lag_9", "DefaultStatus1_Aggr_Prop_Lag_12" )

# - Single-factor modelling results
# Goodness-of-fit
aicTable_LS(datCredit_train, vars, TimeDef=c("Cox_Discrete","LossRate_Real"), genPath=genObjPath, modelType="tweedie")
### RESULTS: Best AIC-results: DefaultStatus1_Aggr_Prop_Lag_6, DefaultStatus1_Aggr_Prop_Lag_5, DefaultStatus1_Aggr_Prop_Lag_4

# Differences are not that major
# Best Deviance: DefaultStatus1_Aggr_Prop_Lag_6, DefaultStatus1_Aggr_Prop_Lag_5, DefaultStatus1_Aggr_Prop_Lag_4
# Similar trend as to AIC
# Best C: DefaultStatus1_Aggr_Prop_Lag_12, DefaultStatus1_Aggr_Prop_Lag_9, DefaultStatus1_Aggr_Prop_Lag_6
# Similar trend as to AIC


### CONCLUSION: Later is better, though the AIC-differences were minuscule.
# Top 3 varibales: DefaultStatus1_Aggr_Prop_Lag_6, DefaultStatus1_Aggr_Prop_Lag_5, DefaultStatus1_Aggr_Prop_Lag_4


# ------ 2.3 How do other portfolio-level delinquency-themed variables fare as single-factor models?

# - Initialize variables to be tested
vars <- c("g0_Delinq_Ave", "ArrearsToBalance_Aggr_Prop", "CuringEvents_Aggr_Prop" )

# - Single-factor modelling results
# Goodness-of-fit
aicTable_LS(datCredit_train, vars, TimeDef=c("Cox_Discrete","LossRate_Real"), genPath=genObjPath, modelType="tweedie")

### RESULTS: Best AIC-results:   g0_Delinq_Ave,ArrearsToBalance_Aggr_Prop,CuringEvents_Aggr_Prop
# However they aren't that big with their being little difference between the lowest and highest
# Best Deviance: g0_Delinq_Ave,ArrearsToBalance_Aggr_Prop,CuringEvents_Aggr_Prop
# C-statistic the same


### Conclusion: All are significant include all 3 of them


# ------ 2.4 How do account-level delinquency-themed variables fare as single-factor models?

# - Initialize variables to be tested
vars <- c("g0_Delinq_fac", "g0_Delinq", "g0_Delinq_Lag_1", "slc_acct_arr_dir_3_Change_Ind",
          "g0_Delinq_Num", "slc_acct_arr_dir_3", "slc_acct_roll_ever_24_imputed_mean", "Arrears", "PrevDefaults","TimeInDelinqState"
          ,"slc_past_due_amt_imputed_med", "slc_curing_ind","DefSpell_Age","DefSpell_Num_binned")

# - Single-factor modelling results
# Goodness-of-fit
aicTable_LS(datCredit_train, vars, TimeDef=c("Cox_Discrete","LossRate_Real"), genPath=genObjPath, modelType="tweedie")
# Discriminatory power (in-sample)
### RESULTS: Best AIC-results:  PrevDefaults,DefSpell_Num_binned, g0_Delinq_Num, g0_Delinq_Lag_1, slc_curing_ind,slc_acct_dir_3
# Best Deviance: PrevDefaults,DefSpell_Num_binned, g0_Delinq_Num, g0_Delinq_Lag_1, slc_curing_ind,slc_acct_dir_3
### CONCLUSION
# Choose:  PrevDeafults, slc_acct_arr_dir_3, DefSpell_Num_binned, DefSpell_Age,g0_Delinq_Num


# ------ 2.5 Combining insights: delinquency-themed variables
# - Initialize variables to be tested
vars <- c("PrevDefaults","g0_Delinq_Any_Aggr_Prop_Lag_2",
          "g0_Delinq_Any_Aggr_Prop_Lag_3", "g0_Delinq_Any_Aggr_Prop_Lag_1",
          "DefaultStatus1_Aggr_Prop_Lag_5","DefaultStatus1_Aggr_Prop_Lag_6", "DefaultStatus1_Aggr_Prop_Lag_4",
          "g0_Delinq_Ave", "ArrearsToBalance_Aggr_Prop", "CuringEvents_Aggr_Prop","g0_Delinq_Num",
          "slc_acct_arr_dir_3","DefSpell_Age","DefSpell_Num_binned")
# - Full model | Stepwise forward selection procedure
modLR_full <- cpglm( as.formula(paste("LossRate_Real ~", paste(vars, collapse = " + "))),
                     data=datCredit_train)
summary(modLR_full);
evalLS(modLR_full,datCredit_train,targetFld="LossRate_Real",modLR_base)

### RESULTS: AIC:   3099.125; R^2:  4.044%; RMSE:  30.54%; MAE:  25.63%
modLR_step <- stepwise_cpglm_both(modLR_base, modLR_full,datCredit_train)
summary(modLR_step)
evalLS(modLR_step,datCredit_train,targetFld="LossRate_Real",modLR_base)

proc.time() - ptm # IGNORE: elapsed runtime; 70m
### RESULTS: AIC: 3094.623;  R^2:  3.97%%; RMSE:  30.55%; MAE:  25.65%
# - Domain expertise


# - Final variables (expert judgement)
# included DefaultStatus1_Aggr_Prop_Lag_6

vars <- c("PrevDefaults","g0_Delinq_Any_Aggr_Prop_Lag_3","DefaultStatus1_Aggr_Prop_Lag_6", 
          "slc_acct_arr_dir_3","DefSpell_Age","DefSpell_Num_binned")
modLR <- cpglm( as.formula(paste("LossRate_Real ~", paste(vars, collapse = " + "))),
                data=datCredit_train)
summary(modLR);
evalLS(modLR,datCredit_train,targetFld="LossRate_Real",modLR_base)

### RESULTS: AIC: 3095.319;  R^2:  3.99%%; RMSE:  30.55%; MAE:  25.65%

vars <- c("PrevDefaults","g0_Delinq_Any_Aggr_Prop_Lag_3","DefaultStatus1_Aggr_Prop_Lag_6", 
          "slc_acct_arr_dir_3","DefSpell_Age","DefSpell_Num_binned")
modLR <- cpglm( as.formula(paste("LossRate_Real ~", paste(vars, collapse = " + "))),
                data=datCredit_train)
summary(modLR);
evalLS(modLR,datCredit_train,targetFld="LossRate_Real",modLR_base)

### RESULTS: AIC: 3095.319;  R^2:  3.99%%; RMSE:  30.55%; MAE:  25.65%




# ------ 3. Other portfolio-level variables


# ------ 3.1 Which lag order is the best in calculating the median interest rate of the portfolio?

# - Initialize variables to be tested
vars <- c("InterestRate_Margin_Aggr_Med", "InterestRate_Margin_Aggr_Med_1", "InterestRate_Margin_Aggr_Med_2",
          "InterestRate_Margin_Aggr_Med_3", "InterestRate_Margin_Aggr_Med_9")

# - Single-factor modelling results
# Goodness-of-fit
aicTable_LS(datCredit_train, vars, TimeDef=c("Cox_Discrete","LossRate_Real"), genPath=genObjPath, modelType="tweedie")

### RESULTS: Best AIC-results: InterestRate_Margin_Aggr_Med_2 , InterestRate_Margin_Aggr_Med_3, InterestRate_Margin_Aggr_Med_9,
# InterestRate_Margin_Aggr_Med_1. InterestRate_Margin_Aggr_Med
# Best Deviance: InterestRate_Margin_Aggr_Med_2 , InterestRate_Margin_Aggr_Med_3, InterestRate_Margin_Aggr_Med_9,
# InterestRate_Margin_Aggr_Med_1. InterestRate_Margin_Aggr_Med
# Best C: InterestRate_Margin_Aggr_Med_9 , InterestRate_Margin_Aggr_Med_3, InterestRate_Margin_Aggr_Med_2,
# InterestRate_Margin_Aggr_Med_1. InterestRate_Margin_Aggr_Med
### Conclusion
#  Later lags performed the best
# However the difference in AIC is minor


# ------ 3.2 How do other portfolio-level (non-delinquency) variables fare as single-factor models?

# - Initialize variables to be tested
vars <- c("InstalmentToBalance_Aggr_Prop", "AgeToTerm_Aggr_Mean", "DefSpell_Maturity_Aggr_Mean", "NewLoans_Aggr_Prop")

# - Single-factor modelling results
# Goodness-of-fit
aicTable_LS(datCredit_train, vars, TimeDef=c("Cox_Discrete","LossRate_Real"), genPath=genObjPath, modelType="tweedie")
### RESULTS: Best AIC-results: InstalmentToBalance_Aggr_Prop,AgeToTerm_Aggr_Mean,NewLoans_Aggr_Prop, DefSpell_Maturity_Aggr_Mean
# Best Deviance: InstalmentToBalance_Aggr_Prop,AgeToTerm_Aggr_Mean, NewLoans_Aggr_Prop, DefSpell_Maturity_Aggr_Mean,AgeToTerm_Aggr_Mean
# Best C: InstalmentToBalance_Aggr_Prop,NewLoans_Aggr_Prop, DefSpell_Maturity_Aggr_Mean,AgeToTerm_Aggr_Mean
### CONCLUSION: InstalmentToBalance_Aggr_Prop,NewLoans_Aggr_Prop ,AgeToTerm_Aggr_Mean


# ------ 3.3 Combining insights: Delinquency-themed and portfolio-level variables

# - Initialize variables to be tested
vars <- c("PrevDefaults","g0_Delinq_Any_Aggr_Prop_Lag_3","DefaultStatus1_Aggr_Prop_Lag_6", 
          "slc_acct_arr_dir_3","DefSpell_Age","DefSpell_Num_binned",
          "InterestRate_Margin_Aggr_Med_2","InterestRate_Margin_Aggr_Med_3", "InterestRate_Margin_Aggr_Med_9",
          "InstalmentToBalance_Aggr_Prop", "AgeToTerm_Aggr_Mean", "NewLoans_Aggr_Prop")
# - Full model | Stepwise forward selection procedure
modLR_full <-cpglm( as.formula(paste("LossRate_Real ~", paste(vars, collapse = " + "))),
                    data=datCredit_train)
summary(modLR_full);
evalLS(modLR_full,datCredit_train,targetFld="LossRate_Real",modLR_base)

### RESULTS: AIC: 3039.05; R^2:  4.61%; RMSE:  30.44%; MAE:  25.51%

# - Stepwise forward selection using AIC
ptm <- proc.time() # for runtime calculations (ignore)
modLR_step <- stepwise_cpglm_both(modLR_base, modLR_full,datCredit_train)

summary(modLR_step)

proc.time() - ptm # IGNORE: elapsed runtime; 503m
evalLS(modLR_step,datCredit_train,targetFld="LossRate_Real",modLR_base)
### RESULTS: AIC: 3063.355;  R^2:  4.27%; RMSE:  30.50%; MAE:  25.56%

# Final variables(Expert judgement)

vars <- c("PrevDefaults","g0_Delinq_Any_Aggr_Prop_Lag_3","DefaultStatus1_Aggr_Prop_Lag_6", 
          "slc_acct_arr_dir_3","DefSpell_Age","DefSpell_Num_binned",
          "NewLoans_Aggr_Prop")
modLR <- cpglm( as.formula(paste("LossRate_Real ~", paste(vars, collapse = " + "))),
                data=datCredit_train)
summary(modLR);
evalLS(modLR,datCredit_train,targetFld="LossRate_Real",modLR_base)
### RESULTS: AIC: 3081.497;  R^2:  4.12%; RMSE:  30.53%; MAE:  25.63%



# ------ 4. Account-level variables

# ------ 4.1 How do various non-delinquency account-level variables fare as single-factor models?

vars <- c("Principal_Real", "Principal", "InterestRate_Margin", 
          "Balance_Real_1", "Balance_1", "Instalment_Real", "InterestRate_Nom", "AgeToTerm",
          "BalanceToPrincipal_1", "slc_acct_pre_lim_perc_imputed_med")

# - Correlation analysis towards obtaining clusters of correlated variables
corrAnalysis(datCredit_train, vars, corrThresh = 0.6, method = 'spearman')
### RESULTS:
# Absolute correlations of  97%  found for  Principal_Real  and  Principal 
# Absolute correlations of  62%  found for  InterestRate_Margin  and  Balance_Real 
# Absolute correlations of  62%  found for  InterestRate_Margin  and  Balance 
# Absolute correlations of  100%  found for  Balance_Real  and  Balance 
# Absolute correlations of  67%  found for  Principal_Real  and  Instalment_Real 
# Absolute correlations of  66%  found for  Principal  and  Instalment_Real 
# Absolute correlations of  87%  found for  InterestRate_Margin  and  InterestRate_Nom 
# Absolute correlations of  65%  found for  Balance_Real  and  InterestRate_Nom 
# Absolute correlations of  64%  found for  Balance  and  InterestRate_Nom 
# Absolute correlations of  64%  found for  InterestRate_Margin  and  BalanceToPrincipal 
# Absolute correlations of  97%  found for  Balance_Real  and  BalanceToPrincipal 
# Absolute correlations of  97%  found for  Balance  and  BalanceToPrincipal 
# Absolute correlations of  68%  found for  InterestRate_Nom  and  BalanceToPrincipal

# - Initialize variables to be tested
vars <- c("Principal_Real", "Principal", "InterestRate_Margin_imputed_mean", "pmnt_method_grp",
          "Balance_Real_1", "Balance_1", "Instalment_Real", "InterestRate_Nom", "AgeToTerm",
          "BalanceToPrincipal_1", "slc_acct_pre_lim_perc_imputed_med")

# - Single-factor modelling results
# Goodness-of-fit
aicTable_LS(datCredit_train, vars, TimeDef=c("Cox_Discrete","LossRate_Real"), genPath=genObjPath, modelType="tweedie")
### RESULTS: Best AIC-results: AgeToTerm, Principal, Principal_Real, Installment_Real, pmnt_method_grp, Balance_1
# Best C-statistics: Principal, Prinipal_Real,Instalment_real, Balance_1, AgeToTerm
# Best Deviance:AgeToTerm, Principal, Principal_Real, Installment_Real, pmnt_method_grp, Balance_1
#Choose AgeToTerm, Principal, Balance_1, pmnt_method_grp


# ------ 4.2 Combining insights: Delinquency-themed, portfolio-level, and account-level variables
#Didn't include AgeToTemr as it caused instablity
# - Initialize variables to be tested
vars <- c("PrevDefaults","g0_Delinq_Any_Aggr_Prop_Lag_3","DefaultStatus1_Aggr_Prop_Lag_6", 
          "slc_acct_arr_dir_3","DefSpell_Age","DefSpell_Num_binned",
          "NewLoans_Aggr_Prop","Balance_1","Principal","pmnt_method_grp")

### RESULTS:

# - Full model | Stepwise forward selection procedure
modLR_full <- cpglm( as.formula(paste("LossRate_Real ~", paste(vars, collapse = " + "))),
                     data=datCredit_train)
summary(modLR_full);
evalLS(modLR_full,datCredit_train,targetFld="LossRate_Real",modLR_base)
### RESULTS: AIC: 440.6771  R^2:  23.74%; RMSE:  27.22%; MAE:  22.31%


# - Stepwise forward selection using AIC
ptm <- proc.time() # for runtime calculations (83m) 
modLR_step <- stepwise_cpglm_both(modLR_base, modLR_full,datCredit_train)
summary(modLR_step)
evalLS(modLR_step,datCredit_train,targetFld="LossRate_Real",modLR_base)
proc.time() - ptm
### RESULTS: AIC: 438.7244  R^2:  23.73%; RMSE:  27.22%; MAE:  22.31%

# - Final variables (expert judgement)
# Swapped PrevDefaults with InterestRate_Nom as they can't work together. Interest Rate_Nom performed
# better as a single factor model
# included DefSpell_Age agin and removed some variables such as NewLoans_Aggr_Prop that caused the model
# to not converge
vars <- c("PrevDefaults","g0_Delinq_Any_Aggr_Prop_Lag_3","DefaultStatus1_Aggr_Prop_Lag_6", 
          "slc_acct_arr_dir_3","DefSpell_Age","DefSpell_Num_binned",
          "NewLoans_Aggr_Prop","Balance_1","Principal","pmnt_method_grp")
modLR <- cpglm( as.formula(paste("LossRate_Real ~", paste(vars, collapse = " + "))),
                data=datCredit_train)
summary(modLR);
evalLS(modLR,datCredit_train,targetFld="LossRate_Real",modLR_base)
### RESULTS: AIC: 438.7244  R^2:  23.73%; RMSE:  27.22%; MAE:  22.31%




# ------ 5. Macroeconomic variables

# ------ 5.1 Which lag order is the best for: M_Repo_Rate

# - Initialize variables to be tested
vars <- c("M_Repo_Rate", "M_Repo_Rate_1 ", "M_Repo_Rate_2", "M_Repo_Rate_3", "M_Repo_Rate_6", "M_Repo_Rate_9", "M_Repo_Rate_12")

# - Single-factor modelling results
# Goodness-of-fit
aicTable_LS(datCredit_train, vars, TimeDef=c("Cox_Discrete","LossRate_Real"), genPath=genObjPath, modelType="tweedie")
# Discriminatory power (in-sample)

### RESULTS: Best AIC-results: M_Repo_Rate_12, M_Repo_Rate_9, M_Repo_Rate, M_Repo_Rate_1, 
# M_Repo_Rate_6, M_Repo_Rate_2, M_Repo_Rate_3.
# Best C: M_Repo_Rate_12, M_Repo_Rate_9, M_Repo_Rate, M_Repo_Rate_1, 
# M_Repo_Rate_6, M_Repo_Rate_2, M_Repo_Rate_3.
# Best Deviance: M_Repo_Rate_12, M_Repo_Rate_9, M_Repo_Rate, M_Repo_Rate_1, 
# M_Repo_Rate_6, M_Repo_Rate_2, M_Repo_Rate_3.


### CONCLUSION
# Choose: M_Repo_Rate_12, M_Repo_Rate_9, M_Repo_Rate


# ------ 5.2 Which lag order is the best for: M_Inflation_Growth

# - Initialize variables to be tested
vars <- c("M_Inflation_Growth", "M_Inflation_Growth_1 ", "M_Inflation_Growth_2", "M_Inflation_Growth_3", 
          "M_Inflation_Growth_6", "M_Inflation_Growth_9", "M_Inflation_Growth_12")

# - Single-factor modelling results
# Goodness-of-fit
aicTable_LS(datCredit_train, vars, TimeDef=c("Cox_Discrete","LossRate_Real"), genPath=genObjPath, modelType="tweedie")
# Discriminatory power (in-sample)
### RESULTS: Best AIC-results: M_Inflation_Growth_12, M_Inflation_Growth_9, M_Inflation_Growth_1, M_Inflation_Growth, 
# M_Inflation_Growth_2,M_Inflation_Growth_6, M_Inflation_Growth_3 .
# Best C: M_Inflation_Growth_12, M_Inflation_Growth_9, M_Inflation_Growth_1, M_Inflation_Growth, 
# M_Inflation_Growth_2,M_Inflation_Growth_6, M_Inflation_Growth_3 .
# Best Deviance: M_Inflation_Growth_12, M_Inflation_Growth_9, M_Inflation_Growth_1, M_Inflation_Growth, 
# M_Inflation_Growth_2,M_Inflation_Growth_6, M_Inflation_Growth_3 .

### CONCLUSION: Small AIC differences
# Best variables: M_Inflation_Growth_12, M_Inflation_Growth_9, M_Inflation_Growth_1


# ------ 5.3 Which lag order is the best for: M_RealGDP_Growth

# - Initialize variables to be tested
vars <- c("M_RealGDP_Growth", "M_RealGDP_Growth_1 ", "M_RealGDP_Growth_2", "M_RealGDP_Growth_3", 
          "M_RealGDP_Growth_6", "M_RealGDP_Growth_9", "M_RealGDP_Growth_12")

# - Single-factor modelling results
# Goodness-of-fit
aicTable_LS(datCredit_train, vars, TimeDef=c("Cox_Discrete","LossRate_Real"), genPath=genObjPath, modelType="tweedie")
# Discriminatory power (in-sample)
### RESULTS: Best AIC-results: M_RealGDP_Growth, M_RealGDP_Growth_1, M_RealGDP_Growth_2, M_RealGDP_Growth_3, 
# M_RealGDP_Growth_6, M_RealGDP_Growth_9, M_RealGDP_Growth_12.
# Best C-index: M_RealGDP_Growth, M_RealGDP_Growth_1, M_RealGDP_Growth_2, M_RealGDP_Growth_3, 
# M_RealGDP_Growth_6, M_RealGDP_Growth_9, M_RealGDP_Growth_12.
# Best Deviance: M_RealGDP_Growth, M_RealGDP_Growth_1, M_RealGDP_Growth_2, M_RealGDP_Growth_3, 
# M_RealGDP_Growth_6, M_RealGDP_Growth_9, M_RealGDP_Growth_12.

### CONCLUSION: early lags seem better, based on very small AIC-differences and c-differences 
# Choose: M_RealGDP_Growth_1, M_RealGDP_Growth_2, M_RealGDP_Growth


# ------ 5.4 Which lag order is the best for: M_RealIncome_Growth

# - Initialize variables to be tested
vars <- c("M_RealIncome_Growth", "M_RealIncome_Growth_1 ", "M_RealIncome_Growth_2", "M_RealIncome_Growth_3", 
          "M_RealIncome_Growth_6", "M_RealIncome_Growth_9", "M_RealIncome_Growth_12")

# - Single-factor modelling results
# Goodness-of-fit
aicTable_LS(datCredit_train, vars, TimeDef=c("Cox_Discrete","LossRate_Real"), genPath=genObjPath, modelType="tweedie")
# Discriminatory power (in-sample)
### RESULTS: Best AIC-results: M_RealIncome_Growth_12, M_RealIncome_Growth_9, M_RealIncome_Growth, M_RealIncome_Growth_1, 
# M_RealIncome_Growth_6, M_RealIncome_Growth_2, M_RealIncome_Growth_3.
# Best C: M_RealIncome_Growth_12, M_RealIncome_Growth_9, M_RealIncome_Growth, M_RealIncome_Growth_1, 
# M_RealIncome_Growth_6, M_RealIncome_Growth_2, M_RealIncome_Growth_3.
# Best Deviance: M_RealIncome_Growth_12, M_RealIncome_Growth_9, M_RealIncome_Growth, M_RealIncome_Growth_1, 
# M_RealIncome_Growth_6, M_RealIncome_Growth_2, M_RealIncome_Growth_3.

### CONCLUSION: Middle lags seem better, based on very small AIC-differences and c-differences (50%-51%)
# # Choose:  M_RealIncome_Growth_12, M_RealIncome_Growth_9, M_RealIncome_Growth


# ------ 5.5 Which lag order is the best for: M_DTI_Growth

# - Initialize variables to be tested
vars <- c("M_DTI_Growth", "M_DTI_Growth_1 ", "M_DTI_Growth_2", "M_DTI_Growth_3", 
          "M_DTI_Growth_6", "M_DTI_Growth_9", "M_DTI_Growth_12")

# - Single-factor modelling results
# Goodness-of-fit
aicTable_LS(datCredit_train, vars, TimeDef=c("Cox_Discrete","LossRate_Real"), genPath=genObjPath, modelType="tweedie")
# Discriminatory power (in-sample)
### RESULTS: Best AIC-results: M_DTI_Growth_3, M_DTI_Growth_2, M_DTI_Growth_6, M_DTI_Growth_12, M_DTI_Growth_1, 
# M_DTI_Growth_9,M_DTI_Growth .
# Best C: M_DTI_Growth_3, M_DTI_Growth_2, M_DTI_Growth_6, M_DTI_Growth_12, M_DTI_Growth_1, 
# M_DTI_Growth_9,M_DTI_Growth .
# Best Deviance: M_DTI_Growth_3, M_DTI_Growth_2, M_DTI_Growth_6, M_DTI_Growth_12, M_DTI_Growth_1, 
# M_DTI_Growth_9,M_DTI_Growth .

### CONCLUSION: Middle lags seem better, based on very small AIC-differences,
# trend exist across c-differences (54-56%)
# Choose: M_DTI_Growth_3, M_DTI_Growth_2, M_DTI_Growth_6


# ------ 5.6 Which lag order is the best for: M_Emp_Growth

# - Initialize variables to be tested
vars <- c("M_Emp_Growth", "M_Emp_Growth_1 ", "M_Emp_Growth_2", "M_Emp_Growth_3", 
          "M_Emp_Growth_6", "M_Emp_Growth_9", "M_Emp_Growth_12")

# - Single-factor modelling results
# Goodness-of-fit
aicTable_LS(datCredit_train, vars, TimeDef=c("Cox_Discrete","LossRate_Real"), genPath=genObjPath, modelType="tweedie")
# Discriminatory power (in-sample)
### RESULTS: Best AIC-results: M_Emp_Growth_12, M_Emp_Growth_9, M_Emp_Growth_6, M_Emp_Growth
# M_Emp_Growth_3, M_Emp_Growth_2, M_Emp_Growth_1
# Best C-statistics: M_Emp_Growth_12, M_Emp_Growth_9, M_Emp_Growth_6, M_Emp_Growth
# M_Emp_Growth_3, M_Emp_Growth_2, M_Emp_Growth_1
#Best Deviance: M_Emp_Growth_12, M_Emp_Growth_9, M_Emp_Growth_6, M_Emp_Growth
# M_Emp_Growth_3, M_Emp_Growth_2, M_Emp_Growth_1
### CONCLUSION: early to middle lags seem better, based on very small AIC-differences, affirmed by the c-differences (50-51%)
# Choose: M_Emp_Growth_12, M_Emp_Growth_9, M_Emp_Growth_6

# ------ 5.7 Combining insights: Macroeconomic variables


# - Initialize variables to be tested
vars <- c("M_Repo_Rate", "M_Repo_Rate_12", "M_Repo_Rate_9", 
          "M_Inflation_Growth_12", "M_Inflation_Growth", "M_Inflation_Growth_9",
          "M_RealGDP_Growth_2", "M_RealGDP_Growth_1", "M_RealGDP_Growth",
          "M_RealIncome_Growth", "M_RealIncome_Growth_9", "M_RealIncome_Growth_12",
          "M_DTI_Growth_6", "M_DTI_Growth_2", "M_DTI_Growth_3", 
          "M_Emp_Growth_12", "M_Emp_Growth_9", "M_Emp_Growth_6")

# - Full model | Stepwise forward selection procedure
modLR_full <- cpglm( as.formula(paste("LossRate_Real ~", paste(vars, collapse = " + "))),
                     data=datCredit_train)
summary(modLR_full);
evalLS(modLR_full,datCredit_train,targetFld="LossRate_Real",modLR_base)
### RESULTS: AIC:   65081.89;   R^2:  3.66%; RMSE:  21.94%; MAE:  13.72%

# - Stepwise forward selection using AIC
ptm <- proc.time() # for runtime calculations (ignore)
modLR_step <- stepwise_cpglm_both(modLR_base, modLR_full,datCredit_train)
summary(modLR_step)
evalLS(modLR_step,datCredit_train,targetFld="LossRate_Real",modLR_base)
proc.time() - ptm # IGNORE: elapsed runtime; 140m
### RESULTS: AIC:   3242.353;   R^2:  2.7%; RMSE:  30.75%; MAE:  25.88%


# - Final variables (Expert Judgement)
vars <- c("M_RealIncome_Growth", "M_Inflation_Growth_12","M_DTI_Growth_3","M_Repo_Rate_9")
modLR <- cpglm( as.formula(paste("LossRate_Real ~", paste(vars, collapse = " + "))),
                data=datCredit_train)
summary(modLR);
evalLS(modLR,datCredit_train,targetFld="LossRate_Real",modLR_base)
### RESULTS: AIC:   3268.6628;   R^2:  2.5%; RMSE:  30.79%; MAE:  25.92%



# ------ 5.8 Combining insights: Delinquency-themed, portfolio-level, account-level, and macroeconomic variables

# - Initialize variables to be tested
vars <- c("PrevDefaults","g0_Delinq_Any_Aggr_Prop_Lag_3","DefaultStatus1_Aggr_Prop_Lag_6", 
          "slc_acct_arr_dir_3","DefSpell_Age","DefSpell_Num_binned",
          "NewLoans_Aggr_Prop","Balance_1","Principal","pmnt_method_grp",
          "M_RealIncome_Growth", "M_Inflation_Growth_12","M_DTI_Growth_3","M_Repo_Rate_9")


# - Full model | Stepwise forward selection procedure
modLR_full <- cpglm( as.formula(paste("LossRate_Real ~", paste(vars, collapse = " + "))),
                     data=datCredit_train)
summary(modLR_full);
evalLS(modLR_full,datCredit_train,targetFld="LossRate_Real",modLR_base)
### RESULTS: AIC:   383.33;   R^2:  24.18%; RMSE:  27.15%; MAE:  22.19%

# - Stepwise forward selection using AIC
ptm <- proc.time() # for runtime calculations (ignore)
modLR_step <- stepwise_cpglm_both(modLR_base, modLR_full,datCredit_train)
summary(modLR_step)
evalLS(modLR_step,datCredit_train,targetFld="LossRate_Real",modLR_base)
proc.time() - ptm # IGNORE: elapsed runtime; 140m