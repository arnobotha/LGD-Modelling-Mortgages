# ======================================= INPUT SPACE: LOSS SEVERITY-GAUSSIAN-TWO-STAGE============================
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
datCredit_train <- datCredit_train[, OOB_Ind := ifelse(LossRate_Real <= 0 | LossRate_Real > 1, 1,0)]
datCredit_valid <- datCredit_valid[, OOB_Ind := ifelse(LossRate_Real <= 0 | LossRate_Real > 1, 1,0)]

# Subset to include only relevant data
datCredit_train <- subset(datCredit_train, OOB_Ind == 0)
datCredit_valid <- subset(datCredit_valid, OOB_Ind == 0)


# remove previous objects from memory
rm(datCredit_train_CDH, datCredit_valid_CDH); gc()

modLR_base<- glm(LossRate_Real~1, family = gaussian(link = "identity"), data = datCredit_train)


# ------ 2. Delinquency-themed variables
#----- 2.1 Which lag order is the best in calculating the portfolio-level fraction of the defaulted proportion with any delinquency?
# indicates the lag order of past data that should be included  when assessing an account
# - Initialize variables to be tested
vars <- c("g0_Delinq_Any_Aggr_Prop", "g0_Delinq_Any_Aggr_Prop_Lag_1", "g0_Delinq_Any_Aggr_Prop_Lag_2",
          "g0_Delinq_Any_Aggr_Prop_Lag_3", "g0_Delinq_Any_Aggr_Prop_Lag_4", "g0_Delinq_Any_Aggr_Prop_Lag_5",
          "g0_Delinq_Any_Aggr_Prop_Lag_6", "g0_Delinq_Any_Aggr_Prop_Lag_9", "g0_Delinq_Any_Aggr_Prop_Lag_12" )

# - Single-factor modelling results
# Goodness-of-fit
aicTable_LS(datCredit_train, vars, TimeDef=c("Cox_Discrete","LossRate_Real"), genPath=genObjPath, modelType="gaussian")
### RESULTS: Best AIC-results: g0_Delinq_Any_Aggr_Prop_Lag_3, g0_Delinq_Any_Aggr_Prop_Lag_2, g0_Delinq_Any_Aggr_Prop_Lag_1, 
# , g0_Delinq_Any_Aggr_Prop_4
# Best Deviance : g0_Delinq_Any_Aggr_Prop_Lag_3, g0_Delinq_Any_Aggr_Prop_Lag_2, g0_Delinq_Any_Aggr_Prop_Lag_1, 
# , g0_Delinq_Any_Aggr_Prop_4
# Conclusion: The differences in AIC and deviance are minor
# Choose the overall top 3 performing variables g0_Delinq_Any_Aggr_Prop_2 g0_Delinq_Any_Aggr_Prop_Lag_4, g0_Delinq_Any_Aggr_Prop_Lag_3
# All are above 50% 


# ------ 2.2 Which lag order is the best in calculating the portfolio-level fraction of defaulted accounts during the default spell?
vars <- c("DefaultStatus1_Aggr_Prop", "DefaultStatus1_Aggr_Prop_Lag_1", "DefaultStatus1_Aggr_Prop_Lag_2",
          "DefaultStatus1_Aggr_Prop_Lag_3", "DefaultStatus1_Aggr_Prop_Lag_4", "DefaultStatus1_Aggr_Prop_Lag_5",
          "DefaultStatus1_Aggr_Prop_Lag_6", "DefaultStatus1_Aggr_Prop_Lag_9", "DefaultStatus1_Aggr_Prop_Lag_12" )

# - Single-factor modelling results
# Goodness-of-fit
aicTable_LS(datCredit_train, vars, TimeDef=c("Cox_Discrete","LossRate_Real"), genPath=genObjPath, modelType="gaussian")
### RESULTS: Best AIC-results: DefaultStatus1_Aggr_Prop_Lag_6, DefaultStatus1_Aggr_Prop_Lag_5, DefaultStatus1_Aggr_Prop_Lag_4 etc in descending order
# Differences are not that major
# Best Deviance: DefaultStatus1_Aggr_Prop_Lag_6, DefaultStatus1_Aggr_Prop_Lag_5, DefaultStatus1_Aggr_Prop_Lag_4 etc in descending order
# Similar trend as to AIC


### CONCLUSION: Later is better, though the AIC-differences were minuscule.
# Top 3 variables: DefaultStatus1_Aggr_Prop_Lag_5, DefaultStatus1_Aggr_Prop_Lag_9, DefaultStatus1_Aggr_Prop_Lag_6


# ------ 2.3 How do other portfolio-level delinquency-themed variables fare as single-factor models?

# - Initialize variables to be tested
vars <- c("g0_Delinq_Ave", "ArrearsToBalance_Aggr_Prop", "CuringEvents_Aggr_Prop" )

# - Single-factor modelling results
# Goodness-of-fit
aicTable_LS(datCredit_train, vars, TimeDef=c("Cox_Discrete","LossRate_Real"), genPath=genObjPath, modelType="gaussian")

### RESULTS: Best AIC-results:  g0_Delinq_Ave, ArrearsToBalance_Aggr_Prop,CuringEvents_Aggr_Prop
# However they aren't that big with their being little difference between the lowest and highest
# Best Deviance: g0_Delinq_Ave, ArrearsToBalance_Aggr_Prop,CuringEvents_Aggr_Prop


### Conclusion: All are significant include all 3 of them, curing event is relevant


# ------ 2.4 How do account-level delinquency-themed variables fare as single-factor models?

# - Initialize variables to be tested
vars <- c("g0_Delinq_fac", "g0_Delinq", "g0_Delinq_Lag_1", "slc_acct_arr_dir_3_Change_Ind",
          "g0_Delinq_Num", "slc_acct_arr_dir_3", "slc_acct_roll_ever_24_imputed_mean", "Arrears", "PrevDefaults","TimeInDelinqState"
          ,"slc_past_due_amt_imputed_med", "slc_curing_ind","DefSpell_Age","DefSpell_Num_binned")

# - Single-factor modelling results
# Goodness-of-fit
aicTable_LS(datCredit_train, vars, TimeDef=c("Cox_Discrete","LossRate_Real"), genPath=genObjPath, modelType="gaussian")
# Discriminatory power (in-sample)
### RESULTS: Best AIC-results:  PrevDefaults, DefSpell_Num_binned
# Best Deviance: g0_Delinq_Lag_1, PrevDeafults, slc_Acct_Arr_dir_3, TimeInDelinqState, Arrears,, slc_past_due_amt_imputed_med
### CONCLUSION
# Choose:  PrevDeafults, slc_acct_arr_dir_3, Arrears,, slc_past_due_amt_imputed_med


# ------ 2.5 Combining insights: delinquency-themed variables
# - Initialize variables to be tested
vars <- c("PrevDefaults","g0_Delinq_Any_Aggr_Prop_Lag_2",
          "g0_Delinq_Any_Aggr_Prop_Lag_3", "g0_Delinq_Any_Aggr_Prop_Lag_4",
          "DefaultStatus1_Aggr_Prop_Lag_5","DefaultStatus1_Aggr_Prop_Lag_6", "DefaultStatus1_Aggr_Prop_Lag_9",
          "g0_Delinq_Ave", "ArrearsToBalance_Aggr_Prop", "CuringEvents_Aggr_Prop",
          "slc_acct_arr_dir_3","DefSpell_Age","DefSpell_Num_binned","slc_past_due_amt_imputed_med")
# - Full model | Stepwise forward selection procedure
modLR_full <- glm( as.formula(paste("LossRate_Real ~", paste(vars, collapse = " + "))),
                   data=datCredit_train,family = gaussian(link = "identity"))
summary(modLR_full);
evalLS(modLR_full,datCredit_train,targetFld="LossRate_Real",modLR_base)

### RESULTS: AIC:   -19654.48; R^2:  14.76%; RMSE:  20.64%; MAE:  12.74%
ptm <- proc.time() # for runtime calculations (ignore)
modLR_step <- stepAIC(modLR_base, scope = list(lower = ~ 1, 
                                               upper = as.formula(paste("~", paste(vars, collapse = " + ")))), 
                      direction = "both", k=log(datCredit_train[,.N]), maxit=50)
summary(modLR_step)
evalLS(modLR_step,datCredit_train,targetFld="LossRate_Real",modLR_base)
proc.time() - ptm # IGNORE: elapsed runtime; 
### RESULTS: AIC:   -19655.75; R^2:  14.75%; RMSE:  20.64%; MAE:  12.74%
# - Domain expertise
# Remove g0_Delinq_Any_Aggr_Prop due to lag 4 being present

# - Final variables (expert judgement)
# Removed g0_Delinq_Any_Aggr_Prop_Lag_2 due to presence of lag 3
vars <- c("PrevDefaults", "g0_Delinq_Any_Aggr_Prop_Lag_3","DefaultStatus1_Aggr_Prop_Lag_9",
          "g0_Delinq_Ave", "CuringEvents_Aggr_Prop",
          "slc_acct_arr_dir_3","DefSpell_Age","DefSpell_Num_binned","slc_past_due_amt_imputed_med")

modLR <- glm( as.formula(paste("LossRate_Real ~", paste(vars, collapse = " + "))),
              data=datCredit_train,family = gaussian(link = "identity"))
summary(modLR);
evalLS(modLR,datCredit_train,targetFld="LossRate_Real",modLR_base)

### RESULTS: AIC: -19644.8; R^2:  14.74$; RMSE:  20.65%; MAE:  12.74%

# - Final variables
vars <- c("PrevDefaults", "g0_Delinq_Any_Aggr_Prop_Lag_3","DefaultStatus1_Aggr_Prop_Lag_9",
          "g0_Delinq_Ave", "CuringEvents_Aggr_Prop",
          "slc_acct_arr_dir_3","DefSpell_Age","DefSpell_Num_binned","slc_past_due_amt_imputed_med")

modLR <- glm( as.formula(paste("LossRate_Real ~", paste(vars, collapse = " + "))),
              data=datCredit_train,family = gaussian(link = "identity"))
summary(modLR);
evalLS(modLR,datCredit_train,targetFld="LossRate_Real",modLR_base)

### RESULTS: AIC: -19644.8; R^2:  14.74$; RMSE:  20.65%; MAE:  12.74%




# ------ 3. Other portfolio-level variables


# ------ 3.1 Which lag order is the best in calculating the median interest rate of the portfolio?

# - Initialize variables to be tested
vars <- c("InterestRate_Margin_Aggr_Med", "InterestRate_Margin_Aggr_Med_1", "InterestRate_Margin_Aggr_Med_2",
          "InterestRate_Margin_Aggr_Med_3", "InterestRate_Margin_Aggr_Med_9")

# - Single-factor modelling results
# Goodness-of-fit
aicTable_LS(datCredit_train, vars, TimeDef=c("Cox_Discrete","LossRate_Real"), genPath=genObjPath, modelType="gaussian")

### RESULTS: Best AIC-results: InterestRate_Margin_Aggr_Med_9 , InterestRate_Margin_Aggr_Med_3, InterestRate_Margin_Aggr_Med_2,
# InterestRate_Margin_Aggr_Med_1. InterestRate_Margin_Aggr_Med
# Best Deviance: InterestRate_Margin_Aggr_Med_9 , InterestRate_Margin_Aggr_Med_3, InterestRate_Margin_Aggr_Med_2,
# InterestRate_Margin_Aggr_Med_1. InterestRate_Margin_Aggr_Med

### Conclusion

#  AIC difference is minor


# ------ 3.2 How do other portfolio-level (non-delinquency) variables fare as single-factor models?

# - Initialize variables to be tested
vars <- c("InstalmentToBalance_Aggr_Prop", "AgeToTerm_Aggr_Mean", "DefSpell_Maturity_Aggr_Mean", "NewLoans_Aggr_Prop")

# - Single-factor modelling results
# Goodness-of-fit
aicTable_LS(datCredit_train, vars, TimeDef=c("Cox_Discrete","LossRate_Real"), genPath=genObjPath, modelType="gaussian")
### RESULTS: Best AIC-results: InstalmentToBalance_Aggr_Prop,NewLoans_Aggr_Prop, DefSpell_Maturity_Aggr_Mean,AgeToTerm_Aggr_Mean
# Best Deviance: InstalmentToBalance_Aggr_Prop,NewLoans_Aggr_Prop, DefSpell_Maturity_Aggr_Mean,AgeToTerm_Aggr_Mean
### CONCLUSION: InstalmentToBalance_Aggr_Prop,NewLoans_Aggr_Prop, DefSpell_Maturity_Aggr_Mean


# ------ 3.3 Combining insights: Delinquency-themed and portfolio-level variables

# - Initialize variables to be tested
vars <- c("PrevDefaults", "g0_Delinq_Any_Aggr_Prop_Lag_3","DefaultStatus1_Aggr_Prop_Lag_9",
          "g0_Delinq_Ave", "CuringEvents_Aggr_Prop",
          "slc_acct_arr_dir_3","DefSpell_Age","DefSpell_Num_binned","slc_past_due_amt_imputed_med",
          "InstalmentToBalance_Aggr_Prop", "DefSpell_Maturity_Aggr_Mean", "NewLoans_Aggr_Prop",
          "InterestRate_Margin_Aggr_Med_2", "InterestRate_Margin_Aggr_Med_3", "InterestRate_Margin_Aggr_Med_9" )
# - Full model | Stepwise forward selection procedure
modLR_full <-glm( as.formula(paste("LossRate_Real ~", paste(vars, collapse = " + "))),
                  data=datCredit_train,family = gaussian(link = "identity"))
summary(modLR_full);
evalLS(modLR_full,datCredit_train,targetFld="LossRate_Real",modLR_base)

### RESULTS: AIC: -19704.19; R^2:  14.83%; RMSE:  20.64%; MAE:  12.70%

# - Stepwise forward selection using AIC
ptm <- proc.time() # for runtime calculations (ignore)
modLR_step <- stepAIC(modLR_base, scope = list(lower = ~ 1, 
                                               upper = as.formula(paste("~", paste(vars, collapse = " + ")))), 
                      direction = "both", k=log(datCredit_train[,.N]), maxit=50)

summary(modLR_step)

proc.time() - ptm # IGNORE: elapsed runtime; 503m
evalLS(modLR_step,datCredit_train,targetFld="LossRate_Real",modLR_base)
### RESULTS: AIC: -19701.28; R^2:  14.81%; RMSE:  20.64%; MAE:  12.70%
# Final variables(Expert judgement)

vars <- c("PrevDefaults", "g0_Delinq_Any_Aggr_Prop_Lag_3","DefaultStatus1_Aggr_Prop_Lag_9",
          "g0_Delinq_Ave", "CuringEvents_Aggr_Prop",
          "slc_acct_arr_dir_3","DefSpell_Age","DefSpell_Num_binned","slc_past_due_amt_imputed_med",
          "InstalmentToBalance_Aggr_Prop", "DefSpell_Maturity_Aggr_Mean", "NewLoans_Aggr_Prop" )
modLR <- glm( as.formula(paste("LossRate_Real ~", paste(vars, collapse = " + "))),
              data=datCredit_train,family = gaussian(link = "identity"))
summary(modLR);
evalLS(modLR_step,datCredit_train,targetFld="LossRate_Real",modLR_base)
### RESULTS: AIC: -2934.127; R^2:  16.49%; RMSE:  21.00%; MAE:  12.91%


# ------ 4. Account-level variables

# ------ 4.1 How do various non-delinquency account-level variables fare as single-factor models?

vars <- c("Principal_Real", "Principal", "InterestRate_Margin", 
          "Balance_Real_1", "Balance_1", "Instalment_Real", "InterestRate_Nom", "AgeToTerm",
          "BalanceToPrincipal_1", "slc_acct_pre_lim_perc_imputed_med")

# - Correlation analysis towards obtaining clusters of correlated variables
corrAnalysis(datCredit_train, vars, corrThresh = 0.6, method = 'spearman')
### RESULTS:
# Absolute correlations of  97%  found for  Principal_Real  and  Principal 
# Absolute correlations of  80%  found for  Principal_Real  and  Balance_Real_1 
# Absolute correlations of  76%  found for  Principal  and  Balance_Real_1 
# Absolute correlations of  79%  found for  Principal_Real  and  Balance_1 
# Absolute correlations of  79%  found for  Principal  and  Balance_1 
# Absolute correlations of  98%  found for  Balance_Real_1  and  Balance_1 
# Absolute correlations of  67%  found for  Principal_Real  and  Instalment_Real 
# Absolute correlations of  66%  found for  Principal  and  Instalment_Real 
# Absolute correlations of  71%  found for  Balance_Real_1  and  Instalment_Real 
# Absolute correlations of  72%  found for  Balance_1  and  Instalment_Real 
# Absolute correlations of  87%  found for  InterestRate_Margin  and  InterestRate_Nom 
# Absolute correlations of  82%  found for  Balance_Real_1  and  BalanceToPrincipal_1 
# Absolute correlations of  79%  found for  Balance_1  and  BalanceToPrincipal_1 

# - Initialize variables to be tested
vars <- c("Principal_Real", "Principal", "InterestRate_Margin_imputed_mean", "pmnt_method_grp",
          "Balance_Real_1", "Balance_1", "Instalment_Real", "InterestRate_Nom", "AgeToTerm",
          "BalanceToPrincipal_1", "slc_acct_pre_lim_perc_imputed_med")

# - Single-factor modelling results
# Goodness-of-fit
aicTable_LS(datCredit_train, vars, TimeDef=c("Cox_Discrete","LossRate_Real"), genPath=genObjPath, modelType="gaussian")
### RESULTS: Best AIC-results: InterestRate_Nom, Interestrate_Margin_imputed_mean, pmnt_method_grp, Balance_1, Principal_real
# Best C-statistics: InterestRate_Nom, Interestrate_Margin_imputed_mean, pmnt_method_grp, Balance_1, Principal_real
# Best Deviance:InterestRate_Nom, Interestrate_Margin_imputed_mean, pmnt_method_grp, Balance_1, Principal_real



# ------ 4.2 Combining insights: Delinquency-themed, portfolio-level, and account-level variables

# - Initialize variables to be tested
vars <- c("PrevDefaults", "g0_Delinq_Any_Aggr_Prop_Lag_3","DefaultStatus1_Aggr_Prop_Lag_9",
          "g0_Delinq_Ave", "CuringEvents_Aggr_Prop",
          "slc_acct_arr_dir_3","DefSpell_Age","DefSpell_Num_binned","slc_past_due_amt_imputed_med",
          "InstalmentToBalance_Aggr_Prop", "DefSpell_Maturity_Aggr_Mean", "NewLoans_Aggr_Prop",
          "Principal", "InterestRate_Margin_imputed_mean", "pmnt_method_grp","InterestRate_Nom","Balance_1")

### RESULTS:

# - Full model | Stepwise forward selection procedure
modLR_full <- glm( as.formula(paste("LossRate_Real ~", paste(vars, collapse = " + "))),
                   data=datCredit_train,family = gaussian(link = "identity"))
summary(modLR_full);
evalLS(modLR_full,datCredit_train,targetFld="LossRate_Real",modLR_base)
### RESULTS: AIC: -26954.29;  R^2:  24.24%; RMSE:  19.46%; MAE:  11.82%


# - Stepwise forward selection using AIC
ptm <- proc.time() # for runtime calculations (83m) 
modLR_step <- stepAIC(modLR_base, scope = list(lower = ~ 1, 
                                               upper = as.formula(paste("~", paste(vars, collapse = " + ")))), 
                      direction = "both", k=log(datCredit_train[,.N]), maxit=50)
summary(modLR_step)
evalLS(modLR_step,datCredit_train,targetFld="LossRate_Real",modLR_base)
proc.time() - ptm
### RESULTS: AIC: -26933.5;  R^2:  24.21%; RMSE:  19.46%; MAE:  11.83%

# - Final variables
vars <- c("PrevDefaults", "g0_Delinq_Any_Aggr_Prop_Lag_3","DefaultStatus1_Aggr_Prop_Lag_9",
          "g0_Delinq_Ave", "CuringEvents_Aggr_Prop",
          "slc_acct_arr_dir_3","DefSpell_Age","DefSpell_Num_binned","slc_past_due_amt_imputed_med",
          "InstalmentToBalance_Aggr_Prop", "DefSpell_Maturity_Aggr_Mean", "NewLoans_Aggr_Prop",
          "Principal", "InterestRate_Margin_imputed_mean", "pmnt_method_grp","InterestRate_Nom","Balance_1")
modLR <-glm( as.formula(paste("LossRate_Real ~", paste(vars, collapse = " + "))),
             data=datCredit_train,family = gaussian(link = "identity"))
summary(modLR);
evalLS(modLR,datCredit_train,targetFld="LossRate_Real",modLR_base)
### RESULTS: AIC: -26954.29;  R^2:  24.24%; RMSE:  19.46%; MAE:  11.82%





# ------ 5. Macroeconomic variables

# ------ 5.1 Which lag order is the best for: M_Repo_Rate

# - Initialize variables to be tested
vars <- c("M_Repo_Rate", "M_Repo_Rate_1 ", "M_Repo_Rate_2", "M_Repo_Rate_3", "M_Repo_Rate_6", "M_Repo_Rate_9", "M_Repo_Rate_12")

# - Single-factor modelling results
# Goodness-of-fit
aicTable_LS(datCredit_train, vars, TimeDef=c("Cox_Discrete","LossRate_Real"), genPath=genObjPath, modelType="gaussian")
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
aicTable_LS(datCredit_train, vars, TimeDef=c("Cox_Discrete","LossRate_Real"), genPath=genObjPath, modelType="gaussian")
# Discriminatory power (in-sample)
### RESULTS: Best AIC-results: M_Inflation_Growth, M_Inflation_Growth_12, M_Inflation_Growth_1, M_Inflation_Growth_2, 
# M_Inflation_Growth_3,M_Inflation_Growth_9, M_Inflation_Growth_6 .
# Best C: M_Inflation_Growth, M_Inflation_Growth_12, M_Inflation_Growth_1, M_Inflation_Growth_2, 
# M_Inflation_Growth_3,M_Inflation_Growth_9, M_Inflation_Growth_6 .
# Best Deviance: M_Inflation_Growth, M_Inflation_Growth_12, M_Inflation_Growth_1, M_Inflation_Growth_2, 
# M_Inflation_Growth_3,M_Inflation_Growth_9, M_Inflation_Growth_6 .

### CONCLUSION: Small AIC differences
# Best variables: M_Inflation_Growth_12, M_Inflation_Growth, M_Inflation_Growth_1


# ------ 5.3 Which lag order is the best for: M_RealGDP_Growth

# - Initialize variables to be tested
vars <- c("M_RealGDP_Growth", "M_RealGDP_Growth_1 ", "M_RealGDP_Growth_2", "M_RealGDP_Growth_3", 
          "M_RealGDP_Growth_6", "M_RealGDP_Growth_9", "M_RealGDP_Growth_12")

# - Single-factor modelling results
# Goodness-of-fit
aicTable_LS(datCredit_train, vars, TimeDef=c("Cox_Discrete","LossRate_Real"), genPath=genObjPath, modelType="gaussian")
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
aicTable_LS(datCredit_train, vars, TimeDef=c("Cox_Discrete","LossRate_Real"), genPath=genObjPath, modelType="gaussian")
# Discriminatory power (in-sample)
### RESULTS: Best AIC-results: M_RealIncome_Growth_2, M_RealIncome_Growth_1, M_RealIncome_Growth_3, M_RealIncome_Growth, 
# M_RealIncome_Growth_6, M_RealIncome_Growth_9, M_RealIncome_Growth_12.
# Best C: M_RealIncome_Growth_2, M_RealIncome_Growth_1, M_RealIncome_Growth_3, M_RealIncome_Growth, 
# M_RealIncome_Growth_6, M_RealIncome_Growth_9, M_RealIncome_Growth_12.
# Best Deviance: M_RealIncome_Growth_2, M_RealIncome_Growth_1, M_RealIncome_Growth_3, M_RealIncome_Growth, 
# M_RealIncome_Growth_6, M_RealIncome_Growth_9, M_RealIncome_Growth_12.

### CONCLUSION: Middle lags seem better, based on very small AIC-differences and c-differences (50%-51%)
# # Choose:  M_RealIncome_Growth_2, M_RealIncome_Growth_1, M_RealIncome_Growth_3


# ------ 5.5 Which lag order is the best for: M_DTI_Growth

# - Initialize variables to be tested
vars <- c("M_DTI_Growth", "M_DTI_Growth_1 ", "M_DTI_Growth_2", "M_DTI_Growth_3", 
          "M_DTI_Growth_6", "M_DTI_Growth_9", "M_DTI_Growth_12")

# - Single-factor modelling results
# Goodness-of-fit
aicTable_LS(datCredit_train, vars, TimeDef=c("Cox_Discrete","LossRate_Real"), genPath=genObjPath, modelType="gaussian")
# Discriminatory power (in-sample)
### RESULTS: Best AIC-results: M_DTI_Growth_12, M_DTI_Growth_9, M_DTI_Growth_6, M_DTI_Growth_3, M_DTI_Growth_2, 
# M_DTI_Growth,M_DTI_Growth_1 .
# Best C: M_DTI_Growth_12, M_DTI_Growth_9, M_DTI_Growth_6, M_DTI_Growth_3, M_DTI_Growth_2, 
# M_DTI_Growth,M_DTI_Growth_1 .
# Best Deviance: M_DTI_Growth_12, M_DTI_Growth_9, M_DTI_Growth_6, M_DTI_Growth_3, M_DTI_Growth_2, 
# M_DTI_Growth,M_DTI_Growth_1 .

### CONCLUSION: Longer lags seem better, based on very small AIC-differences,
# trend exist across c-differences (54-56%)
# Choose: M_DTI_Growth_12, M_DTI_Growth_9, M_DTI_Growth_6


# ------ 5.6 Which lag order is the best for: M_Emp_Growth

# - Initialize variables to be tested
vars <- c("M_Emp_Growth", "M_Emp_Growth_1 ", "M_Emp_Growth_2", "M_Emp_Growth_3", 
          "M_Emp_Growth_6", "M_Emp_Growth_9", "M_Emp_Growth_12")

# - Single-factor modelling results
# Goodness-of-fit
aicTable_LS(datCredit_train, vars, TimeDef=c("Cox_Discrete","LossRate_Real"), genPath=genObjPath, modelType="gaussian")

# Discriminatory power (in-sample)
### RESULTS: Best AIC-results: M_Emp_Growth, M_Emp_Growth_3, M_Emp_Growth_2, M_Emp_Growth_1
# M_Emp_Growth_6, M_Emp_Growth_9, M_Emp_Growth_12
# Best C-statistics: M_Emp_Growth_9, M_Emp_Growth_2, M_Emp_Growth_6, M_Emp_Growth_1
# M_Emp_Growth_3, M_Emp_Growth, M_Emp_Growth_12
#Best Deviance: M_Emp_Growth, M_Emp_Growth_3, M_Emp_Growth_2, M_Emp_Growth_1
# M_Emp_Growth_6, M_Emp_Growth_9, M_Emp_Growth_12
### CONCLUSION: early to middle lags seem better, based on very small AIC-differences, affirmed by the c-differences (50-51%)
# Choose: M_Emp_Growth_2, M_Emp_Growth_1, M_Emp_Growth_3 

# ------ 5.7 Combining insights: Macroeconomic variables


# - Initialize variables to be tested
vars <- c("M_Repo_Rate", "M_Repo_Rate_12", "M_Repo_Rate_9", 
          "M_Inflation_Growth_12", "M_Inflation_Growth", "M_Inflation_Growth_1",
          "M_RealGDP_Growth_2", "M_RealGDP_Growth_3", "M_RealGDP_Growth_6",
          "M_RealIncome_Growth_9", "M_RealIncome_Growth_3", "M_RealIncome_Growth_6",
          "M_DTI_Growth_9", "M_DTI_Growth_12", "M_DTI_Growth_6", 
          "M_Emp_Growth_12", "M_Emp_Growth_9", "M_Emp_Growth_6")

# - Full model | Stepwise forward selection procedure
modLR_full <- glm( as.formula(paste("LossRate_Real ~", paste(vars, collapse = " + "))),
                   data=datCredit_train,family = gaussian(link = "identity"))
summary(modLR_full);
evalLS(modLR_full,datCredit_train,targetFld="LossRate_Real",modLR_base)
### RESULTS: AIC: -11972.27;  R^2:  3.52%; RMSE:  21.96%; MAE:  13.78%

# - Stepwise forward selection using BIC
ptm <- proc.time() # for runtime calculations (ignore)
modLR_step <- stepAIC(modLR_base, scope = list(lower = ~ 1, 
                                               upper = as.formula(paste("~", paste(vars, collapse = " + ")))), 
                      direction = "both", k=log(datCredit_train[,.N]), maxit=50)
summary(modLR_step)
evalLS(modLR_step,datCredit_train,targetFld="LossRate_Real",modLR_base)
proc.time() - ptm # IGNORE: elapsed runtime; 140m
### RESULTS: AIC: -11967.85;  R^2:  3.49%; RMSE:  21.96%; MAE:  13.78%


# - Final variables (Expert Judgement)
vars <- c("M_Repo_Rate_12","M_DTI_Growth_12","M_Inflation_Growth", "M_RealIncome_Growth_2")
modLR <-glm( as.formula(paste("LossRate_Real ~", paste(vars, collapse = " + "))),
             data=datCredit_train,family = gaussian(link = "identity"))
summary(modLR);
evalLS(modLR,datCredit_train,targetFld="LossRate_Real",modLR_base)
### RESULTS: AIC: -11218.51;  R^2:  2.30%; RMSE:  22.10%; MAE:  13.98%

### RESULTS: AIC:   214,631;   McFadden R^2:  3.22%; AUC:  67.61%.
# Included the Repo rate as it is vital. GDP growth and emp growth have a negative effect
# They increase the AIC and R^2 values


# ------ 7.8 Combining insights: Delinquency-themed, portfolio-level, account-level, and macroeconomic variables

# - Initialize variables to be tested
vars <- c("PrevDefaults", "g0_Delinq_Any_Aggr_Prop_Lag_3","DefaultStatus1_Aggr_Prop_Lag_9",
          "g0_Delinq_Ave", "CuringEvents_Aggr_Prop",
          "slc_acct_arr_dir_3","DefSpell_Age","DefSpell_Num_binned","slc_past_due_amt_imputed_med",
          "InstalmentToBalance_Aggr_Prop", "DefSpell_Maturity_Aggr_Mean", "NewLoans_Aggr_Prop",
          "Principal", "InterestRate_Margin_imputed_mean", "pmnt_method_grp","InterestRate_Nom","Balance_1",
          "M_Repo_Rate_12","M_DTI_Growth_12","M_Inflation_Growth", "M_RealIncome_Growth_2")


# - Full model | Stepwise forward selection procedure
modLR_full <- glm( as.formula(paste("LossRate_Real ~", paste(vars, collapse = " + "))),
                   data=datCredit_train,family = gaussian(link = "identity"))
summary(modLR_full)
evalLS(modLR_full,datCredit_train,targetFld="LossRate_Real",modLR_base)
### RESULTS: AIC: -27175.37;  R^2:  24.52%; RMSE:  19.42%; MAE:  11.85%


# - Stepwise forward selection using BIC
ptm <- proc.time() # for runtime calculations (ignore)
modLR_step <- stepAIC(modLR_base, scope = list(lower = ~ 1, 
                                               upper = as.formula(paste("~", paste(vars, collapse = " + ")))), 
                      direction = "both", k=log(datCredit_train[,.N]), maxit=50)
summary(modLR_step)
evalLS(modLR_step,datCredit_train,targetFld="LossRate_Real",modLR_base)
### RESULTS: AIC: -27175.37;  R^2:  24.52%; RMSE:  19.42%; MAE:  11.85%
