# ======================================= INPUT SPACE: LOGISTIC REGRESSION============================
# Divide data into thematic groups and perform data analysis on them to compile an input space for 
# an advanced logistic regression model.
# ------------------------------------------------------------------------------------------------------
# PROJECT TITLE: Loss Modelling (LGD) for FNB Mortgages
# SCRIPT AUTHOR(S): Dr Arno Botha (AB), Mohammed Gabru (MG)
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

# - Use only default spells and first counter
datCredit_train <- datCredit_train_CDH[!is.na(DefSpell_Key)&DefSpell_Counter==1,]
datCredit_valid <- datCredit_valid_CDH[!is.na(DefSpell_Key)&DefSpell_Counter==1,]
# remove previous objects from memory
rm(datCredit_train_CDH, datCredit_valid_CDH); gc()

# Create an indicator to identify a write-off
datCredit_train[, DefSpell_Event := ifelse(DefSpellResol_Type_Hist=="WOFF", 1, 0)]
datCredit_valid[, DefSpell_Event := ifelse(DefSpellResol_Type_Hist=="WOFF", 1, 0)]


modLR_base <- glm(DefSpell_Event ~ 1, data=datCredit_train, family="binomial")

# Insight interactively mined from modelling theme 2a-b was used in fitting this model.


# ------ 2. Delinquency-themed variables


# ------ 2.1 Which time window length is the best in calculating delinquency volatility?

# - Initialize variables to be tested
vars <- c("g0_Delinq_SD_4", "g0_Delinq_SD_5", "g0_Delinq_SD_6", "g0_Delinq_SD_9", "g0_Delinq_SD_12")

# - Single-factor modelling results
# Goodness-of-fit
aicTable(datCredit_train, vars, TimeDef=c("Cox_Discrete","DefSpell_Event"), genPath=genObjPath, modelType="Cox_Discrete")
# Discriminatory power (in-sample)
concTable(datCredit_train, datCredit_valid, vars, TimeDef=c("Cox_Discrete","DefSpell_Event"), genPath=genObjPath, modelType="Cox_Discrete")
### RESULTS: Best AIC-results: g0_Delinq_SD_12, g0_Delinq_SD_9, g0_Delinq_SD_5, g0_Delinq_SD_6, g0_Delinq_SD_4
# Best Harrell's C-statistics: g0_Delinq_SD_6, g0_Delinq_SD_5, g0_Delinq_SD_12, g0_Delinq_SD_4, g0_Delinq_SD_9
# All of them are significant on their own

### Conclusion: The Harell's C-statistic decreases with a linear trend 
# g0_Delinq_SD_12 seems to perform the best but also include g0_Delinq_SD_5 and g0_Delinq_SD_6


#----- 2.2 Which lag order is the best in calculating the portfolio-level fraction of the defaulted proportion with any delinquency?
# indicates the lag order of past data that should be included  when assessing an account
# - Initialize variables to be tested
vars <- c("g0_Delinq_Any_Aggr_Prop", "g0_Delinq_Any_Aggr_Prop_Lag_1", "g0_Delinq_Any_Aggr_Prop_Lag_2",
          "g0_Delinq_Any_Aggr_Prop_Lag_3", "g0_Delinq_Any_Aggr_Prop_Lag_4", "g0_Delinq_Any_Aggr_Prop_Lag_5",
          "g0_Delinq_Any_Aggr_Prop_Lag_6", "g0_Delinq_Any_Aggr_Prop_Lag_9", "g0_Delinq_Any_Aggr_Prop_Lag_12" )

# - Single-factor modelling results
# Goodness-of-fit
aicTable(datCredit_train, vars, TimeDef=c("Cox_Discrete","DefSpell_Event"), genPath=genObjPath, modelType="Cox_Discrete")
# Discriminatory power (in-sample)
concTable(datCredit_train, datCredit_valid, vars, TimeDef=c("Cox_Discrete","DefSpell_Event"), genPath=genObjPath, modelType="Cox_Discrete")
### RESULTS: Best AIC-results: g0_Delinq_Any_Aggr_Prop_12, g0_Delinq_Any_Aggr_Prop_Lag_9, g0_Delinq_Any_Aggr_Prop_Lag_6, g0_Delinq_Any_Aggr_Prop_Lag_5
# , g0_Delinq_Any_Aggr_Prop_Lag_3
# Best C-statistics: g0_Delinq_Any_Aggr_Prop_12, g0_Delinq_Any_Aggr_Prop_Lag_9, g0_Delinq_Any_Aggr_Prop_Lag_4, g0_Delinq_Any_Aggr_Prop_Lag_3

# Conclusion: The differences in AIC and Harell's C-statiistic are minor
# Choose the overall top 3 performing variables g0_Delinq_Any_Aggr_Prop_12, g0_Delinq_Any_Aggr_Prop_Lag_9, g0_Delinq_Any_Aggr_Prop_Lag_6
# All are above 50% 


# ------ 2.3 Which lag order is the best in calculating the portfolio-level fraction of defaulted accounts during the default spell?
vars <- c("DefaultStatus1_Aggr_Prop", "DefaultStatus1_Aggr_Prop_Lag_1", "DefaultStatus1_Aggr_Prop_Lag_2",
          "DefaultStatus1_Aggr_Prop_Lag_3", "DefaultStatus1_Aggr_Prop_Lag_4", "DefaultStatus1_Aggr_Prop_Lag_5",
          "DefaultStatus1_Aggr_Prop_Lag_6", "DefaultStatus1_Aggr_Prop_Lag_9", "DefaultStatus1_Aggr_Prop_Lag_12" )

# - Single-factor modelling results
# Goodness-of-fit
aicTable(datCredit_train, vars, TimeDef=c("Cox_Discrete","DefSpell_Event"), genPath=genObjPath, modelType="Cox_Discrete")
# Discriminatory power (in-sample)
concTable(datCredit_train, datCredit_valid, vars, TimeDef=c("Cox_Discrete","DefSpell_Event"), genPath=genObjPath, modelType="Cox_Discrete")
### RESULTS: Best AIC-results: DefaultStatus1_Aggr_Prop, DefaultStatus1_Aggr_Prop_Lag_1, DefaultStatus1_Aggr_Prop_Lag_2, 
# Differences are not that major
# Best C-statistics: DefaultStatus1_Aggr_Prop, DefaultStatus1_Aggr_Prop_Lag_1, DefaultStatus1_Aggr_Prop_Lag_12, DefaultStatus1_Aggr_Prop_Lag_2
# Similar trend as to AIC

### CONCLUSION: Later is better, though the AIC-differences were minuscule. Concordance-differences were slightly bigger than the AIC values
# Top 3 varibales: DefaultStatus1_Aggr_Prop_Lag_2, DefaultStatus1_Aggr_Prop_Lag_1, DefaultStatus1_Aggr_Prop_Lag


# ------ 2.4 How do other portfolio-level delinquency-themed variables fare as single-factor models?

# - Initialize variables to be tested
vars <- c("g0_Delinq_Ave", "ArrearsToBalance_Aggr_Prop", "CuringEvents_Aggr_Prop" )

# - Single-factor modelling results
# Goodness-of-fit
aicTable(datCredit_train, vars, TimeDef=c("Cox_Discrete","DefSpell_Event"), genPath=genObjPath, modelType="Cox_Discrete")
# Discriminatory power (in-sample)
concTable(datCredit_train, datCredit_valid, vars, TimeDef=c("Cox_Discrete","DefSpell_Event"), genPath=genObjPath, modelType="Cox_Discrete")
### RESULTS: Best AIC-results: CuringEvents_Aggr_Prop, g0_Delinq_Ave, ArrearsToBalance_Aggr_Prop,
# However they aren't that big with their being a 100 difference between the lowest and highest
# Best C-statistics: CuringEvents_Aggr_Prop, g0_Delinq_Ave, ArrearsToBalance_Aggr_Prop,

### Conclusion: All are significant include all 3 of them, curing event is relevant


# ------ 2.5 How do account-level delinquency-themed variables fare as single-factor models?

# - Initialize variables to be tested
vars <- c("DefSpell_Age", "g0_Delinq_fac", "g0_Delinq", "g0_Delinq_Lag_1", "slc_acct_arr_dir_3_Change_Ind",
          "g0_Delinq_Num", "slc_acct_arr_dir_3", "slc_acct_roll_ever_24_imputed_mean", "Arrears", "PrevDefaults","TimeInDelinqState"
          ,"slc_past_due_amt_imputed_med", "slc_curing_ind", "DefSpell_Num_binned")

# - Single-factor modelling results
# Goodness-of-fit
aicTable(datCredit_train, vars, TimeDef=c("Cox_Discrete","DefSpell_Event"), genPath=genObjPath, modelType="Cox_Discrete")
# Discriminatory power (in-sample)
concTable(datCredit_train, datCredit_valid, vars, TimeDef=c("Cox_Discrete","DefSpell_Event"), genPath=genObjPath, modelType="Cox_Discrete")
### RESULTS: Best AIC-results: PrevDefaults, DefSpell_Age, g0_Delinq_Num, Arrears,DefSpell_Num_binned, slc_acct_arr_dir_3,slc_past_due_amt_imputed_med,slc_curing_ind
# Best C-statistics: PrevDefaults, DefSpell_Age, g0_Delinq_Num, Arrears, slc_acct_arr_dir_3,slc_past_due_amt_imputed_med,slc_curing_ind
# Do not include g0_Delinq and g0_Delinq_fac as these would lead to unstable models
### CONCLUSION
# Choose: PrevDefaults, DefSpell_Age, g0_Delinq_Num, Arrears, DefSpell_Num_binned,slc_past_due_amt_imputed_med,
# These variables performed significantly better than the rest with th lowest AIC 


# ------ 2.6 Combining insights: delinquency-themed variables

# - Initialize variables to be tested
vars <- c("g0_Delinq_SD_5", "g0_Delinq_SD_12","g0_Delinq_SD_6",
          "g0_Delinq_Any_Aggr_Prop_Lag_12", "g0_Delinq_Any_Aggr_Prop_Lag_9","g0_Delinq_Any_Aggr_Prop_Lag_6",
          "DefaultStatus1_Aggr_Prop_Lag_1", "DefaultStatus1_Aggr_Prop_Lag_2", "DefaultStatus1_Aggr_Prop",
          "g0_Delinq_Ave", "ArrearsToBalance_Aggr_Prop","CuringEvents_Aggr_Prop","PrevDefaults",
          "DefSpell_Age", "g0_Delinq_Num", "Arrears", "slc_past_due_amt_imputed_med",
          "DefSpell_Num_binned")


# - Full model | Stepwise forward selection procedure
modLR_full <- glm( as.formula(paste("DefSpell_Event ~", paste(vars, collapse = " + "))),
                   data=datCredit_train, family="binomial")
summary(modLR_full);
evalLR(modLR_full, modLR_base, datCredit_train, targetFld="DefSpell_Event", predClass=1)
### RESULTS: AIC:   69,432; McFadden R^2:  30.62%; AUC:  86.47%.

ptm <- proc.time() # for runtime calculations (ignore)
modLR_step <- stepAIC(modLR_base, scope = list(lower = ~ 1, 
                                                upper = as.formula(paste("~", paste(vars, collapse = " + ")))), 
                      direction = "both", k=log(datCredit_train[,.N]), maxit=50)
summary(modLR_step)
evalLR(modLR_step, modLR_base, datCredit_train, targetFld="DefSpell_Event", predClass=1)
proc.time() - ptm # IGNORE: elapsed runtime; 147m
### RESULTS: AIC:   69,435; McFadden R^2:  30.62%; AUC:  86.27%.
# - Domain expertise
# Remove g0_Delinq_Any_Aggr_Prop_Lag_9 and g0_Delinq_Any_Aggr_Prop_Lag_6 since g0_Delinq_Any_Aggr_Prop_Lag_12 is already present
# Remove g0_Delinq_SD_6 and g0_Delinq_SD_5

# - Final variables (expert judgement)

vars <- c("g0_Delinq_Any_Aggr_Prop_Lag_12","DefaultStatus1_Aggr_Prop",
          "CuringEvents_Aggr_Prop","PrevDefaults",
          "DefSpell_Age", "g0_Delinq_Ave", "Arrears","g0_Delinq_Num",
          "slc_past_due_amt_imputed_med", "DefSpell_Num_binned", "ArrearsToBalance_Aggr_Prop")
modLR <- glm( as.formula(paste("DefSpell_Event ~", paste(vars, collapse = " + "))),
              data=datCredit_train, family="binomial")
summary(modLR);
evalLR(modLR, modLR_base, datCredit_train, targetFld="DefSpell_Event", predClass=1)
### RESULTS: AIC:   70,611; McFadden R^2:  29.43%; AUC:  85.97%.

# - Final variables 

# Included curing ind
vars <- c("g0_Delinq_Any_Aggr_Prop_Lag_12","DefaultStatus1_Aggr_Prop",
          "CuringEvents_Aggr_Prop","PrevDefaults",
          "DefSpell_Age", "g0_Delinq_Num", "Arrears",
          "slc_past_due_amt_imputed_med", "DefSpell_Num_binned")
modLR <- glm( as.formula(paste("DefSpell_Event ~", paste(vars, collapse = " + "))),
              data=datCredit_train, family="binomial")
summary(modLR);
evalLR(modLR, modLR_base, datCredit_train, targetFld="DefSpell_Event", predClass=1)
### RESULTS: AIC:   70,738; McFadden R^2:  29.30%; AUC:  85.89%.


# ------ 3. Other portfolio-level variables


# ------ 3.1 Which lag order is the best in calculating the median interest rate of the portfolio?

# - Initialize variables to be tested
vars <- c("InterestRate_Margin_Aggr_Med", "InterestRate_Margin_Aggr_Med_1", "InterestRate_Margin_Aggr_Med_2",
          "InterestRate_Margin_Aggr_Med_3", "InterestRate_Margin_Aggr_Med_9")

# - Single-factor modelling results
# Goodness-of-fit
aicTable(datCredit_train, vars, TimeDef=c("Cox_Discrete","DefSpell_Event"), genPath=genObjPath, modelType="Cox_Discrete")
# Discriminatory power (in-sample)
concTable(datCredit_train, datCredit_valid, vars, TimeDef=c("Cox_Discrete","DefSpell_Event"), genPath=genObjPath, modelType="Cox_Discrete")
### RESULTS: Best AIC-results: InterestRate_Margin_Aggr_Med , InterestRate_Margin_Aggr_Med_1, InterestRate_Margin_Aggr_Med_2,
# InterestRate_Margin_Aggr_Med_3. InterestRate_Margin_Aggr_Med_9
# Best C-statistics: InterestRate_Margin_Aggr_Med , InterestRate_Margin_Aggr_Med_1, InterestRate_Margin_Aggr_Med_2,
# InterestRate_Margin_Aggr_Med_3. InterestRate_Margin_Aggr_Med_9

### Conclusion
# InterestRate_Margin_Aggr_Med_2 , InterestRate_Margin_Aggr_Med, InterestRate_Margin_Aggr_Med_1
# However the difference in AIC and Harell's c is minor


# ------ 3.2 How do other portfolio-level (non-delinquency) variables fare as single-factor models?

# - Initialize variables to be tested
vars <- c("InstalmentToBalance_Aggr_Prop", "AgeToTerm_Aggr_Mean", "DefSpell_Maturity_Aggr_Mean", "NewLoans_Aggr_Prop")

# - Single-factor modelling results
# Goodness-of-fit
aicTable(datCredit_train, vars, TimeDef=c("Cox_Discrete","DefSpell_Event"), genPath=genObjPath, modelType="Cox_Discrete")
# Discriminatory power (in-sample)
concTable(datCredit_train, datCredit_valid, vars, TimeDef=c("Cox_Discrete","DefSpell_Event"), genPath=genObjPath, modelType="Cox_Discrete")
### RESULTS: Best AIC-results: AgeToTerm_Aggr_Mean,  DefSpell_Maturity_Aggr_Mean,InstalmentToBalance_Aggr_Prop, NewLoans_Aggr_Prop
# Best C-statistics: AgeToTerm_Aggr_Mean, InstalmentToBalance_Aggr_Prop,DefSpell_Maturity_Aggr_Mean, NewLoans_Aggr_Prop,

### CONCLUSION: Choose DefSpell_Maturity_Aggr_Mean + InstalmentToBalance_Aggr_Prop +  AgeToTerm_Aggr_Mean


# ------ 3.3 Combining insights: Delinquency-themed and portfolio-level variables

# - Initialize variables to be tested
vars <- c("g0_Delinq_Any_Aggr_Prop_Lag_12","DefaultStatus1_Aggr_Prop",
          "CuringEvents_Aggr_Prop","PrevDefaults",
          "DefSpell_Age", "g0_Delinq_Num", "Arrears",
          "slc_past_due_amt_imputed_med", "DefSpell_Num_binned",
          "InterestRate_Margin_Aggr_Med", "InterestRate_Margin_Aggr_Med_1", "InterestRate_Margin_Aggr_Med_2",
          "InstalmentToBalance_Aggr_Prop", "AgeToTerm_Aggr_Mean", "DefSpell_Maturity_Aggr_Mean")

# - Full model | Stepwise forward selection procedure
modLR_full <- glm( as.formula(paste("DefSpell_Event ~", paste(vars, collapse = " + "))),
                   data=datCredit_train, family="binomial")
summary(modLR_full);
evalLR(modLR_full, modLR_base, datCredit_train, targetFld="DefSpell_Event", predClass=1)
### RESULTS: AIC:   69,459; McFadden R^2:  30.59%; AUC:  86.43%.

# - Stepwise forward selection using BIC
ptm <- proc.time() # for runtime calculations (ignore)
modLR_step <- stepAIC(modLR_base, scope = list(lower = ~ 1, 
                                                upper = as.formula(paste("~", paste(vars, collapse = " + ")))), 
                      direction = "both", k=log(datCredit_train[,.N]), maxit=50)
summary(modLR_step)
evalLR(modLR_step, modLR_base, datCredit_train, targetFld="DefSpell_Event", predClass=1)
proc.time() - ptm # IGNORE: elapsed runtime; 73m
### RESULTS: AIC:   69,464; McFadden R^2:  30.58%; AUC:  86.42%.

# Final variables (expert judgement)
vars <- c("g0_Delinq_Any_Aggr_Prop_Lag_12","DefaultStatus1_Aggr_Prop",
          "CuringEvents_Aggr_Prop","PrevDefaults",
          "DefSpell_Age", "g0_Delinq_Num", "Arrears",
          "slc_past_due_amt_imputed_med", "DefSpell_Num_binned",
          "InterestRate_Margin_Aggr_Med", "AgeToTerm_Aggr_Mean")


modLR <- glm( as.formula(paste("DefSpell_Event ~", paste(vars, collapse = " + "))),
              data=datCredit_train, family="binomial")
summary(modLR);
evalLR(modLR, modLR_base, datCredit_train, targetFld="DefSpell_Event", predClass=1)
### RESULTS: AIC:   70,280; McFadden R^2:  29,76%; AUC:  86.09%.




# ------ 4. Account-level variables

# ------ 4.1 How do various non-delinquency account-level variables fare as single-factor models?
vars <- c("Principal_Real", "Principal", "InterestRate_Margin", 
          "Balance_Real", "Balance", "Instalment_Real", "InterestRate_Nom", "AgeToTerm",
          "BalanceToPrincipal", "slc_acct_pre_lim_perc_imputed_med")

# - Correlation analysis towards obtaining clusters of correlated variables
corrAnalysis(datCredit_train, vars, corrThresh = 0.6, method = 'spearman')
### RESULTS:
# Absolute correlations of  96%  found for  Principal_Real  and  Principal 
# Absolute correlations of  92%  found for  Principal_Real  and  Balance_Real 
# Absolute correlations of  87%  found for  Principal  and  Balance_Real 
# Absolute correlations of  90%  found for  Principal_Real  and  Balance 
# Absolute correlations of  91%  found for  Principal  and  Balance 
# Absolute correlations of  97%  found for  Balance_Real  and  Balance 
# Absolute correlations of  91%  found for  Principal_Real  and  Instalment_Real 
# Absolute correlations of  86%  found for  Principal  and  Instalment_Real 
# Absolute correlations of  96%  found for  Balance_Real  and  Instalment_Real 
# Absolute correlations of  92%  found for  Balance  and  Instalment_Real 
# Absolute correlations of  -60%  found for  Balance_Real  and  AgeToTerm 
# Absolute correlations of  76%  found for  Balance_Real  and  BalanceToPrincipal 
# Absolute correlations of  69%  found for  Balance  and  BalanceToPrincipal 
# Absolute correlations of  69%  found for  Instalment_Real  and  BalanceToPrincipal 
# Absolute correlations of  -68%  found for  AgeToTerm  and  BalanceToPrincipal

# - Initialize variables to be tested
vars <- c("Principal_Real", "Principal", "InterestRate_Margin_imputed_mean", "pmnt_method_grp",
          "Balance_Real", "Balance", "Instalment_Real", "InterestRate_Nom", "AgeToTerm",
          "BalanceToPrincipal", "slc_acct_pre_lim_perc_imputed_med")

# - Single-factor modelling results
# Goodness-of-fit
aicTable(datCredit_train, vars, TimeDef=c("Cox_Discrete","DefSpell_Event"), genPath=genObjPath, modelType="Cox_Discrete")
# Discriminatory power (in-sample)
concTable(datCredit_train, datCredit_valid, vars, TimeDef=c("Cox_Discrete","DefSpell_Event"), genPath=genObjPath, modelType="Cox_Discrete")
### RESULTS: Best AIC-results: Balance_Real, InterestRate_Nom, Instalment_Real, AgeToTerm, Pricipal_Real, BalanceToPrincipal , pmnt_mehtod_grp etc
# Best C-statistics: AgeToTerm, Balance_Real, Instalment_Real, BalanceToPrinical, InterestRate_Nom
# AIC and Harrel's c all very similar to each other
# Choose these 5


# ------ 4.2 Combining insights: Delinquency-themed, portfolio-level, and account-level variables

# - Initialize variables to be tested
vars <- c("g0_Delinq_Any_Aggr_Prop_Lag_12","DefaultStatus1_Aggr_Prop",
          "CuringEvents_Aggr_Prop","PrevDefaults",
          "DefSpell_Age", "g0_Delinq_Num", "Arrears",
          "slc_past_due_amt_imputed_med", "DefSpell_Num_binned",
          "InterestRate_Margin_Aggr_Med", "AgeToTerm_Aggr_Mean", "AgeToTerm",
          "BalanceToPrincipal", "pmnt_method_grp")
### RESULTS:

# - Full model | Stepwise forward selection procedure
modLR_full <- glm( as.formula(paste("DefSpell_Event ~", paste(vars, collapse = " + "))),
                   data=datCredit_train, family="binomial")
summary(modLR_full);
evalLR(modLR_full, modLR_base, datCredit_train, targetFld="DefSpell_Event", predClass=1)
### RESULTS: AIC:   68,938; McFadden R^2:  31.11%; AUC:  86.50%.


# - Stepwise forward selection using BIC
ptm <- proc.time() # for runtime calculations (ignore)
modLR_step <- stepAIC(modLR_base, scope = list(lower = ~ 1, 
                                                upper = as.formula(paste("~", paste(vars, collapse = " + ")))), 
                      direction = "both", k=log(datCredit_train[,.N]), maxit=50)
summary(modLR_step)
evalLR(modLR_step, modLR_base, datCredit_train, targetFld="DefSpell_Event", predClass=1)
proc.time() - ptm
### RESULTS: AIC:   68,934; McFadden R^2:  31.11%; AUC:  86.50%.

# All variables remained thus keep as is

# - Final variables (expert judgement)
# All variables remain 
vars <- c("g0_Delinq_Any_Aggr_Prop_Lag_12","DefaultStatus1_Aggr_Prop",
          "CuringEvents_Aggr_Prop","PrevDefaults",
          "DefSpell_Age", "g0_Delinq_Num", "Arrears",
          "slc_past_due_amt_imputed_med", "DefSpell_Num_binned",
          "InterestRate_Margin_Aggr_Med", "AgeToTerm_Aggr_Mean", "AgeToTerm",
          "BalanceToPrincipal", "pmnt_method_grp")
### RESULTS:

# - Full model | Stepwise forward selection procedure
modLR_full <- glm( as.formula(paste("DefSpell_Event ~", paste(vars, collapse = " + "))),
                   data=datCredit_train, family="binomial")
summary(modLR_full);
evalLR(modLR_full, modLR_base, datCredit_train, targetFld="DefSpell_Event", predClass=1)
### RESULTS: AIC:   68,938; McFadden R^2:  31.11%; AUC:  86.50%.




# ------ 5. Macroeconomic variables

# ------ 5.1 Which lag order is the best for: M_Repo_Rate

# - Initialize variables to be tested
vars <- c("M_Repo_Rate", "M_Repo_Rate_1 ", "M_Repo_Rate_2", "M_Repo_Rate_3", "M_Repo_Rate_6", "M_Repo_Rate_9", "M_Repo_Rate_12")

# - Single-factor modelling results
# Goodness-of-fit
aicTable(datCredit_train, vars, TimeDef=c("Cox_Discrete","DefSpell_Event"), genPath=genObjPath, modelType="Cox_Discrete")
# Discriminatory power (in-sample)
concTable(datCredit_train, datCredit_valid, vars, TimeDef=c("Cox_Discrete","DefSpell_Event"), genPath=genObjPath, modelType="Cox_Discrete")
### RESULTS: Best AIC-results:  M_Repo_Rate_3, M_Repo_Rate_2, M_Repo_Rate_6, 
# M_Repo_Rate_1, M_Repo_Rate_9, M_Repo_Rate.
# Best C-statistics: M_Repo_Rate_3, M_Repo_Rate_6, M_Repo_Rate_2, 
# M_Repo_Rate_1, M_Repo_Rate_9, M_Repo_Rate.

### CONCLUSION
# There are minor difference between AIC valuea
# Choose: M_Repo_Rate_2, M_Repo_Rate_3, M_Repo_Rate_6


# ------ 5.2 Which lag order is the best for: M_Inflation_Growth

# - Initialize variables to be tested
vars <- c("M_Inflation_Growth", "M_Inflation_Growth_1 ", "M_Inflation_Growth_2", "M_Inflation_Growth_3", 
          "M_Inflation_Growth_6", "M_Inflation_Growth_9", "M_Inflation_Growth_12")

# - Single-factor modelling results
# Goodness-of-fit
aicTable(datCredit_train, vars, TimeDef=c("Cox_Discrete","DefSpell_Event"), genPath=genObjPath, modelType="Cox_Discrete")
# Discriminatory power (in-sample)
concTable(datCredit_train, datCredit_valid, vars, TimeDef=c("Cox_Discrete","DefSpell_Event"), genPath=genObjPath, modelType="Cox_Discrete")
### RESULTS: Best AIC-results: M_Inflation_Growth_3, M_Inflation_Growth_2, M_Inflation_Growth_1, M_Inflation_Growth_6, 
# M_Inflation_Growth,M_Inflation_Growth_9, M_Inflation_Growth_12 .
# Best C-statistics: M_Inflation_Growth_6, M_Inflation_Growth_3, M_Inflation_Growth_1, M_Inflation_Growth_2, 
# M_Inflation_Growth,M_Inflation_Growth_9, M_Inflation_Growth_12 .

### CONCLUSION: Small AIC and Harell's c-statsitics differences
# Best variables: M_Inflation_Growth_2, M_Inflation_Growth_3, M_Inflation_Growth_1


# ------ 5.3 Which lag order is the best for: M_RealGDP_Growth

# - Initialize variables to be tested
vars <- c("M_RealGDP_Growth", "M_RealGDP_Growth_1 ", "M_RealGDP_Growth_2", "M_RealGDP_Growth_3", 
          "M_RealGDP_Growth_6", "M_RealGDP_Growth_9", "M_RealGDP_Growth_12")

# - Single-factor modelling results
# Goodness-of-fit
aicTable(datCredit_train, vars, TimeDef=c("Cox_Discrete","DefSpell_Event"), genPath=genObjPath, modelType="Cox_Discrete")
# Discriminatory power (in-sample)
concTable(datCredit_train, datCredit_valid, vars, TimeDef=c("Cox_Discrete","DefSpell_Event"), genPath=genObjPath, modelType="Cox_Discrete")
### RESULTS: Best AIC-results: M_RealGDP_Growth_12, M_RealGDP_Growth_9, M_RealGDP_Growth_6, M_RealGDP_Growth_3, 
# M_RealGDP_Growth_2, M_RealGDP_Growth_1, M_RealGDP_Growth.
# Best C-statistics: M_RealGDP_Growth_12, M_RealGDP_Growth_9, M_RealGDP_Growth_6, M_RealGDP_Growth_3, 
# M_RealGDP_Growth_2, M_RealGDP_Growth_1, M_RealGDP_Growth.

### CONCLUSION: Middle lags seem better, based on very small AIC-differences and c-differences (57-54%)
# Choose: M_RealGDP_Growth_12, M_RealGDP_Growth_9, M_RealGDP_Growth_6


# ------ 5.4 Which lag order is the best for: M_RealIncome_Growth

# - Initialize variables to be tested
vars <- c("M_RealIncome_Growth", "M_RealIncome_Growth_1 ", "M_RealIncome_Growth_2", "M_RealIncome_Growth_3", 
          "M_RealIncome_Growth_6", "M_RealIncome_Growth_9", "M_RealIncome_Growth_12")

# - Single-factor modelling results
# Goodness-of-fit
aicTable(datCredit_train, vars, TimeDef=c("Cox_Discrete","DefSpell_Event"), genPath=genObjPath, modelType="Cox_Discrete")
# Discriminatory power (in-sample)
concTable(datCredit_train, datCredit_valid, vars, TimeDef=c("Cox_Discrete","DefSpell_Event"), genPath=genObjPath, modelType="Cox_Discrete")
### RESULTS: Best AIC-results: M_RealIncome_Growth_12, M_RealIncome_Growth_9, M_RealIncome_Growth_6, M_RealIncome_Growth_3, 
# M_RealIncome_Growth_2, M_RealIncome_Growth_1, M_RealIncome_Growth.
# Best C-statistics: M_RealIncome_Growth_12, M_RealIncome_Growth_9, M_RealIncome_Growth_6, M_RealIncome_Growth_3, 
# M_RealIncome_Growth_2, M_RealIncome_Growth_1, M_RealIncome_Growth.

### CONCLUSION: Middle lags seem better, based on very small AIC-differences and c-differences (50%-51%)
# # Choose:  M_RealIncome_Growth_6, M_RealIncome_Growth_9, M_RealIncome_Growth_12


# ------ 5.5 Which lag order is the best for: M_DTI_Growth

# - Initialize variables to be tested
vars <- c("M_DTI_Growth", "M_DTI_Growth_1 ", "M_DTI_Growth_2", "M_DTI_Growth_3", 
          "M_DTI_Growth_6", "M_DTI_Growth_9", "M_DTI_Growth_12")

# - Single-factor modelling results
# Goodness-of-fit
aicTable(datCredit_train, vars, TimeDef=c("Cox_Discrete","DefSpell_Event"), genPath=genObjPath, modelType="Cox_Discrete")
# Discriminatory power (in-sample)
concTable(datCredit_train, datCredit_valid, vars, TimeDef=c("Cox_Discrete","DefSpell_Event"), genPath=genObjPath, modelType="Cox_Discrete")
### RESULTS: Best AIC-results: M_DTI_Growth_3, M_DTI_Growth_6, M_DTI_Growth_2, M_DTI_Growth_1, M_DTI_Growth_9, 
# M_DTI_Growth,M_DTI_Growth_12 .
# Best C-statistics: _DTI_Growth_3, M_DTI_Growth_6, M_DTI_Growth_2, M_DTI_Growth_1, M_DTI_Growth_9, 
# M_DTI_Growth,M_DTI_Growth_12 

### CONCLUSION: Longer lags seem better, based on very small AIC-differences,
# trend exist across c-differences (54-56%)
# Choose: M_DTI_Growth_2, M_DTI_Growth_3, M_DTI_Growth_6


# ------ 5.6 Which lag order is the best for: M_Emp_Growth

# - Initialize variables to be tested
vars <- c("M_Emp_Growth", "M_Emp_Growth_1 ", "M_Emp_Growth_2", "M_Emp_Growth_3", 
          "M_Emp_Growth_6", "M_Emp_Growth_9", "M_Emp_Growth_12")

# - Single-factor modelling results
# Goodness-of-fit
aicTable(datCredit_train, vars, TimeDef=c("Cox_Discrete","DefSpell_Event"), genPath=genObjPath, modelType="Cox_Discrete")
# Discriminatory power (in-sample)
concTable(datCredit_train, datCredit_valid, vars, TimeDef=c("Cox_Discrete","DefSpell_Event"), genPath=genObjPath, modelType="Cox_Discrete")
### RESULTS: Best AIC-results: M_Emp_Growth_12, M_Emp_Growth_9, M_Emp_Growth_6, M_Emp_Growth_3
# M_Emp_Growth_2, M_Emp_Growth_1, M_Emp_Growth
# Best C-statistics: M_Emp_Growth_12, M_Emp_Growth_9, M_Emp_Growth_6, M_Emp_Growth_3
# M_Emp_Growth_2, M_Emp_Growth_1, M_Emp_Growth

### CONCLUSION: Middle to late lags seem better, based on very small AIC-differences, affirmed by the c-differences (50-51%)


# ------ 7.7 Combining insights: Macroeconomic variables

# - Initialize variables to be tested
vars <- c("M_Repo_Rate_6", "M_Repo_Rate_3", "M_Repo_Rate_2", 
          "M_Inflation_Growth_2", "M_Inflation_Growth_3", "M_Inflation_Growth_1",
          "M_RealGDP_Growth_12", "M_RealGDP_Growth_9", "M_RealGDP_Growth_6",
          "M_RealIncome_Growth_9", "M_RealIncome_Growth_12", "M_RealIncome_Growth_6",
          "M_DTI_Growth_6", "M_DTI_Growth_2", "M_DTI_Growth_3", 
          "M_Emp_Growth_12", "M_Emp_Growth_9", "M_Emp_Growth_6")

# - Full model | Stepwise forward selection procedure
modLR_full <- glm( as.formula(paste("DefSpell_Event ~", paste(vars, collapse = " + "))),
                   data=datCredit_train, family="binomial")
summary(modLR_full);
evalLR(modLR_full, modLR_base, datCredit_train, targetFld="DefSpell_Event", predClass=1)
### RESULTS: AIC:  93,786; McFadden R^2:  6.28%; AUC:  67.27%.

# - Stepwise forward selection using BIC
ptm <- proc.time() # for runtime calculations (ignore)
modLR_step <- stepAIC(modLR_base, scope = list(lower = ~ 1, 
                                                upper = as.formula(paste("~", paste(vars, collapse = " + ")))), 
                      direction = "both", k=log(datCredit_train[,.N]), maxit=50)
summary(modLR_step)
evalLR(modLR_step, modLR_base, datCredit_train, targetFld="DefSpell_Event", predClass=1)
proc.time() - ptm # IGNORE: elapsed runtime; 34m
### RESULTS: AIC:   93,983;   McFadden R^2:  6.06%; AUC:  66.98%.


# - Final variables
vars <- c( "M_RealIncome_Growth_12", "M_Inflation_Growth_3","M_DTI_Growth_6", "M_Emp_Growth_9",
           "M_RealIncome_Growth_9", "M_Repo_Rate_2")
modLR <- glm( as.formula(paste("DefSpell_Event ~", paste(vars, collapse = " + "))),
              data=datCredit_train, family="binomial")
summary(modLR);
evalLR(modLR, modLR_base, datCredit_train, targetFld="DefSpell_Event", predClass=1)
### RESULTS: AIC:   93,983;   McFadden R^2:  6.06%; AUC:  66.98%.


# - Final variables (Expert Judgement)
vars <- c("M_RealIncome_Growth_12", "M_Inflation_Growth_3","M_DTI_Growth_6","M_Repo_Rate_2")
modLR <- glm( as.formula(paste("DefSpell_Event ~", paste(vars, collapse = " + "))),
              data=datCredit_train, family="binomial")
summary(modLR);
evalLR(modLR, modLR_base, datCredit_train, targetFld="DefSpell_Event", predClass=1)
### RESULTS: AIC:   94,140;   McFadden R^2:  5.89%; AUC:  67.07%.
# Included the Repo rate as it is vital. 



# ------ 5.8 Combining insights: Delinquency-themed, portfolio-level, account-level, and macroeconomic variables

# - Initialize variables to be tested
vars <- c("g0_Delinq_Any_Aggr_Prop_Lag_12","DefaultStatus1_Aggr_Prop",
          "CuringEvents_Aggr_Prop","PrevDefaults",
          "DefSpell_Age", "g0_Delinq_Num", "Arrears",
          "slc_past_due_amt_imputed_med", "DefSpell_Num_binned",
          "InterestRate_Margin_Aggr_Med", "AgeToTerm_Aggr_Mean", "AgeToTerm",
          "BalanceToPrincipal", "pmnt_method_grp",
          "M_RealIncome_Growth_12", "M_Inflation_Growth_3","M_DTI_Growth_6","M_Repo_Rate_2")


# - Full model | Stepwise forward selection procedure
modLR_full <- glm( as.formula(paste("DefSpell_Event ~", paste(vars, collapse = " + "))),
                   data=datCredit_train, family="binomial")
summary(modLR_full);
evalLR(modLR_full, modLR_base, datCredit_train, targetFld="DefSpell_Event", predClass=1)
### RESULTS: AIC:   67,744;   McFadden R^2:  32.32%; AUC:  87.10%.

# - Stepwise forward selection using BIC
ptm <- proc.time() # for runtime calculations (ignore)
modLR_step <- stepAIC(modLR_base, scope = list(lower = ~ 1, 
                                                upper = as.formula(paste("~", paste(vars, collapse = " + ")))), 
                      direction = "both", k=log(datCredit_train[,.N]), maxit=50)
summary(modLR_step)
evalLR(modLR_step, modLR_base, datCredit_train, targetFld="DefSpell_Event", predClass=1)
proc.time() - ptm # IGNORE: elapsed runtime; 117m
### RESULTS: AIC:  67,750;  McFadden R^2:  32.31%; AUC: 87.10%.



# ------ 6. Final 

# - Confirm prepared datasets are loaded into memory
if (!exists('datCredit_train_CDH')) unpack.ffdf(paste0(genPath,"creditdata_train_CDH"), tempPath);gc()
if (!exists('datCredit_valid_CDH')) unpack.ffdf(paste0(genPath,"creditdata_valid_CDH"), tempPath);gc()

# - Use only default spells
datCredit_train <- datCredit_train_CDH[!is.na(DefSpell_Key)&DefSpell_Counter==1,]
datCredit_valid <- datCredit_valid_CDH[!is.na(DefSpell_Key)&DefSpell_Counter==1,]

datCredit_train[, DefSpell_Event := ifelse(DefSpellResol_Type_Hist=="WOFF", 1, 0)]
datCredit_valid[, DefSpell_Event := ifelse(DefSpellResol_Type_Hist=="WOFF", 1, 0)]
# remove previous objects from memory
rm(datCredit_train_CDH, datCredit_valid_CDH); gc()

# - Weigh default cases heavier. as determined interactively based on calibration success (script 6e)
datCredit_train[, Weight := ifelse(DefSpell_Event==1,1,1)]
datCredit_valid[, Weight := ifelse(DefSpell_Event==1,1,1)]

# - Fit an "empty" model as a performance gain, used within some diagnostic functions
modLR_base <- glm(DefSpell_Event ~ 1, data=datCredit_train, family="binomial")

# - Final variables
vars <- c("g0_Delinq_Any_Aggr_Prop_Lag_12","DefaultStatus1_Aggr_Prop",
          "CuringEvents_Aggr_Prop","PrevDefaults",
          "DefSpell_Age", "g0_Delinq_Num", "Arrears",
          "slc_past_due_amt_imputed_med", "DefSpell_Num_binned",
          "InterestRate_Margin_Aggr_Med", "AgeToTerm_Aggr_Mean", "AgeToTerm",
          "BalanceToPrincipal", "pmnt_method_grp",
          "M_RealIncome_Growth_12", "M_Inflation_Growth_3","M_DTI_Growth_6","M_Repo_Rate_2")
modLR <- glm( as.formula(paste("DefSpell_Event ~", paste(vars, collapse = " + "))),
              data=datCredit_train, family="binomial")
summary(modLR);
# Robust (sandwich) standard errors
robust_se <- vcovHC(modLR, type="HC0")
# Summary with robust SEs
coeftest(modLR, vcov.=robust_se)

# - Other diagnostics
evalLR(modLR, modLR_base, datCredit_train, targetFld="DefSpell_Event", predClass=1)
### RESULTS: AIC:  67,733;  McFadden R^2:  32.32%; AUC:  87.10%.

# - Test goodness-of-fit using AIC-measure over single-factor models
aicTable_LR <- aicTable(datCredit_train, vars, TimeDef=c("Cox_Discrete","DefSpell_Event"), genPath=genObjPath, modelType="Cox_Discrete")
# Top variables: Prev_Defaults, DefSpell_Age, M_Repo_Rate_2, AgeToTerm

# Test accuracy using c-statistic over single-factor models
concTable_LR <- concTable(datCredit_train, datCredit_valid, vars, TimeDef=c("Cox_Discrete","DefSpell_Event"), genPath=genObjPath, modelType="Cox_Discrete")
# Top variables:  Prev_Defaults, DefSpell_Age, M_Repo_Rate_2, AgeToTerm

# - Combine results into a single object
Table_LR <- concTable_LR[,1:2] %>% left_join(aicTable_LR, by ="Variable")

GoF_CoxSnell_KS(modLR_classic, datTrain_classic, GraphInd=TRUE, ,legPos=c(0.6,0.4), panelTitle="Logistic Regression",
                fileName = paste0(genFigPath, "KS_Test_CoxSnellResiduals_Exp_CDH_LR", ".png"), dpi=280,fldLstRowInd="DefSpell_Counter")

# Save objects
pack.ffdf(paste0(genObjPath,"LR_fits"), Table_LR)



