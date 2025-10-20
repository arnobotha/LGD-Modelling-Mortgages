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

# - Fit a baseline model for stepwise forward selection
modLR_base2 <- glm(DefSpell_Event ~ log(DefSpell_Age)*DefSpell_Num_binned, 
                   data=datCredit_train, family="binomial")
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
### RESULTS: Best AIC-results: g0_Delinq_SD_12, g0_Delinq_SD_9, g0_Delinq_SD_6, g0_Delinq_SD_5, g0_Delinq_SD_4
# Best Harrell's C-statistics: g0_Delinq_SD_9, g0_Delinq_SD_6, g0_Delinq_SD_12
# All of them are significant on their own

### Conclusion: The Harell's C-statistic decreases with a linear trend 
# g0_Delinq_SD_9 seems to perform the best but also include g0_Delinq_SD_12 and g0_Delinq_SD_6


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
### RESULTS: Best AIC-results: g0_Delinq_Any_Aggr_Prop_9, g0_Delinq_Any_Aggr_Prop_Lag_12, g0_Delinq_Any_Aggr_Prop_Lag_6, g0_Delinq_Any_Aggr_Prop_Lag_5
# , g0_Delinq_Any_Aggr_Prop_Lag_3
# Best C-statistics: g0_Delinq_Any_Aggr_Prop_12, g0_Delinq_Any_Aggr_Prop_Lag_9, g0_Delinq_Any_Aggr_Prop_Lag_4, g0_Delinq_Any_Aggr_Prop_Lag_3

# Conclusion: The differences in AIC and Harell's C-statiistic are minor
# Choose the overall top 3 performing variables g0_Delinq_Any_Aggr_Prop_12, g0_Delinq_Any_Aggr_Prop_Lag_9, g0_Delinq_Any_Aggr_Prop_Lag_3
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
### RESULTS: Best AIC-results: DefaultStatus1_Aggr_Prop_Lag, DefaultStatus1_Aggr_Prop_Lag_1, DefaultStatus1_Aggr_Prop_Lag_3, 
# This decreases from the highest lag to lowest lag in decreasing order
# Differences are not that major
# Best C-statistics: DefaultStatus1_Aggr_Prop_Lag_12, DefaultStatus1_Aggr_Prop_Lag, DefaultStatus1_Aggr_Prop_Lag_1, 
# Similar trend as to AIC
# Opposite of PD side

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
### RESULTS: Best AIC-results: CuringEvents_Aggr_Prop, ArrearsToBalance_Aggr_Prop, g0_Delinq_Ave
# However they aren't that big with their being a e00 difference between the lowest and highest
# Best C-statistics: g0_Delinq_Ave, ArrearsToBalance_Aggr_Prop, CuringEvents_Aggr_Prop
# Similar trend with all being significant here with 0.58 for CuringEvent_Aggr_prop and 0.52 for 
# ArrearsToBalance_Aggr_Prop

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
### RESULTS: Best AIC-results: PrevDefaults, DefSpell_Age, g0_Delinq_Num, Arrears, slc_acct_arr_dir_3,slc_past_due_amt_imputed_med,slc_curing_ind
# Best C-statistics: PrevDefaults, DefSpell_Age, g0_Delinq_Num, Arrears, slc_acct_arr_dir_3,slc_past_due_amt_imputed_med,slc_curing_ind
# Do not include g0_Delinq and g0_Delinq_fac as these would lead to unstable models
### CONCLUSION
# Choose: PrevDefaults, DefSpell_Age, g0_Delinq_Num, Arrears, slc_acct_arr_dir_3,slc_past_due_amt_imputed_med,slc_curing_ind
# These variables performed significantly better than the rest with th lowest AIC 
# values and Harrell's c values greater than 0.87

# ------ 2.6 Combining insights: delinquency-themed variables

# - Initialize variables to be tested
vars <- c("g0_Delinq_SD_9", "g0_Delinq_SD_12","g0_Delinq_SD_6",
          "g0_Delinq_Any_Aggr_Prop_Lag_12", "g0_Delinq_Any_Aggr_Prop_Lag_9","g0_Delinq_Any_Aggr_Prop_Lag_3",
          "DefaultStatus1_Aggr_Prop_Lag_1", "DefaultStatus1_Aggr_Prop_Lag_2", "DefaultStatus1_Aggr_Prop",
          "g0_Delinq_Ave", "ArrearsToBalance_Aggr_Prop","CuringEvents_Aggr_Prop","PrevDefaults",
          "DefSpell_Age", "g0_Delinq_Num", "Arrears", "slc_acct_arr_dir_3", "slc_past_due_amt_imputed_med",
          "slc_curing_ind","DefSpell_Num_binned")


# - Full model | Stepwise forward selection procedure
modLR_full <- glm( as.formula(paste("DefSpell_Event ~", paste(vars, collapse = " + "))),
                   data=datCredit_train, family="binomial")
summary(modLR_full);
evalLR(modLR_full, modLR_base, datCredit_train, targetFld="DefSpell_Event", predClass=1)
### RESULTS: AIC:   12,489; McFadden R^2:  26.55%; AUC:  84.08%.
# Including g0_Delinq_SD_4,g0_Delinq_SD_5,g0_Delinq_SD_6 caused the model to not converge
# and quasi complete separation so exclude them. Only consider one that being lag 4
# Arrears also causes quasi complete separation so from expret judgement remove it

ptm <- proc.time() # for runtime calculations (ignore)
modLR_step <- stepAIC(modLR_base, scope = list(lower = ~ 1, 
                                                upper = as.formula(paste("~", paste(vars, collapse = " + ")))), 
                      direction = "both", k=log(datCredit_train[,.N]), maxit=50)
summary(modLR_step)
evalLR(modLR_step, modLR_base, datCredit_train, targetFld="DefSpell_Event", predClass=1)
proc.time() - ptm # IGNORE: elapsed runtime; 147m
### RESULTS: AIC:   12,569; McFadden R^2:  25.96%; AUC:  83.74%.
# - Domain expertise
# Remove g0_Delinq_Any_Aggr_Prop_Lag_9 and g0_Delinq_Any_Aggr_Prop_Lag_3 since g0_Delinq_Any_Aggr_Prop_Lag_12 is already present

# - Final variables (expert judgement)

# Included curing ind
vars <- c("g0_Delinq_SD_12","g0_Delinq_Any_Aggr_Prop_Lag_12","DefaultStatus1_Aggr_Prop_Lag_2",
          "CuringEvents_Aggr_Prop","PrevDefaults",
          "DefSpell_Age", "g0_Delinq_Num", "Arrears",
          "slc_curing_ind", "DefSpell_Num_binned")
modLR <- glm( as.formula(paste("DefSpell_Event ~", paste(vars, collapse = " + "))),
              data=datCredit_train, family="binomial")
summary(modLR);
evalLR(modLR, modLR_base, datCredit_train, targetFld="DefSpell_Event", predClass=1)
### RESULTS: AIC:   12,863; McFadden R^2:  24.20%; AUC:  83.95%.

# - Final variables 

# Included curing ind
vars <- c("g0_Delinq_SD_12","g0_Delinq_Any_Aggr_Prop_Lag_12","DefaultStatus1_Aggr_Prop_Lag_2",
          "CuringEvents_Aggr_Prop","PrevDefaults",
          "DefSpell_Age", "g0_Delinq_Num", "Arrears",
          "slc_curing_ind", "DefSpell_Num_binned")
modLR <- glm( as.formula(paste("DefSpell_Event ~", paste(vars, collapse = " + "))),
              data=datCredit_train, family="binomial")
summary(modLR);
evalLR(modLR, modLR_base, datCredit_train, targetFld="DefSpell_Event", predClass=1)
### RESULTS: AIC:   12,863; McFadden R^2:  24.20%; AUC:  83.95%.




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
### RESULTS: Best AIC-results: InterestRate_Margin_Aggr_Med_2 , InterestRate_Margin_Aggr_Med_3, InterestRate_Margin_Aggr_Med_1,
# InterestRate_Margin_Aggr_Med. InterestRate_Margin_Aggr_Med_9
# Best C-statistics: InterestRate_Margin_Aggr_Med_2 , InterestRate_Margin_Aggr_Med_1, InterestRate_Margin_Aggr_Med_3,
# InterestRate_Margin_Aggr_Med, InterestRate_Margin_Aggr_Med_9.

### Conclusion
# InterestRate_Margin_Aggr_Med_2 , InterestRate_Margin_Aggr_Med_3, InterestRate_Margin_Aggr_Med_1
# However the difference in AIC and Harell's c is minor


# ------ 3.2 How do other portfolio-level (non-delinquency) variables fare as single-factor models?

# - Initialize variables to be tested
vars <- c("InstalmentToBalance_Aggr_Prop", "AgeToTerm_Aggr_Mean", "DefSpell_Maturity_Aggr_Mean", "NewLoans_Aggr_Prop")

# - Single-factor modelling results
# Goodness-of-fit
aicTable(datCredit_train, vars, TimeDef=c("Cox_Discrete","DefSpell_Event"), genPath=genObjPath, modelType="Cox_Discrete")
# Discriminatory power (in-sample)
concTable(datCredit_train, datCredit_valid, vars, TimeDef=c("Cox_Discrete","DefSpell_Event"), genPath=genObjPath, modelType="Cox_Discrete")
### RESULTS: Best AIC-results: AgeToTerm_Aggr_Mean,  DefSpell_Maturity_Aggr_Mean, NewLoans_Aggr_Prop,InstalmentToBalance_Aggr_Prop
# Best C-statistics: AgeToTerm_Aggr_Mean, InstalmentToBalance_Aggr_Prop,DefSpell_Maturity_Aggr_Mean, NewLoans_Aggr_Prop,

### CONCLUSION: Choose DefSpell_Maturity_Aggr_Mean + InstalmentToBalance_Aggr_Prop + NewLoans_Aggr_Prop + AgeToTerm_Aggr_Mean


# ------ 3.3 Combining insights: Delinquency-themed and portfolio-level variables

# - Initialize variables to be tested
vars <- c("g0_Delinq_Any_Aggr_Prop_Lag_12","DefaultStatus1_Aggr_Prop_Lag_2",
          "CuringEvents_Aggr_Prop","PrevDefaults",
          "DefSpell_Age", "g0_Delinq_Num", "Arrears",
          "slc_curing_ind","InterestRate_Margin_Aggr_Med_1", "InterestRate_Margin_Aggr_Med_2",
          "InterestRate_Margin_Aggr_Med_3", "InstalmentToBalance_Aggr_Prop", "AgeToTerm_Aggr_Mean", 
          "DefSpell_Maturity_Aggr_Mean", "NewLoans_Aggr_Prop","DefSpell_Num_binned")

# - Full model | Stepwise forward selection procedure
modLR_full <- glm( as.formula(paste("DefSpell_Event ~", paste(vars, collapse = " + "))),
                   data=datCredit_train, family="binomial")
summary(modLR_full);
evalLR(modLR_full, modLR_base, datCredit_train, targetFld="DefSpell_Event", predClass=1)
### RESULTS: AIC:   12,545; McFadden R^2:  26.14%; AUC:  83.88%.

# - Stepwise forward selection using BIC
ptm <- proc.time() # for runtime calculations (ignore)
modLR_step <- stepAIC(modLR_base, scope = list(lower = ~ 1, 
                                                upper = as.formula(paste("~", paste(vars, collapse = " + ")))), 
                      direction = "both", k=log(datCredit_train[,.N]), maxit=50)
summary(modLR_step)
evalLR(modLR_step, modLR_base, datCredit_train, targetFld="DefSpell_Event", predClass=1)
proc.time() - ptm # IGNORE: elapsed runtime; 73m
### RESULTS: AIC:   23,958; McFadden R^2:  54,09%; AUC:  98.56%.

# Final variables (expert judgement)
# Included curing ind as proven to be vital
vars <- c("g0_Delinq_Any_Aggr_Prop_Lag_12","DefaultStatus1_Aggr_Prop_Lag_2","PrevDefaults",
          "DefSpell_Age", "g0_Delinq_Num", "Arrears", "InterestRate_Margin_Aggr_Med_2", "slc_curing_ind"
          , "InstalmentToBalance_Aggr_Prop", "AgeToTerm_Aggr_Mean", "DefSpell_Num_binned" )

modLR <- glm( as.formula(paste("DefSpell_Event ~", paste(vars, collapse = " + "))),
              data=datCredit_train, family="binomial")
summary(modLR);
evalLR(modLR, modLR_base, datCredit_train, targetFld="DefSpell_Event", predClass=1)
### RESULTS: AIC:   12,563; McFadden R^2:  25.98%; AUC:  83.93%.




# ------ 4. Account-level variables

# ------ 4.1 How do various non-delinquency account-level variables fare as single-factor models?
vars <- c("Principal_Real", "Principal", "InterestRate_Margin", 
          "Balance_Real", "Balance", "Instalment_Real", "InterestRate_Nom", "AgeToTerm",
          "BalanceToPrincipal", "slc_acct_pre_lim_perc_imputed_med")

# - Correlation analysis towards obtaining clusters of correlated variables
corrAnalysis(datCredit_train, vars, corrThresh = 0.6, method = 'spearman')
### RESULTS:
# Absolute correlations of  91%  found for  Principal  and  Balance 
# Absolute correlations of  -61%  found for  Balance  and  AgeToTerm 
# Absolute correlations of  67%  found for  Balance  and  BalanceToPrincipal 
# Absolute correlations of  -79%  found for  AgeToTerm  and  BalanceToPrincipal 

# - Initialize variables to be tested
vars <- c("Principal_Real", "Principal", "InterestRate_Margin_imputed_mean", "pmnt_method_grp",
          "Balance_Real", "Balance", "Instalment_Real", "InterestRate_Nom", "AgeToTerm",
          "BalanceToPrincipal", "slc_acct_pre_lim_perc_imputed_med")

# - Single-factor modelling results
# Goodness-of-fit
aicTable(datCredit_train, vars, TimeDef=c("Cox_Discrete","DefSpell_Event"), genPath=genObjPath, modelType="Cox_Discrete")
# Discriminatory power (in-sample)
concTable(datCredit_train, datCredit_valid, vars, TimeDef=c("Cox_Discrete","DefSpell_Event"), genPath=genObjPath, modelType="Cox_Discrete")
### RESULTS: Best AIC-results: Balance_Real, Instalment_Real, Balance, Principal_Real, InterestRate_Nom, BalanceToPrincipal , pmnt_mehtod_grp etc
# Best C-statistics: BalancetoPrincipal, pmnt_method_grp,  Balance_Real, InterestRate_Nom, Installment
# ALL of the have very low AIC and good Harell's c values>0 
# Choose these 5


# ------ 4.2 Combining insights: Delinquency-themed, portfolio-level, and account-level variables

# - Initialize variables to be tested
vars <- c("g0_Delinq_Any_Aggr_Prop_Lag_12","DefaultStatus1_Aggr_Prop_Lag_2","PrevDefaults",
          "DefSpell_Age", "g0_Delinq_Num", "Arrears", "InterestRate_Margin_Aggr_Med_2", "slc_curing_ind"
          , "InstalmentToBalance_Aggr_Prop", "AgeToTerm_Aggr_Mean","BalanceToPrincipal"
          ,"InterestRate_Nom","pmnt_method_grp", "DefSpell_Num_binned")
### RESULTS:

# - Full model | Stepwise forward selection procedure
modLR_full <- glm( as.formula(paste("DefSpell_Event ~", paste(vars, collapse = " + "))),
                   data=datCredit_train, family="binomial")
summary(modLR_full);
evalLR(modLR_full, modLR_base, datCredit_train, targetFld="DefSpell_Event", predClass=1)
### RESULTS: AIC:   11,958; McFadden R^2:  29.62%; AUC:  85,86%.


# - Stepwise forward selection using BIC
ptm <- proc.time() # for runtime calculations (ignore)
modLR_step <- stepAIC(modLR_base, scope = list(lower = ~ 1, 
                                                upper = as.formula(paste("~", paste(vars, collapse = " + ")))), 
                      direction = "both", k=log(datCredit_train[,.N]), maxit=50)
summary(modLR_step)
evalLR(modLR_step, modLR_base, datCredit_train, targetFld="DefSpell_Event", predClass=1)
proc.time() - ptm
### RESULTS: AIC:   11,968; McFadden R^2:  29.53%; AUC:  85.78%.

# All variables remained thus keep as is

# - Final variables (expert judgement)
# All variables remain 
vars <- c("g0_Delinq_Any_Aggr_Prop_Lag_12","DefaultStatus1_Aggr_Prop_Lag_2","PrevDefaults",
          "DefSpell_Age", "g0_Delinq_Num", "Arrears", "InterestRate_Margin_Aggr_Med_2", "slc_curing_ind"
          , "InstalmentToBalance_Aggr_Prop", "AgeToTerm_Aggr_Mean","BalanceToPrincipal"
          ,"InterestRate_Nom","pmnt_method_grp", "DefSpell_Num_binned")
### RESULTS:

# - Full model | Stepwise forward selection procedure
modLR_full <- glm( as.formula(paste("DefSpell_Event ~", paste(vars, collapse = " + "))),
                   data=datCredit_train, family="binomial")
summary(modLR_full);
evalLR(modLR_full, modLR_base, datCredit_train, targetFld="DefSpell_Event", predClass=1)
### RESULTS: AIC:   11,958; McFadden R^2:  29.62%; AUC:  85,86%.




# ------ 5. Macroeconomic variables

# ------ 5.1 Which lag order is the best for: M_Repo_Rate

# - Initialize variables to be tested
vars <- c("M_Repo_Rate", "M_Repo_Rate_1 ", "M_Repo_Rate_2", "M_Repo_Rate_3", "M_Repo_Rate_6", "M_Repo_Rate_9", "M_Repo_Rate_12")

# - Single-factor modelling results
# Goodness-of-fit
aicTable(datCredit_train, vars, TimeDef=c("Cox_Discrete","DefSpell_Event"), genPath=genObjPath, modelType="Cox_Discrete")
# Discriminatory power (in-sample)
concTable(datCredit_train, datCredit_valid, vars, TimeDef=c("Cox_Discrete","DefSpell_Event"), genPath=genObjPath, modelType="Cox_Discrete")
### RESULTS: Best AIC-results:  M_Repo_Rate_12, M_Repo_Rate_9, M_Repo_Rate_6, 
# M_Repo_Rate_3, M_Repo_Rate_2, M_Repo_Rate_1.
# Best C-statistics: M_Repo_Rate_12, M_Repo_Rate_9, M_Repo_Rate_6, 
# M_Repo_Rate_3, M_Repo_Rate_2, M_Repo_Rate_1.

### CONCLUSION
# There are minor difference between AIC value which suggest the main repo rate
# and lags 1 and 0 are best. C-differences are small toe (ranging from 50%-52%).
# Choose: M_Repo_Rate_12, M_Repo_Rate_9, M_Repo_Rate_6


# ------ 5.2 Which lag order is the best for: M_Inflation_Growth

# - Initialize variables to be tested
vars <- c("M_Inflation_Growth", "M_Inflation_Growth_1 ", "M_Inflation_Growth_2", "M_Inflation_Growth_3", 
          "M_Inflation_Growth_6", "M_Inflation_Growth_9", "M_Inflation_Growth_12")

# - Single-factor modelling results
# Goodness-of-fit
aicTable(datCredit_train, vars, TimeDef=c("Cox_Discrete","DefSpell_Event"), genPath=genObjPath, modelType="Cox_Discrete")
# Discriminatory power (in-sample)
concTable(datCredit_train, datCredit_valid, vars, TimeDef=c("Cox_Discrete","DefSpell_Event"), genPath=genObjPath, modelType="Cox_Discrete")
### RESULTS: Best AIC-results: M_RealIncome_Growth_1, M_RealIncome_Growth, M_RealIncome_Growth_2, M_RealIncome_Growth_3, 
# M_RealIncome_Growth_6,M_RealIncome_Growth_9, M_RealIncome_Growth_12 .
# Best C-statistics: MM_RealIncome_Growth, M_RealIncome_Growth_1, M_RealIncome_Growth_6, M_RealIncome_Growth_3, 
# M_RealIncome_Growth_2,M_RealIncome_Growth_9, M_RealIncome_Growth_ .

### CONCLUSION: Small AIC and Harell's c-statsitics differences
# Best variables: M_RealIncome_Growth_2, M_RealIncome_Growth, M_RealIncome_Growth_1


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
### RESULTS: Best AIC-results: M_Emp_Growth_12, M_Emp_Growth_2, M_Emp_Growth_3, M_Emp_Growth_1
# M_Emp_Growth_9, M_Emp_Growth_6, M_Emp_Growth
# Best C-statistics: M_Emp_Growth_12, M_Emp_Growth_9, M_Emp_Growth_6, M_Emp_Growth_3
# M_Emp_Growth_12, M_Emp_Growth_3, M_Emp_Growth_9

### CONCLUSION: Middle to late lags seem better, based on very small AIC-differences, affirmed by the c-differences (50-51%)


# ------ 7.7 Combining insights: Macroeconomic variables

# - Initialize variables to be tested
vars <- c("M_Repo_Rate", "M_Repo_Rate_1", "M_Repo_Rate_2", 
          "M_Inflation_Growth_2", "M_Inflation_Growth", "M_Inflation_Growth_1",
          "M_RealGDP_Growth_12", "M_RealGDP_Growth_9", "M_RealGDP_Growth_6",
          "M_RealIncome_Growth_9", "M_RealIncome_Growth_12", "M_RealIncome_Growth_6",
          "M_DTI_Growth_9", "M_DTI_Growth_2", "M_DTI_Growth_3", 
          "M_Emp_Growth_2", "M_Emp_Growth_3", "M_Emp_Growth_6")

# - Full model | Stepwise forward selection procedure
modLR_full <- glm( as.formula(paste("DefSpell_Event ~", paste(vars, collapse = " + "))),
                   data=datCredit_train, family="binomial")
summary(modLR_full);
evalLR(modLR_full, modLR_base, datCredit_train, targetFld="DefSpell_Event", predClass=1)
### RESULTS: AIC:  16,213; McFadden R^2:  4.61%; AUC:  64.86%.

# - Stepwise forward selection using BIC
ptm <- proc.time() # for runtime calculations (ignore)
modLR_step <- stepAIC(modLR_base, scope = list(lower = ~ 1, 
                                                upper = as.formula(paste("~", paste(vars, collapse = " + ")))), 
                      direction = "both", k=log(datCredit_train[,.N]), maxit=50)
summary(modLR_step)
evalLR(modLR_step, modLR_base, datCredit_train, targetFld="DefSpell_Event", predClass=1)
proc.time() - ptm # IGNORE: elapsed runtime; 34m
### RESULTS: AIC:   12,214;   McFadden R^2:  4.38%; AUC:  64.83%.


# - Final variables
vars <- c( "M_RealIncome_Growth_12", "M_Inflation_Growth","M_DTI_Growth_3", "M_DTI_Growth_9",
           "M_RealGDP_Growth_12", "M_Repo_Rate_2")
modLR <- glm( as.formula(paste("DefSpell_Event ~", paste(vars, collapse = " + "))),
              data=datCredit_train, family="binomial")
summary(modLR);
evalLR(modLR, modLR_base, datCredit_train, targetFld="DefSpell_Event", predClass=1)
### RESULTS: AIC:   35,318;   McFadden R^2:  4.38%; AUC:  64.83%.


# - Final variables (Expert Judgement)
vars <- c("M_RealIncome_Growth_12", "M_Inflation_Growth","M_DTI_Growth_3","M_Repo_Rate_2")
modLR <- glm( as.formula(paste("DefSpell_Event ~", paste(vars, collapse = " + "))),
              data=datCredit_train, family="binomial")
summary(modLR);
evalLR(modLR, modLR_base, datCredit_train, targetFld="DefSpell_Event", predClass=1)
### RESULTS: AIC:   16,288;   McFadden R^2:  3.92%; AUC:  63.24%.
# Included the Repo rate as it is vital. GDP growth and emp growth have a negative effect
# They increase the AIC and R^2 values


# ------ 5.8 Combining insights: Delinquency-themed, portfolio-level, account-level, and macroeconomic variables

# - Initialize variables to be tested
vars <- c("g0_Delinq_Any_Aggr_Prop_Lag_12","DefaultStatus1_Aggr_Prop_Lag_2","PrevDefaults",
          "DefSpell_Age", "g0_Delinq_Num", "Arrears", "InterestRate_Margin_Aggr_Med_2", "slc_curing_ind"
          , "InstalmentToBalance_Aggr_Prop", "AgeToTerm_Aggr_Mean","BalanceToPrincipal"
          ,"InterestRate_Nom","pmnt_method_grp", "DefSpell_Num_binned",
          "M_RealIncome_Growth_12", "M_Inflation_Growth","M_DTI_Growth_3","M_Repo_Rate_2")


# - Full model | Stepwise forward selection procedure
modLR_full <- glm( as.formula(paste("DefSpell_Event ~", paste(vars, collapse = " + "))),
                   data=datCredit_train, family="binomial")
summary(modLR_full);
evalLR(modLR_full, modLR_base, datCredit_train, targetFld="DefSpell_Event", predClass=1)
### RESULTS: AIC:   11,897;   McFadden R^2:  30.03%; AUC:  85.95%.

# - Stepwise forward selection using BIC
ptm <- proc.time() # for runtime calculations (ignore)
modLR_step <- stepAIC(modLR_base, scope = list(lower = ~ 1, 
                                                upper = as.formula(paste("~", paste(vars, collapse = " + ")))), 
                      direction = "both", k=log(datCredit_train[,.N]), maxit=50)
summary(modLR_step)
evalLR(modLR_step, modLR_base, datCredit_train, targetFld="DefSpell_Event", predClass=1)
proc.time() - ptm # IGNORE: elapsed runtime; 117m
### RESULTS: AIC:  11,900;  McFadden R^2:  29.94%; AUC: 85.55%.

# - Domain expertise
# Exchange log(TimeInDefSpell) with Time_Binned for final modelling iteration


