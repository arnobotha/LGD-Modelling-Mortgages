# ======================================= INPUT SPACE: DISCRETE COX ADVANCED============================
# Divide data into thematic groups and perform data analysis on them to compile an input space for 
# and advanced discrete-time hazard model.
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
if (!exists('datCredit_prep')) unpack.ffdf(paste0(genPath,"creditdata_final_CDH_smp2"), tempPath)
# - Use only Default spells
datCredit<- datCredit_prep[!is.na(DefSpell_Key),]
# remove previous objects from memory
rm(datCredit_prep); gc()
# - Fit an "empty" model as a performance gain, used within some diagnostic functions
modLR_base <- glm(DefSpell_Event ~ 1, data=datCredit, family="binomial")

# - Fit a baseline model for stepwise forward selection
modLR_base2 <- glm(DefSpell_Event ~ log(TimeInDefSpell)*DefSpell_Num_binned, 
                   data=datCredit, family="binomial")
# Insight interactively mined from modelling theme 2a-b was used in fitting this model.




# ------ 2. Embedding the baseline hazard h_0(t)
# Which of the following methods is the best to embed the baseline hazard
# The are done in order from least onerous to most onerous


# --- 2.1 Single time variable (raw duration time)
modLR <- glm(DefSpell_Event ~  TimeInDefSpell,
             data = datCredit, family="binomial")
summary(modLR)
evalLR(modLR, modLR_base, datCredit, targetFld="DefSpell_Event", predClass=1)
### RESULTS: AIC: 51,881; McFadden R^2: 0.47%; AUC: 64.62%


# --- 2.2 Single time variable: Function of raw duration time (log transform)
modLR <- glm(DefSpell_Event ~  log(TimeInDefSpell),
             data = datCredit, family="binomial")
summary(modLR)
evalLR(modLR, modLR_base, datCredit, targetFld="DefSpell_Event", predClass=1)
### Results: AIC: 51,077; McFadden R^2: 2.01%; AUC: 64.62%
# The log transform produces a better goodness of fit with the AUC remaining the
# same


# --- 2.3 Single time variable: Spline
modLR <- glm(DefSpell_Event ~  ns(TimeInDefSpell,df=8),# DF was set by trial and error
             data = datCredit, family="binomial")
summary(modLR)
evalLR(modLR, modLR_base, datCredit, targetFld="DefSpell_Event", predClass=1)
### Results: AIC: 50,548; McFadden R^2: 3,05%; AUC: 65.55%
# Splines seem to improve the results significantly
# With each metric seeming to perform better


# ---2.4 Single time variable: Single time variable binned
table(datCredit$Time_Binned) %>% prop.table()
### RESULTS: Between 1% and 15% of observations in each bin; deemed appropriate, particulalarly in
# as the eralier bins cover a vast majority of the data

modLR <- glm(DefSpell_Event ~  Time_Binned,
             data = datCredit, family="binomial")
summary(modLR)
evalLR(modLR, modLR_base, datCredit, targetFld="DefSpell_Event", predClass=1)
### Results: AIC: 50,704; McFadden R^2: 2,78%; AUC: 65.25%
# Substantially outperforms  2.1 and 2.2
# Binning done so bins have enough data points

### Conclusion: Binning and splines prove to be the best. The baseline hazard has 
# nonlinearities so these allow for this




# ------ 3. Embedding the baseline hazard h_0(t): Further model improvements
# Here the inetraction between TimeInDefSpell and and Defspell_num
# This is done as each spell will have its own baseline and slope


# --- 3.1 Single-factor models: Single time variable (transform)
modLR <- glm(DefSpell_Event ~  log(TimeInDefSpell)*DefSpell_Num_binned,
             data = datCredit, family="binomial")
summary(modLR)
evalLR(modLR, modLR_base, datCredit, targetFld="DefSpell_Event", predClass=1)
### RESULTS: AIC: 50,967; McFadden R^2: 2.23%; AUC: 65.54%
# The inclusion of this interaction improves performance significantly across all 
# of the metrics


# ---3.2 Single-factor models: Binned Variable plus interaction
modLR <- glm(DefSpell_Event ~  Time_Binned + log(TimeInDefSpell):DefSpell_Num_binned,
             data = datCredit, family="binomial")
summary(modLR)
evalLR(modLR, modLR_base, datCredit, targetFld="DefSpell_Event", predClass=1)
### RESULTS: AIC: 50,577; McFadden R^2: 3.04%; AUC: 66.19%
# Binning improves the performance as shown in 2.4


# ---3.3 Single-factor models: Spline plus interaction
modLR <- glm(DefSpell_Event ~  ns(TimeInDefSpell,df=8) + log(TimeInDefSpell):DefSpell_Num_binned,
             data = datCredit, family="binomial")
summary(modLR)
evalLR(modLR, modLR_base, datCredit, targetFld="DefSpell_Event", predClass=1)
### RESULTS: AIC: 50,400; McFadden R^2: 3.34%; AUC: 66.80%
# Splines improve the performance as shown in 2.3




# ------ 4. Delinquency-themed variables


# ------ 4.1 Which time window length is the best in calculating delinquency volatility?

# - Initialize variables to be tested
vars <- c("g0_Delinq_SD_4", "g0_Delinq_SD_5", "g0_Delinq_SD_6", "g0_Delinq_SD_9", "g0_Delinq_SD_12")

# - Single-factor modelling results
# Goodness-of-fit
aicTable(datCredit, vars, TimeDef=c("Cox_Discrete","DefSpell_Event"), genPath=genObjPath, modelType="Cox_Discrete")
# Discriminatory power (in-sample)
concTable(datCredit, datCredit, vars, TimeDef=c("Cox_Discrete","DefSpell_Event"), genPath=genObjPath, modelType="Cox_Discrete")
### RESULTS: Best AIC-results: g0_Delinq_SD_4, g0_Delinq_SD_5, g0_Delinq_SD_6, g0_Delinq_SD_9, g0_Delinq_SD_12
# Best Harrell's C-statistics: g0_Delinq_SD_4, g0_Delinq_SD_5, g0_Delinq_SD_6. Other two below 0.8 reducing discrimination power
# All of them are significant on their own

### Conclusion: The Harell's C-statistic decreases with a linear trend 
# g0_Delinq_SD_4 seems to perform the best but also include g0_Delinq_SD_5 and g0_Delinq_SD_6


#----- 4.2 Which lag order is the best in calculating the portfolio-level fraction of the defaulted proportion with any delinquency?
# indicates the lag order of past data that should be included  when assessing an account
# - Initialize variables to be tested
vars <- c("g0_Delinq_Any_Aggr_Prop", "g0_Delinq_Any_Aggr_Prop_Lag_1", "g0_Delinq_Any_Aggr_Prop_Lag_2",
          "g0_Delinq_Any_Aggr_Prop_Lag_3", "g0_Delinq_Any_Aggr_Prop_Lag_4", "g0_Delinq_Any_Aggr_Prop_Lag_5",
          "g0_Delinq_Any_Aggr_Prop_Lag_6", "g0_Delinq_Any_Aggr_Prop_Lag_9", "g0_Delinq_Any_Aggr_Prop_Lag_12" )

# - Single-factor modelling results
# Goodness-of-fit
aicTable(datCredit, vars, TimeDef=c("Cox_Discrete","DefSpell_Event"), genPath=genObjPath, modelType="Cox_Discrete")
# Discriminatory power (in-sample)
concTable(datCredit, datCredit, vars, TimeDef=c("Cox_Discrete","DefSpell_Event"), genPath=genObjPath, modelType="Cox_Discrete")
### RESULTS: Best AIC-results: g0_Delinq_Any_Aggr_Prop, g0_Delinq_Any_Aggr_Prop_Lag_5, g0_Delinq_Any_Aggr_Prop_Lag_1, g0_Delinq_Any_Aggr_Prop_Lag_9
# , g0_Delinq_Any_Aggr_Prop_Lag_6
# Best C-statistics: g0_Delinq_Any_Aggr_Prop, g0_Delinq_Any_Aggr_Prop_Lag_1, g0_Delinq_Any_Aggr_Prop_Lag_5, g0_Delinq_Any_Aggr_Prop_Lag_3

# Conclusion: The differences in AIC and Harell's C-statiistic is minor
# Choose the overall top 3 performing variables g0_Delinq_Any_Aggr_Prop, g0_Delinq_Any_Aggr_Prop_Lag_5, g0_Delinq_Any_Aggr_Prop_Lag_1
# All are above 50% 


# ------ 4.3 Which lag order is the best in calculating the portfolio-level fraction of defaulted accounts during the default spell?
vars <- c("DefaultStatus1_Aggr_Prop", "DefaultStatus1_Aggr_Prop_Lag_1", "DefaultStatus1_Aggr_Prop_Lag_2",
          "DefaultStatus1_Aggr_Prop_Lag_3", "DefaultStatus1_Aggr_Prop_Lag_4", "DefaultStatus1_Aggr_Prop_Lag_5",
          "DefaultStatus1_Aggr_Prop_Lag_6", "DefaultStatus1_Aggr_Prop_Lag_9", "DefaultStatus1_Aggr_Prop_Lag_12" )

# - Single-factor modelling results
# Goodness-of-fit
aicTable(datCredit, vars, TimeDef=c("Cox_Discrete","DefSpell_Event"), genPath=genObjPath, modelType="Cox_Discrete")
# Discriminatory power (in-sample)
concTable(datCredit, datCredit, vars, TimeDef=c("Cox_Discrete","DefSpell_Event"), genPath=genObjPath, modelType="Cox_Discrete")
### RESULTS: Best AIC-results: DefaultStatus1_Aggr_Prop_Lag_12, DefaultStatus1_Aggr_Prop_Lag_9, DefaultStatus1_Aggr_Prop_Lag_6, 
# This decreases from the highest lag to lowest lag in decreasing order
# Differences are not that major
# Best C-statistics: DefaultStatus1_Aggr_Prop_Lag_12, DefaultStatus1_Aggr_Prop_Lag_9, DefaultStatus1_Aggr_Prop_Lag_6, 
# Similar trend as to AIC

### CONCLUSION: Later is better, though the AIC-differences were minuscule. Concordance-differences were slightly bigger than the AIC values
# Top 3 varibales: DefaultStatus1_Aggr_Prop_Lag_12, DefaultStatus1_Aggr_Prop_Lag_9, DefaultStatus1_Aggr_Prop_Lag_6


# ------ 4.4 How do other portfolio-level delinquency-themed variables fare as single-factor models?

# - Initialize variables to be tested
vars <- c("g0_Delinq_Ave", "ArrearsToBalance_Aggr_Prop", "CuringEvents_Aggr_Prop" )

# - Single-factor modelling results
# Goodness-of-fit
aicTable(datCredit, vars, TimeDef=c("Cox_Discrete","DefSpell_Event"), genPath=genObjPath, modelType="Cox_Discrete")
# Discriminatory power (in-sample)
concTable(datCredit, datCredit, vars, TimeDef=c("Cox_Discrete","DefSpell_Event"), genPath=genObjPath, modelType="Cox_Discrete")
### RESULTS: Best AIC-results: ArrearsToBalance_Aggr_Prop, g0_Delinq_Ave, CuringEvents_Aggr_Prop
# However they aren't that big with their being a 500 difference between th elowest and highest
# Best C-statistics: g0_Delinq_Ave, ArrearsToBalance_Aggr_Prop, CuringEvents_Aggr_Prop
# Similar trend with all being significant here with 0.58 for ArrearsToBalance_Aggr_Prop and 0.52 for 
# CuringEvent_Aggr_prop

### Conclusion: All are significant include all 3 of them, curing event is relevant


# ------ 4.5 How do account-level delinquency-themed variables fare as single-factor models?

# - Initialize variables to be tested
vars <- c("TimeInDefSpell", "g0_Delinq_fac", "g0_Delinq", "g0_Delinq_Lag_1", "slc_acct_arr_dir_3_Change_Ind",
          "g0_Delinq_Num", "slc_acct_arr_dir_3", "slc_acct_roll_ever_24_imputed_mean", "Arrears", "PrevDefaults","TimeInDelinqState"
          , "TimeInDelinqState_Lag_1","slc_past_due_amt_imputed_med", "slc_curing_ind_impute_missing")

# - Single-factor modelling results
# Goodness-of-fit
aicTable(datCredit, vars, TimeDef=c("Cox_Discrete","DefSpell_Event"), genPath=genObjPath, modelType="Cox_Discrete")
# Discriminatory power (in-sample)
concTable(datCredit, datCredit, vars, TimeDef=c("Cox_Discrete","DefSpell_Event"), genPath=genObjPath, modelType="Cox_Discrete")
### RESULTS: Best AIC-results: g0_Delinq_fac, g0_Delinq, slc_past_due_amt_imputed_med, Arrears, slc_curing_ind, slc_acct_dir_3, TimeInDelinqState, slc_acct_arr_dir_3_Change_Ind, PrevDefaults 
# Best C-statistics: g0_Delinq_fac, g0_Delinq, slc_past_due_amt_imputed_med, Arrears, slc_acct_arr_dir_3, slc_curing_ind,  slc_acct_arr_dir_3_Change_Ind
# Do not include g0_Delinq and g0_Delinq_fac as these would lead to unstable models
### CONCLUSION
# Choose: slc_past_due_amt_imputed_med, Arrears, slc_acct_dir_3, slc_curing_ind, TimeInDelinqState
# These variables performed significantly better than the rest with th lowest AIC 
# values and Harrell's c values gretaer than 0.87


# ------ 4.6 Combining insights: delinquency-themed variables

# - Initialize variables to be tested
vars <- c("log(TimeInDefSpell)*DefSpell_Num_binned",
          # Portfolio-level (Delinquency-themed)
          "g0_Delinq_SD_4", "g0_Delinq_SD_5","g0_Delinq_SD_6",
          "g0_Delinq_Any_Aggr_Prop", "g0_Delinq_Any_Aggr_Prop_Lag_1","g0_Delinq_Any_Aggr_Prop_Lag_5",
          "DefaultStatus1_Aggr_Prop_Lag_12", "DefaultStatus1_Aggr_Prop_Lag_9", "DefaultStatus1_Aggr_Prop_Lag_6",
          "g0_Delinq_Ave", "ArrearsToBalance_Aggr_Prop","CuringEvents_Aggr_Prop","slc_acct_arr_dir_3" , "Arrears", "TimeInDelinqState"
          , "slc_past_due_amt_imputed_med" , "slc_curing_ind_impute_missing")



# - Full model | Stepwise forward selection procedure
modLR_full <- glm( as.formula(paste("DefSpell_Event ~", paste(vars, collapse = " + "))),
                   data=datCredit, family="binomial")
summary(modLR_full);
evalLR(modLR_full, modLR_base, datCredit, targetFld="DefSpell_Event", predClass=1)
### RESULTS: AIC:   23,365; McFadden R^2:  55.26%; AUC:  98.65%.

ptm <- proc.time() # for runtime calculations (ignore)
modLR_step <- stepAIC(modLR_base2, scope = list(lower = ~ log(TimeInDefSpell)*DefSpell_Num_binned, 
                                                upper = as.formula(paste("~", paste(vars, collapse = " + ")))), 
                      direction = "both", k=log(datCredit[,.N]), maxit=50)
summary(modLR_step)
evalLR(modLR_step, modLR_base, datCredit, targetFld="DefSpell_Event", predClass=1)
proc.time() - ptm # IGNORE: elapsed runtime; 147m
### RESULTS: AIC:   23,376; McFadden R^2:  55.21%; AUC:  98.64%.
# - Domain expertise
# Remove g0_Delinq_SD_5 and g0_Delinq_SD_6 since g0_Delinq_SD_4 is already present

# - Final variables
vars <- c("log(TimeInDefSpell)*DefSpell_Num_binned", "g0_Delinq_SD_4", "Arrears", 
          "TimeInDelinqState", "DefaultStatus1_Aggr_Prop_Lag_12","slc_acct_arr_dir_3", 
          "slc_curing_ind_impute_missing","g0_Delinq_Any_Aggr_Prop_Lag_1"
          ,"ArrearsToBalance_Aggr_Prop")
modLR <- glm( as.formula(paste("DefSpell_Event ~", paste(vars, collapse = " + "))),
              data=datCredit, family="binomial")
summary(modLR);
evalLR(modLR, modLR_base, datCredit, targetFld="DefSpell_Event", predClass=1)
### RESULTS: AIC:   23,978; McFadden R^2:  54.05%; AUC:  98.56%.




# ------ 5. Other portfolio-level variables


# ------ 5.1 Which lag order is the best in calculating the median interest rate of the portfolio?

# - Initialize variables to be tested
vars <- c("InterestRate_Margin_Aggr_Med", "InterestRate_Margin_Aggr_Med_1", "InterestRate_Margin_Aggr_Med_2",
          "InterestRate_Margin_Aggr_Med_3", "InterestRate_Margin_Aggr_Med_9")

# - Single-factor modelling results
# Goodness-of-fit
aicTable(datCredit, vars, TimeDef=c("Cox_Discrete","DefSpell_Event"), genPath=genObjPath, modelType="Cox_Discrete")
# Discriminatory power (in-sample)
concTable(datCredit, datCredit, vars, TimeDef=c("Cox_Discrete","DefSpell_Event"), genPath=genObjPath, modelType="Cox_Discrete")
### RESULTS: Best AIC-results: InterestRate_Margin_Aggr_Med_9 , InterestRate_Margin_Aggr_Med_1, InterestRate_Margin_Aggr_Med,
# InterestRate_Margin_Aggr_Med_3. InterestRate_Margin_Aggr_Med_2
# Best C-statistics: InterestRate_Margin_Aggr_Med_9 , InterestRate_Margin_Aggr_Med_3, InterestRate_Margin_Aggr_Med_1,
# InterestRate_Margin_Aggr_Med_2, InterestRate_Margin_Aggr_Med.

### Conclusion
#  Lag 9 performed the best over the two-metrics with lag 1 performing 2nd best
# However the difference in AIC and Harell's c is minor


# ------ 5.2 How do other portfolio-level (non-delinquency) variables fare as single-factor models?

# - Initialize variables to be tested
vars <- c("InstalmentToBalance_Aggr_Prop", "AgeToTerm_Aggr_Mean", "DefSpell_Maturity_Aggr_Mean",
          , "NewLoans_Aggr_Prop")

# - Single-factor modelling results
# Goodness-of-fit
aicTable(datCredit, vars, TimeDef=c("Cox_Discrete","DefSpell_Event"), genPath=genObjPath, modelType="Cox_Discrete")
# Discriminatory power (in-sample)
concTable(datCredit, datCredit, vars, TimeDef=c("Cox_Discrete","DefSpell_Event"), genPath=genObjPath, modelType="Cox_Discrete")
### RESULTS: Best AIC-results: InstalmentToBalance_Aggr_Prop, NewLoans_Aggr_Prop,AgeToTerm_Aggr_Mean,  DefSpell_Maturity_Aggr_Mean
# Best C-statistics: InstalmentToBalance_Aggr_Prop, NewLoans_Aggr_Prop,AgeToTerm_Aggr_Mean,  DefSpell_Maturity_Aggr_Mean

### CONCLUSION: Choose AgeToTerm_Aggr_Mean + InstalmentToBalance_Aggr_Prop + NewLoans_Aggr_Prop


# ------ 5.3 Combining insights: Delinquency-themed and portfolio-level variables

# - Initialize variables to be tested
vars <- c("log(TimeInDefSpell)*DefSpell_Num_binned", "g0_Delinq_SD_4", "Arrears", 
          "TimeInDelinqState", "DefaultStatus1_Aggr_Prop_Lag_12","slc_acct_arr_dir_3", 
          "slc_curing_ind_impute_missing","g0_Delinq_Any_Aggr_Prop_Lag_1"
          ,"ArrearsToBalance_Aggr_Prop","InterestRate_Margin_Aggr_Med_9", "InterestRate_Margin_Aggr_Med_1",
          "AgeToTerm_Aggr_Mean", "InstalmentToBalance_Aggr_Prop", "NewLoans_Aggr_Prop")

# - Full model | Stepwise forward selection procedure
modLR_full <- glm( as.formula(paste("DefSpell_Event ~", paste(vars, collapse = " + "))),
                   data=datCredit, family="binomial")
summary(modLR_full);
evalLR(modLR_full, modLR_base, datCredit, targetFld="DefSpell_Event", predClass=1)
### RESULTS: AIC:   23,964; McFadden R^2:  54.1%; AUC:  98.56%.

# - Stepwise forward selection using BIC
ptm <- proc.time() # for runtime calculations (ignore)
modLR_step <- stepAIC(modLR_base2, scope = list(lower = ~ log(TimeInDefSpell)*DefSpell_Num_binned, 
                                                upper = as.formula(paste("~", paste(vars, collapse = " + ")))), 
                      direction = "both", k=log(datCredit[,.N]), maxit=50)
summary(modLR_step)
evalLR(modLR_step, modLR_base, datCredit, targetFld="DefSpell_Event", predClass=1)
proc.time() - ptm # IGNORE: elapsed runtime; 73m
### RESULTS: AIC:   23,958; McFadden R^2:  54,09%; AUC:  98.56%.

# Final variables
vars <- c("log(TimeInDefSpell)*DefSpell_Num_binned", "g0_Delinq_SD_4", "Arrears"
          ,"TimeInDelinqState", "DefaultStatus1_Aggr_Prop_Lag_12","slc_acct_arr_dir_3", 
          "slc_curing_ind_impute_missing","g0_Delinq_Any_Aggr_Prop_Lag_1"
          ,"ArrearsToBalance_Aggr_Prop"#"InterestRate_Margin_Aggr_Med_9", "InterestRate_Margin_Aggr_Med_1", "AgeToTerm_Aggr_Mean"
          , "InstalmentToBalance_Aggr_Prop")#, "NewLoans_Aggr_Prop")

modLR <- glm( as.formula(paste("DefSpell_Event ~", paste(vars, collapse = " + "))),
              data=datCredit, family="binomial")
summary(modLR);
evalLR(modLR, modLR_base, datCredit, targetFld="DefSpell_Event", predClass=1)
### RESULTS: AIC:   23,958; McFadden R^2:  54,09%; AUC:  98.56%.




# ------ 7. Macroeconomic variables


# ------ 7.1 Which lag order is the best for: M_Repo_Rate

# - Initialize variables to be tested
vars <- c("M_Repo_Rate", "M_Repo_Rate_1 ", "M_Repo_Rate_2", "M_Repo_Rate_3", "M_Repo_Rate_6", "M_Repo_Rate_9", "M_Repo_Rate_12")

# - Single-factor modelling results
# Goodness-of-fit
aicTable(datCredit, vars, TimeDef=c("Cox_Discrete","DefSpell_Event"), genPath=genObjPath, modelType="Cox_Discrete")
# Discriminatory power (in-sample)
concTable(datCredit, datCredit, vars, TimeDef=c("Cox_Discrete","DefSpell_Event"), genPath=genObjPath, modelType="Cox_Discrete")
### RESULTS: Best AIC-results: M_Repo_Rate, M_Repo_Rate_2, M_Repo_Rate_1, M_Repo_Rate_12, 
# M_Repo_Rate_3, M_Repo_Rate_9, M_Repo_Rate_6.
# Best C-statistics: M_Repo_Rate, M_Repo_Rate_12, M_Repo_Rate_1, M_Repo_Rate_2, 
# M_Repo_Rate_3, M_Repo_Rate_9, M_Repo_Rate_6.

### CONCLUSION
# There are minor difference between AIC value which suggest the main repo rate
# and lags 1 and 12 are best. C-differences are small toe (ranging from 50%-52%).
# Choose: M_Repo_Rate, M_Repo_Rate_1, M_Repo_Rate_12


# ------ 7.2 Which lag order is the best for: M_Inflation_Growth

# - Initialize variables to be tested
vars <- c("M_Inflation_Growth", "M_Inflation_Growth_1 ", "M_Inflation_Growth_2", "M_Inflation_Growth_3", 
          "M_Inflation_Growth_6", "M_Inflation_Growth_9", "M_Inflation_Growth_12")

# - Single-factor modelling results
# Goodness-of-fit
aicTable(datCredit, vars, TimeDef=c("Cox_Discrete","DefSpell_Event"), genPath=genObjPath, modelType="Cox_Discrete")
# Discriminatory power (in-sample)
concTable(datCredit, datCredit, vars, TimeDef=c("Cox_Discrete","DefSpell_Event"), genPath=genObjPath, modelType="Cox_Discrete")
### RESULTS: Best AIC-results: M_RealIncome_Growth_12, M_RealIncome_Growth, M_RealIncome_Growth_1, M_RealIncome_Growth_2, 
# M_RealIncome_Growth_3,M_RealIncome_Growth_9, M_RealIncome_Growth_6 .
# Best C-statistics: MM_RealIncome_Growth, M_RealIncome_Growth_1, M_RealIncome_Growth_2, M_RealIncome_Growth_12, 
# M_RealIncome_Growth_3,M_RealIncome_Growth_9, M_RealIncome_Growth_6 .

### CONCLUSION: Small AIC and Harell's c-statsitics differences
# Best variables: M_RealIncome_Growth_12, M_RealIncome_Growth, M_RealIncome_Growth_1


# ------ 7.3 Which lag order is the best for: M_RealGDP_Growth

# - Initialize variables to be tested
vars <- c("M_RealGDP_Growth", "M_RealGDP_Growth_1 ", "M_RealGDP_Growth_2", "M_RealGDP_Growth_3", 
          "M_RealGDP_Growth_6", "M_RealGDP_Growth_9", "M_RealGDP_Growth_12")

# - Single-factor modelling results
# Goodness-of-fit
aicTable(datCredit, vars, TimeDef=c("Cox_Discrete","DefSpell_Event"), genPath=genObjPath, modelType="Cox_Discrete")
# Discriminatory power (in-sample)
concTable(datCredit, datCredit, vars, TimeDef=c("Cox_Discrete","DefSpell_Event"), genPath=genObjPath, modelType="Cox_Discrete")
### RESULTS: Best AIC-results: M_RealGDP_Growth_12, M_RealGDP_Growth_3, M_RealGDP_Growth_2, M_RealGDP_Growth_9, 
# M_RealGDP_Growth_1, M_RealGDP_Growth_6, M_RealGDP_Growth.
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
aicTable(datCredit, vars, TimeDef=c("Cox_Discrete","DefSpell_Event"), genPath=genObjPath, modelType="Cox_Discrete")
# Discriminatory power (in-sample)
concTable(datCredit, datCredit, vars, TimeDef=c("Cox_Discrete","DefSpell_Event"), genPath=genObjPath, modelType="Cox_Discrete")
### RESULTS: Best AIC-results: M_RealIncome_Growth_6, M_RealIncome_Growth_3, M_RealIncome_Growth_2, M_RealIncome_Growth_9, 
# M_RealIncome_Growth_1, M_RealIncome_Growth_12, M_RealIncome_Growth.
# Best C-statistics: M_RealIncome_Growth_6, M_RealIncome_Growth_3, M_RealIncome_Growth_2, M_RealIncome_Growth_9, 
# M_RealIncome_Growth_1, M_RealIncome_Growth, M_RealIncome_Growth_12.

### CONCLUSION: Middle lags seem better, based on very small AIC-differences and c-differences (50%-51%)
# # Choose:  M_RealIncome_Growth_6, M_RealIncome_Growth_3, M_RealIncome_Growth_2


# ------ 7.5 Which lag order is the best for: M_DTI_Growth

# - Initialize variables to be tested
vars <- c("M_DTI_Growth", "M_DTI_Growth_1 ", "M_DTI_Growth_2", "M_DTI_Growth_3", 
          "M_DTI_Growth_6", "M_DTI_Growth_9", "M_DTI_Growth_12")

# - Single-factor modelling results
# Goodness-of-fit
aicTable(datCredit, vars, TimeDef=c("Cox_Discrete","DefSpell_Event"), genPath=genObjPath, modelType="Cox_Discrete")
# Discriminatory power (in-sample)
concTable(datCredit, datCredit, vars, TimeDef=c("Cox_Discrete","DefSpell_Event"), genPath=genObjPath, modelType="Cox_Discrete")
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
aicTable(datCredit, vars, TimeDef=c("Cox_Discrete","DefSpell_Event"), genPath=genObjPath, modelType="Cox_Discrete")
# Discriminatory power (in-sample)
concTable(datCredit, datCredit, vars, TimeDef=c("Cox_Discrete","DefSpell_Event"), genPath=genObjPath, modelType="Cox_Discrete")
### RESULTS: Best AIC-results: M_Emp_Growth_6, M_Emp_Growth_9, M_Emp_Growth_3, M_Emp_Growth_12
# M_Emp_Growth_2, M_Emp_Growth, M_Emp_Growth_1
# Best C-statistics: M_Emp_Growth_6, M_Emp_Growth_9, M_Emp_Growth_3, M_Emp_Growth_12
# M_Emp_Growth, M_Emp_Growth_2, M_Emp_Growth_1

### CONCLUSION: Middle lags seem better, based on very small AIC-differnces, affirmed by the c-differences (50-51%)


# ------ 7.7 Combining insights: Macroeconomic variables

# - Initialize variables to be tested
vars <- c("log(TimeInDefSpell)*DefSpell_Num_binned",
          "M_Repo_Rate", "M_Repo_Rate_1", "M_Repo_Rate_12", 
         "M_RealIncome_Growth_12", "M_RealIncome_Growth", "M_RealIncome_Growth_1",
          "M_RealGDP_Growth_2", "M_RealGDP_Growth_3", "M_RealGDP_Growth_6",
          "M_RealIncome_Growth_2", "M_RealIncome_Growth_3", "M_RealIncome_Growth_6",
          "M_DTI_Growth_9", "M_DTI_Growth_12", "M_DTI_Growth_6", 
          "M_Emp_Growth_3", "M_Emp_Growth_9", "M_Emp_Growth_6")
datCredit
# - Full model | Stepwise forward selection procedure
modLR_full <- glm( as.formula(paste("DefSpell_Event ~", paste(vars, collapse = " + "))),
                   data=datCredit, family="binomial")
summary(modLR_full);
evalLR(modLR_full, modLR_base, datCredit, targetFld="DefSpell_Event", predClass=1)
### RESULTS: AIC:  176,311; McFadden R^2:  0.94%; AUC:  58.88%.






