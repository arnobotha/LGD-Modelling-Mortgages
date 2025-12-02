# ======================================= INPUT SPACE: DISCRETE COX ADVANCED============================
# Partition data into thematic groups and perform data analysis on them to compile an input space for 
# and advanced discrete-time hazard model.
# ------------------------------------------------------------------------------------------------------
# PROJECT TITLE: Loss Modelling (LGD) for FNB Mortgages
# SCRIPT AUTHOR(S): Dr Arno Botha (AB), Mohammed Gabru (MG), Marcel Muller (MM)
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

# --- 1.1 Load and subset data
# - Confirm prepared datasets are loaded into memory
# Training
if (!exists('datCredit_train_CDH')) unpack.ffdf(paste0(genPath,"creditdata_train_CDH"), tempPath);gc()
# Validation
if (!exists('datCredit_valid_CDH')) unpack.ffdf(paste0(genPath,"creditdata_valid_CDH"), tempPath);gc()

# - Use only default spells
datCredit_train <- datCredit_train_CDH[!is.na(DefSpell_Key),]
datCredit_valid <- datCredit_valid_CDH[!is.na(DefSpell_Key),]

# - Remove previous objects from memory
rm(datCredit_train_CDH, datCredit_valid_CDH); gc()


# --- 1.2 Fit prelimenary models
# - Fit an "empty" model as a performance gain, used within some diagnostic functions
modLR_base <- glm(DefSpell_Event ~ 1, data=datCredit_train, family="binomial")

# - Fit a baseline model for stepwise forward selection
modLR_base2 <- glm(DefSpell_Event ~ log(TimeInDefSpell)*DefSpell_Num_binned, 
                   data=datCredit_train, family="binomial")
### NOTE: Insight interactively mined from modelling theme 2a-b was used in fitting this model.




# ------ 2. Embedding the baseline hazard h_0(t) | Part I
# Which of the following methods is the best to embed the baseline hazard
# The are done in order from least onerous to most onerous

# --- 2.1 Single time variable (raw duration time)
# - Fit model
modLR <- glm(DefSpell_Event ~  TimeInDefSpell,
             data = datCredit_train, family="binomial")

# - Evaluate model
summary(modLR)
evalLR(modLR, modLR_base, datCredit_train, targetFld="DefSpell_Event", predClass=1)
### RESULTS: AIC: 220 783; McFadden R^2: 0.44%; AUC: 65.76%


# --- 2.2 Single time variable: Function of raw duration time (log transform)
# - Fit model
modLR <- glm(DefSpell_Event ~  log(TimeInDefSpell),
             data = datCredit_train, family="binomial")

# - Evaluate model
summary(modLR)
evalLR(modLR, modLR_base, datCredit_train, targetFld="DefSpell_Event", predClass=1)
### Results: AIC: 216 493; McFadden R^2: 2.38%; AUC: 65.76%
### CONCLUSION: The log transform produces a better goodness of fit with the AUC remaining the
###             same compared to the model in 2.1


# --- 2.3 Single time variable: Spline
# - Fit model
modLR <- glm(DefSpell_Event ~  ns(TimeInDefSpell,df=8),# DF was set by trial and error
             data = datCredit_train, family="binomial")

# - Evaluate model
summary(modLR)
evalLR(modLR, modLR_base, datCredit_train, targetFld="DefSpell_Event", predClass=1)
### Results: AIC: 213 908; McFadden R^2: 3.55%; AUC: 66.64%
### CONCLUSION: Splines seem to improve the model fit and accuracy substantially


# --- 2.4 Single time variable: Single time variable binned
# - Analyse data to inform on model inputs
table(datCredit_train$Time_Binned) %>% prop.table()
### RESULTS: Between 1% and 15% of observations in each bin; deemed appropriate, particularly in
###          the lower bins that cover the majority of the data

# - Fit model
modLR <- glm(DefSpell_Event ~  Time_Binned,
             data = datCredit_train, family="binomial")

# - Evaluate model
summary(modLR)
evalLR(modLR, modLR_base, datCredit_train, targetFld="DefSpell_Event", predClass=1)
### Results: AIC: 214 314; McFadden R^2: 3.38%; AUC: 66.56%
### CONCLUSION: This model substantially outperforms against the model in 2.1 and 2.2, but
###             underperforms compared to the model in 2.3

### OVERALL Conclusion: Binning and splines prove to be the best. The baseline hazard has 
###                     non-linearities, hence the stronger performance by the time-binned model




# ------ 3. Embedding the baseline hazard h_0(t) | Part II
### NOTE: Here an interaction between [TimeInDefSpell] and [Defspell_num] is enforced
###       This is done since each spell will have its own baseline and slope

# --- 3.1 Single-factor models: Single time variable (transform)
# - Fit model
modLR <- glm(DefSpell_Event ~  log(TimeInDefSpell)*DefSpell_Num_binned,
             data = datCredit_train, family="binomial")

# - Evaluate model
summary(modLR)
evalLR(modLR, modLR_base, datCredit_train, targetFld="DefSpell_Event", predClass=1)
### RESULTS: AIC: 216 078; McFadden R^2: 2.57%; AUC: 66.24%
### CONCLUSION: The inclusion of this interaction improves performance substantially across all 
###             of the metrics compared to the log-transformed model in 2.2


# --- 3.2 Single-factor models: Binned Variable plus interaction
# - Fit model
modLR <- glm(DefSpell_Event ~ Time_Binned + log(TimeInDefSpell):DefSpell_Num_binned,
             data = datCredit_train, family="binomial")

# - Evaluate model
summary(modLR)
evalLR(modLR, modLR_base, datCredit_train, targetFld="DefSpell_Event", predClass=1)
### RESULTS: AIC: 213 963; McFadden R^2: 3.53%; AUC: 67.14%
### CONCLUSION: Binning improves the performance, as previously shown in 2.4


# --- 3.3 Single-factor models: Spline plus interaction
modLR <- glm(DefSpell_Event ~  ns(TimeInDefSpell,df=8) + log(TimeInDefSpell):DefSpell_Num_binned,
             data = datCredit_train, family="binomial")
summary(modLR)
evalLR(modLR, modLR_base, datCredit_train, targetFld="DefSpell_Event", predClass=1)
### RESULTS: AIC: 213 488; McFadden R^2: 3.74%; AUC: 67.52%
### CONCLUSION: Splines improve the performance as shown in 2.3




# ------ 4. Delinquency-themed variables

# --- 4.1 Selection of the best version of delinquency-volatility variables
### NOTE: These variables are calcualted using various time time window lengths.
###       E.g., [g0_Delinq_SD_4] is the volatility of a loan's delinquency on a 4-month
###       rolling window

# - Initialize variables to be tested
vars <- c("g0_Delinq_SD_4", "g0_Delinq_SD_5", "g0_Delinq_SD_6", "g0_Delinq_SD_9", "g0_Delinq_SD_12")

# - Single-factor modelling results
# Goodness-of-fit
aicTable(datCredit_train, vars, TimeDef=c("Cox_Discrete","DefSpell_Event"), genPath=genObjPath, modelType="Cox_Discrete")
# Discriminatory power (in-sample)
concTable(datCredit_train, datCredit_valid, vars, TimeDef=c("Cox_Discrete","DefSpell_Event"), genPath=genObjPath, modelType="Cox_Discrete")
### RESULTS: Best AIC-results: [g0_Delinq_SD_4]; [g0_Delinq_SD_5]; [g0_Delinq_SD_6]
###          Best Harrell's C-statistics: [g0_Delinq_SD_4]; [g0_Delinq_SD_5]; [g0_Delinq_SD_6]
###          All of the statistics are significant

### CONCLUSION: The model's AIC decreases with shorter rolling windows on which an account's delinquency volatility is calculated
###             In contrast, Harell's C-statistic decreases with larger rolling windows
###             Choose the overall top 3 performing variables:
###               [g0_Delinq_SD_4]; [g0_Delinq_SD_5]; [g0_Delinq_SD_6]


# --- 4.2 Selection of the best version of portfolio-level delinquency variables
### NOTE: These variables are calculated on a portfolio-level
###       E.g., [g0_Delinq_Any_Aggr_Prop_Lag_1] is the proportion of accounts that are at
###       least one month in arrears one month ago

# - Initialize variables to be tested
vars <- c("g0_Delinq_Any_Aggr_Prop", "g0_Delinq_Any_Aggr_Prop_Lag_1", "g0_Delinq_Any_Aggr_Prop_Lag_2",
          "g0_Delinq_Any_Aggr_Prop_Lag_3", "g0_Delinq_Any_Aggr_Prop_Lag_4", "g0_Delinq_Any_Aggr_Prop_Lag_5",
          "g0_Delinq_Any_Aggr_Prop_Lag_6", "g0_Delinq_Any_Aggr_Prop_Lag_9", "g0_Delinq_Any_Aggr_Prop_Lag_12" )

# - Single-factor modelling results
# Goodness-of-fit
aicTable(datCredit_train, vars, TimeDef=c("Cox_Discrete","DefSpell_Event"), genPath=genObjPath, modelType="Cox_Discrete")
# Discriminatory power (in-sample)
concTable(datCredit_train, datCredit_valid, vars, TimeDef=c("Cox_Discrete","DefSpell_Event"), genPath=genObjPath, modelType="Cox_Discrete")
### RESULTS: Best AIC-results: [g0_Delinq_Any_Aggr_Prop]; [g0_Delinq_Any_Aggr_Prop_Lag_1]; [g0_Delinq_Any_Aggr_Prop_Lag_12]
###          Best C-statistics: [g0_Delinq_Any_Aggr_Prop]; [g0_Delinq_Any_Aggr_Prop_Lag_12]; [g0_Delinq_Any_Aggr_Prop_Lag_1]
###          All statistics are significant

### CONCLUSION: The differences in AIC and Harell's C-statistic are minor
###             Select top 3 variables:
###               [g0_Delinq_Any_Aggr_Prop]; [g0_Delinq_Any_Aggr_Prop_Lag_12]; and [g0_Delinq_Any_Aggr_Prop_Lag_1]


# --- 4.3 Selection of the best version of portfolio-level default variables
### NOTE: These variables are calculated on a portfolio-level
###       E.g., [DefaultStatus1_Aggr_Prop_Lag_1] is the proportion of accounts that are at
###       least one month in arrears one month ago
vars <- c("DefaultStatus1_Aggr_Prop", "DefaultStatus1_Aggr_Prop_Lag_1", "DefaultStatus1_Aggr_Prop_Lag_2",
          "DefaultStatus1_Aggr_Prop_Lag_3", "DefaultStatus1_Aggr_Prop_Lag_4", "DefaultStatus1_Aggr_Prop_Lag_5",
          "DefaultStatus1_Aggr_Prop_Lag_6", "DefaultStatus1_Aggr_Prop_Lag_9", "DefaultStatus1_Aggr_Prop_Lag_12" )

# - Single-factor modelling results
# Goodness-of-fit
aicTable(datCredit_train, vars, TimeDef=c("Cox_Discrete","DefSpell_Event"), genPath=genObjPath, modelType="Cox_Discrete")
# Discriminatory power (in-sample)
concTable(datCredit_train, datCredit_valid, vars, TimeDef=c("Cox_Discrete","DefSpell_Event"), genPath=genObjPath, modelType="Cox_Discrete")
### RESULTS: Best AIC-results: [DefaultStatus1_Aggr_Prop_Lag_12]; [DefaultStatus1_Aggr_Prop_Lag_9]; [DefaultStatus1_Aggr_Prop_Lag_6]
###          This decreases from the highest lag to lowest lag in decreasing order and the differences are minuscule
###          Best C-statistics: [DefaultStatus1_Aggr_Prop_Lag_12]; [DefaultStatus1_Aggr_Prop_Lag_9]; [DefaultStatus1_Aggr_Prop_Lag_6]
###          Similar trend as to AIC

### CONCLUSION: Longer lags are better, though the AIC-differences were minuscule. 
###             Concordance-differences were slightly bigger than the AIC values
###             Select top 3 variables:
###               [DefaultStatus1_Aggr_Prop_Lag_12]; [DefaultStatus1_Aggr_Prop_Lag_9]; and [DefaultStatus1_Aggr_Prop_Lag_6]


# --- 4.4 Selection of other portfolio-level delinquency-themed variables as single-factor models

# - Initialize variables to be tested
vars <- c("g0_Delinq_Ave", "ArrearsToBalance_Aggr_Prop", "CuringEvents_Aggr_Prop")

# - Single-factor modelling results
# Goodness-of-fit
aicTable(datCredit_train, vars, TimeDef=c("Cox_Discrete","DefSpell_Event"), genPath=genObjPath, modelType="Cox_Discrete")
# Discriminatory power (in-sample)
concTable(datCredit_train, datCredit_valid, vars, TimeDef=c("Cox_Discrete","DefSpell_Event"), genPath=genObjPath, modelType="Cox_Discrete")
### RESULTS: Best AIC-results: [g0_Delinq_Ave], [ArrearsToBalance_Aggr_Prop], [CuringEvents_Aggr_Prop]
###          Differences in AICs are small, no more than ~1 500
###          Best C-statistics: [g0_Delinq_Ave], [ArrearsToBalance_Aggr_Prop], [CuringEvents_Aggr_Prop]
###          All statistics are significant

### Conclusion: Include all 3 of the variables


# --- 4.5 Selection of account-level delinquency-themed variables as single-factor models
# - Initialize variables to be tested
vars <- c("g0_Delinq_Lag_1", "slc_acct_arr_dir_3_Change_Ind",
          "g0_Delinq_Num", "slc_acct_arr_dir_3", "slc_acct_roll_ever_24_imputed_mean", "Arrears", "PrevDefaults","TimeInDelinqState",
          "slc_past_due_amt_imputed_med", "slc_curing_ind")

# - Single-factor modelling results
# Goodness-of-fit
aicTable(datCredit_train, vars, TimeDef=c("Cox_Discrete","DefSpell_Event"), genPath=genObjPath, modelType="Cox_Discrete")
# Discriminatory power (in-sample)
concTable(datCredit_train, datCredit_valid, vars, TimeDef=c("Cox_Discrete","DefSpell_Event"), genPath=genObjPath, modelType="Cox_Discrete")
### RESULTS: Best AIC-results: [Arrears]; [slc_past_due_amt_imputed_med]; [slc_curing_ind]; [slc_acct_arr_dir_3]; [TimeInDelinqState]; [slc_acct_arr_dir_3_Change_Ind]
###          Best C-statistics: [Arrears]; [slc_past_due_amt_imputed_med]; [TimeInDelinqState]; [slc_curing_ind]; [slc_acct_arr_dir_3]; [slc_acct_arr_dir_3_Change_Ind]
### CONCLUSION: Select top 3 variables (preference given to Concordance statistics):
###              [Arrears]; [slc_past_due_amt_imputed_med]; [slc_curing_ind]; [slc_acct_arr_dir_3]; [slc_acct_arr_dir_3_Change_Ind]
### NOTE: We exclude [TimeInDelinqState] as it is known to be highly correlated with [TimeInDefSpell]


# --- 4.6 Combining insights: delinquency-themed variables
### NOTE: Including [g0_Delinq_SD_4], [g0_Delinq_SD_5], [g0_Delinq_SD_6] with the full model space causes 
###       quasi-complete separation; excluding them from this exercise.
###       [Arrears] and [slc_acct_arr_dir_3_Change_Ind] also causes quasi-complete separation, excluding it from this exercise

# - Initialize variables to be tested
vars <- c("log(TimeInDefSpell)*DefSpell_Num_binned", 
          "slc_past_due_amt_imputed_med", "slc_curing_ind","slc_acct_arr_dir_3",
          "g0_Delinq_Ave", "ArrearsToBalance_Aggr_Prop_adj_WOff", "CuringEvents_Aggr_Prop",
          "DefaultStatus1_Aggr_Prop_Lag_6", "DefaultStatus1_Aggr_Prop_Lag_9", "DefaultStatus1_Aggr_Prop_Lag_12",
          "g0_Delinq_Any_Aggr_Prop", "g0_Delinq_Any_Aggr_Prop_Lag_1", "g0_Delinq_Any_Aggr_Prop_Lag_12")

# - Full model | Stepwise forward selection procedure
modLR_full <- glm( as.formula(paste("DefSpell_Event ~", paste(vars, collapse = " + "))),
                   data=datCredit_train, family="binomial")

# - Evaluate model
summary(modLR_full);
evalLR(modLR_full, modLR_base, datCredit_train, targetFld="DefSpell_Event", predClass=1)
### RESULTS: AIC: 121 336; McFadden R^2:  45.30%; AUC:  97.07%.

# - Run a step-wise selection
### NOTE [Arrears] and [TimeInDelinqState] excluded as they caused model non-convergence
ptm <- proc.time() # for runtime calculations (ignore)
modLR_step <- stepAIC(modLR_base2, scope = list(lower = ~ log(TimeInDefSpell)*DefSpell_Num_binned, 
                                                upper = as.formula(paste("~", paste(vars, collapse = " + ")))), 
                      direction = "both", k=log(datCredit_train[,.N]), maxit=50)
summary(modLR_step)
evalLR(modLR_step, modLR_base, datCredit_train, targetFld="DefSpell_Event", predClass=1)
proc.time() - ptm # IGNORE: elapsed runtime; ~51 minutes
### RESULTS: AIC:   130 202; McFadden R^2:  41.30%; AUC:  96.29%.
###          Variables from step-wise selection procedure:
###             [slc_past_due_amt_imputed_med]; [slc_acct_arr_dir_3]; slc_curing_ind];
###             [DefaultStatus1_Aggr_Prop_Lag_12]; [g0_Delinq_Any_Aggr_Prop_Lag_1];
###             [g0_Delinq_Ave]; [ArrearsToBalance_Aggr_Prop_adj_WOff]; g0_Delinq_Any_Aggr_Prop_Lag_12]

### CONCLUSION: Use the selection from the automated variable selection




# ------ 5. Other portfolio-level variables

# --- 5.1 Median portfolio-level interest rates

# - Initialize variables to be tested
vars <- c("InterestRate_Margin_Aggr_Med", "InterestRate_Margin_Aggr_Med_1", "InterestRate_Margin_Aggr_Med_2",
          "InterestRate_Margin_Aggr_Med_3", "InterestRate_Margin_Aggr_Med_9")

# - Single-factor modelling results
# Goodness-of-fit
aicTable(datCredit_train, vars, TimeDef=c("Cox_Discrete","DefSpell_Event"), genPath=genObjPath, modelType="Cox_Discrete")
### RESULTS: Best AIC-results: [InterestRate_Margin_Aggr_Med_9]; [InterestRate_Margin_Aggr_Med_3]; [InterestRate_Margin_Aggr_Med_2]
# Discriminatory power (in-sample)
concTable(datCredit_train, datCredit_valid, vars, TimeDef=c("Cox_Discrete","DefSpell_Event"), genPath=genObjPath, modelType="Cox_Discrete")
### RESULTS: Best C-statistics: [InterestRate_Margin_Aggr_Med_9]; [InterestRate_Margin_Aggr_Med_3]; [InterestRate_Margin_Aggr_Med_2]

### CONCLUSION: Select top 3 variables:
###               [InterestRate_Margin_Aggr_Med_9]; [InterestRate_Margin_Aggr_Med_3]; [InterestRate_Margin_Aggr_Med_2]


# --- 5.2 Other portfolio-level (non-delinquency) variables

# - Initialise variables to be tested
vars <- c("InstalmentToBalance_Aggr_Prop_adj_WOff", "AgeToTerm_Aggr_Mean", "DefSpell_Maturity_Aggr_Mean", "NewLoans_Aggr_Prop")

# - Single-factor modelling results
# Goodness-of-fit
aicTable(datCredit_train, vars, TimeDef=c("Cox_Discrete","DefSpell_Event"), genPath=genObjPath, modelType="Cox_Discrete")
### RESULTS: Best AIC-results: [InstalmentToBalance_Aggr_Prop_adj_WOff]; [NewLoans_Aggr_Prop]; [AgeToTerm_Aggr_Mean];  [DefSpell_Maturity_Aggr_Mean]
# Discriminatory power (in-sample)
concTable(datCredit_train, datCredit_valid, vars, TimeDef=c("Cox_Discrete","DefSpell_Event"), genPath=genObjPath, modelType="Cox_Discrete")
### RESULTS: Best C-statistics: [InstalmentToBalance_Aggr_Prop_adj_WOff]; [NewLoans_Aggr_Prop]; [AgeToTerm_Aggr_Mean];  [DefSpell_Maturity_Aggr_Mean]

### CONCLUSION: Select top three:
###               [InstalmentToBalance_Aggr_Prop_adj_WOff]; [NewLoans_Aggr_Prop]; [AgeToTerm_Aggr_Mean]


# --- 5.3 Combining insights: Account- and portfolio-level, delinquency themed, variables

# - Initialize variables to be tested
vars <- c("log(TimeInDefSpell)*DefSpell_Num_binned",
          "slc_past_due_amt_imputed_med", "slc_acct_arr_dir_3", "slc_curing_ind",
          "DefaultStatus1_Aggr_Prop_Lag_12", "g0_Delinq_Any_Aggr_Prop_Lag_1",
          "g0_Delinq_Ave", "ArrearsToBalance_Aggr_Prop_adj_WOff", "g0_Delinq_Any_Aggr_Prop_Lag_12",
          "InterestRate_Margin_Aggr_Med_2", "InterestRate_Margin_Aggr_Med_3",
          "InterestRate_Margin_Aggr_Med_9", "AgeToTerm_Aggr_Mean",
          "InstalmentToBalance_Aggr_Prop_adj_WOff", "NewLoans_Aggr_Prop")
# - Full model | Stepwise forward selection procedure
modLR_full <- glm( as.formula(paste("DefSpell_Event ~", paste(vars, collapse = " + "))),
                   data=datCredit_train, family="binomial")
summary(modLR_full);
evalLR(modLR_full, modLR_base, datCredit_train, targetFld="DefSpell_Event", predClass=1)
### RESULTS: AIC: 130 053; McFadden R^2:  41.37%; AUC: 96.31%

# - Stepwise forward selection using AIC
ptm <- proc.time() # for runtime calculations (ignore)
modLR_step <- stepAIC(modLR_base2, scope = list(lower = ~ log(TimeInDefSpell)*DefSpell_Num_binned, 
                                                upper = as.formula(paste("~", paste(vars, collapse = " + ")))), 
                      direction = "both", k=log(datCredit_train[,.N]), maxit=50)
summary(modLR_step)
evalLR(modLR_step, modLR_base, datCredit_train, targetFld="DefSpell_Event", predClass=1)
proc.time() - ptm # IGNORE: elapsed runtime; ~7.5 minutes
### RESULTS: AIC: 154 320; McFadden R^2: 30.42%; AUC: 92.85%
###          Variables from step-wise selection procedure:
###             [slc_past_due_amt_imputed_med]; [slc_acct_arr_dir_3]; slc_curing_ind];
###             [DefaultStatus1_Aggr_Prop_Lag_12]; [g0_Delinq_Any_Aggr_Prop_Lag_1];
###             [g0_Delinq_Ave]; [ArrearsToBalance_Aggr_Prop_adj_WOff]; g0_Delinq_Any_Aggr_Prop_Lag_12]

### CONCLUSION: Use the selection from the automated variable selection




# ------ 6. Account-level variables

# --- 6.1 How do various non-delinquency account-level variables fare as single-factor models?
vars <- c("Principal_Real", "Principal", "InterestRate_Margin", 
          "Balance_Real_1", "Balance_1", "Instalment_Real", "InterestRate_Nom", "AgeToTerm",
          "BalanceToPrincipal_1", "slc_acct_pre_lim_perc_imputed_med")

# - Correlation analysis towards obtaining clusters of correlated variables
corrAnalysis(datCredit_train, vars, corrThresh = 0.6, method = 'spearman')
### RESULTS:
###           Absolute correlations of  97%  found for [Principal_Real] and [Principal] 
###           Absolute correlations of  91%  found for [Principal_Real] and [Balance_Real_1] 
###           Absolute correlations of  87%  found for [Principal]  and [Balance_Real_1] 
###           Absolute correlations of  90%  found for [Principal_Real] and [Balance_1] 
###           Absolute correlations of  90%  found for [Principal]  and [Balance_1] 
###           Absolute correlations of  98%  found for [Balance_Real_1] and [Balance_1] 
###           Absolute correlations of  89%  found for [Principal_Real] and [Instalment_Real] 
###           Absolute correlations of  86%  found for [Principal] and [Instalment_Real] 
###           Absolute correlations of  93%  found for [Balance_Real_1] and [Instalment_Real] 
###           Absolute correlations of  92%  found for [Balance_1] and [Instalment_Real] 
###           Absolute correlations of  75%  found for [Balance_Real_1] and [BalanceToPrincipal_1] 
###           Absolute correlations of  72%  found for [Balance_1] and [BalanceToPrincipal_1] 
###           Absolute correlations of  63%  found for [Instalment_Real] and [BalanceToPrincipal_1]

# - Initialize variables to be tested
### MM: Should this selection not be refined according to the results of the correlation analysis?
vars <- c("Principal_Real", "Principal", "InterestRate_Margin_imputed_mean", "pmnt_method_grp",
          "Balance_Real_1", "Balance_1", "Instalment_Real", "InterestRate_Nom", "AgeToTerm",
          "BalanceToPrincipal_1", "slc_acct_pre_lim_perc_imputed_med")

# - Single-factor modelling results
# Goodness-of-fit
aicTable(datCredit_train, vars, TimeDef=c("Cox_Discrete","DefSpell_Event"), genPath=genObjPath, modelType="Cox_Discrete")
### RESULTS: Best AIC-results: [InterestRate_Nom]; [InterestRate_Margin_imputed_mean]; [pmnt_method_grp]; [AgeToTerm]; [Principal_Real]
###          The remainder of the variables have very similar AICs
# Discriminatory power (in-sample)
concTable(datCredit_train, datCredit_valid, vars, TimeDef=c("Cox_Discrete","DefSpell_Event"), genPath=genObjPath, modelType="Cox_Discrete")
### RESULTS: Best C-statistics: [InterestRate_Nom]; [InterestRate_Margin_imputed_mean]; [BalanceToPrinpal_1]; [Principal_Rea]; [pmnt_method_grp]
###          The remainder of the variables have very similar AICs
### CONLUSION: ALL variables have very low AIC and good Harell's c values>0 
###            Select top 5 variables:
###               [InterestRate_Nom]; [InterestRate_Margin_imputed_mean]; [BalanceToPrincipal_1];
###               [Principal_Real]; [pmnt_method_grp]
### MM: Why are we selecting the top 5 variables here whilst the other sub-sections we select only the top three?
### MM: I changed the selection, the previous was: 
###               [InterestRate_Nom]; [InterestRate_Margin_imputed_mean]; [Balance_adj_WOff]
###               [Balance_Real_adj_WOff]; [Principal_Real]


# --- 6.2 Combining insights: Delinquency-themed, portfolio-level, and account-level variables

# - Initialize variables to be tested
### MM: No convergence with [BalanceToPrincipal_1]
vars <- c("log(TimeInDefSpell)*DefSpell_Num_binned", "g0_Delinq_Lag_1", 
          "DefaultStatus1_Aggr_Prop_Lag_12","g0_Delinq_Ave", 
          "InterestRate_Margin_Aggr_Med_9","NewLoans_Aggr_Prop","InterestRate_Nom",
          "Balance_1","Principal_Real","InterestRate_Margin_imputed_mean","pmnt_method_grp")

# - Full model | Stepwise forward selection procedure
modLR_full <- glm( as.formula(paste("DefSpell_Event ~", paste(vars, collapse = " + "))),
                   data=datCredit_train, family="binomial")
summary(modLR_full);
evalLR(modLR_full, modLR_base, datCredit_train, targetFld="DefSpell_Event", predClass=1)
### RESULTS: AIC: 81 718; McFadden R^2: 64.21%; AUC: 98.43%.

# - Stepwise forward selection using AIC
ptm <- proc.time() # for runtime calculations (83m) 
modLR_step <- stepAIC(modLR_base2, scope = list(lower = ~ log(TimeInDefSpell)*DefSpell_Num_binned, 
                                                upper = as.formula(paste("~", paste(vars, collapse = " + ")))), 
                      direction = "both", k=log(datCredit_train[,.N]), maxit=50)
summary(modLR_step)
evalLR(modLR_step, modLR_base, datCredit_train, targetFld="DefSpell_Event", predClass=1)
proc.time() - ptm
### RESULTS: AIC: 82 721; McFadden R^2: 62.71%; AUC: 98.43%

# - Final variables
vars <- c("log(TimeInDefSpell)*DefSpell_Num_binned", "g0_Delinq_Lag_1", 
          "DefaultStatus1_Aggr_Prop_Lag_12","g0_Delinq_Ave", 
          "InterestRate_Margin_Aggr_Med_9","NewLoans_Aggr_Prop","InterestRate_Nom",
          "Balance_adj_WOff","Principal","pmnt_method_grp")
### MM: Why is [InterestRate_Margin_Aggr_Med_9] included instead of [InterestRate_Margin_imputed_mean], where the latter is output from step-wise selection?
modLR <- glm( as.formula(paste("DefSpell_Event ~", paste(vars, collapse = " + "))),
              data=datCredit_train, family="binomial")
summary(modLR);
evalLR(modLR, modLR_base, datCredit_train, targetFld="DefSpell_Event", predClass=1)
### RESULTS: AIC: 83 384; McFadden R^2: 62.41%; AUC: 98.41%




# ------ 7. Macroeconomic variables

# --- 7.1 Which lag order is the best for: [M_Repo_Rate]

# - Initialize variables to be tested
vars <- c("M_Repo_Rate", "M_Repo_Rate_1 ", "M_Repo_Rate_2", "M_Repo_Rate_3", "M_Repo_Rate_6", "M_Repo_Rate_9", "M_Repo_Rate_12")

# - Single-factor modelling results
# Goodness-of-fit
aicTable(datCredit_train, vars, TimeDef=c("Cox_Discrete","DefSpell_Event"), genPath=genObjPath, modelType="Cox_Discrete")
### RESULTS: Best AIC-results: [M_Repo_Rate_12], [M_Repo_Rate_9], [M_Repo_Rate], [M_Repo_Rate_1], 
###                            [M_Repo_Rate_2], [M_Repo_Rate_6], [M_Repo_Rate_3]
# Discriminatory power (in-sample)
concTable(datCredit_train, datCredit_valid, vars, TimeDef=c("Cox_Discrete","DefSpell_Event"), genPath=genObjPath, modelType="Cox_Discrete")
### RESULTS: Best C-statistics: [M_Repo_Rate_12]; [M_Repo_Rate_9]; [M_Repo_Rate_6]; [M_Repo_Rate];
###                             [M_Repo_Rate_1]; [M_Repo_Rate_2]; [M_Repo_Rate_3]

### CONCLUSION: There are minor difference between AIC value which suggest the main repo rate
###             and lags 1 and 0 are best. C-statistic differences are also small (ranging from 50%-52%).
###             Select two best variables and the [M_Repo_Rate] base variable (the latter is an intuitive choice):
###               [M_Repo_Rate_12]; [M_Repo_Rate_9]; [M_Repo_Rate]


# --- 7.2 Which lag order is the best for: M_Inflation_Growth

# - Initialize variables to be tested
vars <- c("M_Inflation_Growth", "M_Inflation_Growth_1 ", "M_Inflation_Growth_2", "M_Inflation_Growth_3", 
          "M_Inflation_Growth_6", "M_Inflation_Growth_9", "M_Inflation_Growth_12")

# - Single-factor modelling results
# Goodness-of-fit
aicTable(datCredit_train, vars, TimeDef=c("Cox_Discrete","DefSpell_Event"), genPath=genObjPath, modelType="Cox_Discrete")
### RESULTS: Best AIC-results: [M_Inflation_Growth_12]; [M_Inflation_Growth]; [M_Inflation_Growth_1]; [M_Inflation_Growth_2]; 
###                            [M_Inflation_Growth_3]; [M_Inflation_Growth_9]; [M_Inflation_Growth_6]
# Discriminatory power (in-sample)
concTable(datCredit_train, datCredit_valid, vars, TimeDef=c("Cox_Discrete","DefSpell_Event"), genPath=genObjPath, modelType="Cox_Discrete")
### RESULTS: Best C-statistics: [M_Inflation_Growth_12]; [M_Inflation_Growth]; [M_Inflation_Growth_1]; [M_Inflation_Growth_2];
###                             M_Inflation_Growth_3]; [M_Inflation_Growth_9]; [M_Inflation_Growth_6]

### CONCLUSION: Small AIC and Harell's c-statistics differences
###             Select two best variables and the [M_Inflation_Growth] base variable (the latter is an intuitive choice):
###               Best variables: [M_Inflation_Growth_12]; [M_Inflation_Growth]; [M_Inflation_Growth_1]


# --- 7.3 Which lag order is the best for: [M_RealGDP_Growth]

# - Initialize variables to be tested
vars <- c("M_RealGDP_Growth", "M_RealGDP_Growth_1 ", "M_RealGDP_Growth_2", "M_RealGDP_Growth_3", 
          "M_RealGDP_Growth_6", "M_RealGDP_Growth_9", "M_RealGDP_Growth_12")

# - Single-factor modelling results
# Goodness-of-fit
aicTable(datCredit_train, vars, TimeDef=c("Cox_Discrete","DefSpell_Event"), genPath=genObjPath, modelType="Cox_Discrete")
### RESULTS: Best AIC-results: [M_RealGDP_Growth_6]; [M_RealGDP_Growth_3]; [M_RealGDP_Growth_2]; [M_RealGDP_Growth_1]; 
###                            [M_RealGDP_Growth]; [M_RealGDP_Growth_9]; [M_RealGDP_Growth_12]
# Discriminatory power (in-sample)
concTable(datCredit_train, datCredit_valid, vars, TimeDef=c("Cox_Discrete","DefSpell_Event"), genPath=genObjPath, modelType="Cox_Discrete")
### RESULTS: Best C-statistics: [M_RealGDP_Growth_3]; [M_RealGDP_Growth_6]; [M_RealGDP_Growth_2]; [M_RealGDP_Growth_1]; 
###                             [M_RealGDP_Growth_9]; [M_RealGDP_Growth]; [M_RealGDP_Growth_12]

### CONCLUSION: Middle lags seem better, based on very small AIC-differences and C-statistic differences (57-54%)
###             Select: [M_RealGDP_Growth_3]; [M_RealGDP_Growth_6]; [M_RealGDP_Growth_2]


# --- 7.4 Which lag order is the best for: [M_RealIncome_Growth]

# - Initialize variables to be tested
vars <- c("M_RealIncome_Growth", "M_RealIncome_Growth_1 ", "M_RealIncome_Growth_2", "M_RealIncome_Growth_3", 
          "M_RealIncome_Growth_6", "M_RealIncome_Growth_9", "M_RealIncome_Growth_12")

# - Single-factor modelling results
# Goodness-of-fit
aicTable(datCredit_train, vars, TimeDef=c("Cox_Discrete","DefSpell_Event"), genPath=genObjPath, modelType="Cox_Discrete")
### RESULTS: Best AIC-results: [M_RealIncome_Growth_6]; [M_RealIncome_Growth_3]; [M_RealIncome_Growth_9]; [M_RealIncome_Growth_2]; 
###                            [M_RealIncome_Growth_12]; [M_RealIncome_Growth_1]; [M_RealIncome_Growth]
# Discriminatory power (in-sample)
concTable(datCredit_train, datCredit_valid, vars, TimeDef=c("Cox_Discrete","DefSpell_Event"), genPath=genObjPath, modelType="Cox_Discrete")
### RESULTS: Best C-statistics: [M_RealIncome_Growth_6]; [M_RealIncome_Growth_3]; [M_RealIncome_Growth_9]; [M_RealIncome_Growth_12]; 
###                             [M_RealIncome_Growth_2]; [M_RealIncome_Growth_1]; [M_RealIncome_Growth]

### CONCLUSION: Middle lags seem better, based on very small AIC-differences and c-differences (50%-51%)
###             Select: [M_RealIncome_Growth_6]; [M_RealIncome_Growth_3]; [M_RealIncome_Growth_9]


# --- 7.5 Which lag order is the best for: [M_DTI_Growth]

# - Initialize variables to be tested
vars <- c("M_DTI_Growth", "M_DTI_Growth_1 ", "M_DTI_Growth_2", "M_DTI_Growth_3", 
          "M_DTI_Growth_6", "M_DTI_Growth_9", "M_DTI_Growth_12")

# - Single-factor modelling results
# Goodness-of-fit
aicTable(datCredit_train, vars, TimeDef=c("Cox_Discrete","DefSpell_Event"), genPath=genObjPath, modelType="Cox_Discrete")
### RESULTS: Best AIC-results: [M_DTI_Growth_12]; [M_DTI_Growth_9]; [M_DTI_Growth_6]; [M_DTI_Growth_3]; [M_DTI_Growth_2]; 
###                            [M_DTI_Growth]; [M_DTI_Growth_1]
# Discriminatory power (in-sample)
concTable(datCredit_train, datCredit_valid, vars, TimeDef=c("Cox_Discrete","DefSpell_Event"), genPath=genObjPath, modelType="Cox_Discrete")
### RESULTS: Best C-statistics: [M_DTI_Growth_12]; [M_DTI_Growth_9]; [M_DTI_Growth_6]; [M_DTI_Growth_3]; [M_DTI_Growth]; 
###                             [M_DTI_Growth_2]; [M_DTI_Growth_1]

### CONCLUSION: Longer lags seem better, based on very small AIC-differences,
###             trend exist across c-differences (54-56%)
###             Select top three variables:
###               [M_DTI_Growth_12]; [M_DTI_Growth_9]; [M_DTI_Growth_6]


# --- 7.6 Which lag order is the best for: [M_Emp_Growth]

# - Initialize variables to be tested
vars <- c("M_Emp_Growth", "M_Emp_Growth_1 ", "M_Emp_Growth_2", "M_Emp_Growth_3", 
          "M_Emp_Growth_6", "M_Emp_Growth_9", "M_Emp_Growth_12")

# - Single-factor modelling results
# Goodness-of-fit
aicTable(datCredit_train, vars, TimeDef=c("Cox_Discrete","DefSpell_Event"), genPath=genObjPath, modelType="Cox_Discrete")
### RESULTS: Best AIC-results: [M_Emp_Growth_6]; [M_Emp_Growth_9]; [M_Emp_Growth_12]; [M_Emp_Growth_3];
###                            [M_Emp_Growth_2]; [M_Emp_Growth_1]; [M_Emp_Growth]
# Discriminatory power (in-sample)
concTable(datCredit_train, datCredit_valid, vars, TimeDef=c("Cox_Discrete","DefSpell_Event"), genPath=genObjPath, modelType="Cox_Discrete")
### RESULTS: Best C-statistics: [M_Emp_Growth_6]; [M_Emp_Growth_9]; [M_Emp_Growth_12]; [M_Emp_Growth_3];
###                             [M_Emp_Growth_2]; [M_Emp_Growth_1]; [M_Emp_Growth]

### CONCLUSION: Middle to late lags seem better, based on very small AIC-differences, affirmed by the c-differences (50-51%)
###             Select top three variables:
###               [M_Emp_Growth_6]; [M_Emp_Growth_12]; [M_Emp_Growth_9] 


# --- 7.7 Combining insights: Macroeconomic variables

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
### RESULTS: AIC: 213 158; McFadden R^2: 3.9%; AUC: 68.81%.

# - Stepwise forward selection using AIC
ptm <- proc.time() # for runtime calculations (ignore)
modLR_step <- stepAIC(modLR_base2, scope = list(lower = ~ log(TimeInDefSpell)*DefSpell_Num_binned, 
                                                upper = as.formula(paste("~", paste(vars, collapse = " + ")))), 
                      direction = "both", k=log(datCredit_train[,.N]), maxit=50)
summary(modLR_step)
evalLR(modLR_step, modLR_base, datCredit_train, targetFld="DefSpell_Event", predClass=1)
proc.time() - ptm # IGNORE: elapsed runtime; 140 minutes
### RESULTS: AIC: 213 183; McFadden R^2: 3.88%; AUC: 68.76%.

# - Final variables (Expert Judgement)
### MM: Why are we reducing the number of macro variables in the list below from what we got in the step-wise selection?
vars <- c("log(TimeInDefSpell)*DefSpell_Num_binned",
          "M_RealIncome_Growth_9", "M_Inflation_Growth_12","M_DTI_Growth_12","M_Repo_Rate_12")
modLR <- glm( as.formula(paste("DefSpell_Event ~", paste(vars, collapse = " + "))),
              data=datCredit_train, family="binomial")
summary(modLR);
evalLR(modLR, modLR_base, datCredit_train, targetFld="DefSpell_Event", predClass=1)
### RESULTS: AIC: 214 631; McFadden R^2: 3.22%; AUC: 67.61%.
###         Included the Repo rate as it is vital. GDP growth and emp growth have a negative effect
###         They increase the AIC and R^2 values


# --- 7.8 Combining insights: Delinquency-themed, portfolio-level, account-level, and macroeconomic variables

# - Initialize variables to be tested
vars <- c("log(TimeInDefSpell)*DefSpell_Num_binned", "g0_Delinq_Lag_1", 
          "DefaultStatus1_Aggr_Prop_Lag_12","g0_Delinq_Ave", "g0_Delinq_Any_Aggr_Prop_Lag_1",
          "InterestRate_Margin_Aggr_Med_9","NewLoans_Aggr_Prop","InterestRate_Nom",
          "Balance_adj_WOff","Principal","pmnt_method_grp",
          "M_RealIncome_Growth_9", "M_Inflation_Growth_12","M_DTI_Growth_12","M_Repo_Rate_12")

# - Full model | Stepwise forward selection procedure
modLR_full <- glm( as.formula(paste("DefSpell_Event ~", paste(vars, collapse = " + "))),
                   data=datCredit_train, family="binomial")
summary(modLR_full);
evalLR(modLR_full, modLR_base, datCredit_train, targetFld="DefSpell_Event", predClass=1)
### RESULTS: AIC: 81 809; McFadden R^2: 63.13%; AUC: 98.48%.

# - Stepwise forward selection using AIC
ptm <- proc.time() # for runtime calculations (ignore)
modLR_step <- stepAIC(modLR_base2, scope = list(lower = ~ log(TimeInDefSpell)*DefSpell_Num_binned, 
                                                upper = as.formula(paste("~", paste(vars, collapse = " + ")))), 
                      direction = "both", k=log(datCredit_train[,.N]), maxit=50)
summary(modLR_step)
evalLR(modLR_step, modLR_base, datCredit_train, targetFld="DefSpell_Event", predClass=1)
proc.time() - ptm # IGNORE: elapsed runtime; 117m
### RESULTS: AIC: 81 808; McFadden R^2: 63.13%; AUC: 98.48%.

# - Final variables
### NOTE: Include log(TimeInDefSpell) with Time_Binned for final modelling iteration
### MM: Why do we have [Time_Binned] in here as well as log(TimeInDefSpell)?
vars <- c("Time_Binned","log(TimeInDefSpell)*DefSpell_Num_binned", 
          "DefaultStatus1_Aggr_Prop_Lag_12","g0_Delinq_Ave",
          "InterestRate_Margin_Aggr_Med_9","NewLoans_Aggr_Prop","InterestRate_Nom",
          "Balance_adj_WOff ","pmnt_method_grp","Principal","g0_Delinq_Lag_1",
          "M_RealIncome_Growth_9", "M_Inflation_Growth_12","M_DTI_Growth_12","M_Repo_Rate_12","g0_Delinq_Any_Aggr_Prop_Lag_1")
modLR <- glm( as.formula(paste("DefSpell_Event ~", paste(vars, collapse = " + "))),
              data=datCredit_train, family="binomial")
summary(modLR)
evalLR(modLR, modLR_base, datCredit_train, targetFld="DefSpell_Event", predClass=1)
### MM: [g0_Delinq_Any_Aggr_Prop_Lag_1] is insignificant
### RESULTS: AIC: 79 503; McFadden R^2: 64.18%; AUC: 98.55%




# ------ 8. Final 

# - Confirm prepared datasets are loaded into memory
if (!exists('datCredit_train_CDH')) unpack.ffdf(paste0(genPath,"creditdata_train_CDH"), tempPath);gc()
if (!exists('datCredit_valid_CDH')) unpack.ffdf(paste0(genPath,"creditdata_valid_CDH"), tempPath);gc()

# - Use only default spells
datCredit_train <- datCredit_train_CDH[!is.na(DefSpell_Key),]
datCredit_valid <- datCredit_valid_CDH[!is.na(DefSpell_Key),]
datCredit_train <- datCredit_train[,DefSpell_ExitInd:= ifelse(DefSpell_Age==TimeInDefSpell,1,0)]
datCredit_valid <- datCredit_valid[,DefSpell_ExitInd:= ifelse(DefSpell_Age==TimeInDefSpell,1,0)]
# remove previous objects from memory
rm(datCredit_train_CDH, datCredit_valid_CDH); gc()

# - Weigh default cases heavier. as determined interactively based on calibration success (script 6e)
### MM: We aren't weighting these higher/lower? I thought the the results showed that the best "weight" was not one?
datCredit_train[, Weight := ifelse(DefSpell_Event==1,1,1)]

# - Fit an "empty" model as a performance gain, used within some diagnostic functions
modLR_base <- glm(DefSpell_Event ~ 1, data=datCredit_train, family="binomial")

# - Final variables
vars <- c("Time_Binned","log(TimeInDefSpell)*DefSpell_Num_binned", 
          "DefaultStatus1_Aggr_Prop_Lag_12","g0_Delinq_Ave",
          "InterestRate_Margin_Aggr_Med_9","NewLoans_Aggr_Prop","InterestRate_Nom",
          "Balance_adj_WOff","pmnt_method_grp","Principal","g0_Delinq_Lag_1",
          "M_RealIncome_Growth_9", "M_Inflation_Growth_12","M_DTI_Growth_12","M_Repo_Rate_12","g0_Delinq_Any_Aggr_Prop_Lag_1")
modLR <- glm( as.formula(paste("DefSpell_Event ~", paste(vars, collapse = " + "))),
              data=datCredit_train, family="binomial")
summary(modLR)
evalLR(modLR, modLR_base, datCredit_train, targetFld="DefSpell_Event", predClass=1)
### RESULTS: AIC: 79 503; McFadden R^2: 64.18%; AUC: 98.55%

# - Summary with robust SEs
robust_se <- vcovHC(modLR, type="HC0")
coeftest(modLR, vcov.=robust_se)
### MM: [g0_Delinq_Any_Aggr_Prop_Lag_1] is insignificant

# - Test goodness-of-fit using AIC-measure from single-factor models
(aicTable_CoxDisc <- aicTable(datCredit_train, vars, TimeDef=c("Cox_Discrete","DefSpell_Event"), genPath=genObjPath, modelType="Cox_Discrete"))
### RESULTS: Top variables: [InterestRate_Nom]; [Time_Binned]; [log(TimeInDefSpell)DefSpell_Num_binned]; [g0_Delinq_Lag_1];
###                         [Balance_adj_WOff]; [DefaultStatus1_Aggr_Prop_Lag_12]

# - Test accuracy using c-statistic from single-factor models
(concTable_CoxDisc <- concTable(datCredit_train, datCredit_valid, vars, TimeDef=c("Cox_Discrete","DefSpell_Event"), genPath=genObjPath, modelType="Cox_Discrete"))
### RESULTS: Top variables: [InterestRate_Nom]; [Time_Binned]; [log(TimeInDefSpell)*DefSpell_Num_binned]; [Balance_adj_WOff];
###                         [g0_Delinq_Lag_1]; [DefaultStatus1_Aggr_Prop_Lag_12]

# - Combine results into a single object
(Table_CoxDisc <- concTable_CoxDisc[,1:2] %>% left_join(aicTable_CoxDisc, by ="Variable"))

GoF_CoxSnell_KS(modLR, datCredit_train, GraphInd=TRUE, legPos=c(0.6,0.4), panelTitle="Survival Analysis: Advanced",
                fileName = paste0(genFigPath, "KS_Test_CoxSnellResiduals_Exp_CDH_Adv", ".png"), dpi=280)
### MM: Can't find this function?

# - Save objects
pack.ffdf(paste0(genObjPath,"CoxDisc_advanced_fits"), Table_CoxDisc)







