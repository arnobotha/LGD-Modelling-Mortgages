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
### RESULTS: AIC:   121 346; McFadden R^2:  45.29%; AUC:  97.07%.
###          Variables from step-wise selection procedure:
###             [log(TimeInDefSpell)]; [DefSpell_Num_Binned]; [log(TimeInDefSpell):DefSpell_Num_binned];
###             [slc_past_due_amt_imputed_med]; [slc_acct_arr_dir_3]; [slc_curing_ind];
###             [DefaultStatus1_Aggr_Prop_Lag_9]; [DefaultStatus1_Aggr_Prop_Lag_12];
###             [g0_Delinq_Any_Aggr_Prop_Lag_1]; [g0_Delinq_Any_Aggr_Prop_Lag_12];
###             [g0_Delinq_Ave]; [CuringEvents_Aggr_Prop]

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
vars <- c("InstalmentToBalance_1_Aggr_Prop", "AgeToTerm_Aggr_Mean", "DefSpell_Maturity_Aggr_Mean", "NewLoans_Aggr_Prop")

# - Single-factor modelling results
# Goodness-of-fit
aicTable(datCredit_train, vars, TimeDef=c("Cox_Discrete","DefSpell_Event"), genPath=genObjPath, modelType="Cox_Discrete")
### RESULTS: Best AIC-results: [InstalmentToBalance_1_Aggr_Prop]; [AgeToTerm_Aggr_Mean]; [NewLoans_Aggr_Prop]; [DefSpell_Maturity_Aggr_Mean]
# Discriminatory power (in-sample)
concTable(datCredit_train, datCredit_valid, vars, TimeDef=c("Cox_Discrete","DefSpell_Event"), genPath=genObjPath, modelType="Cox_Discrete")
### RESULTS: Best C-statistics: [InstalmentToBalance_1_Aggr_Prop]; [NewLoans_Aggr_Prop]; [AgeToTerm_Aggr_Mean];  [DefSpell_Maturity_Aggr_Mean]

### CONCLUSION: Select top three:
###               [InstalmentToBalance_1_Aggr_Prop]; [NewLoans_Aggr_Prop]; [AgeToTerm_Aggr_Mean]


# --- 5.3 Combining insights: Account- and portfolio-level, delinquency themed, variables

# - Initialize variables to be tested
vars <- c("log(TimeInDefSpell)*DefSpell_Num_binned",
          "slc_past_due_amt_imputed_med", "slc_acct_arr_dir_3", "slc_curing_ind",
          "DefaultStatus1_Aggr_Prop_Lag_9", "DefaultStatus1_Aggr_Prop_Lag_12",
          "g0_Delinq_Any_Aggr_Prop_Lag_1", "g0_Delinq_Any_Aggr_Prop_Lag_12",
          "g0_Delinq_Ave", "CuringEvents_Aggr_Prop",
          "InterestRate_Margin_Aggr_Med_2", "InterestRate_Margin_Aggr_Med_3",
          "InterestRate_Margin_Aggr_Med_9", "AgeToTerm_Aggr_Mean",
          "InstalmentToBalance_1_Aggr_Prop", "NewLoans_Aggr_Prop")
# - Full model | Stepwise forward selection procedure
modLR_full <- glm( as.formula(paste("DefSpell_Event ~", paste(vars, collapse = " + "))),
                   data=datCredit_train, family="binomial")
summary(modLR_full);
evalLR(modLR_full, modLR_base, datCredit_train, targetFld="DefSpell_Event", predClass=1)
### RESULTS: AIC: 121 261; McFadden R^2:  45.34%; AUC: 97.08%

# - Stepwise forward selection using AIC
ptm <- proc.time() # for runtime calculations (ignore)
modLR_step <- stepAIC(modLR_base2, scope = list(lower = ~ log(TimeInDefSpell)*DefSpell_Num_binned, 
                                                upper = as.formula(paste("~", paste(vars, collapse = " + ")))), 
                      direction = "both", k=log(datCredit_train[,.N]), maxit=50)
summary(modLR_step)
evalLR(modLR_step, modLR_base, datCredit_train, targetFld="DefSpell_Event", predClass=1)
proc.time() - ptm # IGNORE: elapsed runtime; ~7.5 minutes
### RESULTS: AIC: 121 299; McFadden R^2: 45.32%; AUC: 97.08%
###          Variables from step-wise selection procedure:
###             [log(TimeInDefSpell)]; [DefSpell_Num_binned]; [log(TimeInDefSpell):DefSpell_Num_binned]
###             [slc_past_due_amt_imputed_med]; [slc_acct_arr_dir_3]; [slc_curing_ind]
###             [DefaultStatus1_Aggr_Prop_Lag_9]; [DefaultStatus1_Aggr_Prop_Lag_12]
###             [g0_Delinq_Any_Aggr_Prop_Lag_1]; [NewLoans_Aggr_Prop]; 
###             [g0_Delinq_Ave]

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
### NOTE: - Inflation adjusted variables preferred over their non-adjusted counterparts
###       - Principal, balance, and instalment all kept regardless of their correlations, each contains unique information
vars <- c("Principal_Real", "InterestRate_Margin_imputed_mean", "pmnt_method_grp",
          "Balance_Real_1", "Instalment_Real", "InterestRate_Nom", "AgeToTerm",
          "BalanceToPrincipal_1", "slc_acct_pre_lim_perc_imputed_med")

# - Single-factor modelling results
# Goodness-of-fit
aicTable(datCredit_train, vars, TimeDef=c("Cox_Discrete","DefSpell_Event"), genPath=genObjPath, modelType="Cox_Discrete")
### RESULTS: Best AIC-results: [InterestRate_Nom]; [InterestRate_Margin_imputed_mean]; [pmnt_method_grp]; [AgeToTerm]; [Principal_Real]; [Balance_Real_1]
# Discriminatory power (in-sample)
concTable(datCredit_train, datCredit_valid, vars, TimeDef=c("Cox_Discrete","DefSpell_Event"), genPath=genObjPath, modelType="Cox_Discrete")
### RESULTS: Best C-statistics: [InterestRate_Nom]; [InterestRate_Margin_imputed_mean]; [BalanceToPrinpal_1]; [Principal_Rea]; [pmnt_method_grp]; [AgeToTerm]
### CONLUSION: Select top 5 variables (preference given to [BalanceToPrincipal_1] since
###                                    it is highly correlated with [AgeToTerm] (-56% correlation) and
###                                    it is more well known (LTV-type variable)):
###               [InterestRate_Nom]; [InterestRate_Margin_imputed_mean]; [BalanceToPrincipal_1];
###               [Principal_Real]; [pmnt_method_grp]


# --- 6.2 Combining insights: Delinquency-themed, portfolio-level, and account-level variables

# - Initialize variables to be tested
### NOTE: [BalanceToPrincipal_1] causes issues when fitting the model, replacing it with [Balance_Real_1]
vars <- c("log(TimeInDefSpell)*DefSpell_Num_binned",
          "slc_past_due_amt_imputed_med", "slc_acct_arr_dir_3", "slc_curing_ind",
          "DefaultStatus1_Aggr_Prop_Lag_9", "DefaultStatus1_Aggr_Prop_Lag_12",
          "g0_Delinq_Any_Aggr_Prop_Lag_1", "NewLoans_Aggr_Prop", "g0_Delinq_Ave",
          "InterestRate_Nom", "InterestRate_Margin_imputed_mean", "Balance_Real_1",
          "Principal_Real", "pmnt_method_grp"
          )

# - Full model | Stepwise forward selection procedure
modLR_full <- glm( as.formula(paste("DefSpell_Event ~", paste(vars, collapse = " + "))),
                   data=datCredit_train, family="binomial")
summary(modLR_full)
evalLR(modLR_full, modLR_base, datCredit_train, targetFld="DefSpell_Event", predClass=1)
### RESULTS: AIC: 53 769; McFadden R^2: 75.77%; AUC: 99.58%.

# - Stepwise forward selection using AIC
ptm <- proc.time() # for runtime calculations (~37m) 
modLR_step <- stepAIC(modLR_base2, scope = list(lower = ~ log(TimeInDefSpell)*DefSpell_Num_binned, 
                                                upper = as.formula(paste("~", paste(vars, collapse = " + ")))), 
                      direction = "both", k=log(datCredit_train[,.N]), maxit=50)
summary(modLR_step)
evalLR(modLR_step, modLR_base, datCredit_train, targetFld="DefSpell_Event", predClass=1)
proc.time() - ptm
### RESULTS: AIC: 53 855; McFadden R^2: 75.77%; AUC: 99.58%
###          Variables from step-wise selection procedure:
###             [log(TimeInDefSpell)]; [DefSpell_Num_binned]; [log(TimeInDefSpell):DefSpell_Num_binned]
###             [slc_past_due_amt_imputed_med]; [slc_curing_ind]; [pmnt_method_grp];
###             [DefaultStatus1_Aggr_Prop_Lag_9]; [g0_Delinq_Any_Aggr_Prop_Lag_1];
###`            [NewLoans_Aggr_Prop]; 
###             [InterestRate_Nom]; [Balance_Real_1]; [Principal_Real]

### CONCLUSION: Use the selection from the automated variable selection
### NOTE:       Including [slc_acct_arr_dir_3] since post-hoc analysis reveals it results in more accurate term-structures




# ------ 7. Macroeconomic variables

# --- 7.1 Which lag order is the best for: [M_Repo_Rate]

# - Initialize variables to be tested
vars <- c("M_Repo_Rate", "M_Repo_Rate_1 ", "M_Repo_Rate_2", "M_Repo_Rate_3", "M_Repo_Rate_6", "M_Repo_Rate_9", "M_Repo_Rate_12")

# - Single-factor modelling results
# Goodness-of-fit
aicTable(datCredit_train, vars, TimeDef=c("Cox_Discrete","DefSpell_Event"), genPath=genObjPath, modelType="Cox_Discrete")
### RESULTS: Best AIC-results: [M_Repo_Rate_12], [M_Repo_Rate_9], [M_Repo_Rate]
# Discriminatory power (in-sample)
concTable(datCredit_train, datCredit_valid, vars, TimeDef=c("Cox_Discrete","DefSpell_Event"), genPath=genObjPath, modelType="Cox_Discrete")
### RESULTS: Best C-statistics: [M_Repo_Rate_12]; [M_Repo_Rate_9]; [M_Repo_Rate_6]

### CONCLUSION: Select top two variables and [M_Repo_Rate] (the latter is an intuitive choice):
###               [M_Repo_Rate_12]; [M_Repo_Rate_9]; [M_Repo_Rate]


# --- 7.2 Which lag order is the best for: M_Inflation_Growth

# - Initialize variables to be tested
vars <- c("M_Inflation_Growth", "M_Inflation_Growth_1 ", "M_Inflation_Growth_2", "M_Inflation_Growth_3", 
          "M_Inflation_Growth_6", "M_Inflation_Growth_9", "M_Inflation_Growth_12")

# - Single-factor modelling results
# Goodness-of-fit
aicTable(datCredit_train, vars, TimeDef=c("Cox_Discrete","DefSpell_Event"), genPath=genObjPath, modelType="Cox_Discrete")
### RESULTS: Best AIC-results: [M_Inflation_Growth_12]; [M_Inflation_Growth]; [M_Inflation_Growth_1]
# Discriminatory power (in-sample)
concTable(datCredit_train, datCredit_valid, vars, TimeDef=c("Cox_Discrete","DefSpell_Event"), genPath=genObjPath, modelType="Cox_Discrete")
### RESULTS: Best C-statistics: [M_Inflation_Growth_12]; [M_Inflation_Growth]; [M_Inflation_Growth_1]

### CONCLUSION: select two best variables and the [M_Inflation_Growth] base variable (the latter is an intuitive choice):
###               [M_Inflation_Growth_12]; [M_Inflation_Growth]; [M_Inflation_Growth_1]


# --- 7.3 Which lag order is the best for: [M_RealGDP_Growth]

# - Initialize variables to be tested
vars <- c("M_RealGDP_Growth", "M_RealGDP_Growth_1 ", "M_RealGDP_Growth_2", "M_RealGDP_Growth_3", 
          "M_RealGDP_Growth_6", "M_RealGDP_Growth_9", "M_RealGDP_Growth_12")

# - Single-factor modelling results
# Goodness-of-fit
aicTable(datCredit_train, vars, TimeDef=c("Cox_Discrete","DefSpell_Event"), genPath=genObjPath, modelType="Cox_Discrete")
### RESULTS: Best AIC-results: [M_RealGDP_Growth_6]; [M_RealGDP_Growth_3]; [M_RealGDP_Growth_2]
# Discriminatory power (in-sample)
concTable(datCredit_train, datCredit_valid, vars, TimeDef=c("Cox_Discrete","DefSpell_Event"), genPath=genObjPath, modelType="Cox_Discrete")
### RESULTS: Best C-statistics: [M_RealGDP_Growth_3]; [M_RealGDP_Growth_6]; [M_RealGDP_Growth_2]

### CONCLUSION: Select top three variables:
###               [M_RealGDP_Growth_3]; [M_RealGDP_Growth_6]; [M_RealGDP_Growth_2]


# --- 7.4 Which lag order is the best for: [M_RealIncome_Growth]

# - Initialize variables to be tested
vars <- c("M_RealIncome_Growth", "M_RealIncome_Growth_1 ", "M_RealIncome_Growth_2", "M_RealIncome_Growth_3", 
          "M_RealIncome_Growth_6", "M_RealIncome_Growth_9", "M_RealIncome_Growth_12")

# - Single-factor modelling results
# Goodness-of-fit
aicTable(datCredit_train, vars, TimeDef=c("Cox_Discrete","DefSpell_Event"), genPath=genObjPath, modelType="Cox_Discrete")
### RESULTS: Best AIC-results: [M_RealIncome_Growth_6]; [M_RealIncome_Growth_3]; [M_RealIncome_Growth_9]
# Discriminatory power (in-sample)
concTable(datCredit_train, datCredit_valid, vars, TimeDef=c("Cox_Discrete","DefSpell_Event"), genPath=genObjPath, modelType="Cox_Discrete")
### RESULTS: Best C-statistics: [M_RealIncome_Growth_6]; [M_RealIncome_Growth_3]; [M_RealIncome_Growth_9]

### CONCLUSION: Select top three variables:
###               [M_RealIncome_Growth_6]; [M_RealIncome_Growth_3]; [M_RealIncome_Growth_9]


# --- 7.5 Which lag order is the best for: [M_DTI_Growth]

# - Initialize variables to be tested
vars <- c("M_DTI_Growth", "M_DTI_Growth_1 ", "M_DTI_Growth_2", "M_DTI_Growth_3", 
          "M_DTI_Growth_6", "M_DTI_Growth_9", "M_DTI_Growth_12")

# - Single-factor modelling results
# Goodness-of-fit
aicTable(datCredit_train, vars, TimeDef=c("Cox_Discrete","DefSpell_Event"), genPath=genObjPath, modelType="Cox_Discrete")
### RESULTS: Best AIC-results: [M_DTI_Growth_12]; [M_DTI_Growth_9]; [M_DTI_Growth_6]
# Discriminatory power (in-sample)
concTable(datCredit_train, datCredit_valid, vars, TimeDef=c("Cox_Discrete","DefSpell_Event"), genPath=genObjPath, modelType="Cox_Discrete")
### RESULTS: Best C-statistics: [M_DTI_Growth_12]; [M_DTI_Growth_9]; [M_DTI_Growth_6]

### CONCLUSION: Select top three variables:
###               [M_DTI_Growth_12]; [M_DTI_Growth_9]; [M_DTI_Growth_6]


# --- 7.6 Which lag order is the best for: [M_Emp_Growth]

# - Initialize variables to be tested
vars <- c("M_Emp_Growth", "M_Emp_Growth_1 ", "M_Emp_Growth_2", "M_Emp_Growth_3", 
          "M_Emp_Growth_6", "M_Emp_Growth_9", "M_Emp_Growth_12")

# - Single-factor modelling results
# Goodness-of-fit
aicTable(datCredit_train, vars, TimeDef=c("Cox_Discrete","DefSpell_Event"), genPath=genObjPath, modelType="Cox_Discrete")
### RESULTS: Best AIC-results: [M_Emp_Growth_6]; [M_Emp_Growth_9]; [M_Emp_Growth_12]
# Discriminatory power (in-sample)
concTable(datCredit_train, datCredit_valid, vars, TimeDef=c("Cox_Discrete","DefSpell_Event"), genPath=genObjPath, modelType="Cox_Discrete")
### RESULTS: Best C-statistics: [M_Emp_Growth_6]; [M_Emp_Growth_12]; [M_Emp_Growth_9]

### CONCLUSION: Select top three variables:
###               [M_Emp_Growth_6]; [M_Emp_Growth_9]; [M_Emp_Growth_12] 


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
### RESULTS: AIC: 213 183; McFadden R^2: 3.88%; AUC: 68.76%
###          Variables from step-wise selection procedure:
###             [M_DTI_Growth_12]; [M_DTI_Growth_6];
###             [M_RealIncome_Growth_3]; [M_RealIncome_Growth_6]; [M_RealIncome_Growth_9];
###             [M_Repo_Rate]; [M_Repo_Rate_12];
###             [M_RealGDP_Growth_3];
###             [M_Inflation_Growth_1]; [M_Inflation_Growth_12];
###             [M_Emp_Growth_9]

### CONCLUSION: Use the selection from the automated variable selection


# --- 7.8 Combining insights: Delinquency-themed, portfolio-level, account-level, and macroeconomic variables

# - Initialize variables to be tested
vars <- c("log(TimeInDefSpell)*DefSpell_Num_binned",
          "slc_past_due_amt_imputed_med", "slc_curing_ind", "pmnt_method_grp",
          "DefaultStatus1_Aggr_Prop_Lag_9", "g0_Delinq_Any_Aggr_Prop_Lag_1",
          "slc_acct_arr_dir_3", # This variable is kept in the selection since post-hoc analysis has shown that it produces more accurate term structures
          "NewLoans_Aggr_Prop",
          "InterestRate_Nom", "Balance_Real_1", "Principal_Real",
          "M_DTI_Growth_12", "M_DTI_Growth_6",
          "M_RealIncome_Growth_3", "M_RealIncome_Growth_6", "M_RealIncome_Growth_9",
          "M_Repo_Rate", "M_Repo_Rate_12",
          "M_RealGDP_Growth_3",
          "M_Inflation_Growth_1", "M_Inflation_Growth_12",
          "M_Emp_Growth_9")

# - Full model | Stepwise forward selection procedure
modLR_full <- glm( as.formula(paste("DefSpell_Event ~", paste(vars, collapse = " + "))),
                   data=datCredit_train, family="binomial")
summary(modLR_full);
evalLR(modLR_full, modLR_base, datCredit_train, targetFld="DefSpell_Event", predClass=1)
### RESULTS: AIC: 53 413; McFadden R^2: 75.94%; AUC: 99.59%

# - Stepwise forward selection using AIC
ptm <- proc.time() # for runtime calculations (ignore)
modLR_step <- stepAIC(modLR_base2, scope = list(lower = ~ log(TimeInDefSpell)*DefSpell_Num_binned, 
                                                upper = as.formula(paste("~", paste(vars, collapse = " + ")))), 
                      direction = "both", k=log(datCredit_train[,.N]), maxit=50)
summary(modLR_step)
evalLR(modLR_step, modLR_base, datCredit_train, targetFld="DefSpell_Event", predClass=1)
proc.time() - ptm # IGNORE: elapsed runtime; 119m
### RESULTS: AIC: 53 526; McFadden R^2: 75.88%; AUC: 99.59%.
###          Variables from step-wise selection procedure:
###          Variables from step-wise selection procedure:
###             [log(TimeInDefSpell)]; [DefSpell_Num_binned]; [log(TimeInDefSpell):DefSpell_Num_binned]
###             [slc_curing_ind]; [pmnt_method_grp];
###             [DefaultStatus1_Aggr_Prop_Lag_9]; [g0_Delinq_Any_Aggr_Prop_Lag_1];
###             [NewLoans_Aggr_Prop]; 
###             [InterestRate_Nom]; [Balance_Real_1]; [Principal_Real]
###             [M_Repo_Rate_12];
###             [M_Inflation_Growth_1]; [M_Inflation_Growth_12];
###             [M_Emp_Growth_9]


### RESULTS: Insignificant variables:
###           [DefSpell_Num_Binned]

### CONCLUSION: Remove insignificant variable [DefSpell_Num_Binned], include [slc_acct_arr_dir_3],
###             refit model, and assess the resulting model


# --- 7.8 Refinement of final input space | Part I
# - Initialize variables to be tested
vars <- c("log(TimeInDefSpell)", "log(TimeInDefSpell):DefSpell_Num_binned", 
          "slc_curing_ind", "pmnt_method_grp",
          "slc_acct_arr_dir_3", # This variable is kept in the selection since post-hoc analysis has shown that it produces more accurate term structures
          "DefaultStatus1_Aggr_Prop_Lag_9", "g0_Delinq_Any_Aggr_Prop_Lag_1",
          "NewLoans_Aggr_Prop",
          "InterestRate_Nom", "Balance_Real_1", "Principal_Real",
          "M_Repo_Rate_12", "M_Inflation_Growth_1", "M_Inflation_Growth_12",
          "M_Emp_Growth_9")

# - Fit model
modLR_full <- glm( as.formula(paste("DefSpell_Event ~", paste(vars, collapse = " + "))),
                   data=datCredit_train, family="binomial")
summary(modLR_full);
evalLR(modLR_full, modLR_base, datCredit_train, targetFld="DefSpell_Event", predClass=1)
### RESULTS: AIC: 53 659; McFadden R^2: 75.92%; AUC: 99.31%.
###          Variables from step-wise selection procedure:
###             [log(TimeInDefSpell)]; [log(TimeInDefSpell):DefSpell_Num_binned]
###             [slc_curing_ind]; [pmnt_method_grp]; [slc_acct_arr_dir_3]
###             [DefaultStatus1_Aggr_Prop_Lag_9]; [g0_Delinq_Any_Aggr_Prop_Lag_1];
###             [NewLoans_Aggr_Prop]; 
###             [InterestRate_Nom]; [Balance_Real_1]; [Principal_Real]
###             [M_Repo_Rate_12];
###             [M_Inflation_Growth_1]; [M_Inflation_Growth_12];
###             [M_Emp_Growth_9]

### RESULTS: [Principal_Real] is no longer significant

### CONCLUSION: Remove [Principal_Real] and refit the model


# --- 7.9 Refinement of final input space | Part II
# - Initialize variables to be tested
vars <- c("log(TimeInDefSpell)", "log(TimeInDefSpell):DefSpell_Num_binned", 
          "slc_curing_ind", "pmnt_method_grp",
          "slc_acct_arr_dir_3", # This variable is kept in the selection since post-hoc analysis has shown that it produces more accurate term structures
          "DefaultStatus1_Aggr_Prop_Lag_9", "g0_Delinq_Any_Aggr_Prop_Lag_1",
          "NewLoans_Aggr_Prop",
          "InterestRate_Nom", "Balance_Real_1",
          "M_Repo_Rate_12", "M_Inflation_Growth_1", "M_Inflation_Growth_12",
          "M_Emp_Growth_9")

# - Fit model
modLR_full <- glm( as.formula(paste("DefSpell_Event ~", paste(vars, collapse = " + "))),
                   data=datCredit_train, family="binomial")
summary(modLR_full);
evalLR(modLR_full, modLR_base, datCredit_train, targetFld="DefSpell_Event", predClass=1)
### RESULTS: AIC: 53 657; McFadden R^2: 75.92%; AUC: 99.31%
###          Variables from step-wise selection procedure:
###             [log(TimeInDefSpell)]; [log(TimeInDefSpell):DefSpell_Num_binned]
###             [slc_curing_ind]; [pmnt_method_grp]; [slc_acct_arr_dir_3]
###             [DefaultStatus1_Aggr_Prop_Lag_9]; [g0_Delinq_Any_Aggr_Prop_Lag_1];
###             [NewLoans_Aggr_Prop]; 
###             [InterestRate_Nom]; [Balance_Real_1]
###             [M_Repo_Rate_12];
###             [M_Inflation_Growth_1]; [M_Inflation_Growth_12];
###             [M_Emp_Growth_9]

### RESULTS: No insignificant variables




# ------ 8. Final model

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

# - Weigh default cases (assuming 1 in this instance for simplicity)
datCredit_train[, Weight := ifelse(DefSpell_Event==1,1,1)]

# - Fit an "empty" model as a performance gain, used within some diagnostic functions
modLR_base <- glm(DefSpell_Event ~ 1, data=datCredit_train, family="binomial")

# - Final variables
vars <- c("log(TimeInDefSpell)", "log(TimeInDefSpell):DefSpell_Num_binned", 
          "slc_curing_ind", "pmnt_method_grp",
          "slc_acct_arr_dir_3", # This variable is kept in the selection since post-hoc analysis has shown that it produces more accurate term structures
          "DefaultStatus1_Aggr_Prop_Lag_9", "g0_Delinq_Any_Aggr_Prop_Lag_1",
          "NewLoans_Aggr_Prop",
          "InterestRate_Nom", "Balance_Real_1",
          "M_Repo_Rate_12", "M_Inflation_Growth_1", "M_Inflation_Growth_12",
          "M_Emp_Growth_9")
modLR <- glm( as.formula(paste("DefSpell_Event ~", paste(vars, collapse = " + "))),
              data=datCredit_train, family="binomial")
summary(modLR)
evalLR(modLR, modLR_base, datCredit_train, targetFld="DefSpell_Event", predClass=1)
### RESULTS: AIC: 53 657; McFadden R^2: 75.92%; AUC: 99.31%

# - Summary with robust SEs
robust_se <- vcovHC(modLR, type="HC0")
coeftest(modLR, vcov.=robust_se)

# - Test goodness-of-fit using AIC-measure from single-factor models
(aicTable_CoxDisc <- aicTable(datCredit_train, vars, TimeDef=c("Cox_Discrete","DefSpell_Event"), genPath=genObjPath, modelType="Cox_Discrete"))
### RESULTS: Top variables: [InterestRate_Nom]; [slc_curing_ind]; [slc_acct_arr_dir_3]; [log(TimeInDefSpell)]; [DefaultStatus1_Aggr_Prop_Lag_9];
###                         [pmnt_method_grp; [NewLoans_Aggr_Prop]

# - Test accuracy using c-statistic from single-factor models
(concTable_CoxDisc <- concTable(datCredit_train, datCredit_valid, vars, TimeDef=c("Cox_Discrete","DefSpell_Event"), genPath=genObjPath, modelType="Cox_Discrete"))
### RESULTS: Top variables: [InterestRate_Nom]; [slc_curing_ind]; [slc_acct_arr_dir_3]; [log(TimeInDefSpell)]; [DefaultStatus1_Aggr_Prop_Lag_9];
###                         [NewLoans_Aggr_Prop]; [pmnt_method_grp]

# - Combine results into a single object
(Table_CoxDisc <- concTable_CoxDisc[,1:2] %>% left_join(aicTable_CoxDisc, by ="Variable"))

GoF_CoxSnell_KS(modLR, datCredit_train, GraphInd=TRUE, legPos=c(0.6,0.4), panelTitle="Survival Analysis: Advanced",
                fileName = paste0(genFigPath, "KS_Test_CoxSnellResiduals_Exp_CDH_Adv", ".png"), dpi=280)
### RESULTS: KS-statistic = 95%; Harell's c = 99.306%; AIC = 55 657

# - Save objects
# Model analytics
pack.ffdf(paste0(genObjPath,"CoxDisc_advanced_fits"), Table_CoxDisc)
# Modeling object
modLR_Adv <- copy(modLR); rm(modLR); gc()
save(modLR_Adv, file=paste0(genObjPath,"CoxDisc_Advanced_Model.rds"))






