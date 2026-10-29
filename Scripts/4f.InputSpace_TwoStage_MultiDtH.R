# ================== INPUT SPACE: MULTINOMIAL DISCRETE COX =====================
# Partition data into thematic groups and perform data analysis on them in
# compiling an input space for a multinomial discrete-time hazard (DtH) model#
# amidst competing risks.
# ------------------------------------------------------------------------------
# PROJECT TITLE: Loss Modelling (LGD) for Residential Mortgages
# SCRIPT AUTHOR(S): Dr Arno Botha (AB)
# ------------------------------------------------------------------------------
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
#   - datCredit_train_CDH | Training dataset as prepared in script 2g
#   - datCredit_valid_CDH | Validation dataset prepared in script 2g
#
# -- Outputs:
#   - modLR_Adv | Final discrete-time hazard modeling object
# ------------------------------------------------------------------------------


# ------ 1. Preliminaries ------

# --- 1.1 Load and subset data
# - Confirm prepared datasets are loaded into memory
if (!exists('datCredit_train_CDH')) unpack.ffdf(paste0(genPath,"creditdata_train_CDH"), tempPath);gc()
if (!exists('datCredit_valid_CDH')) unpack.ffdf(paste0(genPath,"creditdata_valid_CDH"), tempPath);gc()

# - Ensure order of data is correct
setorder(datCredit_train_CDH, LoanID, Date)
setorder(datCredit_valid_CDH, LoanID, Date)

# - Use only default spells
datCredit_train <- datCredit_train_CDH[!is.na(DefSpell_Key),]
datCredit_valid <- datCredit_valid_CDH[!is.na(DefSpell_Key),]

# - Remove previous objects from memory
rm(datCredit_train_CDH, datCredit_valid_CDH); gc()


# --- 1.2 Fit preliminary models
# - Fit an "empty" model as a performance gain, used within some diagnostic functions
modMLR_base <- multinom(DefSpell_EventType ~  1, data = datCredit_train, maxit=1000)




# ------ 2a. Embedding the baseline hazard h_0(t) | Part I ------
# Which of the following methods is the best to embed the baseline hazard
# They are done in order from least onerous to most onerous

# --- 2.1 Single time variable (raw duration time)
modMLR <- multinom(DefSpell_EventType ~  TimeInDefSpell, 
                  data = datCredit_train, family="binomial", maxit=1000)
evalLR(modMLR, modMLR_base, datCredit_train, targetFld="DefSpell_EventType", predClass=1, predType="probs")
### RESULTS: AIC: 799,277; McFadden R^2: 0.21%; AUC: 46.20%


# --- 2.2 Single time variable: Function of raw duration time (log transform)
modMLR <- multinom(DefSpell_EventType ~  log(TimeInDefSpell), 
                   data = datCredit_train, family="binomial", maxit=1000)
evalLR(modMLR, modMLR_base, datCredit_train, targetFld="DefSpell_EventType", predClass=1, predType="probs")
### RESULTS: AIC: 795,470; McFadden R^2: 0.68%; AUC: 53.78%


# --- 2.3 Single time variable: Spline
modMLR <- multinom(DefSpell_EventType ~  ns(TimeInDefSpell, df=8), # DF was set by trial and error
                   data = datCredit_train, family="binomial", maxit=1000)
evalLR(modMLR, modMLR_base, datCredit_train, targetFld="DefSpell_EventType", predClass=1, predType="probs")
### RESULTS: AIC: 769,818; McFadden R^2: 3.89%; AUC: 64.23%


# --- 2.4 Single time variable: binned
# - Analyse data to inform on model inputs
table(datCredit_train$Time_Binned) %>% prop.table()
### RESULTS: Between 1% and 15% of observations in each bin; deemed appropriate, particularly in
###          the lower bins that cover the majority of the data
modMLR <- multinom(DefSpell_EventType ~  Time_Binned,
                   data = datCredit_train, family="binomial", maxit=1000)
evalLR(modMLR, modMLR_base, datCredit_train, targetFld="DefSpell_EventType", predClass=1, predType="probs")
### RESULTS: AIC: 764,819; McFadden R^2: 4.53%; AUC: 65.27%

# - Statistical significance: Likelihood Ratio Test
ptm <- proc.time() # for runtime calculations (ignore)
modMLR_Result_int  <- dropterm(modMLR, trace=F, test="Chisq", maxit=50)
proc.time() - ptm # IGNORE: elapsed runtime; 4.3h
### RESULTS: All variables are significant



# ------ 2b. Embedding the baseline hazard h_0(t) | Part II ------
### NOTE: Here an interaction between [TimeInDefSpell] and [Defspell_num] is enforced
###       such that each spell will have its own baseline and slope

# --- 3.1 Single-factor models: Single time variable (transform), binned num, plus intereaction
modMLR <- multinom(DefSpell_EventType ~  log(TimeInDefSpell)*DefSpell_Num_binned,
                   data = datCredit_train, family="binomial", maxit=1000)
evalLR(modMLR, modMLR_base, datCredit_train, targetFld="DefSpell_EventType", predClass=1, predType="probs")
### RESULTS: AIC: 794,525; McFadden R^2: 0.80%; AUC: 54.12%


# --- 3.2 Single-factor models: Binned Variable plus interaction
modMLR <- multinom(DefSpell_EventType ~ Time_Binned + log(TimeInDefSpell):DefSpell_Num_binned,
                   data = datCredit_train, family="binomial", maxit=1000)
evalLR(modMLR, modMLR_base, datCredit_train, targetFld="DefSpell_EventType", predClass=1, predType="probs")
### RESULTS: AIC: 764,105; McFadden R^2: 4.61%; AUC: 65.37%


# --- 3.3 Single-factor models: Binned Variable plus interaction
modMLR <- multinom(DefSpell_EventType ~ Time_Binned*DefSpell_Num_binned,
                   data = datCredit_train, family="binomial", maxit=1000)
evalLR(modMLR, modMLR_base, datCredit_train, targetFld="DefSpell_EventType", predClass=1, predType="probs")
### RESULTS: AIC: 763,439; McFadden R^2: 4.71%; AUC: 65.49%

### CONCLUSION: Use Time_Binned + log(TimeInDefSpell):DefSpell_Num_binned given favourable model performance and 
# balancing model parsimony



# ------ 3 Final Model | Expert Judgment ------

# - Confirm prepared datasets are loaded into memory
if (!exists('datCredit_train_CDH')) unpack.ffdf(paste0(genPath,"creditdata_train_CDH"), tempPath);gc()
if (!exists('datCredit_valid_CDH')) unpack.ffdf(paste0(genPath,"creditdata_valid_CDH"), tempPath);gc()

# - Ensure order of data is correct
setorder(datCredit_train_CDH, LoanID, Date)
setorder(datCredit_valid_CDH, LoanID, Date)

# - Use only default spells
datCredit_train <- datCredit_train_CDH[!is.na(DefSpell_Key),]
datCredit_valid <- datCredit_valid_CDH[!is.na(DefSpell_Key),]

# - Remove previous objects from memory
rm(datCredit_train_CDH, datCredit_valid_CDH); gc()

# - Fit an "empty" model as a performance gain, used within some diagnostic functions
modMLR_base <- multinom(DefSpell_EventType ~  1, data = datCredit_train, maxit=1000)

# - Final variables
### NOTE: Variable selection dependent on single-risk DtH-model for write-off
# Rebinned time to a finer grain
# Removed g0_Delinq_Lag_1 due to adverse effect on term-structure
# Inserted macroeconomic variables due to high AIC-gains in variable importance graph
# other variables sequentially tested, though did not improve the term-structure diagnostic | Removed therefore
vars <- c("Time_Binned", "DefSpell_Num_binned", "log(TimeInDefSpell):DefSpell_Num_binned",
          "InterestRate_Nom", 
          "M_Repo_Rate_12", "M_DTI_Growth_12", "M_RealIncome_Growth_9", "M_Inflation_Growth_12",
          "NewLoans_Aggr_Prop"
          # worsened term-structure
          #""Principal_Real"", "Balance_Real_1", 
          #"InterestRate_Margin_Aggr_Med_2", "slc_acct_roll_ever_24_imputed_mean", g0_Delinq_Lag_1, 
          # "slc_acct_arr_dir_3", "pmnt_method_grp", "InterestRate_Margin_Aggr_Med_2",
          #"DefaultStatus1_Aggr_Prop_Lag_12"
)

# - Fit model
modMLR <- multinom(as.formula(paste("DefSpell_EventType ~", paste(vars, collapse = " + "))),
                   data = datCredit_train, family="binomial", maxit=1000)
evalLR(modMLR, modMLR_base, datCredit_train, targetFld="DefSpell_EventType", predClass=1, predType="probs")
### RESULTS: AIC:   584,745; McFadden R^2: 27.01%; AUC: 81.55%

# - Statistical significance: Likelihood Ratio Test
ptm <- proc.time() # for runtime calculations (ignore)
modMLR_Result  <- dropterm(modMLR, trace=F, test="Chisq", maxit=50)
proc.time() - ptm # IGNORE: elapsed runtime; 4.3h
### RESULTS: All variables are significant

# - Store objects on disc
pack.ffdf(paste0(genObjPath,"MLR_DtH_results"), modMLR_Result)
pack.ffdf(paste0(genPath,"MultiDtH_WOff"), modMLR)


# - AIC-based variable importance
(datResult <- as.data.table(modMLR_Result[rownames(modMLR_Result) != "<none>", "AIC", drop=F] - 
                           modMLR_Result["<none>", "AIC"], keep.rownames = T)[order(-AIC),])
ggplot(datResult, aes(x=reorder(rn,AIC), y=AIC)) + geom_col(fill="steelblue") + coord_flip() + theme_minimal() + 
  labs(y=bquote(Delta * AIC), x="Variable")
# Define # \Delta AIC = AIC_{-j} - AIC_full, where AIC_{-j} is the AIC of the reduced model excluding jth variable.
# Top x Variables ranked by greatest deterioration in penalised model fit (\Delta AIC) upon their individual removal:
# InterestRate_Nom, g0_Delinq_Lag_1, M_Inflation_Growth_12, M_DTI_Growth_12, pmnt_method_grp, M_Repo_Rate_12,
# g0_Delinq_Ave, Balance_Real_1, Principal_Real

### NOTE: Model complexity matters in this ranking; categorical variables are penalized more heavily by AIC, so 
# the (binned) spell time variable and its interaction are therefore ranked much lower, despite their
# strategic importance in qualifying the MLR-model as a DtH-model.

# - Cleanup
rm(modMLR_Result_int, modMLR_Result, modMLR)