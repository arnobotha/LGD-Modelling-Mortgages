# ======================================= INPUT SPACE: DISCRETE COX Basic============================
# Divide data into thematic groups and perform data analysis on them to compile an input space for 
# a basic discrete-time hazard model.
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



# ------ 1. Final model

# - Confirm prepared datasets are loaded into memory
if (!exists('datCredit_train_PWPST')) unpack.ffdf(paste0(genPath,"creditdata_train_PWPST"), tempPath);gc()
if (!exists('datCredit_valid_PWPST')) unpack.ffdf(paste0(genPath,"creditdata_valid_PWPST"), tempPath);gc()

# - Use only performance spells
datCredit_train <- datCredit_train_PWPST[!is.na(PerfSpell_Num),]
datCredit_valid <- datCredit_valid_PWPST[!is.na(PerfSpell_Num),]
# remove previous objects from memory
rm(datCredit_train_PWPST, datCredit_valid_PWPST); gc()

# - Weigh default cases heavier. as determined interactively based on calibration success (script 6e)
datCredit_train[, Weight := ifelse(DefaultStatus1==1,10,1)]

# - Fit an "empty" model as a performance gain, used within some diagnostic functions
modLR_base <- glm(PerfSpell_Event ~ 1, data=datCredit_train, family="binomial")

# - Final variables
# Selection based on expert judgement alone, where the initial list being based on thematic selection
vars_basic <- c("-1", "Time_Binned", "log(TimeInPerfSpell):PerfSpell_Num_binned",
                "Arrears", "InterestRate_Nom", "M_Inflation_Growth_6")
modLR_basic <- glm( as.formula(paste("PerfSpell_Event ~", paste(vars_basic, collapse = " + "))),
                    data=datCredit_train, family="binomial", weights = Weight)
#summary(modLR);
# Robust (sandwich) standard errors
robust_se <- vcovHC(modLR_basic, type="HC0")
# Summary with robust SEs
coeftest(modLR_basic, vcov.=robust_se)

# - Other diagnostics
evalLR(modLR_basic, modLR_base, datCredit_train, targetFld="PerfSpell_Event", predClass=1)
### RESULTS: AIC:  653,566;  McFadden R^2:  -267.27% (??); AUC:  94.62%.

# - Test goodness-of-fit using AIC-measure over single-factor models
aicTable_CoxDisc_basic <- aicTable(datCredit_train, vars_basic, TimeDef=c("Cox_Discrete","PerfSpell_Event"), genPath=genObjPath, modelType="Cox_Discrete")
# Top variables: Arrears, Time_Binned, InterestRate_Nom, M_Inflation_Growth_6

# Test accuracy using c-statistic over single-factor models
concTable_CoxDisc_basic <- concTable(datCredit_train, datCredit_valid, vars_basic, TimeDef=c("Cox_Discrete","PerfSpell_Event"), genPath=genObjPath, modelType="Cox_Discrete")
# Top variables: Arrears, Time_Binned, InterestRate_Nom, M_Inflation_Growth_6 

# - Combine results into a single object
Table_CoxDisc_basic <- concTable_CoxDisc_basic[,1:2] %>% left_join(aicTable_CoxDisc_basic, by ="Variable")

# Save objects
pack.ffdf(paste0(genObjPath,"CoxDisc_basic_fits"), Table_CoxDisc_basic)
