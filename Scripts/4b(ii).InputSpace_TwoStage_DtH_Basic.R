# =================== INPUT SPACE: DISCRETE COX Basic ==========================
# Divide data into thematic groups and perform data analysis on them towards
# compiling an input space for a basic discrete-time hazard model.
# ------------------------------------------------------------------------------
# PROJECT TITLE: Loss Modelling (LGD) for Residential Mortgages
# SCRIPT AUTHOR(S): Mohammed Gabru (MG), Marcel Muller (MM), Dr Arno Botha (AB)
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
#   - datCredit_train_CDH | Training dataset prepared in script 2g
#   - datCredit_valid_CDH | Training dataset prepared in script 2g
#
# -- Outputs:
#   - modLR_basic | Final discrete-time hazard modeling object
# ------------------------------------------------------------------------------



# ============ 1. Preliminaries ============

# --- 1.1 Load data
# - Confirm prepared datasets are loaded into memory
if (!exists('datCredit_train_CDH_sub')) unpack.ffdf(paste0(genPath,"creditdata_train_CDH_sub"), tempPath); gc()
if (!exists('datCredit_valid_CDH_sub')) unpack.ffdf(paste0(genPath,"creditdata_valid_CDH_sub"), tempPath); gc()

# Sampling window start for model fitting purposes
sDateStr <- as.Date("2008-01-31")
sDateStp <- as.Date("2019-12-31")

# - Use only default spells
datCredit_train <- datCredit_train_CDH_sub[!is.na(DefSpell_Key)]
datCredit_valid <- datCredit_valid_CDH_sub[!is.na(DefSpell_Key)]

# - Remove previous objects from memory
rm(datCredit_train_CDH_sub, datCredit_valid_CDH_sub); gc()


# --- 1.2 Fit preliminary models
# - Fit an "empty" model as a performance gain, used within some diagnostic functions
modLR_base <- glm(DefSpell_Event ~ 1, data=datCredit_train, family="binomial")




# ============ 2. Final model ============
### NOTE: Selection based on expert judgement only and using restricted training set
###       that excludes observations otuside of predetermined range as set in script 2g

# - Final variables
vars_basic <- c("log(TimeInDefSpell)", "DefSpell_Num_binned", "g0_Delinq_Lag_1",
                "M_Inflation_Growth_9", "g0_Delinq_Any_Aggr_Prop_Lag_1")
### NOTE: These variables reflect basic information relating to:
###         1) the time spent in default;
###         2) the account-level delinquency;
###         3) inflationary pressures within the macro environment; and
###         4) the degree of delinquency within the broader portfolio

# - Fit model
modLR_basic <- glm( as.formula(paste("DefSpell_Event ~", paste(vars_basic, collapse = " + "))),
                    data=datCredit_train, family="binomial")
summary(modLR_basic)
evalLR(modLR_basic, modLR_base, datCredit_train, targetFld="DefSpell_Event", predClass=1)
### RESULTS: AIC: 180 893;  McFadden R^2: 4.08%; AUC:  68.85%

# - Variable significance using robust SEs
robust_se <- vcovHC(modLR_basic, type="HC0")
coeftest(modLR_basic, vcov.=robust_se)
### RESULTS: All variables are significant

# - Test goodness-of-fit using AIC-measure over single-factor models
(aicTable_CoxDisc_basic <- aicTable(datCredit_train, vars_basic,
                                    TimeDef=c("Cox_Discrete","DefSpell_Event"), genPath=genObjPath, modelType="Cox_Discrete"))
### RESULTS: Top variables (ranked in order): [g0_Delinq_Lag_1]; [[log(TimeInDefSpell)]]; [DefSpell_Num_binned];
###                                           [M_Inflation_Growth_9]; [g0_Delinq_Any_Aggr_Prop_Lag_1] 

# - Test accuracy using c-statistic over single-factor models
(concTable_CoxDisc_basic <- concTable(datCredit_train, datCredit_valid, vars_basic,
                                      TimeDef=c("Cox_Discrete","DefSpell_Event"), genPath=genObjPath, modelType="Cox_Discrete"))
### RESULTS: Top variables (ranked in order): [log(TimeInDefSpell)]; [[g0_Delinq_Lag_1]; [DefSpell_Num_binned];
###                                           [M_Inflation_Growth_9]; [g0_Delinq_Any_Aggr_Prop_Lag_1]

# - Combine results into a single object
(Table_CoxDisc_basic <- concTable_CoxDisc_basic[,1:2] %>% left_join(aicTable_CoxDisc_basic, by ="Variable"))

# - Save objects
# Model analytics
pack.ffdf(paste0(genObjPath,"CoxDisc_basic_fits"), Table_CoxDisc_basic)
# Modeling object
saveRDS(modLR_basic, file=paste0(genObjPath,"CoxDisc_Basic_Model.rds"))




# ============ 3. Cleanup ============

cleanEnviron()



