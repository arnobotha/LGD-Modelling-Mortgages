# ========================= TIME-DEPENDENT ROC-ANALYSIS ==========================
# Compare various functions from various packages in conducting time-dependent 
# ROC-analyses on the same fitted Cox regression model, having used the 
# prepared credit data
# --------------------------------------------------------------------------------
# PROJECT TITLE: Default Survival Modelling
# SCRIPT AUTHOR(S): Dr Arno Botha (AB)
# --------------------------------------------------------------------------------
# -- Script dependencies:
#   - 0.Setup.R
#   - 1.Data_Import.R
#   - 2a.Data_Prepare_Credit_Basic.R
#   - 2b.Data_Prepare_Credit_Advanced.R
#   - 2c.Data_Prepare_Credit_Advanced2.R
#   - 2d.Data_Enrich.R
#   - 2f.Data_Fusion1.R
#   - 3b.Data_Fusion2_PWP_ST.R
#   - 5a(i).CoxPropHaz_PWP_Advanced.R
#   - 5a(i).CoxPropHaz_PWP_Basic.R
#   - 5b(i).CoxDiscreteTime_Advanced.R
#   - 5b(ii).CoxDiscreteTime_Basic.R

# -- Inputs:
#   - datCredit_train_TFD | Prepared from script 3b
#   - datCredit_valid_TFD | Prepared from script 3b
#
# -- Outputs:
#   - <Analytics> | tROC-graphs
# ================================================================================




# ----------------- 1. Load & prepare data for tROC-analyses

# ------ Prentice-Williams-Peterson (PWP) Spell-time definition
# - Confirm prepared datasets are loaded into memory
if (!exists('datCredit_train_CDH')) unpack.ffdf(paste0(genPath,"creditdata_train_CDH"), tempPath);gc()
if (!exists('datCredit_valid_CDH')) unpack.ffdf(paste0(genPath,"creditdata_valid_CDH"), tempPath);gc()

# - Use only default spells
datCredit_train <- datCredit_train_CDH[!is.na(DefSpell_Key),]
datCredit_valid <- datCredit_valid_CDH[!is.na(DefSpell_Key),]

# Create start and stop columns
datCredit_train[, Start := TimeInDefSpell - 1]
datCredit_valid[, Start := TimeInDefSpell - 1]

# - Weigh default cases heavier. as determined interactively based on calibration success (script 6e)
datCredit_train[, Weight := ifelse(DefSpell_Event==1,1,1)]

# ----------------- 2b. Fit a discrete-time hazard model on the resampled prepared data


# ------ Prentice-Williams-Peterson (PWP) Spell-time definition | Basic discrete-time hazard model
# - Initialize variables
vars_basic <- c("log(TimeInDefSpell)*DefSpell_Num_binned","M_DTI_Growth_12",
                "Balance_adj_WOff","InterestRate_Nom")

# - Fit discrete-time hazard model with selected variables
modLR_basic <- glm( as.formula(paste("DefSpell_Event ~", paste(vars_basic, collapse = " + "))),
                    data=datCredit_train, family="binomial", weights = Weight)



# ------ Prentice-Williams-Peterson (PWP) Spell-time definition | Advanced discrete-time hazard model
# - Initialize variables
vars <- c("Time_Binned","log(TimeInDefSpell)*DefSpell_Num_binned", 
          "DefaultStatus1_Aggr_Prop_Lag_12","g0_Delinq_Ave", "g0_Delinq_Lag_1",
          "InterestRate_Margin_Aggr_Med_9","NewLoans_Aggr_Prop","InterestRate_Nom",
          "Balance_adj_WOff","pmnt_method_grp","Principal",
          "M_RealIncome_Growth_9", "M_Inflation_Growth_12","M_DTI_Growth_12","M_Repo_Rate_12","g0_Delinq_Any_Aggr_Prop_Lag_1")

# - Fit discrete-time hazard model with selected variables
modLR <- glm( as.formula(paste("DefSpell_Event ~", paste(vars, collapse = " + "))),
              data=datCredit_train, family="binomial", weights = Weight)




# ------ Prentice-Williams-Peterson (PWP) Spell-time definition | Basic discrete-time hazard model

# --- Package: tROCkit() | custom "package"/function
# NOTE: Using custom tROC()-function from script 0b(iii) under the CD-approach with an NN-estimator and 0/1-kernel

# -- Multi-threaded calculation of the # AUC from given start up to given prediction time 3 in following the CD-approach
# NOTE: Uses the superior Nearest Neighbour Estimator (NNE) method for S(t) with a 0/1-kernelNNE-kernel for S(t)
# NOTE2: Assume dependence (by specifying ID-field) amongst certain observations clustered around ID-values
ptm <- proc.time() #IGNORE: for computation time calculation;
predictTime <- 6
objROC1_CDH_CoxDisc_bas <- tROC.multi(datGiven=datCredit_valid, modGiven=modLR_basic, month_End=predictTime, sLambda=0.05, estMethod="NN-0/1", numDigits=4, 
                                        fld_ID="DefSpell_Key", fld_Event="DefSpell_Event", eventVal=1, fld_StartTime="Start", fld_EndTime="TimeInDefSpell",
                                        graphName="WOffSurvModel-ROC_CoxDisc_Basic_TimeVar", genFigPathGiven=paste0(genFigPath, "tROC-Analyses/"), 
                                        caseStudyName=paste0("CoxDisc_CDH_", predictTime), numThreads=12, logPath=genPath, 
                                        predType="response")
objROC1_CDH_CoxDisc_bas$AUC; objROC1_CDH_CoxDisc_bas$ROC_graph
proc.time() - ptm
### RESULTS: AUC up to t: 93.76%, achieved in 754.31 secs

