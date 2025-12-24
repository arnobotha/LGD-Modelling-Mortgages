# ========================= TIME-DEPENDENT ROC-ANALYSIS ========================
# Compare various functions from various packages in conducting time-dependent 
# ROC-analyses on the same fitted Cox regression model, having used the 
# prepared credit data
# ------------------------------------------------------------------------------
# PROJECT TITLE: Default Survival Modelling
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
#   - 4b(i).InputSpace_DiscreteCox.R
#   - 4b(ii).InputSpace_DiscreteCox_Basic.R
#   - 4c.InputSpace_LogisticRegression.R
#   - 4e.Dichotomisation
#
# -- Inputs:
#   - datCredit_train_CDH | Prepared from script 2g
#   - datCredit_valid_CDH | Prepared from script 2g
#   - modLR_Bas | Basic discrete time model as fitted in script 4b(ii)
#   - modLR_Adv | Advanced discrete time model as fitted in script 4b(i)
#   - modLR_Classic | Classical logistic regression model as fitted in script 4c
#   - thres_lst | Thresholds for classifying predictions as determined in script 4e
# -- Outputs:
#   - <Analytics> | tROC-graphs
# ==============================================================================


# ------ 1. Prelimanaries

# --- 1.1 Load and prepare data
# - Confirm prepared datasets are loaded into memory
if (!exists('datCredit_train_CDH')) unpack.ffdf(paste0(genPath,"creditdata_train_CDH"), tempPath);gc()
if (!exists('datCredit_valid_CDH')) unpack.ffdf(paste0(genPath,"creditdata_valid_CDH"), tempPath);gc()

# - Use only default spells
datCredit_train <- datCredit_train_CDH[!is.na(DefSpell_Key),]
datCredit_valid <- datCredit_valid_CDH[!is.na(DefSpell_Key),]

# Create start and stop columns
datCredit_train[, Start:=TimeInDefSpell-1]
datCredit_valid[, Start:=TimeInDefSpell-1]

# - Weigh default cases heavier. as determined interactively based on calibration success (script 6e)
datCredit_train[, Weight:=ifelse(DefSpell_Event==1,1,1)]
datCredit_valid[, Weight:=ifelse(DefSpell_Event==1,1,1)]

# - Score data using classic model for each instance of [TimeInDefSpell] as [DefSpell_Age]
datCredit_train[, DefSpell_Age2:=DefSpell_Age]; datCredit_train[, DefSpell_Age:=TimeInDefSpell]
datCredit_valid[, DefSpell_Age2:=DefSpell_Age]; datCredit_valid[, DefSpell_Age:=TimeInDefSpell]

# - Combine training and validation dataset to enable more smooth graphs
datCredit <- rbind(datCredit_train, datCredit_valid)


# --- 1.2 Load models
# - Basic discrete-time hazard model
modLR_Bas <- readRDS(paste0(genObjPath,"CoxDisc_Basic_Model.rds"))

# - Advanced discrete-time hazard model
modLR_Adv <- readRDS(paste0(genObjPath,"CoxDisc_Advanced_Model.rds"))

# - Classical logit model
modLR_Classic <- readRDS(paste0(genObjPath,"LR_Model.rds"))


# --- 1.3 Additional parameters
# - Youden Index cut-offs
# Load thresholds
thresh_lst <- readRDS(file=paste0(genObjPath,"Classification_Thresholds.rds"))
# Basic discrete-time model
(thresh_dth_bas <- thresh_lst[["Basic"]]) # 0.0525335
# Advanced discrete-time model
(thresh_dth_adv <- thresh_lst[["Advanced"]]) # 0.3894059
# Classical logit model
(thresh_classic <- thresh_lst[["Classical"]]) # 0.5652506


# --- 1.4 Estimate event rates to facilitate the application of dichotomisation
# - Predict hazard h(t) = P(T=t | T>= t) in discrete-time
datCredit[, Hazard_adv:=predict(modLR_Adv, newdata=datCredit, type = "response")]
datCredit[, Hazard_bas:=predict(modLR_Bas, newdata=datCredit, type = "response")]
datCredit[, Hazard_classic:=predict(modLR_Classic, newdata=.SD[], type="response")]

# - Derive survival probability S(t) = prod(1 - hazard)
datCredit[, Survival_adv:=cumprod(1-Hazard_adv), by=list(DefSpell_Key)]
datCredit[, Survival_bas:=cumprod(1-Hazard_bas), by=list(DefSpell_Key)]
datCredit[, Survival_classic:=cumprod(1-Hazard_classic), by=list(DefSpell_Key)]

# - Derive discrete density, or event probability f(t) = S(t-1) - S(t)
datCredit[, EventRate_adv:=shift(Survival_adv, type="lag", n=1, fill=1) - Survival_adv, by=list(DefSpell_Key)]
datCredit[, EventRate_bas:=shift(Survival_bas, type="lag", n=1, fill=1) - Survival_bas, by=list(DefSpell_Key)]
datCredit[, EventRate_classic:=shift(Survival_classic, type="lag", n=1, fill=1) - Survival_classic, by=list(DefSpell_Key)]




# ------ 2. tROC analyses | Basic discrete-time hazard model A (non-dichotomised)
### NOTE: - Using custom tROC()-function from script 0b(iii) under the CD-approach with an NN-estimator and 0/1-kernel
###       - Uses the superior Nearest Neighbour Estimator (NNE) method for S(t) with a 0/1-kernelNNE-kernel for S(t)
###       - Assume dependence (by specifying ID-field) amongst certain observations clustered around ID-values

# --- 2.1 tROC analyses using the CD-approach | Time-window chosen as first 6 months in default
predictTime <- 6
ptm <- proc.time() #IGNORE: for computation time calculation;
objROC6_CDH_CoxDisc_bas <- tROC.multi(datGiven=datCredit, modGiven=modLR_Bas, month_End=predictTime, sLambda=0.05, estMethod="NN-0/1", numDigits=4, 
                                      fld_ID="DefSpell_Key", fld_Event="DefSpell_Event", eventVal=1, fld_StartTime="Start", fld_EndTime="TimeInDefSpell",
                                      graphName="WOffSurvModel-ROC_CoxDisc_Basic_TimeVar", genFigPathGiven=paste0(genFigPath, "tROC-Analyses/"), 
                                      caseStudyName=paste0("CoxDisc_CDH_", predictTime), numThreads=12, logPath=genPath, 
                                      predType="response")
proc.time() - ptm
objROC6_CDH_CoxDisc_bas$AUC; objROC6_CDH_CoxDisc_bas$ROC_graph
### RESULTS: AUC up to t: 50.26%, achieved in 109 secs


# --- 2.2 tROC analyses using the CD-approach | Time-window chosen as first 12 months in default
predictTime <- 12
ptm <- proc.time() #IGNORE: for computation time calculation;
objROC12_CDH_CoxDisc_bas <- tROC.multi(datGiven=datCredit, modGiven=modLR_Bas, month_End=predictTime, sLambda=0.05, estMethod="NN-0/1", numDigits=4, 
                                       fld_ID="DefSpell_Key", fld_Event="DefSpell_Event", eventVal=1, fld_StartTime="Start", fld_EndTime="TimeInDefSpell",
                                       graphName="WOffSurvModel-ROC_CoxDisc_Basic_TimeVar", genFigPathGiven=paste0(genFigPath, "tROC-Analyses/"), 
                                       caseStudyName=paste0("CoxDisc_CDH_", predictTime), numThreads=12, logPath=genPath, 
                                       predType="response")
proc.time() - ptm
objROC12_CDH_CoxDisc_bas$AUC; objROC12_CDH_CoxDisc_bas$ROC_graph
### RESULTS: AUC up to t: 55.10%, achieved in  171 secs


# --- 2.3 tROC analyses using the CD-approach | Time-window chosen as first 24 months in default
predictTime <- 24
ptm <- proc.time() #IGNORE: for computation time calculation;
objROC24_CDH_CoxDisc_bas <- tROC.multi(datGiven=datCredit, modGiven=modLR_Bas, month_End=predictTime, sLambda=0.05, estMethod="NN-0/1", numDigits=4, 
                                       fld_ID="DefSpell_Key", fld_Event="DefSpell_Event", eventVal=1, fld_StartTime="Start", fld_EndTime="TimeInDefSpell",
                                       graphName="WOffSurvModel-ROC_CoxDisc_Basic_TimeVar", genFigPathGiven=paste0(genFigPath, "tROC-Analyses/"), 
                                       caseStudyName=paste0("CoxDisc_CDH_", predictTime), numThreads=12, logPath=genPath, 
                                       predType="response")
proc.time() - ptm
objROC24_CDH_CoxDisc_bas$AUC; objROC24_CDH_CoxDisc_bas$ROC_graph
### RESULTS: AUC up to t: 60.17%, achieved in 218 secs


# --- 2.4 tROC analyses using the CD-approach | Time-window chosen as first 48 months in default
predictTime <- 48
ptm <- proc.time() #IGNORE: for computation time calculation;
objROC48_CDH_CoxDisc_bas <- tROC.multi(datGiven=datCredit, modGiven=modLR_Bas, month_End=predictTime, sLambda=0.05, estMethod="NN-0/1", numDigits=4, 
                                       fld_ID="DefSpell_Key", fld_Event="DefSpell_Event", eventVal=1, fld_StartTime="Start", fld_EndTime="TimeInDefSpell",
                                       graphName="WOffSurvModel-ROC_CoxDisc_Basic_TimeVar", genFigPathGiven=paste0(genFigPath, "tROC-Analyses/"), 
                                       caseStudyName=paste0("CoxDisc_CDH_", predictTime), numThreads=12, logPath=genPath, 
                                       predType="response")
proc.time() - ptm
objROC48_CDH_CoxDisc_bas$AUC; objROC48_CDH_CoxDisc_bas$ROC_graph
### RESULTS: AUC up to t: 62.56%, achieved in 360 secs


# --- 2.5 Store experimental objects | Memory optimisation
# CDH-model: Basic Cox-regression model
saveRDS(objROC6_CDH_CoxDisc_bas, paste0(genPath,"WOffSurvModel-CoxDisc-CDH-ROC_Depedendence_06_bas.rds"))
saveRDS(objROC12_CDH_CoxDisc_bas, paste0(genPath,"WOffSurvModel-CoxDisc-CDH-ROC_Depedendence_12_bas.rds"))
saveRDS(objROC24_CDH_CoxDisc_bas, paste0(genPath,"WOffSurvModel-CoxDisc-CDH-ROC_Depedendence_24_bas.rds"))
saveRDS(objROC48_CDH_CoxDisc_bas, paste0(genPath,"WOffSurvModel-CoxDisc-CDH-ROC_Depedendence_48_bas.rds"))




# ------ 3. tROC analyses | Advanced discrete-time hazard model A (non-dichotomised)
### NOTE: - Using custom tROC()-function from script 0b(iii) under the CD-approach with an NN-estimator and 0/1-kernel
###       - Uses the superior Nearest Neighbour Estimator (NNE) method for S(t) with a 0/1-kernelNNE-kernel for S(t)
###       - Assume dependence (by specifying ID-field) amongst certain observations clustered around ID-values

# --- 3.1 tROC analyses using the CD-approach | Time-window chosen as first 6 months in default
predictTime <- 6
ptm <- proc.time() #IGNORE: for computation time calculation;
objROC6_CDH_CoxDisc_adv <- tROC.multi(datGiven=datCredit, modGiven=modLR_Adv, month_End=predictTime, sLambda=0.05, estMethod="NN-0/1", numDigits=4, 
                                      fld_ID="DefSpell_Key", fld_Event="DefSpell_Event", eventVal=1, fld_StartTime="Start", fld_EndTime="TimeInDefSpell",
                                      graphName="WOffSurvModel-ROC_CoxDisc_Advanced_TimeVar", genFigPathGiven=paste0(genFigPath, "tROC-Analyses/"), 
                                      caseStudyName=paste0("CoxDisc_CDH_", predictTime), numThreads=12, logPath=genPath, 
                                      predType="response")
proc.time() - ptm
objROC6_CDH_CoxDisc_adv$AUC; objROC6_CDH_CoxDisc_adv$ROC_graph
### RESULTS: AUC up to t: 94.56%, achieved in 895 secs


# --- 3.2 tROC analyses using the CD-approach | Time-window chosen as first 12 months in default
predictTime <- 12
ptm <- proc.time() #IGNORE: for computation time calculation;
objROC12_CDH_CoxDisc_adv <- tROC.multi(datGiven=datCredit, modGiven=modLR_Adv, month_End=predictTime, sLambda=0.05, estMethod="NN-0/1", numDigits=4, 
                                       fld_ID="DefSpell_Key", fld_Event="DefSpell_Event", eventVal=1, fld_StartTime="Start", fld_EndTime="TimeInDefSpell",
                                       graphName="WOffSurvModel-ROC_CoxDisc_Advanced_TimeVar", genFigPathGiven=paste0(genFigPath, "tROC-Analyses/"), 
                                       caseStudyName=paste0("CoxDisc_CDH_", predictTime), numThreads=12, logPath=genPath, 
                                       predType="response")
proc.time() - ptm
objROC12_CDH_CoxDisc_adv$AUC; objROC12_CDH_CoxDisc_adv$ROC_graph
### RESULTS: AUC up to t: 94.97%, achieved in 1256  secs


# --- 3.3 tROC analyses using the CD-approach | Time-window chosen as first 24 months in default
predictTime <- 24
ptm <- proc.time() #IGNORE: for computation time calculation;
objROC24_CDH_CoxDisc_adv <- tROC.multi(datGiven=datCredit, modGiven=modLR_Adv, month_End=predictTime, sLambda=0.05, estMethod="NN-0/1", numDigits=4, 
                                       fld_ID="DefSpell_Key", fld_Event="DefSpell_Event", eventVal=1, fld_StartTime="Start", fld_EndTime="TimeInDefSpell",
                                       graphName="WOffSurvModel-ROC_CoxDisc_Advanced_TimeVar", genFigPathGiven=paste0(genFigPath, "tROC-Analyses/"), 
                                       caseStudyName=paste0("CoxDisc_CDH_", predictTime), numThreads=12, logPath=genPath, 
                                       predType="response")
proc.time() - ptm
objROC24_CDH_CoxDisc_adv$AUC; objROC24_CDH_CoxDisc_adv$ROC_grap
### RESULTS: AUC up to t: 96.57%, achieved in 2053 secs


# --- 3.4 tROC analyses using the CD-approach | Time-window chosen as first 48 months in default
predictTime <- 48
ptm <- proc.time() #IGNORE: for computation time calculation;
objROC48_CDH_CoxDisc_adv <- tROC.multi(datGiven=datCredit, modGiven=modLR_Adv, month_End=predictTime, sLambda=0.05, estMethod="NN-0/1", numDigits=4, 
                                       fld_ID="DefSpell_Key", fld_Event="DefSpell_Event", eventVal=1, fld_StartTime="Start", fld_EndTime="TimeInDefSpell",
                                       graphName="WOffSurvModel-ROC_CoxDisc_Advanced_TimeVar", genFigPathGiven=paste0(genFigPath, "tROC-Analyses/"), 
                                       caseStudyName=paste0("CoxDisc_CDH_", predictTime), numThreads=12, logPath=genPath, 
                                       predType="response")
proc.time() - ptm
objROC48_CDH_CoxDisc_adv$AUC; objROC48_CDH_CoxDisc_adv$ROC_graph
### RESULTS: AUC up to t: 96.90%, achieved in 2053 secs


# --- 3.5 Store experimental objects | Memory optimisation
# CDH-model: Basic Cox-regression model
saveRDS(objROC6_CDH_CoxDisc_adv, paste0(genPath,"WOffSurvModel-CoxDisc-CDH-ROC_Depedendence_06_adv.rds"))
saveRDS(objROC12_CDH_CoxDisc_adv, paste0(genPath,"WOffSurvModel-CoxDisc-CDH-ROC_Depedendence_12_adv.rds"))
saveRDS(objROC24_CDH_CoxDisc_adv, paste0(genPath,"WOffSurvModel-CoxDisc-CDH-ROC_Depedendence_24_adv.rds"))
saveRDS(objROC48_CDH_CoxDisc_adv, paste0(genPath,"WOffSurvModel-CoxDisc-CDH-ROC_Depedendence_48_adv.rds"))




# ------ 4. tROC analyses | Basic discrete-time hazard model B (dichotomised)
### NOTE: - Using custom tROC()-function from script 0b(iii) under the CD-approach with an NN-estimator and 0/1-kernel
###       - Uses the superior Nearest Neighbour Estimator (NNE) method for S(t) with a 0/1-kernelNNE-kernel for S(t)
###       - Assume dependence (by specifying ID-field) amongst certain observations clustered around ID-values

# --- 4.1 tROC analyses using the CD-approach | Time-window chosen as first 6 months in default
predictTime <- 6
ptm <- proc.time() #IGNORE: for computation time calculation;
objROC6_CDH_CoxDisc_bas_B <- tROC.multi(datGiven=datCredit, modGiven=modLR_Bas, month_End=predictTime, sLambda=0.05, estMethod="NN-0/1", numDigits=4, 
                                        fld_ID="DefSpell_Key", fld_Event="DefSpell_Event", eventVal=1, fld_StartTime="Start", fld_EndTime="TimeInDefSpell",
                                        graphName="WOffSurvModel-ROC_CoxDisc_Basic_TimeVar_Dichotomised", genFigPathGiven=paste0(genFigPath, "tROC-Analyses/"), 
                                        caseStudyName=paste0("CoxDisc_CDH_", predictTime), numThreads=12, logPath=genPath, 
                                        predType="response", MarkerGiven="EventRate_bas", threshold=thresh_dth_bas)
proc.time() - ptm
objROC6_CDH_CoxDisc_bas_B$AUC; objROC6_CDH_CoxDisc_bas_B$ROC_graph
### RESULTS: AUC up to t: 50%, achieved in 36 secs


# --- 4.2 tROC analyses using the CD-approach | Time-window chosen as first 12 months in default
predictTime <- 12
ptm <- proc.time() #IGNORE: for computation time calculation;
objROC12_CDH_CoxDisc_bas_B <- tROC.multi(datGiven=datCredit, modGiven=modLR_Bas, month_End=predictTime, sLambda=0.05, estMethod="NN-0/1", numDigits=4, 
                                       fld_ID="DefSpell_Key", fld_Event="DefSpell_Event", eventVal=1, fld_StartTime="Start", fld_EndTime="TimeInDefSpell",
                                       graphName="WOffSurvModel-ROC_CoxDisc_Basic_TimeVar_Dichotomised", genFigPathGiven=paste0(genFigPath, "tROC-Analyses/"), 
                                       caseStudyName=paste0("CoxDisc_CDH_", predictTime), numThreads=12, logPath=genPath, 
                                       predType="response", MarkerGiven="EventRate_bas", threshold=thresh_dth_bas)
proc.time() - ptm
objROC12_CDH_CoxDisc_bas_B$AUC; objROC12_CDH_CoxDisc_bas_B$ROC_graph
### RESULTS: AUC up to t: 50%, achieved in  34 secs


# --- 4.3 tROC analyses using the CD-approach | Time-window chosen as first 24 months in default
predictTime <- 24
ptm <- proc.time() #IGNORE: for computation time calculation;
objROC24_CDH_CoxDisc_bas_B <- tROC.multi(datGiven=datCredit, modGiven=modLR_Bas, month_End=predictTime, sLambda=0.05, estMethod="NN-0/1", numDigits=4, 
                                         fld_ID="DefSpell_Key", fld_Event="DefSpell_Event", eventVal=1, fld_StartTime="Start", fld_EndTime="TimeInDefSpell",
                                         graphName="WOffSurvModel-ROC_CoxDisc_Basic_TimeVar_Dichotomised", genFigPathGiven=paste0(genFigPath, "tROC-Analyses/"), 
                                         caseStudyName=paste0("CoxDisc_CDH_", predictTime), numThreads=12, logPath=genPath, 
                                         predType="response", MarkerGiven="EventRate_bas", threshold=thresh_dth_bas)
proc.time() - ptm
objROC24_CDH_CoxDisc_bas_B$AUC; objROC24_CDH_CoxDisc_bas_B$ROC_graph
### RESULTS: AUC up to t: 50%, achieved in 35 secs


# --- 4.4 tROC analyses using the CD-approach | Time-window chosen as first 48 months in default
predictTime <- 48
ptm <- proc.time() #IGNORE: for computation time calculation;
objROC48_CDH_CoxDisc_bas_B <- tROC.multi(datGiven=datCredit, modGiven=modLR_Bas, month_End=predictTime, sLambda=0.05, estMethod="NN-0/1", numDigits=4, 
                                         fld_ID="DefSpell_Key", fld_Event="DefSpell_Event", eventVal=1, fld_StartTime="Start", fld_EndTime="TimeInDefSpell",
                                         graphName="WOffSurvModel-ROC_CoxDisc_Basic_TimeVar_Dichotomised", genFigPathGiven=paste0(genFigPath, "tROC-Analyses/"), 
                                         caseStudyName=paste0("CoxDisc_CDH_", predictTime), numThreads=12, logPath=genPath, 
                                         predType="response",MarkerGiven="EventRate_bas", threshold=thresh_dth_bas)
proc.time() - ptm
objROC48_CDH_CoxDisc_bas_B$AUC; objROC48_CDH_CoxDisc_bas_B$ROC_graph
### RESULTS: AUC up to t: 50%, achieved in 35 secs


# --- 4.5 Store experimental objects | Memory optimisation
# CDH-model: Basic Cox-regression model
saveRDS(objROC6_CDH_CoxDisc_bas_B, paste0(genPath,"WOffSurvModel-CoxDisc-CDH-Dichotomised-ROC_Depedendence_06_bas.rds"))
saveRDS(objROC12_CDH_CoxDisc_bas_B, paste0(genPath,"WOffSurvModel-CoxDisc-CDH-Dichotomised-ROC_Depedendence_12_bas.rds"))
saveRDS(objROC24_CDH_CoxDisc_bas_B, paste0(genPath,"WOffSurvModel-CoxDisc-CDH-Dichotomised-ROC_Depedendence_24_bas.rds"))
saveRDS(objROC48_CDH_CoxDisc_bas_B, paste0(genPath,"WOffSurvModel-CoxDisc-CDH-Dichotomised-ROC_Depedendence_48_bas.rds"))




# ------ 5. tROC analyses | Advanced discrete-time hazard model B (dichotomised)
### NOTE: - Using custom tROC()-function from script 0b(iii) under the CD-approach with an NN-estimator and 0/1-kernel
###       - Uses the superior Nearest Neighbour Estimator (NNE) method for S(t) with a 0/1-kernelNNE-kernel for S(t)
###       - Assume dependence (by specifying ID-field) amongst certain observations clustered around ID-values

# --- 5.1 tROC analyses using the CD-approach | Time-window chosen as first 6 months in default
predictTime <- 6
ptm <- proc.time() #IGNORE: for computation time calculation;
objROC6_CDH_CoxDisc_adv_B <- tROC.multi(datGiven=datCredit, modGiven=modLR_Adv, month_End=predictTime, sLambda=0.05, estMethod="NN-0/1", numDigits=4, 
                                        fld_ID="DefSpell_Key", fld_Event="DefSpell_Event", eventVal=1, fld_StartTime="Start", fld_EndTime="TimeInDefSpell",
                                        graphName="WOffSurvModel-ROC_CoxDisc_Advanced_TimeVar_Dichotomised", genFigPathGiven=paste0(genFigPath, "tROC-Analyses/"), 
                                        caseStudyName=paste0("CoxDisc_CDH_", predictTime), numThreads=12, logPath=genPath, 
                                        predType="response", MarkerGiven="EventRate_adv", threshold=thresh_dth_adv)
proc.time() - ptm
objROC6_CDH_CoxDisc_adv_B$AUC; objROC6_CDH_CoxDisc_adv_B$ROC_graph
### RESULTS: AUC up to t: 50.18%, achieved in 33 secs


# --- 5.2 tROC analyses using the CD-approach | Time-window chosen as first 12 months in default
predictTime <- 12
ptm <- proc.time() #IGNORE: for computation time calculation;
objROC12_CDH_CoxDisc_adv_B <- tROC.multi(datGiven=datCredit, modGiven=modLR_Adv, month_End=predictTime, sLambda=0.05, estMethod="NN-0/1", numDigits=4, 
                                         fld_ID="DefSpell_Key", fld_Event="DefSpell_Event", eventVal=1, fld_StartTime="Start", fld_EndTime="TimeInDefSpell",
                                         graphName="WOffSurvModel-ROC_CoxDisc_Advanced_TimeVar_Dichotomised", genFigPathGiven=paste0(genFigPath, "tROC-Analyses/"), 
                                         caseStudyName=paste0("CoxDisc_CDH_", predictTime), numThreads=12, logPath=genPath, 
                                         predType="response", MarkerGiven="EventRate_adv", threshold=thresh_dth_adv)
proc.time() - ptm
objROC12_CDH_CoxDisc_adv_B$AUC; objROC12_CDH_CoxDisc_adv_B$ROC_graph
### RESULTS: AUC up to t: 50.42%, achieved in 34  secs


# --- 5.3 tROC analyses using the CD-approach | Time-window chosen as first 24 months in default
predictTime <- 24
ptm <- proc.time() #IGNORE: for computation time calculation;
objROC24_CDH_CoxDisc_adv_B <- tROC.multi(datGiven=datCredit, modGiven=modLR_Adv, month_End=predictTime, sLambda=0.05, estMethod="NN-0/1", numDigits=4, 
                                         fld_ID="DefSpell_Key", fld_Event="DefSpell_Event", eventVal=1, fld_StartTime="Start", fld_EndTime="TimeInDefSpell",
                                         graphName="WOffSurvModel-ROC_CoxDisc_Advanced_TimeVar_Dichotomised", genFigPathGiven=paste0(genFigPath, "tROC-Analyses/"), 
                                         caseStudyName=paste0("CoxDisc_CDH_", predictTime), numThreads=12, logPath=genPath, 
                                         predType="response", MarkerGiven="EventRate_Adv", threshold=thresh_dth_adv)
proc.time() - ptm
objROC24_CDH_CoxDisc_adv_B$AUC; objROC24_CDH_CoxDisc_adv_B$ROC_graph
### RESULTS: AUC up to t: 50.87%, achieved in 34 secs


# --- 5.4 tROC analyses using the CD-approach | Time-window chosen as first 48 months in default
predictTime <- 48
ptm <- proc.time() #IGNORE: for computation time calculation;
objROC48_CDH_CoxDisc_adv_B <- tROC.multi(datGiven=datCredit, modGiven=modLR_Adv, month_End=predictTime, sLambda=0.05, estMethod="NN-0/1", numDigits=4, 
                                         fld_ID="DefSpell_Key", fld_Event="DefSpell_Event", eventVal=1, fld_StartTime="Start", fld_EndTime="TimeInDefSpell",
                                         graphName="WOffSurvModel-ROC_CoxDisc_Advanced_TimeVar_Dichotomised", genFigPathGiven=paste0(genFigPath, "tROC-Analyses/"), 
                                         caseStudyName=paste0("CoxDisc_CDH_", predictTime), numThreads=12, logPath=genPath, 
                                         predType="response", MarkerGiven="EventRate_adv", threshold=thresh_dth_adv)
proc.time() - ptm
objROC48_CDH_CoxDisc_adv_B$AUC; objROC48_CDH_CoxDisc_adv_B$ROC_graph
### RESULTS: AUC up to t: 50.67%, achieved in 34 secs


# --- 5.5 Store experimental objects | Memory optimisation
# CDH-model: Basic Cox-regression model
saveRDS(objROC6_CDH_CoxDisc_adv_B, paste0(genPath,"WOffSurvModel-CoxDisc-CDH-Dichotomised-ROC_Depedendence_06_adv.rds"))
saveRDS(objROC12_CDH_CoxDisc_adv_B, paste0(genPath,"WOffSurvModel-CoxDisc-CDH-Dichotomised-ROC_Depedendence_12_adv.rds"))
saveRDS(objROC24_CDH_CoxDisc_adv_B, paste0(genPath,"WOffSurvModel-CoxDisc-CDH-Dichotomised-ROC_Depedendence_24_adv.rds"))
saveRDS(objROC48_CDH_CoxDisc_adv_B, paste0(genPath,"WOffSurvModel-CoxDisc-CDH-Dichotomised-ROC_Depedendence_48_adv.rds"))




# ------ 6. Create combined ROC-graphs across multiple prediction times

# --- 6.1 Basic discrete-time hazard model | A series (non-dichotomised)
# - Ensure required objects exist in memory
if (!exists('objROC6_CDH_CoxDisc_bas')) objROC6_CDH_CoxDisc_bas <- readRDS(paste0(genPath,"WOffSurvModel-CoxDisc-CDH-ROC_Depedendence_06_bas.rds")); gc()
if (!exists('objROC12_CDH_CoxDisc_bas')) objROC12_CDH_CoxDisc_bas <- readRDS(paste0(genPath,"WOffSurvModel-CoxDisc-CDH-ROC_Depedendence_12_bas.rds")); gc()
if (!exists('objROC24_CDH_CoxDisc_bas')) objROC24_CDH_CoxDisc_bas <- readRDS(paste0(genPath,"WOffSurvModel-CoxDisc-CDH-ROC_Depedendence_24_bas.rds")); gc()
if (!exists('objROC48_CDH_CoxDisc_bas')) objROC48_CDH_CoxDisc_bas <- readRDS(paste0(genPath,"WOffSurvModel-CoxDisc-CDH-ROC_Depedendence_48_bas.rds")); gc()

# - Set ROC-parameters and initialize data structures
vecPercTimepoint <- c(6,12,24,48)
vecTROC <- list(objROC6_CDH_CoxDisc_bas, objROC12_CDH_CoxDisc_bas,
                objROC24_CDH_CoxDisc_bas, objROC48_CDH_CoxDisc_bas)
vLabels <- vector("list", length=length(vecPercTimepoint))

# - Create a combined data object for plotting purposes
for (i in 1:length(vecPercTimepoint)) {
  # i <-1 # testing condition
  # datGraph <- data.frame(x = vFPR[-(nThresh+1)], y=vTPR[-1])
  
  # - Create a data object for the current prediction time
  if (i == 1) {
    datGraph <- data.table(PredictTime=paste0(letters[i], "_", vecPercTimepoint[i]), Threshold=vecTROC[[i]]$Thresholds, 
                           x=vecTROC[[i]]$FPR, y=vecTROC[[i]]$TPR)
  } else {
    datGraph <- rbind(datGraph, 
                      data.table(PredictTime= paste0(letters[i], "_", vecPercTimepoint[i]), Threshold=vecTROC[[i]]$Thresholds, 
                                 x=vecTROC[[i]]$FPR, y=vecTROC[[i]]$TPR))
  }
  vLabels[[i]] <- bquote("Prediction time "*italic(t)==.(vecPercTimepoint[i])*"; AUC: "*.(percent(vecTROC[[i]]$AUC, accuracy=0.01)))
}

# - Graph a combined ROC-graph across prediction times t
# Aesthetic parameters
datGraph[, FacetLabel:="Basic discrete-time hazard model - A"]
vCol <- brewer.pal(8,"Set1")
vLabels_F <- setNames(vLabels, paste0(letters[1:length(vecPercTimepoint)],"_", vecPercTimepoint))
chosenFont <- "Cambria"
# Create ROC-graph
(gg <- ggplot(datGraph, aes(x=x,y=y,group=PredictTime)) + theme_minimal() + 
    theme(text = element_text(family=chosenFont), legend.position="inside", 
          strip.background=element_rect(fill="snow2", colour="snow2"),
          strip.text=element_text(size=8, colour="gray50"), strip.text.y.right=element_text(angle=90),
          legend.position.inside = c(0.65,0.25),
          legend.background = element_rect(fill="snow2", color="black",
                                           linetype="solid", linewidth=0.1)) +
    labs(x = bquote("False Positive Rate "*italic(F^"+")), y = 
           bquote("True Positive Rate "*italic(T^"+"))) + 
    # Add 45-degree line
    geom_segment(x = 0, y = 0, xend = 1, yend = 1, color = "grey", linewidth=0.2) +
    # Main line graph
    geom_step(aes(x=x, y=y, linetype=PredictTime, colour=PredictTime), linewidth=0.5) + 
    #geom_point(aes(x=x, y=y, shape=PredictTime, colour=PredictTime), size=0.25) +
    # Facets and scales
    facet_grid(FacetLabel ~ .) +  
    scale_color_manual(name=bquote("ROC"*(italic(t))), values=vCol, labels=vLabels) + 
    scale_linetype_discrete(name=bquote("ROC"*(italic(t))), labels=vLabels) + 
    scale_shape_discrete(name=bquote("ROC"*(italic(t))), labels=vLabels) + 
    scale_y_continuous(label=percent) + scale_x_continuous(label=percent))

# - Save graph
dpi <- 400
ggsave(gg, file=paste0(paste0(genFigPath,"/tROC-Analyses/", "WOffSurvModel-CoxDisc-CDH-CombinedROC_Depedendence_bas.png")), 
       width=1800/dpi, height=1500/dpi, dpi=dpi, bg="white")


# --- 6.2 Advanced discrete-time hazard model | A series (non-dichotomised)
# - Ensure required objects exist in memory
if (!exists('objROC6_CDH_CoxDisc_adv')) objROC6_CDH_CoxDisc_adv <- readRDS(paste0(genPath,"WOffSurvModel-CoxDisc-CDH-ROC_Depedendence_06_adv.rds")); gc()
if (!exists('objROC12_CDH_CoxDisc_adv')) objROC12_CDH_CoxDisc_adv <- readRDS(paste0(genPath,"WOffSurvModel-CoxDisc-CDH-ROC_Depedendence_12_adv.rds")); gc()
if (!exists('objROC24_CDH_CoxDisc_adv')) objROC24_CDH_CoxDisc_adv <- readRDS(paste0(genPath,"WOffSurvModel-CoxDisc-CDH-ROC_Depedendence_24_adv.rds")); gc()
if (!exists('objROC48_CDH_CoxDisc_adv')) objROC48_CDH_CoxDisc_adv <- readRDS(paste0(genPath,"WOffSurvModel-CoxDisc-CDH-ROC_Depedendence_48_adv.rds")); gc()

# - Set ROC-parameters and initialize data structures
vecPercTimepoint <- c(6,12,24,48)
vecTROC <- list(objROC6_CDH_CoxDisc_adv, objROC12_CDH_CoxDisc_adv,
                objROC24_CDH_CoxDisc_adv, objROC48_CDH_CoxDisc_adv)
vLabels <- vector("list", length=length(vecPercTimepoint))

# - Create a combined data object for plotting purposes
for (i in 1:length(vecPercTimepoint)) {
  # i <-1 # testing condition
  # datGraph <- data.frame(x = vFPR[-(nThresh+1)], y=vTPR[-1])
  
  # Create a data object for the current prediction time
  if (i == 1) {
    datGraph <- data.table(PredictTime=paste0(letters[i], "_", vecPercTimepoint[i]), Threshold=vecTROC[[i]]$Thresholds, 
                           x=vecTROC[[i]]$FPR, y=vecTROC[[i]]$TPR)
    
  } else {
    datGraph <- rbind(datGraph, 
                      data.table(PredictTime= paste0(letters[i], "_", vecPercTimepoint[i]), Threshold=vecTROC[[i]]$Thresholds, 
                                 x=vecTROC[[i]]$FPR, y=vecTROC[[i]]$TPR))
  }
  vLabels[[i]] <- bquote("Prediction time "*italic(t)==.(vecPercTimepoint[i])*"; AUC: "*.(percent(vecTROC[[i]]$AUC, accuracy=0.01)))
}

# - Graph a combined ROC-graph across prediction times t
#  Aesthetic parameters
datGraph[, FacetLabel := "Advanced discrete-time hazard model - A"]
vCol <- brewer.pal(8,"Set1")
vLabels_F <- setNames(vLabels, paste0(letters[1:length(vecPercTimepoint)],"_", vecPercTimepoint))
chosenFont <- "Cambria"
# Create ROC-graph
(gg <- ggplot(datGraph, aes(x=x,y=y,group=PredictTime)) + theme_minimal() + 
    theme(text = element_text(family=chosenFont), legend.position="inside", 
          strip.background=element_rect(fill="snow2", colour="snow2"),
          strip.text=element_text(size=8, colour="gray50"), strip.text.y.right=element_text(angle=90),
          legend.position.inside = c(0.55,0.45),
          legend.background = element_rect(fill="snow2", color="black",
                                           linetype="solid", linewidth=0.1)) +
    labs(x = bquote("False Positive Rate "*italic(F^"+")), y = 
           bquote("True Positive Rate "*italic(T^"+"))) + 
    # Add 45-degree line
    geom_segment(x = 0, y = 0, xend = 1, yend = 1, color = "grey", linewidth=0.2) +
    # Main line graph
    geom_step(aes(x=x, y=y, linetype=PredictTime, colour=PredictTime), linewidth=0.5) + 
    #geom_point(aes(x=x, y=y, shape=PredictTime, colour=PredictTime), size=0.25) +
    # Facets and scales
    facet_grid(FacetLabel ~ .) +  
    scale_color_manual(name=bquote("ROC"*(italic(t))), values=vCol, labels=vLabels) + 
    scale_linetype_discrete(name=bquote("ROC"*(italic(t))), labels=vLabels) + 
    scale_shape_discrete(name=bquote("ROC"*(italic(t))), labels=vLabels) + 
    scale_y_continuous(label=percent) + scale_x_continuous(label=percent))

# - Save graph
dpi <- 300
ggsave(gg, file=paste0(paste0(genFigPath,"/tROC-Analyses/", "WOffSurvModel-CoxDisc-CDH-CombinedROC_Depedendence_adv.png")), 
       width=1200/dpi, height=1000/dpi, dpi=dpi, bg="white")

# - cleanup
suppressWarnings( rm(gg, vLabels, vLabels_F, vecTROC, datGraph, dat, 
                     objROC1_CDH_bas, objROC2_CDH_bas, objROC3_CDH_bas, objROC4_CDH_bas,
                     objROC1_CDH_adv, objROC2_CDH_adv, objROC3_CDH_adv, objROC4_CDH_adv,
                     cox_CDH_adv, cox_CDH_basic, modLR_Adv, modLR_Bas, 
                     objROC1_CDH_CoxDisc_adv, objROC2_CDH_CoxDisc_adv, objROC3_CDH_CoxDisc_adv, objROC4_CDH_CoxDisc_adv,
                     objROC1_CDH_CoxDisc_bas, objROC2_CDH_CoxDisc_bas, objROC3_CDH_CoxDisc_bas, objROC4_CDH_CoxDisc_bas,
                     datCredit_train_CDH, datCredit_valid_CDH, datCredit_train, datCredit
) )


# --- 6.3 Basic discrete-time hazard model | A series (dichotomised)
# - Ensure required objects exist in memory
if (!exists('objROC6_CDH_CoxDisc_bas_B')) objROC6_CDH_CoxDisc_bas <- readRDS(paste0(genPath,"WOffSurvModel-CoxDisc-CDH-Dichotomised-ROC_Depedendence_06_bas.rds")); gc()
if (!exists('objROC12_CDH_CoxDisc_bas_B')) objROC12_CDH_CoxDisc_bas <- readRDS(paste0(genPath,"WOffSurvModel-CoxDisc-CDH-Dichotomised-ROC_Depedendence_12_bas.rds")); gc()
if (!exists('objROC24_CDH_CoxDisc_bas_B')) objROC24_CDH_CoxDisc_bas <- readRDS(paste0(genPath,"WOffSurvModel-CoxDisc-CDH-Dichotomised-ROC_Depedendence_24_bas.rds")); gc()
if (!exists('objROC48_CDH_CoxDisc_bas_B')) objROC48_CDH_CoxDisc_bas <- readRDS(paste0(genPath,"WOffSurvModel-CoxDisc-CDH-Dichotomised-ROC_Depedendence_48_bas.rds")); gc()

# - Set ROC-parameters and initialize data structures
vecPercTimepoint <- c(6,12,24,48)
vecTROC <- list(objROC6_CDH_CoxDisc_bas_B, objROC12_CDH_CoxDisc_bas_B,
                objROC24_CDH_CoxDisc_bas_B, objROC48_CDH_CoxDisc_bas_B)
vLabels <- vector("list", length=length(vecPercTimepoint))

# - Create a combined data object for plotting purposes
for (i in 1:length(vecPercTimepoint)) {
  # i <-1 # testing condition
  # datGraph <- data.frame(x = vFPR[-(nThresh+1)], y=vTPR[-1])
  
  # - Create a data object for the current prediction time
  if (i == 1) {
    datGraph <- data.table(PredictTime=paste0(letters[i], "_", vecPercTimepoint[i]), Threshold=vecTROC[[i]]$Thresholds, 
                           x=vecTROC[[i]]$FPR, y=vecTROC[[i]]$TPR)
  } else {
    datGraph <- rbind(datGraph, 
                      data.table(PredictTime= paste0(letters[i], "_", vecPercTimepoint[i]), Threshold=vecTROC[[i]]$Thresholds, 
                                 x=vecTROC[[i]]$FPR, y=vecTROC[[i]]$TPR))
  }
  vLabels[[i]] <- bquote("Prediction time "*italic(t)==.(vecPercTimepoint[i])*"; AUC: "*.(percent(vecTROC[[i]]$AUC, accuracy=0.01)))
}

# - Graph a combined ROC-graph across prediction times t
# Aesthetic parameters
datGraph[, FacetLabel:="Basic discrete-time hazard model - B"]
vCol <- brewer.pal(8,"Set1")
vLabels_F <- setNames(vLabels, paste0(letters[1:length(vecPercTimepoint)],"_", vecPercTimepoint))
chosenFont <- "Cambria"
# Create ROC-graph
(gg <- ggplot(datGraph, aes(x=x,y=y,group=PredictTime)) + theme_minimal() + 
    theme(text = element_text(family=chosenFont), legend.position="inside", 
          strip.background=element_rect(fill="snow2", colour="snow2"),
          strip.text=element_text(size=8, colour="gray50"), strip.text.y.right=element_text(angle=90),
          legend.position.inside = c(0.75,0.25),
          legend.background = element_rect(fill="snow2", color="black",
                                           linetype="solid", linewidth=0.1)) +
    labs(x = bquote("False Positive Rate "*italic(F^"+")), y = 
           bquote("True Positive Rate "*italic(T^"+"))) + 
    # Add 45-degree line
    geom_segment(x = 0, y = 0, xend = 1, yend = 1, color = "grey", linewidth=0.2) +
    # Main line graph
    geom_step(aes(x=x, y=y, linetype=PredictTime, colour=PredictTime), linewidth=0.5) + 
    #geom_point(aes(x=x, y=y, shape=PredictTime, colour=PredictTime), size=0.25) +
    # Facets and scales
    facet_grid(FacetLabel ~ .) +  
    scale_color_manual(name=bquote("ROC"*(italic(t))), values=vCol, labels=vLabels) + 
    scale_linetype_discrete(name=bquote("ROC"*(italic(t))), labels=vLabels) + 
    scale_shape_discrete(name=bquote("ROC"*(italic(t))), labels=vLabels) + 
    scale_y_continuous(label=percent) + scale_x_continuous(label=percent))

# - Save graph
dpi <- 300
ggsave(gg, file=paste0(paste0(genFigPath,"/tROC-Analyses/", "WOffSurvModel-CoxDisc-CDH-Dichotomised-CombinedROC_Depedendence_bas.png")), 
       width=1800/dpi, height=1500/dpi, dpi=dpi, bg="white")


# --- 6.4 Advanced discrete-time hazard model | B series (dichotomised)
# - Ensure required objects exist in memory
if (!exists('objROC6_CDH_CoxDisc_adv_B')) objROC6_CDH_CoxDisc_adv <- readRDS(paste0(genPath,"WOffSurvModel-CoxDisc-CDH-Dichotomised-ROC_Depedendence_06_adv.rds")); gc()
if (!exists('objROC12_CDH_CoxDisc_adv_B')) objROC12_CDH_CoxDisc_adv <- readRDS(paste0(genPath,"WOffSurvModel-CoxDisc-CDH-Dichotomised-ROC_Depedendence_12_adv.rds")); gc()
if (!exists('objROC24_CDH_CoxDisc_adv_B')) objROC24_CDH_CoxDisc_adv <- readRDS(paste0(genPath,"WOffSurvModel-CoxDisc-CDH-Dichotomised-ROC_Depedendence_24_adv.rds")); gc()
if (!exists('objROC48_CDH_CoxDisc_adv_B')) objROC48_CDH_CoxDisc_adv <- readRDS(paste0(genPath,"WOffSurvModel-CoxDisc-CDH-Dichotomised-ROC_Depedendence_48_adv.rds")); gc()

# - Set ROC-parameters and initialize data structures
vecPercTimepoint <- c(6,12,24,48)
vecTROC <- list(objROC6_CDH_CoxDisc_adv_B, objROC12_CDH_CoxDisc_adv_B,
                objROC24_CDH_CoxDisc_adv_B, objROC48_CDH_CoxDisc_adv_B)
vLabels <- vector("list", length=length(vecPercTimepoint))

# - Create a combined data object for plotting purposes
for (i in 1:length(vecPercTimepoint)) {
  # i <-1 # testing condition
  # datGraph <- data.frame(x = vFPR[-(nThresh+1)], y=vTPR[-1])
  
  # Create a data object for the current prediction time
  if (i == 1) {
    datGraph <- data.table(PredictTime=paste0(letters[i], "_", vecPercTimepoint[i]), Threshold=vecTROC[[i]]$Thresholds, 
                           x=vecTROC[[i]]$FPR, y=vecTROC[[i]]$TPR)
    
  } else {
    datGraph <- rbind(datGraph, 
                      data.table(PredictTime= paste0(letters[i], "_", vecPercTimepoint[i]), Threshold=vecTROC[[i]]$Thresholds, 
                                 x=vecTROC[[i]]$FPR, y=vecTROC[[i]]$TPR))
  }
  vLabels[[i]] <- bquote("Prediction time "*italic(t)==.(vecPercTimepoint[i])*"; AUC: "*.(percent(vecTROC[[i]]$AUC, accuracy=0.01)))
}

# - Graph a combined ROC-graph across prediction times t
#  Aesthetic parameters
datGraph[, FacetLabel := "Advanced discrete-time hazard model - B"]
vCol <- brewer.pal(8,"Set1")
vLabels_F <- setNames(vLabels, paste0(letters[1:length(vecPercTimepoint)],"_", vecPercTimepoint))
chosenFont <- "Cambria"
# Create ROC-graph
(gg <- ggplot(datGraph, aes(x=x,y=y,group=PredictTime)) + theme_minimal() + 
    theme(text = element_text(family=chosenFont), legend.position="inside", 
          strip.background=element_rect(fill="snow2", colour="snow2"),
          strip.text=element_text(size=8, colour="gray50"), strip.text.y.right=element_text(angle=90),
          legend.position.inside = c(0.55,0.45),
          legend.background = element_rect(fill="snow2", color="black",
                                           linetype="solid", linewidth=0.1)) +
    labs(x = bquote("False Positive Rate "*italic(F^"+")), y = 
           bquote("True Positive Rate "*italic(T^"+"))) + 
    # Add 45-degree line
    geom_segment(x = 0, y = 0, xend = 1, yend = 1, color = "grey", linewidth=0.2) +
    # Main line graph
    geom_step(aes(x=x, y=y, linetype=PredictTime, colour=PredictTime), linewidth=0.5) + 
    #geom_point(aes(x=x, y=y, shape=PredictTime, colour=PredictTime), size=0.25) +
    # Facets and scales
    facet_grid(FacetLabel ~ .) +  
    scale_color_manual(name=bquote("ROC"*(italic(t))), values=vCol, labels=vLabels) + 
    scale_linetype_discrete(name=bquote("ROC"*(italic(t))), labels=vLabels) + 
    scale_shape_discrete(name=bquote("ROC"*(italic(t))), labels=vLabels) + 
    scale_y_continuous(label=percent) + scale_x_continuous(label=percent))

# - Save graph
dpi <- 300
ggsave(gg, file=paste0(paste0(genFigPath,"/tROC-Analyses/", "WOffSurvModel-CoxDisc-CDH-Dichotomised-CombinedROC_Depedendence_adv.png")), 
       width=1200/dpi, height=1000/dpi, dpi=dpi, bg="white")

# - cleanup
suppressWarnings(rm(gg, vLabels, vLabels_F, vecTROC, datGraph, dat, 
                    objROC1_CDH_bas, objROC2_CDH_bas, objROC3_CDH_bas, objROC4_CDH_bas,
                    objROC1_CDH_adv, objROC2_CDH_adv, objROC3_CDH_adv, objROC4_CDH_adv,
                    objROC1_CDH_bas_b, objROC2_CDH_bas_b, objROC3_CDH_bas_b, objROC4_CDH_bas_b,
                    objROC1_CDH_adv_b, objROC2_CDH_adv_b, objROC3_CDH_adv_b, objROC4_CDH_adv_b,
                    objROC1_CDH_CoxDisc_adv, objROC2_CDH_CoxDisc_adv, objROC3_CDH_CoxDisc_adv, objROC4_CDH_CoxDisc_adv,
                    objROC1_CDH_CoxDisc_bas, objROC2_CDH_CoxDisc_bas, objROC3_CDH_CoxDisc_bas, objROC4_CDH_CoxDisc_bas,
) )




# ------ 7. Comparing ROC analyses at t=44 for all models
### NOTE: - Using custom tROC()-function from script 0b(iii) under the CD-approach with an NN-estimator and 0/1-kernel
###       - Uses the superior Nearest Neighbour Estimator (NNE) method for S(t) with a 0/1-kernelNNE-kernel for S(t)
###       - Assume dependence (by specifying ID-field) amongst certain observations clustered around ID-values

# --- 7.1 Classical logistic regression model | A-series (non-dichotomised)
predictTime <- 44
ptm <- proc.time() #IGNORE: for computation time calculation
objROC44_LR <- tROC.multi(datGiven=datCredit, modGiven=modLR_Classic, month_End=predictTime, sLambda=0.05, estMethod="NN-0/1", numDigits=4, 
                          fld_ID="DefSpell_Key", fld_Event="DefSpell_Event", eventVal=1, fld_StartTime="Start", fld_EndTime="DefSpell_Age",
                          graphName="WOffLRModel-ROC_TimeVar", genFigPathGiven=paste0(genFigPath, "tROC-Analyses/"), 
                          caseStudyName=paste0("LR_", predictTime), numThreads=12, logPath=genPath, 
                          predType="response")
proc.time() - ptm
objROC44_LR$AUC; objROC44_LR$ROC_graph
### RESULTS: AUC up to t: 65.46%, achieved in 4260 secs


# --- 7.2 Basic discrete-time model | A-series (non-dichotomised)
predictTime <- 44
ptm <- proc.time() #IGNORE: for computation time calculation
objROC44_CDH_CoxDisc_bas <- tROC.multi(datGiven=datCredit, modGiven=modLR_Bas, month_End=predictTime, sLambda=0.05, estMethod="NN-0/1", numDigits=4, 
                                       fld_ID="DefSpell_Key", fld_Event="DefSpell_Event", eventVal=1, fld_StartTime="Start", fld_EndTime="TimeInDefSpell",
                                       graphName="WOffSurvModel-ROC_CoxDisc_Basic_TimeVar", genFigPathGiven=paste0(genFigPath, "tROC-Analyses/"), 
                                       caseStudyName=paste0("CoxDisc_CDH_", predictTime), numThreads=12, logPath=genPath, 
                                       predType="response")
proc.time() - ptm
objROC44_CDH_CoxDisc_bas$AUC; objROC44_CDH_CoxDisc_bas$ROC_graph
### RESULTS: AUC up to t: 61.98%, achieved in 332 secs


# --- 7.3 Advanced discrete-time model | A-series (non-dichotomised)
predictTime <- 44
ptm <- proc.time() #IGNORE: for computation time calculation
objROC44_CDH_CoxDisc_adv <- tROC.multi(datGiven=datCredit, modGiven=modLR_Adv, month_End=predictTime, sLambda=0.05, estMethod="NN-0/1", numDigits=4, 
                                       fld_ID="DefSpell_Key", fld_Event="DefSpell_Event", eventVal=1, fld_StartTime="Start", fld_EndTime="TimeInDefSpell",
                                       graphName="WOffSurvModel-ROC_CoxDisc_Advanced_TimeVar", genFigPathGiven=paste0(genFigPath, "tROC-Analyses/"), 
                                       caseStudyName=paste0("CoxDisc_CDH_", predictTime), numThreads=12, logPath=genPath, 
                                       predType="response")
proc.time() - ptm
objROC44_CDH_CoxDisc_adv$AUC; objROC44_CDH_CoxDisc_adv$ROC_graph
### RESULTS: AUC up to t: 96.82%, achieved in 3847.32  secs


# --- 7.4 Classical logistic regression model | B-series (dichotomised)
predictTime <- 44
ptm <- proc.time() #IGNORE: for computation time calculation
objROC44_LR_B <- tROC.multi(datGiven=datCredit, modGiven=modLR_Classic, month_End=predictTime, sLambda=0.05, estMethod="NN-0/1", numDigits=4, 
                            fld_ID="DefSpell_Key", fld_Event="DefSpell_Event", eventVal=1, fld_StartTime="Start", fld_EndTime="DefSpell_Age",
                            graphName="WOffLRModel-ROC_TimeVar", genFigPathGiven=paste0(genFigPath, "tROC-Analyses/"), 
                            caseStudyName=paste0("LR_", predictTime), numThreads=12, logPath=genPath, 
                            predType="response", MarkerGiven="EventRate_classic", threshold=thresh_classic)
proc.time() - ptm
objROC44_LR_B$AUC; objROC44_LR_B$ROC_graph
### RESULTS: AUC up to t: 50%, achieved in 3318 secs


# --- 7.5 Basic discrete-time model | B-series (dichotomised)
predictTime <- 44
ptm <- proc.time() #IGNORE: for computation time calculation
objROC44_CDH_CoxDisc_bas_B <- tROC.multi(datGiven=datCredit, modGiven=modLR_Bas, month_End=predictTime, sLambda=0.05, estMethod="NN-0/1", numDigits=4, 
                                       fld_ID="DefSpell_Key", fld_Event="DefSpell_Event", eventVal=1, fld_StartTime="Start", fld_EndTime="TimeInDefSpell",
                                       graphName="WOffSurvModel-ROC_CoxDisc_Basic_TimeVar", genFigPathGiven=paste0(genFigPath, "tROC-Analyses/"), 
                                       caseStudyName=paste0("CoxDisc_CDH_", predictTime), numThreads=12, logPath=genPath, 
                                       predType="response", MarkerGiven="EventRate_bas", threshold=thresh_dth_bas)
proc.time() - ptm
objROC44_CDH_CoxDisc_bas_B$AUC; objROC44_CDH_CoxDisc_bas_B$ROC_graph
### RESULTS: AUC up to t: 50%, achieved in 33 secs


# --- 7.6 Advanced discrete-time model | B-series (dichotomised)
predictTime <- 44
ptm <- proc.time() #IGNORE: for computation time calculation
objROC44_CDH_CoxDisc_adv_B <- tROC.multi(datGiven=datCredit, modGiven=modLR_Adv, month_End=predictTime, sLambda=0.05, estMethod="NN-0/1", numDigits=4, 
                                         fld_ID="DefSpell_Key", fld_Event="DefSpell_Event", eventVal=1, fld_StartTime="Start", fld_EndTime="TimeInDefSpell",
                                         graphName="WOffSurvModel-ROC_CoxDisc_Advanced_TimeVar", genFigPathGiven=paste0(genFigPath, "tROC-Analyses/"), 
                                         caseStudyName=paste0("CoxDisc_CDH_", predictTime), numThreads=12, logPath=genPath, 
                                         predType="response", MarkerGiven="EventRate_adv", threshold=thresh_dth_adv)
proc.time() - ptm
objROC44_CDH_CoxDisc_adv_B$AUC; objROC44_CDH_CoxDisc_adv_B$ROC_graph
### RESULTS: AUC up to t: 50.65%, achieved in 35 secs


# --- 7.7 Save objects
# - Series A models (non-dichotomised)
saveRDS(objROC44_LR, paste0(genPath,"WOffSurvModel-LR-Dichotomised-ROC_Depedendence_44_bas.rds"))
saveRDS(objROC44_CDH_CoxDisc_bas, paste0(genPath,"WOffSurvModel-CoxDisc-CDH-Dichotomised-ROC_Depedendence_44_bas.rds"))
saveRDS(objROC44_CDH_CoxDisc_adv, paste0(genPath,"WOffSurvModel-CoxDisc-CDH-Dichotomised-ROC_Depedendence_44_adv.rds"))

# - Series B models (dichotomised)
saveRDS(objROC44_LR_B, paste0(genPath,"WOffSurvModel-LR-Dichotomised-ROC_Depedendence_44_bas.rds"))
saveRDS(objROC44_CDH_CoxDisc_bas_B, paste0(genPath,"WOffSurvModel-CoxDisc-CDH-Dichotomised-ROC_Depedendence_44_bas.rds"))
saveRDS(objROC44_CDH_CoxDisc_adv_B, paste0(genPath,"WOffSurvModel-CoxDisc-CDH-Dichotomised-ROC_Depedendence_44_adv.rds"))




# ------ 8. Create combined ROC-graphs across multiple prediction times

# --- 8.1 Ensure required objects exist in memory
# - Series A models (non-dichotomised)
if (!exists('objROC44_LR')) objROC44_LR <- readRDS(paste0(genPath,"WOffSurvModel-LR-Dichotomised-ROC_Depedendence_44_bas.rds")); gc()
if (!exists('objROC44_CDH_CoxDisc_bas')) objROC44_CDH_CoxDisc_bas <- readRDS(paste0(genPath,"WOffSurvModel-CoxDisc-CDH-ROC_Depedendence_44_bas.rds")); gc()
if (!exists('objROC44_CDH_CoxDisc_adv')) objROC44_CDH_CoxDisc_adv <- readRDS(paste0(genPath,"WOffSurvModel-CoxDisc-CDH-ROC_Depedendence_44_adv.rds")); gc()

# - Series B models (dichotomised)
if (!exists('objROC44_LR_B')) objROC44_LR_B <- readRDS(paste0(genPath,"WOffSurvModel-LR-Dichotomised-ROC_Depedendence_44_bas.rds")); gc()
if (!exists('objROC44_CDH_CoxDisc_bas_B')) objROC44_CDH_CoxDisc_bas_B <- readRDS(paste0(genPath,"WOffSurvModel-CoxDisc-CDH-Dichotomised-ROC_Depedendence_44_bas.rds")); gc()
if (!exists('objROC44_CDH_CoxDisc_adv_B')) objROC44_CDH_CoxDisc_adv_B <- readRDS(paste0(genPath,"WOffSurvModel-CoxDisc-CDH-Dichotomised-ROC_Depedendence_44_adv.rds")); gc()


# --- 8.2 Compare ROCs of the A-series models (non-dichotomised)
# - Set ROC-parameters and initialize data structures
vecPercTimepoint <- c(44,44,44)
vecTROC <- list(objROC44_CDH_CoxDisc_adv, objROC44_CDH_CoxDisc_bas ,objROC44_LR)
vLabels <- vector("list", length=length(vecPercTimepoint))
vModel <- c("DtH-Advanced A","DtH-Basic A", "Logistic Regression A")

# - Create a combined data object for plotting purposes
for (i in 1:length(vecPercTimepoint)) {
  # i <-1 # testing condition
  # datGraph <- data.frame(x = vFPR[-(nThresh+1)], y=vTPR[-1])
  
  # Create a data object for the current prediction time
  if (i == 1) {
    datGraph <- data.table(PredictTime=paste0(letters[i], "_", vecPercTimepoint[i]), Threshold=vecTROC[[i]]$Thresholds, 
                           x=vecTROC[[i]]$FPR, y=vecTROC[[i]]$TPR)
    
  } else {
    datGraph <- rbind(datGraph, 
                      data.table(PredictTime= paste0(letters[i], "_", vecPercTimepoint[i]), Threshold=vecTROC[[i]]$Thresholds, 
                                 x=vecTROC[[i]]$FPR, y=vecTROC[[i]]$TPR))
  }
  vLabels[[i]] <- bquote(.(vModel[i]) * "; AUC: " * .(formatC(vecTROC[[i]]$AUC * 100, format = "f", digits = 2)) * "%")
}

# - Set graphing parameters
datGraph[, FacetLabel := "ROC(italic(t))~'Prediction time t=44'"]
vCol <- brewer.pal(6,"Set1")
vLabels_F <- setNames(vLabels, paste0(letters[1:length(vecPercTimepoint)],"_", vecPercTimepoint))
chosenFont <- "Cambria"

# - Create ROC-graph
(gg <- ggplot(datGraph, aes(x=x,y=y,group=PredictTime)) + theme_minimal() + 
    theme(text = element_text(family=chosenFont), legend.position="inside", 
          strip.background=element_rect(fill="snow2", colour="snow2"),
          strip.text=element_text(size=8, colour="gray50"), strip.text.y.right=element_text(angle=90),
          legend.position.inside = c(0.725,0.2),
          legend.background = element_rect(fill="snow2", color="black",
                                           linetype="solid", linewidth=0.1)) +
    labs(x = bquote("False Positive Rate "*italic(F^"+")), y = 
           bquote("True Positive Rate "*italic(T^"+"))) + 
    # Add 45-degree line
    geom_segment(x = 0, y = 0, xend = 1, yend = 1, color = "grey", linewidth=0.2) +
    # Main line graph
    geom_step(aes(x=x, y=y, linetype=PredictTime, colour=PredictTime), linewidth=0.5) + 
    #geom_point(aes(x=x, y=y, shape=PredictTime, colour=PredictTime), size=0.25) +
    # Facets and scales
    facet_grid(FacetLabel ~ .) +  
    facet_wrap(~FacetLabel, labeller = label_parsed, strip.position = "right")+
    scale_color_manual(name=bquote("ROC"*(italic(t))), values=vCol, labels=vLabels) + 
    scale_linetype_discrete(name=bquote("ROC"*(italic(t))), labels=vLabels) + 
    scale_shape_discrete(name=bquote("ROC"*(italic(t))), labels=vLabels) + 
    scale_y_continuous(label=percent) + scale_x_continuous(label=percent))

# - Save graph
dpi <- 300
ggsave(gg, file=paste0(paste0(genFigPath,"/tROC-Analyses/", "WOffModel-Combined.png")), 
       width=1800/dpi, height=1500/dpi, dpi=dpi, bg="white")


# --- 8.3 Compare ROCs of the B-series models (dichotomised)
# - Set ROC-parameters and initialize data structures
vecPercTimepoint <- c(44,44,44)
vecTROC <- list(objROC44_CDH_CoxDisc_adv_B, objROC44_CDH_CoxDisc_bas_B ,objROC44_LR_B)
vLabels <- vector("list", length=length(vecPercTimepoint))
vModel <- c("DtH-Advanced B","DtH-Basic B", "Logistic Regression B")

# - Create a combined data object for plotting purposes
for (i in 1:length(vecPercTimepoint)) {
  # i <-1 # testing condition
  # datGraph <- data.frame(x = vFPR[-(nThresh+1)], y=vTPR[-1])
  
  # Create a data object for the current prediction time
  if (i == 1) {
    datGraph <- data.table(PredictTime=paste0(letters[i], "_", vecPercTimepoint[i]), Threshold=vecTROC[[i]]$Thresholds, 
                           x=vecTROC[[i]]$FPR, y=vecTROC[[i]]$TPR)
    
  } else {
    datGraph <- rbind(datGraph, 
                      data.table(PredictTime= paste0(letters[i], "_", vecPercTimepoint[i]), Threshold=vecTROC[[i]]$Thresholds, 
                                 x=vecTROC[[i]]$FPR, y=vecTROC[[i]]$TPR))
  }
  vLabels[[i]] <- bquote(.(vModel[i]) * "; AUC: " * .(formatC(vecTROC[[i]]$AUC * 100, format = "f", digits = 2)) * "%")
}

# - Set graphing parameters
datGraph[, FacetLabel := "ROC(italic(t))~'Prediction time t=44'"]
vCol <- brewer.pal(6,"Set1")
vLabels_F <- setNames(vLabels, paste0(letters[1:length(vecPercTimepoint)],"_", vecPercTimepoint))
chosenFont <- "Cambria"

# - Create ROC-graph
(gg <- ggplot(datGraph, aes(x=x,y=y,group=PredictTime)) + theme_minimal() + 
    theme(text = element_text(family=chosenFont), legend.position="inside", 
          strip.background=element_rect(fill="snow2", colour="snow2"),
          strip.text=element_text(size=8, colour="gray50"), strip.text.y.right=element_text(angle=90),
          legend.position.inside = c(0.725,0.2),
          legend.background = element_rect(fill="snow2", color="black",
                                           linetype="solid", linewidth=0.1)) +
    labs(x = bquote("False Positive Rate "*italic(F^"+")), y = 
           bquote("True Positive Rate "*italic(T^"+"))) + 
    # Add 45-degree line
    geom_segment(x = 0, y = 0, xend = 1, yend = 1, color = "grey", linewidth=0.2) +
    # Main line graph
    geom_step(aes(x=x, y=y, linetype=PredictTime, colour=PredictTime), linewidth=0.5) + 
    #geom_point(aes(x=x, y=y, shape=PredictTime, colour=PredictTime), size=0.25) +
    # Facets and scales
    facet_grid(FacetLabel ~ .) +  
    facet_wrap(~FacetLabel, labeller = label_parsed, strip.position = "right")+
    scale_color_manual(name=bquote("ROC"*(italic(t))), values=vCol, labels=vLabels) + 
    scale_linetype_discrete(name=bquote("ROC"*(italic(t))), labels=vLabels) + 
    scale_shape_discrete(name=bquote("ROC"*(italic(t))), labels=vLabels) + 
    scale_y_continuous(label=percent) + scale_x_continuous(label=percent))

# - Save graph
dpi <- 300
ggsave(gg, file=paste0(paste0(genFigPath,"/tROC-Analyses/", "WOffModel-Combined-B.png")), 
       width=1800/dpi, height=1500/dpi, dpi=dpi, bg="white")









