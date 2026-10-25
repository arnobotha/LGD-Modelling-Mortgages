# ========================= TIME-DEPENDENT ROC-ANALYSIS ========================
# Conduct time-dependent ROC-analyses on the same fitted Cox regression model
# ------------------------------------------------------------------------------
# PROJECT TITLE: Loss Modelling (LGD) for Residential Mortgages
# SCRIPT AUTHOR(S): Mohammed Gabru (MG), Marcel Muller (MM), Dr Arno Botha (AB)
# ------------------------------------------------------------------------------
# -- Script dependencies:
#   - 0.Setup.R
#   - 4b(i).InputSpace_DiscreteCox.R
#   - 4b(ii).InputSpace_DiscreteCox_Basic.R
#   - 4c.InputSpace_LogisticRegression.R
#   - 4e.SurvivalTrees.R
#
# -- Inputs:
#   - datCredit_train_CDH | Prepared from script 2g
#   - datCredit_valid_CDH | Prepared from script 2g
#   - modLR_Bas | Basic discrete time model as fitted in script 4b(ii)
#   - modLR_Adv | Advanced discrete time model as fitted in script 4b(i)
#   - modLR_Classic | Classical logistic regression model as fitted in script 4c
#   - SurvTree_CTree | Survival tree as fitted in script 4e
#
# -- Outputs:
#   - <Analytics> | tROC-graphs
# ==============================================================================


# ============ 1. Preliminaries ============

# --- 1.1 Load and prepare data
# - Confirm prepared datasets are loaded into memory
if (!exists('datCredit_train_CDH')) unpack.ffdf(paste0(genPath,"creditdata_train_CDH"), tempPath);gc()
if (!exists('datCredit_valid_CDH')) unpack.ffdf(paste0(genPath,"creditdata_valid_CDH"), tempPath);gc()

# - Ensure order of data is correct
setorder(datCredit_train_CDH, LoanID, Date)
setorder(datCredit_valid_CDH, LoanID, Date)

# - Use only default spells
datCredit_train <- datCredit_train_CDH[!is.na(DefSpell_Key),]
datCredit_valid <- datCredit_valid_CDH[!is.na(DefSpell_Key),]

# - Create a spell-level target variable for write-off instance
datCredit_train[, DefSpell_Event2:=DefSpell_Event]; datCredit_valid[, DefSpell_Event2:=DefSpell_Event]
datCredit_train[, DefSpell_Event:=DefSpellResol_Type_Hist=="WOFF"]; datCredit_valid[, DefSpell_Event:=DefSpellResol_Type_Hist=="WOFF"]

# - Transform categorical variables to factor
# Payment method
datCredit_train[,pmnt_method_grp_fac:=as.factor(pmnt_method_grp)]
datCredit_valid[,pmnt_method_grp_fac:=as.factor(pmnt_method_grp)]
# Previous defaults
datCredit_train[,PrevDefaults_fac:=as.factor(PrevDefaults)]
datCredit_valid[,PrevDefaults_fac:=as.factor(PrevDefaults)]

# - Combine training and validation datasets to facilitate "better" (smooth) graphs
datCredit <- rbind(datCredit_train, datCredit_valid)

# - Handle left-truncated spells by adding a starting record 
### NOTE:  This is necessary for calculating certain survival quantities later
# Create an additional record for each default spell
datAdd <- subset(datCredit, Counter==1 & TimeInDefSpell>1)
datAdd[, TimeInDefSpell:=TimeInDefSpell-1]
datAdd[, Counter:=0]
# Add record to main dataset
datCredit <- rbind(datCredit, datAdd); setorder(datCredit, DefSpell_Key, Date)

# - Remove objects
rm(datCredit_train_CDH, datCredit_valid_CDH); gc()


# --- 1.2 Load models
# - Basic discrete-time hazard model
modLR_Bas <- readRDS(paste0(genObjPath,"CoxDisc_Basic_Model.rds"))

# - Advanced discrete-time hazard model
modLR_Adv <- readRDS(paste0(genObjPath,"CoxDisc_Advanced_Model.rds"))

# - Classic logit model
modLR_Classic <- readRDS(paste0(genObjPath,"LR_Model.rds"))

# - Tree model
SurvTree_CTree <- readRDS(paste0(genObjPath,"SurvTree_CTree.rds"))


# --- 1.3 Estimate event rates
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

# - Remove added rows
datCredit <- subset(datCredit, Counter>0)


# --- 1.4 Predict survival quantities using survival tree
# - Get training data from survival tree
datCredit_train_SurvTree <- SurvTree_CTree$datTrain

# - Get node predictions from tree
datCredit[,Node:=predict(SurvTree_CTree$survTree, datCredit, type="node")]
datCredit_train_SurvTree[,Node:=predict(SurvTree_CTree$survTree, datCredit_train_SurvTree, type="node")]

# - Fit a survival object for each node | Using the training data only to align with how the tree was fitted
datHaz <- predSurv(survTree=SurvTree_CTree$survTree, datGiven=datCredit_train_SurvTree,
                   fld_DefSpell_Age="DefSpell_Age", fld_DefSpell_Event="DefSpell_Event")

# - Join hazards back to main dataset
datCredit <- merge(datCredit, datHaz[, list(Node, Time, Survival_survtree=Surv, Hazard_survtree=Hazard,
                                            EventRate_survtree=EventRate)],
                   by.x=c("Node", "TimeInDefSpell"), by.y=c("Node", "Time"), all.x=T)




# ============ 2. tROC analyses | Basic discrete-time hazard model ============
### NOTE: - Using custom tROC()-function from script 0b(iii) under the CD-approach with an NN-estimator and 0/1-kernel
###       - Uses the superior Nearest Neighbour Estimator (NNE) method for S(t) with a 0/1-kernelNNE-kernel for S(t)
###       - Assume dependence (by specifying ID-field) amongst certain observations clustered around ID-values

# --- 2.1 tROC analyses using the CD-approach | Time-window chosen as first 6 months in default
predictTime <- 6
ptm <- proc.time() #IGNORE: for computation time calculation;
objROC6_CDH_CoxDisc_bas <- tROC.multi(datGiven=datCredit, modGiven=modLR_Bas, month_End=predictTime, sLambda=0.05, estMethod="NN-0/1", numDigits=4, 
                                      fld_ID="DefSpell_Key", fld_Event="DefSpell_Event2", eventVal=1, fld_StartTime="Start", fld_EndTime="TimeInDefSpell",
                                      graphName="WOffSurvModel-ROC_CoxDisc_Basic_TimeVar", genFigPathGiven=paste0(genFigPath, "tROC-Analyses/"), 
                                      caseStudyName=paste0("CoxDisc_CDH_", predictTime), numThreads=12, logPath=genPath, 
                                      predType="response")
proc.time() - ptm
objROC6_CDH_CoxDisc_bas$AUC; objROC6_CDH_CoxDisc_bas$ROC_graph
### RESULTS: AUC up to t: 52.90%, achieved in 384 secs


# --- 2.2 tROC analyses using the CD-approach | Time-window chosen as first 12 months in default
predictTime <- 12
ptm <- proc.time() #IGNORE: for computation time calculation;
objROC12_CDH_CoxDisc_bas <- tROC.multi(datGiven=datCredit, modGiven=modLR_Bas, month_End=predictTime, sLambda=0.05, estMethod="NN-0/1", numDigits=4, 
                                       fld_ID="DefSpell_Key", fld_Event="DefSpell_Event2", eventVal=1, fld_StartTime="Start", fld_EndTime="TimeInDefSpell",
                                       graphName="WOffSurvModel-ROC_CoxDisc_Basic_TimeVar", genFigPathGiven=paste0(genFigPath, "tROC-Analyses/"), 
                                       caseStudyName=paste0("CoxDisc_CDH_", predictTime), numThreads=12, logPath=genPath, 
                                       predType="response")
proc.time() - ptm
objROC12_CDH_CoxDisc_bas$AUC; objROC12_CDH_CoxDisc_bas$ROC_graph
### RESULTS: AUC up to t: 61.19%, achieved in 513 secs


# --- 2.3 tROC analyses using the CD-approach | Time-window chosen as first 24 months in default
predictTime <- 24
ptm <- proc.time() #IGNORE: for computation time calculation;
objROC24_CDH_CoxDisc_bas <- tROC.multi(datGiven=datCredit, modGiven=modLR_Bas, month_End=predictTime, sLambda=0.05, estMethod="NN-0/1", numDigits=4, 
                                       fld_ID="DefSpell_Key", fld_Event="DefSpell_Event2", eventVal=1, fld_StartTime="Start", fld_EndTime="TimeInDefSpell",
                                       graphName="WOffSurvModel-ROC_CoxDisc_Basic_TimeVar", genFigPathGiven=paste0(genFigPath, "tROC-Analyses/"), 
                                       caseStudyName=paste0("CoxDisc_CDH_", predictTime), numThreads=12, logPath=genPath, 
                                       predType="response")
proc.time() - ptm
objROC24_CDH_CoxDisc_bas$AUC; objROC24_CDH_CoxDisc_bas$ROC_graph
### RESULTS: AUC up to t: 62.58%, achieved in 753 secs


# --- 2.4 tROC analyses using the CD-approach | Time-window chosen as first 48 months in default
predictTime <- 48
ptm <- proc.time() #IGNORE: for computation time calculation;
objROC48_CDH_CoxDisc_bas <- tROC.multi(datGiven=datCredit, modGiven=modLR_Bas, month_End=predictTime, sLambda=0.05, estMethod="NN-0/1", numDigits=4, 
                                       fld_ID="DefSpell_Key", fld_Event="DefSpell_Event2", eventVal=1, fld_StartTime="Start", fld_EndTime="TimeInDefSpell",
                                       graphName="WOffSurvModel-ROC_CoxDisc_Basic_TimeVar", genFigPathGiven=paste0(genFigPath, "tROC-Analyses/"), 
                                       caseStudyName=paste0("CoxDisc_CDH_", predictTime), numThreads=12, logPath=genPath, 
                                       predType="response")
proc.time() - ptm
objROC48_CDH_CoxDisc_bas$AUC; objROC48_CDH_CoxDisc_bas$ROC_graph
### RESULTS: AUC up to t: 61.75%, achieved in 1251 secs


# --- 2.5 Store experimental objects | Memory optimisation
# CDH-model: Basic Cox-regression model
saveRDS(objROC6_CDH_CoxDisc_bas, paste0(genPath,"WOffSurvModel-CoxDisc-CDH-ROC_Depedendence_06_bas.rds"))
saveRDS(objROC12_CDH_CoxDisc_bas, paste0(genPath,"WOffSurvModel-CoxDisc-CDH-ROC_Depedendence_12_bas.rds"))
saveRDS(objROC24_CDH_CoxDisc_bas, paste0(genPath,"WOffSurvModel-CoxDisc-CDH-ROC_Depedendence_24_bas.rds"))
saveRDS(objROC48_CDH_CoxDisc_bas, paste0(genPath,"WOffSurvModel-CoxDisc-CDH-ROC_Depedendence_48_bas.rds"))




# ============ 3. tROC analyses | Advanced discrete-time hazard model ============
### NOTE: - Using custom tROC()-function from script 0b(iii) under the CD-approach with an NN-estimator and 0/1-kernel
###       - Uses the superior Nearest Neighbour Estimator (NNE) method for S(t) with a 0/1-kernelNNE-kernel for S(t)
###       - Assume dependence (by specifying ID-field) amongst certain observations clustered around ID-values

# --- 3.1 tROC analyses using the CD-approach | Time-window chosen as first 6 months in default
predictTime <- 6
ptm <- proc.time() #IGNORE: for computation time calculation;
objROC6_CDH_CoxDisc_adv <- tROC.multi(datGiven=datCredit, modGiven=modLR_Adv, month_End=predictTime, sLambda=0.05, estMethod="NN-0/1", numDigits=4, 
                                      fld_ID="DefSpell_Key", fld_Event="DefSpell_Event2", eventVal=1, fld_StartTime="Start", fld_EndTime="TimeInDefSpell",
                                      graphName="WOffSurvModel-ROC_CoxDisc_Advanced_TimeVar", genFigPathGiven=paste0(genFigPath, "tROC-Analyses/"), 
                                      caseStudyName=paste0("CoxDisc_CDH_", predictTime), numThreads=12, logPath=genPath, 
                                      predType="response")
proc.time() - ptm
objROC6_CDH_CoxDisc_adv$AUC; objROC6_CDH_CoxDisc_adv$ROC_graph
### RESULTS: AUC up to t: 96.35%, achieved in 4163 secs


# --- 3.2 tROC analyses using the CD-approach | Time-window chosen as first 12 months in default
predictTime <- 12
ptm <- proc.time() #IGNORE: for computation time calculation;
objROC12_CDH_CoxDisc_adv <- tROC.multi(datGiven=datCredit, modGiven=modLR_Adv, month_End=predictTime, sLambda=0.05, estMethod="NN-0/1", numDigits=4, 
                                       fld_ID="DefSpell_Key", fld_Event="DefSpell_Event2", eventVal=1, fld_StartTime="Start", fld_EndTime="TimeInDefSpell",
                                       graphName="WOffSurvModel-ROC_CoxDisc_Advanced_TimeVar", genFigPathGiven=paste0(genFigPath, "tROC-Analyses/"), 
                                       caseStudyName=paste0("CoxDisc_CDH_", predictTime), numThreads=12, logPath=genPath, 
                                       predType="response")
proc.time() - ptm
objROC12_CDH_CoxDisc_adv$AUC; objROC12_CDH_CoxDisc_adv$ROC_graph
### RESULTS: AUC up to t: 95.65%, achieved in 6131 secs


# --- 3.3 tROC analyses using the CD-approach | Time-window chosen as first 24 months in default
predictTime <- 24
ptm <- proc.time() #IGNORE: for computation time calculation;
objROC24_CDH_CoxDisc_adv <- tROC.multi(datGiven=datCredit, modGiven=modLR_Adv, month_End=predictTime, sLambda=0.05, estMethod="NN-0/1", numDigits=4, 
                                       fld_ID="DefSpell_Key", fld_Event="DefSpell_Event2", eventVal=1, fld_StartTime="Start", fld_EndTime="TimeInDefSpell",
                                       graphName="WOffSurvModel-ROC_CoxDisc_Advanced_TimeVar", genFigPathGiven=paste0(genFigPath, "tROC-Analyses/"), 
                                       caseStudyName=paste0("CoxDisc_CDH_", predictTime), numThreads=12, logPath=genPath, 
                                       predType="response")
proc.time() - ptm
objROC24_CDH_CoxDisc_adv$AUC; objROC24_CDH_CoxDisc_adv$ROC_grap
### RESULTS: AUC up to t: 96.70%, achieved in 9720 secs


# --- 3.4 tROC analyses using the CD-approach | Time-window chosen as first 48 months in default
predictTime <- 48
ptm <- proc.time() #IGNORE: for computation time calculation;
objROC48_CDH_CoxDisc_adv <- tROC.multi(datGiven=datCredit, modGiven=modLR_Adv, month_End=predictTime, sLambda=0.05, estMethod="NN-0/1", numDigits=4, 
                                       fld_ID="DefSpell_Key", fld_Event="DefSpell_Event2", eventVal=1, fld_StartTime="Start", fld_EndTime="TimeInDefSpell",
                                       graphName="WOffSurvModel-ROC_CoxDisc_Advanced_TimeVar", genFigPathGiven=paste0(genFigPath, "tROC-Analyses/"), 
                                       caseStudyName=paste0("CoxDisc_CDH_", predictTime), numThreads=12, logPath=genPath, 
                                       predType="response")
proc.time() - ptm
objROC48_CDH_CoxDisc_adv$AUC; objROC48_CDH_CoxDisc_adv$ROC_graph
### RESULTS: AUC up to t: 97.37%, achieved in 19325 secs


# --- 3.5 Store experimental objects | Memory optimisation
saveRDS(objROC6_CDH_CoxDisc_adv, paste0(genPath,"WOffSurvModel-CoxDisc-CDH-ROC_Depedendence_06_adv.rds"))
saveRDS(objROC12_CDH_CoxDisc_adv, paste0(genPath,"WOffSurvModel-CoxDisc-CDH-ROC_Depedendence_12_adv.rds"))
saveRDS(objROC24_CDH_CoxDisc_adv, paste0(genPath,"WOffSurvModel-CoxDisc-CDH-ROC_Depedendence_24_adv.rds"))
saveRDS(objROC48_CDH_CoxDisc_adv, paste0(genPath,"WOffSurvModel-CoxDisc-CDH-ROC_Depedendence_48_adv.rds"))




# ============ 4. tROC analyses | Survival tree ============
### NOTE: - Using custom tROC()-function from script 0b(iii) under the CD-approach with an NN-estimator and 0/1-kernel
###       - Uses the superior Nearest Neighbour Estimator (NNE) method for S(t) with a 0/1-kernelNNE-kernel for S(t)
###       - Assume dependence (by specifying ID-field) amongst certain observations clustered around ID-values

# --- 4.1 tROC analyses using the CD-approach | Time-window chosen as first 6 months in default
predictTime <- 6
ptm <- proc.time() #IGNORE: for computation time calculation;
objROC6_CDH_st <- tROC.multi(datGiven=datCredit, modGiven=NA, month_End=predictTime, sLambda=0.05, estMethod="NN-0/1", numDigits=4, 
                         fld_ID="DefSpell_Key", fld_Event="DefSpell_Event2", eventVal=1, fld_StartTime="Start", fld_EndTime="TimeInDefSpell",
                         graphName="WOffSurvModel-ROC_SurvTree_TimeVar", genFigPathGiven=paste0(genFigPath, "tROC-Analyses/"), 
                         caseStudyName=paste0("SurvTree_", predictTime), numThreads=12, logPath=genPath, 
                         predType="response", MarkerGiven="Hazard_survtree")
proc.time() - ptm
objROC6_CDH_st$AUC; objROC6_CDH_st$ROC_graph
### RESULTS: AUC up to t: 54.17%, achieved in 554 secs


# --- 4.2 tROC analyses using the CD-approach | Time-window chosen as first 12 months in default
predictTime <- 12
ptm <- proc.time() #IGNORE: for computation time calculation;
objROC12_CDH_st <- tROC.multi(datGiven=datCredit, modGiven=NA, month_End=predictTime, sLambda=0.05, estMethod="NN-0/1", numDigits=4, 
                              fld_ID="DefSpell_Key", fld_Event="DefSpell_Event2", eventVal=1, fld_StartTime="Start", fld_EndTime="TimeInDefSpell",
                              graphName="WOffSurvModel-ROC_SurvTree_TimeVar", genFigPathGiven=paste0(genFigPath, "tROC-Analyses/"), 
                              caseStudyName=paste0("SurvTree_", predictTime), numThreads=12, logPath=genPath, 
                              predType="response", MarkerGiven="Hazard_survtree")
proc.time() - ptm
objROC12_CDH_st$AUC; objROC12_CDH_st$ROC_graph
### RESULTS: AUC up to t: 66.81%, achieved in 749 secs


# --- 4.3 tROC analyses using the CD-approach | Time-window chosen as first 24 months in default
predictTime <- 24
ptm <- proc.time() #IGNORE: for computation time calculation;
objROC24_CDH_st <- tROC.multi(datGiven=datCredit, modGiven=NA, month_End=predictTime, sLambda=0.05, estMethod="NN-0/1", numDigits=4, 
                              fld_ID="DefSpell_Key", fld_Event="DefSpell_Event2", eventVal=1, fld_StartTime="Start", fld_EndTime="TimeInDefSpell",
                              graphName="WOffSurvModel-ROC_SurvTree_TimeVar", genFigPathGiven=paste0(genFigPath, "tROC-Analyses/"), 
                              caseStudyName=paste0("SurvTree_", predictTime), numThreads=12, logPath=genPath, 
                              predType="response", MarkerGiven="Hazard_survtree")
proc.time() - ptm
objROC24_CDH_st$AUC; objROC24_CDH_st$ROC_graph
### RESULTS: AUC up to t: 73.93%, achieved in 1680 secs


# --- 4.4 tROC analyses using the CD-approach | Time-window chosen as first 48 months in default
predictTime <- 48
ptm <- proc.time() #IGNORE: for computation time calculation;
objROC48_CDH_st <- tROC.multi(datGiven=datCredit, modGiven=NA, month_End=predictTime, sLambda=0.05, estMethod="NN-0/1", numDigits=4, 
                              fld_ID="DefSpell_Key", fld_Event="DefSpell_Event2", eventVal=1, fld_StartTime="Start", fld_EndTime="TimeInDefSpell",
                              graphName="WOffSurvModel-ROC_SurvTree_TimeVar", genFigPathGiven=paste0(genFigPath, "tROC-Analyses/"), 
                              caseStudyName=paste0("SurvTree_", predictTime), numThreads=12, logPath=genPath, 
                              predType="response", MarkerGiven="Hazard_survtree")
proc.time() - ptm
objROC48_CDH_st$AUC; objROC48_CDH_st$ROC_graph
### RESULTS: AUC up to t: 75.67%, achieved in 2582 secs


# --- 4.5 Store experimental objects | Memory optimisation
saveRDS(objROC6_CDH_st, paste0(genPath,"WOffSurvModel-CDH-SurvTree-ROC_Depedendence_06.rds"))
saveRDS(objROC12_CDH_st, paste0(genPath,"WOffSurvModel-CDH-SurvTree-ROC_Depedendence_12.rds"))
saveRDS(objROC24_CDH_st, paste0(genPath,"WOffSurvModel-CDH-SurvTree-ROC_Depedendence_24.rds"))
saveRDS(objROC48_CDH_st, paste0(genPath,"WOffSurvModel-CDH-SurvTree-ROC_Depedendence_48.rds"))




# ============ 5. Create combined ROC-graphs across multiple prediction times ============

# --- 5.1 Basic discrete-time hazard model
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
          legend.position.inside=c(0.75,0.25),
          legend.background=element_rect(fill="snow2", color="black",
                                         linetype="solid", linewidth=0.1)) +
    labs(x=bquote("False Positive Rate "*italic(F^"+")),
         y=bquote("True Positive Rate "*italic(T^"+"))) + 
    # Add 45-degree line
    geom_segment(x=0, y=0, xend=1, yend=1, color="grey", linewidth=0.2) +
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
       width=3600/dpi, height=1500/dpi, dpi=dpi, bg="white")


# --- 5.2 Advanced discrete-time hazard model
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


# --- 5.3 Survival tree
# - Ensure required objects exist in memory
if (!exists('objROC6_SurvTree')) objROC6_CDH_st <- readRDS(paste0(genPath,"WOffSurvModel-CDH-SurvTree-ROC_Depedendence_06.rds")); gc()
if (!exists('objROC12_SurvTree')) objROC12_CDH_st <- readRDS(paste0(genPath,"WOffSurvModel-CDH-SurvTree-ROC_Depedendence_12.rds")); gc()
if (!exists('objROC24_SurvTree')) objROC24_CDH_st <- readRDS(paste0(genPath,"WOffSurvModel-CDH-SurvTree-ROC_Depedendence_24.rds")); gc()
if (!exists('objROC48_SurvTree')) objROC48_CDH_st <- readRDS(paste0(genPath,"WOffSurvModel-CDH-SurvTree-ROC_Depedendence_48.rds")); gc()

# - Set ROC-parameters and initialize data structures
vecPercTimepoint <- c(6,12,24,48)
vecTROC <- list(objROC6_CDH_st, objROC12_CDH_st,
                objROC24_CDH_st, objROC48_CDH_st)
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
datGraph[, FacetLabel := "Survival Tree"]
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
ggsave(gg, file=paste0(paste0(genFigPath,"/tROC-Analyses/", "WOffSurvModel-CombinedROC_Depedendence_st.png")), 
       width=1200/dpi, height=1000/dpi, dpi=dpi, bg="white")




# ============ 6. Comparing ROC analyses at t=44 for all models ============
### NOTE: - Using custom tROC()-function from script 0b(iii) under the CD-approach with an NN-estimator and 0/1-kernel
###       - Uses the superior Nearest Neighbour Estimator (NNE) method for S(t) with a 0/1-kernelNNE-kernel for S(t)
###       - Assume dependence (by specifying ID-field) amongst certain observations clustered around ID-values

# --- 6.1 Classical logistic regression model
# - Create an event indicator
datCredit[, DefSpell_Event2 := ifelse(DefSpellResol_Type_Hist=="WOFF", 1, 0)]

# - Run ROC analyses on the subet data
ptm <- proc.time() #IGNORE: for computation time calculation
objROC_LR <- roc(response=datCredit[DefSpell_Counter==1, DefSpell_Event2], predictor=datCredit[DefSpell_Counter==1, Hazard_classic])
proc.time() - ptm
objROC_LR$auc; plot(objROC_LR)
### RESULTS: AUC up to t: 86.29%, achieved in 1 secs


# --- 6.2 Basic discrete-time model
predictTime <- 44
ptm <- proc.time() #IGNORE: for computation time calculation
objROC44_CDH_CoxDisc_bas <- tROC.multi(datGiven=datCredit, modGiven=modLR_Bas, month_End=predictTime, sLambda=0.05, estMethod="NN-0/1", numDigits=4, 
                                       fld_ID="DefSpell_Key", fld_Event="DefSpell_Event2", eventVal=1, fld_StartTime="Start", fld_EndTime="TimeInDefSpell",
                                       graphName="WOffSurvModel-ROC_CoxDisc_Basic_TimeVar", genFigPathGiven=paste0(genFigPath, "tROC-Analyses/"), 
                                       caseStudyName=paste0("CoxDisc_CDH_", predictTime), numThreads=12, logPath=genPath, 
                                       predType="response")
proc.time() - ptm
objROC44_CDH_CoxDisc_bas$AUC; objROC44_CDH_CoxDisc_bas$ROC_graph
### RESULTS: AUC up to t: 61.18%, achieved in 1316 secs


# --- 6.3 Advanced discrete-time model
predictTime <- 44
ptm <- proc.time() #IGNORE: for computation time calculation
objROC44_CDH_CoxDisc_adv <- tROC.multi(datGiven=datCredit, modGiven=modLR_Adv, month_End=predictTime, sLambda=0.05, estMethod="NN-0/1", numDigits=4, 
                                       fld_ID="DefSpell_Key", fld_Event="DefSpell_Event2", eventVal=1, fld_StartTime="Start", fld_EndTime="TimeInDefSpell",
                                       graphName="WOffSurvModel-ROC_CoxDisc_Advanced_TimeVar", genFigPathGiven=paste0(genFigPath, "tROC-Analyses/"), 
                                       caseStudyName=paste0("CoxDisc_CDH_", predictTime), numThreads=12, logPath=genPath, 
                                       predType="response")
proc.time() - ptm
objROC44_CDH_CoxDisc_adv$AUC; objROC44_CDH_CoxDisc_adv$ROC_graph
### RESULTS: AUC up to t: 97.15%, achieved in 16 785  secs


# --- 6.4 Survival tree
predictTime <- 44
ptm <- proc.time() #IGNORE: for computation time calculation;
objROC44_CDH_st <- tROC.multi(datGiven=datCredit, modGiven=NA, month_End=predictTime, sLambda=0.05, estMethod="NN-0/1", numDigits=4, 
                              fld_ID="DefSpell_Key", fld_Event="DefSpell_Event2", eventVal=1, fld_StartTime="Start", fld_EndTime="TimeInDefSpell",
                              graphName="WOffSurvModel-ROC_SurvTree_TimeVar", genFigPathGiven=paste0(genFigPath, "tROC-Analyses/"), 
                              caseStudyName=paste0("SurvTree_", predictTime), numThreads=12, logPath=genPath, 
                              predType="response", MarkerGiven="Hazard_survtree")
proc.time() - ptm
objROC44_CDH_st$AUC; objROC44_CDH_st$ROC_graph
### RESULTS: AUC up to t: 75.75%, achieved in 3049 secs


# --- 6.5 Save objects
saveRDS(objROC_LR, paste0(genPath,"WOffSurvModel-LR-ROC.rds"))
saveRDS(objROC44_CDH_CoxDisc_bas, paste0(genPath,"WOffSurvModel-CoxDisc-CDH-ROC_Depedendence_44_bas.rds"))
saveRDS(objROC44_CDH_CoxDisc_adv, paste0(genPath,"WOffSurvModel-CoxDisc-CDH-ROC_Depedendence_44_adv.rds"))
saveRDS(objROC44_CDH_st, paste0(genPath,"WOffSurvModel-CDH-SurvTree-ROC_Depedendence_44.rds"))




# ============ 7. Create combined ROC-graphs across multiple prediction times ============

# --- 7.1 Ensure required objects exist in memory
if (!exists('objROC44_LR')) objROC44_LR <- readRDS(paste0(genPath,"WOffSurvModel-LR-ROC.rds")); gc()
if (!exists('objROC44_CDH_CoxDisc_bas')) objROC44_CDH_CoxDisc_bas <- readRDS(paste0(genPath,"WOffSurvModel-CoxDisc-CDH-ROC_Depedendence_44_bas.rds")); gc()
if (!exists('objROC44_CDH_CoxDisc_adv')) objROC44_CDH_CoxDisc_adv <- readRDS(paste0(genPath,"WOffSurvModel-CoxDisc-CDH-ROC_Depedendence_44_adv.rds")); gc()
if (!exists('objROC44_CDH_st')) objROC44_CDH_st <- readRDS(paste0(genPath,"WOffSurvModel-CDH-SurvTree-ROC_Depedendence_44.rds")); gc()

# --- 7.2 Compare ROCs of the A-series models
# - Set ROC-parameters and initialize data structures
vecPercTimepoint <- c(44,44,44,44)
vecTROC <- list(objROC44_CDH_CoxDisc_adv, objROC44_CDH_CoxDisc_bas ,objROC44_LR, objROC44_CDH_st)
vLabels <- vector("list", length=length(vecPercTimepoint))
vModel <- c("DtH-Advanced A","DtH-Basic A", "Logistic Regression A", "Survival Tree")

# - Create a combined data object for plotting purposes
for (i in 1:length(vecPercTimepoint)) {
  # i <-1 # testing condition
  # datGraph <- data.frame(x = vFPR[-(nThresh+1)], y=vTPR[-1])
  
  # Create a data object for the current prediction time
  if (i==1) {
    datGraph <- data.table(PredictTime=paste0(letters[i], "_", vecPercTimepoint[i]), Threshold=vecTROC[[i]]$Thresholds, 
                           x=vecTROC[[i]]$FPR, y=vecTROC[[i]]$TPR)
    vLabels[[i]] <- bquote(.(vModel[i]) * "; AUC: " * .(formatC(vecTROC[[i]]$AUC * 100, format = "f", digits = 2)) * "%")
    
  } else if (i==2) {
    datGraph <- rbind(datGraph, 
                      data.table(PredictTime= paste0(letters[i], "_", vecPercTimepoint[i]), Threshold=vecTROC[[i]]$Thresholds, 
                                 x=vecTROC[[i]]$FPR, y=vecTROC[[i]]$TPR))
    vLabels[[i]] <- bquote(.(vModel[i]) * "; AUC: " * .(formatC(vecTROC[[i]]$AUC * 100, format = "f", digits = 2)) * "%")
  } else if (i==3) {
    datGraph <- rbind(datGraph, 
                      data.table(PredictTime= paste0(letters[i], "_", vecPercTimepoint[i]), Threshold=vecTROC[[i]]$thresholds, 
                                 x=1-vecTROC[[i]]$specificities, y=vecTROC[[i]]$sensitivities))
    vLabels[[i]] <- bquote(.(vModel[i]) * "; AUC: " * .(formatC(vecTROC[[i]]$auc * 100, format = "f", digits = 2)) * "%")
  } else if (i==4) {
    datGraph <- rbind(datGraph,
                      data.table(PredictTime=paste0(letters[i], "_", vecPercTimepoint[i]), Threshold=vecTROC[[i]]$Thresholds, 
                                 x=vecTROC[[i]]$FPR, y=vecTROC[[i]]$TPR))
    vLabels[[i]] <- bquote(.(vModel[i]) * "; AUC: " * .(formatC(vecTROC[[i]]$AUC * 100, format = "f", digits = 2)) * "%")
  }
  
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



# ============ 8. Housekeeping ============

cleanEnviron()



