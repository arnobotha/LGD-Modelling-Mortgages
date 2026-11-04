# ========================= TIME-DEPENDENT ROC-ANALYSIS ========================
# Conduct time-dependent ROC-analyses on the same fitted Cox regression model
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
#   - modLR_Classic | Classical cross-sectional (CS) logistic regression (LR) model as fitted in script 4c
#   - SurvTree_CTree | Survival tree as fitted in script 4e
#
# -- Outputs:
#   - <Analytics> | tROC-graphs
# ==============================================================================


# ============ 1. Preliminaries ============

# --- 1.1 Load and prepare data
# - Confirm prepared datasets are loaded into memory
if (!exists('datCredit_train_CDH_sub')) unpack.ffdf(paste0(genPath,"creditdata_train_CDH_sub"), tempPath);gc()
if (!exists('datCredit_valid_CDH_sub')) unpack.ffdf(paste0(genPath,"creditdata_valid_CDH_sub"), tempPath);gc()

# - Ensure order of data is correct
setorder(datCredit_train_CDH_sub, LoanID, Date)
setorder(datCredit_valid_CDH_sub, LoanID, Date)

# - Filter data for default spells and date selections
datCredit_train <- subset(datCredit_train_CDH_sub, !is.na(DefSpell_Key))
datCredit_valid <- subset(datCredit_valid_CDH_sub, !is.na(DefSpell_Key))

# - Combine training and validation datasets to facilitate combined data preparation and prediction
### NOTE: tROC-analyses will still be performed on the validation set
datCredit <- rbind(data.table(datCredit_train, Sample="Train"), 
                   data.table(datCredit_valid, Sample="Validation"))

# - Remove objects
rm(datCredit_train_CDH_sub, datCredit_valid_CDH_sub); gc()

# - Create a spell-level target variable for write-off instance
datCredit[, DefSpell_Event2:=DefSpell_Event]
datCredit[, DefSpell_Event:=DefSpellResol_Type_Hist=="WOFF"]

# - Transform categorical variables to factor
# Payment method
datCredit[,pmnt_method_grp_fac:=as.factor(pmnt_method_grp)]
# Previous defaults
datCredit[,PrevDefaults_fac:=as.factor(PrevDefaults)]

# - Recalculate counter variable with respect to new sampling window
datCredit[, Counter_Artificial := 1:.N, by=.(LoanID)]

# - Handle left-truncated spells by adding a starting record 
### NOTE:  This is necessary for calculating certain survival quantities later
# Create an additional record for each default spell
datAdd <- subset(datCredit, Counter_Artificial==1 & TimeInDefSpell>1)
datAdd[, TimeInDefSpell:=TimeInDefSpell-1]
datAdd[, Time_Binned := timeBinning(TimeInDefSpell)]
datAdd[, Counter_Artificial:=0]
# Add record to main dataset
datCredit <- rbind(datCredit, datAdd); setorder(datCredit, DefSpell_Key, Date)


# --- 1.2 Load models
# - Basic Discrete-time Hazard model | Classical Single-risk survival model
modLR_Bas <- readRDS(paste0(genObjPath,"CoxDisc_Basic_Model.rds"))

# - Advanced Discrete-time Hazard model | Classical Single-risk survival model
modLR_Adv <- readRDS(paste0(genObjPath,"CoxDisc_Advanced_Model.rds"))

# - Cross sectional logistic regression  (CS-LR) model
modLR_Classic <- readRDS(paste0(genObjPath,"LR_Model.rds"))

# - Tree model | Classical Single-risk survival model
SurvTree_CTree <- readRDS(paste0(genObjPath,"SurvTree_CTree.rds"))

# - Multinomial Discrete-time Hazard model | Competing risk survival model
unpack.ffdf(paste0(genPath,"MultiDtH_WOff"),tempPath)

# - Multinomial Conditional inference tree | Competing risk tree model
unpack.ffdf(paste0(genPath,"MultiCI_Tree"),tempPath)


# --- 1.3 Estimate event rates
# - Predict hazard h(t) = P(T=t | T>= t) in discrete-time
datCredit[, Hazard_adv:=predict(modLR_Adv, newdata=datCredit, type = "response")]
datCredit[, Hazard_bas:=predict(modLR_Bas, newdata=datCredit, type = "response")]
datCredit[, Hazard_CSLR:=predict(modLR_Classic, newdata=.SD[], type="response")]

# - Predict cause-specific hazard h_g(t) = P(T=t, G=g | T >= t) in discrete-time | DtH-Multinomial
matPredictions <- predict(modMLR, newdata=datCredit, type = "probs")
datCredit[, Hazard_WriteOff:=matPredictions[,"Write-off"]]
datCredit[, Hazard_Cure:=matPredictions[,"Cured"]]

# - Predict cause-specific hazard h_g(t) = P(T=t, G=g | T >= t) in discrete-time | Multinomial CI-tree
matPredictions_tree <- predict(modMultiSurvTree, newdata=datCredit, type = "prob")
datCredit[, Hazard_WriteOff_tree := matPredictions_tree[, "Write-off"]]
datCredit[, Hazard_Cured_tree := matPredictions_tree[, "Cured"]]

# - Derive survival probability S(t) = prod(1 - hazard)
datCredit[, Survival_adv:=cumprod(1-Hazard_adv), by=list(DefSpell_Key)]
datCredit[, Survival_bas:=cumprod(1-Hazard_bas), by=list(DefSpell_Key)]

# - Derive (event-free) survival probability S(t) = prod(1 - h_1(t) - h_2(t))
datCredit[, Survival_overall:=cumprod(1-Hazard_WriteOff-Hazard_Cure), by=.(DefSpell_Key)]
datCredit[, Survival_overall_tree:=cumprod(1-Hazard_WriteOff_tree-Hazard_Cured_tree), by=.(DefSpell_Key)]

# - Derive discrete density, or event probability f(t) = S(t-1) - S(t)
datCredit[, EventRate_adv:=shift(Survival_adv, type="lag", n=1, fill=1) - Survival_adv, by=list(DefSpell_Key)]
datCredit[, EventRate_bas:=shift(Survival_bas, type="lag", n=1, fill=1) - Survival_bas, by=list(DefSpell_Key)]

# - Derive event rates as f_1(t) = S(t-1) . h_1(t)
datCredit[, EventRate_WriteOff := shift(Survival_overall, n=1,fill=1) * Hazard_WriteOff, by=.(DefSpell_Key)]
datCredit[, EventRate_WriteOff_tree := shift(Survival_overall_tree, n=1,fill=1) * Hazard_WriteOff_tree, by=.(DefSpell_Key)]

# - Remove added rows
datCredit <- subset(datCredit, Counter_Artificial>0) %>% setorder(LoanID, Date)


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
###       - Uses the superior Nearest Neighbour Estimator (NNE) method for S(t) with a 0/1-kernel NNE-based for S(t)
###       - Assume dependence (by specifying ID-field) amongst certain observations clustered around ID-values

# --- 2.1 tROC analyses using the CD-approach | Time-window chosen as first 6 months in default
predictTime <- 6
ptm <- proc.time() #IGNORE: for computation time calculation;
objROC6_CDH_CoxDisc_bas <- tROC.multi(datGiven=datCredit[Sample=="Validation"], 
                                      Marker = datCredit[Sample=="Validation", Hazard_bas],
                                      month_End=predictTime, sLambda=0.05, numDigits=4, fld_ID="DefSpell_Key", 
                                      fld_Event="DefSpell_Event2", eventVal=1, fld_StartTime="Start", fld_EndTime="TimeInDefSpell",
                                      caseStudyName=paste0("tROC_", predictTime), numThreads=4, logPath=genPath, 
                                      Graph=T, genFigPathGiven=paste0(genFigPath, "tROC-Analyses/"),
                                      graphName = "DtH_Basic")
proc.time() - ptm
objROC6_CDH_CoxDisc_bas$AUC; objROC6_CDH_CoxDisc_bas$ROC_graph
### RESULTS: AUC up to t: 49.27%, achieved in 134.63 secs


# --- 2.2 tROC analyses using the CD-approach | Time-window chosen as first 12 months in default
predictTime <- 12
ptm <- proc.time() #IGNORE: for computation time calculation;
objROC12_CDH_CoxDisc_bas <-   tROC.multi(datGiven=datCredit[Sample=="Validation"], 
                                         Marker = datCredit[Sample=="Validation", Hazard_bas],
                                         month_End=predictTime, sLambda=0.05, numDigits=4, fld_ID="DefSpell_Key", 
                                         fld_Event="DefSpell_Event2", eventVal=1, fld_StartTime="Start", fld_EndTime="TimeInDefSpell",
                                         caseStudyName=paste0("tROC_", predictTime), numThreads=4, logPath=genPath, 
                                         Graph=T, genFigPathGiven=paste0(genFigPath, "tROC-Analyses/"),
                                         graphName = "DtH_Basic")
proc.time() - ptm
objROC12_CDH_CoxDisc_bas$AUC; objROC12_CDH_CoxDisc_bas$ROC_graph
### RESULTS: AUC up to t: 56.89%, achieved in 150.97  secs


# --- 2.3 tROC analyses using the CD-approach | Time-window chosen as first 24 months in default
predictTime <- 24
ptm <- proc.time() #IGNORE: for computation time calculation;
objROC24_CDH_CoxDisc_bas <-  tROC.multi(datGiven=datCredit[Sample=="Validation"], 
                                        Marker = datCredit[Sample=="Validation", Hazard_bas],
                                        month_End=predictTime, sLambda=0.05, numDigits=4, fld_ID="DefSpell_Key", 
                                        fld_Event="DefSpell_Event2", eventVal=1, fld_StartTime="Start", fld_EndTime="TimeInDefSpell",
                                        caseStudyName=paste0("tROC_", predictTime), numThreads=4, logPath=genPath, 
                                        Graph=T, genFigPathGiven=paste0(genFigPath, "tROC-Analyses/"),
                                        graphName = "DtH_Basic")
proc.time() - ptm
objROC24_CDH_CoxDisc_bas$AUC; objROC24_CDH_CoxDisc_bas$ROC_graph
### RESULTS: AUC up to t: 63.97%, achieved in 247.96 secs


# --- 2.4 tROC analyses using the CD-approach | Time-window chosen as first 48 months in default
predictTime <- 48
ptm <- proc.time() #IGNORE: for computation time calculation;
objROC48_CDH_CoxDisc_bas <-  tROC.multi(datGiven=datCredit[Sample=="Validation"], 
                                        Marker = datCredit[Sample=="Validation", Hazard_bas],
                                        month_End=predictTime, sLambda=0.05, numDigits=4, fld_ID="DefSpell_Key", 
                                        fld_Event="DefSpell_Event2", eventVal=1, fld_StartTime="Start", fld_EndTime="TimeInDefSpell",
                                        caseStudyName=paste0("tROC_", predictTime), numThreads=4, logPath=genPath, 
                                        Graph=T, genFigPathGiven=paste0(genFigPath, "tROC-Analyses/"),
                                        graphName = "DtH_Basic")
proc.time() - ptm
objROC48_CDH_CoxDisc_bas$AUC; objROC48_CDH_CoxDisc_bas$ROC_graph
### RESULTS: AUC up to t: 59.48%, achieved in 363.83  secs


# --- 2.5 Store experimental objects | Memory optimisation
# CDH-model: Basic Cox-regression model
saveRDS(objROC6_CDH_CoxDisc_bas, paste0(genPath,"WOffSurvModel-CoxDisc-CDH-ROC_Depedendence_06_bas.rds"))
saveRDS(objROC12_CDH_CoxDisc_bas, paste0(genPath,"WOffSurvModel-CoxDisc-CDH-ROC_Depedendence_12_bas.rds"))
saveRDS(objROC24_CDH_CoxDisc_bas, paste0(genPath,"WOffSurvModel-CoxDisc-CDH-ROC_Depedendence_24_bas.rds"))
saveRDS(objROC48_CDH_CoxDisc_bas, paste0(genPath,"WOffSurvModel-CoxDisc-CDH-ROC_Depedendence_48_bas.rds"))




# ============ 3. tROC analyses | Advanced discrete-time hazard model ============
### NOTE: - Using custom tROC()-function from script 0b(iii) under the CD-approach with an NN-estimator and 0/1-kernel
###       - Uses the superior Nearest Neighbour Estimator (NNE) method for S(t) with a 0/1-kernel NNE-based for S(t)
###       - Assume dependence (by specifying ID-field) amongst certain observations clustered around ID-values

# --- 3.1 tROC analyses using the CD-approach | Time-window chosen as first 6 months in default
predictTime <- 6
ptm <- proc.time() #IGNORE: for computation time calculation;
objROC6_CDH_CoxDisc_adv <-  tROC.multi(datGiven=datCredit[Sample=="Validation"], 
                                       Marker = datCredit[Sample=="Validation", Hazard_adv],
                                       month_End=predictTime, sLambda=0.05, numDigits=4, fld_ID="DefSpell_Key", 
                                       fld_Event="DefSpell_Event2", eventVal=1, fld_StartTime="Start", fld_EndTime="TimeInDefSpell",
                                       caseStudyName=paste0("tROC_", predictTime), numThreads=4, logPath=genPath, 
                                       Graph=T, genFigPathGiven=paste0(genFigPath, "tROC-Analyses/"),
                                       graphName = "DtH_Advanced")
proc.time() - ptm
objROC6_CDH_CoxDisc_adv$AUC; objROC6_CDH_CoxDisc_adv$ROC_graph
### RESULTS: AUC up to t: 96.24%, achieved in 1591.73  secs


# --- 3.2 tROC analyses using the CD-approach | Time-window chosen as first 12 months in default
predictTime <- 12
ptm <- proc.time() #IGNORE: for computation time calculation;
objROC12_CDH_CoxDisc_adv <- tROC.multi(datGiven=datCredit[Sample=="Validation"], 
                                       Marker = datCredit[Sample=="Validation", Hazard_adv],
                                       month_End=predictTime, sLambda=0.05, numDigits=4, fld_ID="DefSpell_Key", 
                                       fld_Event="DefSpell_Event2", eventVal=1, fld_StartTime="Start", fld_EndTime="TimeInDefSpell",
                                       caseStudyName=paste0("tROC_", predictTime), numThreads=4, logPath=genPath, 
                                       Graph=T, genFigPathGiven=paste0(genFigPath, "tROC-Analyses/"),
                                       graphName = "DtH_Advanced")
proc.time() - ptm
objROC12_CDH_CoxDisc_adv$AUC; objROC12_CDH_CoxDisc_adv$ROC_graph
### RESULTS: AUC up to t: 94.42%, achieved in 1951.69  secs


# --- 3.3 tROC analyses using the CD-approach | Time-window chosen as first 24 months in default
predictTime <- 24
ptm <- proc.time() #IGNORE: for computation time calculation;
objROC24_CDH_CoxDisc_adv <- tROC.multi(datGiven=datCredit[Sample=="Validation"], 
                                       Marker = datCredit[Sample=="Validation", Hazard_adv],
                                       month_End=predictTime, sLambda=0.05, numDigits=4, fld_ID="DefSpell_Key", 
                                       fld_Event="DefSpell_Event2", eventVal=1, fld_StartTime="Start", fld_EndTime="TimeInDefSpell",
                                       caseStudyName=paste0("tROC_", predictTime), numThreads=4, logPath=genPath, 
                                       Graph=T, genFigPathGiven=paste0(genFigPath, "tROC-Analyses/"),
                                       graphName = "DtH_Advanced")
proc.time() - ptm
objROC24_CDH_CoxDisc_adv$AUC; objROC24_CDH_CoxDisc_adv$ROC_grap
### RESULTS: AUC up to t: 92.43%, achieved in 2690.39 secs


# --- 3.4 tROC analyses using the CD-approach | Time-window chosen as first 48 months in default
predictTime <- 48
ptm <- proc.time() #IGNORE: for computation time calculation;
objROC48_CDH_CoxDisc_adv <- tROC.multi(datGiven=datCredit[Sample=="Validation"], 
                                       Marker = datCredit[Sample=="Validation", Hazard_adv],
                                       month_End=predictTime, sLambda=0.05, numDigits=4, fld_ID="DefSpell_Key", 
                                       fld_Event="DefSpell_Event2", eventVal=1, fld_StartTime="Start", fld_EndTime="TimeInDefSpell",
                                       caseStudyName=paste0("tROC_", predictTime), numThreads=4, logPath=genPath, 
                                       Graph=T, genFigPathGiven=paste0(genFigPath, "tROC-Analyses/"),
                                       graphName = "DtH_Advanced")
proc.time() - ptm
objROC48_CDH_CoxDisc_adv$AUC; objROC48_CDH_CoxDisc_adv$ROC_graph
### RESULTS: AUC up to t: 88.89%, achieved in 4392.67 secs


# --- 3.5 Store experimental objects | Memory optimisation
saveRDS(objROC6_CDH_CoxDisc_adv, paste0(genPath,"WOffSurvModel-CoxDisc-CDH-ROC_Depedendence_06_adv.rds"))
saveRDS(objROC12_CDH_CoxDisc_adv, paste0(genPath,"WOffSurvModel-CoxDisc-CDH-ROC_Depedendence_12_adv.rds"))
saveRDS(objROC24_CDH_CoxDisc_adv, paste0(genPath,"WOffSurvModel-CoxDisc-CDH-ROC_Depedendence_24_adv.rds"))
saveRDS(objROC48_CDH_CoxDisc_adv, paste0(genPath,"WOffSurvModel-CoxDisc-CDH-ROC_Depedendence_48_adv.rds"))




# ============ 4. tROC analyses | Conditional Inference Survival tree ============
### NOTE: - Using custom tROC()-function from script 0b(iii) under the CD-approach with an NN-estimator and 0/1-kernel
###       - Uses the superior Nearest Neighbour Estimator (NNE) method for S(t) with a 0/1-kernel NNE-based for S(t)
###       - Assume dependence (by specifying ID-field) amongst certain observations clustered around ID-values

# --- 4.1 tROC analyses using the CD-approach | Time-window chosen as first 6 months in default
predictTime <- 6
ptm <- proc.time() #IGNORE: for computation time calculation;
objROC6_CDH_st <- tROC.multi(datGiven=datCredit[Sample=="Validation"], 
                             Marker = datCredit[Sample=="Validation", Hazard_survtree],
                             month_End=predictTime, sLambda=0.05, numDigits=4, fld_ID="DefSpell_Key", 
                             fld_Event="DefSpell_Event2", eventVal=1, fld_StartTime="Start", fld_EndTime="TimeInDefSpell",
                             caseStudyName=paste0("tROC_", predictTime), numThreads=4, logPath=genPath, 
                             Graph=T, genFigPathGiven=paste0(genFigPath, "tROC-Analyses/"),
                             graphName = "CI_Tree")
proc.time() - ptm
objROC6_CDH_st$AUC; objROC6_CDH_st$ROC_graph
### RESULTS: AUC up to t: 90.72%, achieved in 108.71 secs


# --- 4.2 tROC analyses using the CD-approach | Time-window chosen as first 12 months in default
predictTime <- 12
ptm <- proc.time() #IGNORE: for computation time calculation;
objROC12_CDH_st <- tROC.multi(datGiven=datCredit[Sample=="Validation"], 
                              Marker = datCredit[Sample=="Validation", Hazard_survtree],
                              month_End=predictTime, sLambda=0.05, numDigits=4, fld_ID="DefSpell_Key", 
                              fld_Event="DefSpell_Event2", eventVal=1, fld_StartTime="Start", fld_EndTime="TimeInDefSpell",
                              caseStudyName=paste0("tROC_", predictTime), numThreads=4, logPath=genPath, 
                              Graph=T, genFigPathGiven=paste0(genFigPath, "tROC-Analyses/"),
                              graphName = "CI_Tree")
proc.time() - ptm
objROC12_CDH_st$AUC; objROC12_CDH_st$ROC_graph
### RESULTS: AUC up to t: 89.97%, achieved in 123.66 secs


# --- 4.3 tROC analyses using the CD-approach | Time-window chosen as first 24 months in default
predictTime <- 24
ptm <- proc.time() #IGNORE: for computation time calculation;
objROC24_CDH_st <- tROC.multi(datGiven=datCredit[Sample=="Validation"], 
                              Marker = datCredit[Sample=="Validation", Hazard_survtree],
                              month_End=predictTime, sLambda=0.05, numDigits=4, fld_ID="DefSpell_Key", 
                              fld_Event="DefSpell_Event2", eventVal=1, fld_StartTime="Start", fld_EndTime="TimeInDefSpell",
                              caseStudyName=paste0("tROC_", predictTime), numThreads=4, logPath=genPath, 
                              Graph=T, genFigPathGiven=paste0(genFigPath, "tROC-Analyses/"),
                              graphName = "CI_Tree")
proc.time() - ptm
objROC24_CDH_st$AUC; objROC24_CDH_st$ROC_graph
### RESULTS: AUC up to t: 83.26%, achieved in 180.04 secs


# --- 4.4 tROC analyses using the CD-approach | Time-window chosen as first 48 months in default
predictTime <- 48
ptm <- proc.time() #IGNORE: for computation time calculation;
objROC48_CDH_st <- tROC.multi(datGiven=datCredit[Sample=="Validation"], 
                              Marker = datCredit[Sample=="Validation", Hazard_survtree],
                              month_End=predictTime, sLambda=0.05, numDigits=4, fld_ID="DefSpell_Key", 
                              fld_Event="DefSpell_Event2", eventVal=1, fld_StartTime="Start", fld_EndTime="TimeInDefSpell",
                              caseStudyName=paste0("tROC_", predictTime), numThreads=4, logPath=genPath, 
                              Graph=T, genFigPathGiven=paste0(genFigPath, "tROC-Analyses/"),
                              graphName = "CI_Tree")
proc.time() - ptm
objROC48_CDH_st$AUC; objROC48_CDH_st$ROC_graph
### RESULTS: AUC up to t: 73.29%, achieved in 276.68  secs


# --- 4.5 Store experimental objects | Memory optimisation
saveRDS(objROC6_CDH_st, paste0(genPath,"WOffSurvModel-CDH-SurvTree-ROC_Depedendence_06.rds"))
saveRDS(objROC12_CDH_st, paste0(genPath,"WOffSurvModel-CDH-SurvTree-ROC_Depedendence_12.rds"))
saveRDS(objROC24_CDH_st, paste0(genPath,"WOffSurvModel-CDH-SurvTree-ROC_Depedendence_24.rds"))
saveRDS(objROC48_CDH_st, paste0(genPath,"WOffSurvModel-CDH-SurvTree-ROC_Depedendence_48.rds"))





# ============ 5. tROC analyses | Multinomial discrete-time hazard model ============
### NOTE: - Using custom tROC()-function from script 0b(iii) under the CD-approach with an NN-estimator and 0/1-kernel
###       - Uses the superior Nearest Neighbour Estimator (NNE) method for S(t) with a 0/1-kernel NNE-based for S(t)
###       - Assume dependence (by specifying ID-field) amongst certain observations clustered around ID-values

# --- 5.1 tROC analyses using the CD-approach | Time-window chosen as first 6 months in default
predictTime <- 6
ptm <- proc.time() #IGNORE: for computation time calculation;
objROC6_CDH_DtH_Multi <- tROC.multi(datGiven=datCredit[Sample=="Validation"], 
                             Marker = datCredit[Sample=="Validation", Hazard_WriteOff],
                             month_End=predictTime, sLambda=0.05, numDigits=4, fld_ID="DefSpell_Key", 
                             fld_Event="DefSpell_Event2", eventVal=1, fld_StartTime="Start", fld_EndTime="TimeInDefSpell",
                             caseStudyName=paste0("tROC_", predictTime), numThreads=4, logPath=genPath, 
                             Graph=T, genFigPathGiven=paste0(genFigPath, "tROC-Analyses/"),
                             graphName = "DtH_Multinomial")
proc.time() - ptm
objROC6_CDH_DtH_Multi$AUC; objROC6_CDH_DtH_Multi$ROC_graph
### RESULTS: AUC up to t: 95.89%, achieved in 935.86 secs


# --- 5.2 tROC analyses using the CD-approach | Time-window chosen as first 12 months in default
predictTime <- 12
ptm <- proc.time() #IGNORE: for computation time calculation;
objROC12_CDH_DtH_Multi <- tROC.multi(datGiven=datCredit[Sample=="Validation"], 
                              Marker = datCredit[Sample=="Validation", Hazard_WriteOff],
                              month_End=predictTime, sLambda=0.05, numDigits=4, fld_ID="DefSpell_Key", 
                              fld_Event="DefSpell_Event2", eventVal=1, fld_StartTime="Start", fld_EndTime="TimeInDefSpell",
                              caseStudyName=paste0("tROC_", predictTime), numThreads=4, logPath=genPath, 
                              Graph=T, genFigPathGiven=paste0(genFigPath, "tROC-Analyses/"),
                              graphName = "DtH_Multinomial")
proc.time() - ptm
objROC12_CDH_DtH_Multi$AUC; objROC12_CDH_DtH_Multi$ROC_graph
### RESULTS: AUC up to t: 94.82%, achieved in 1143.50 secs


# --- 5.3 tROC analyses using the CD-approach | Time-window chosen as first 24 months in default
predictTime <- 24
ptm <- proc.time() #IGNORE: for computation time calculation;
objROC24_CDH_DtH_Multi <- tROC.multi(datGiven=datCredit[Sample=="Validation"], 
                              Marker = datCredit[Sample=="Validation", Hazard_WriteOff],
                              month_End=predictTime, sLambda=0.05, numDigits=4, fld_ID="DefSpell_Key", 
                              fld_Event="DefSpell_Event2", eventVal=1, fld_StartTime="Start", fld_EndTime="TimeInDefSpell",
                              caseStudyName=paste0("tROC_", predictTime), numThreads=4, logPath=genPath, 
                              Graph=T, genFigPathGiven=paste0(genFigPath, "tROC-Analyses/"),
                              graphName = "DtH_Multinomial")
proc.time() - ptm
objROC24_CDH_DtH_Multi$AUC; objROC24_CDH_DtH_Multi$ROC_graph
### RESULTS: AUC up to t: 93.30%, achieved in 1729.17 secs


# --- 5.4 tROC analyses using the CD-approach | Time-window chosen as first 48 months in default
predictTime <- 48
ptm <- proc.time() #IGNORE: for computation time calculation;
objROC48_CDH_DtH_Multi <- tROC.multi(datGiven=datCredit[Sample=="Validation"], 
                              Marker = datCredit[Sample=="Validation", Hazard_WriteOff],
                              month_End=predictTime, sLambda=0.05, numDigits=4, fld_ID="DefSpell_Key", 
                              fld_Event="DefSpell_Event2", eventVal=1, fld_StartTime="Start", fld_EndTime="TimeInDefSpell",
                              caseStudyName=paste0("tROC_", predictTime), numThreads=4, logPath=genPath, 
                              Graph=T, genFigPathGiven=paste0(genFigPath, "tROC-Analyses/"),
                              graphName = "DtH_Multinomial")
proc.time() - ptm
objROC48_CDH_DtH_Multi$AUC; objROC48_CDH_DtH_Multi$ROC_graph
### RESULTS: AUC up to t: 90.73%, achieved in 2773.77 secs


# --- 4.5 Store experimental objects | Memory optimisation
saveRDS(objROC6_CDH_DtH_Multi, paste0(genPath,"WOffSurvModel-CDH-DtHMulti-ROC_Depedendence_06.rds"))
saveRDS(objROC12_CDH_DtH_Multi, paste0(genPath,"WOffSurvModel-CDH-DtHMulti-ROC_Depedendence_12.rds"))
saveRDS(objROC24_CDH_DtH_Multi, paste0(genPath,"WOffSurvModel-CDH-DtHMulti-ROC_Depedendence_24.rds"))
saveRDS(objROC48_CDH_DtH_Multi, paste0(genPath,"WOffSurvModel-CDH-DtHMulti-ROC_Depedendence_48.rds"))




# ============ 6. tROC analyses | Multinomial discrete-time hazard model ============
### NOTE: - Using custom tROC()-function from script 0b(iii) under the CD-approach with an NN-estimator and 0/1-kernel
###       - Uses the superior Nearest Neighbour Estimator (NNE) method for S(t) with a 0/1-kernel NNE-based for S(t)
###       - Assume dependence (by specifying ID-field) amongst certain observations clustered around ID-values

# --- 6.1 tROC analyses using the CD-approach | Time-window chosen as first 6 months in default
predictTime <- 6
ptm <- proc.time() #IGNORE: for computation time calculation;
objROC6_CDH_MultiCI <- tROC.multi(datGiven=datCredit[Sample=="Validation"], 
                                    Marker = datCredit[Sample=="Validation", Hazard_WriteOff_tree],
                                    month_End=predictTime, sLambda=0.05, numDigits=4, fld_ID="DefSpell_Key", 
                                    fld_Event="DefSpell_Event2", eventVal=1, fld_StartTime="Start", fld_EndTime="TimeInDefSpell",
                                    caseStudyName=paste0("tROC_", predictTime), numThreads=4, logPath=genPath, 
                                    Graph=T, genFigPathGiven=paste0(genFigPath, "tROC-Analyses/"),
                                    graphName = "Multi_CI_Tree")
proc.time() - ptm
objROC6_CDH_MultiCI$AUC; objROC6_CDH_MultiCI$ROC_graph
### RESULTS: AUC up to t: 98.69%, achieved in 23.48  secs


# --- 6.2 tROC analyses using the CD-approach | Time-window chosen as first 12 months in default
predictTime <- 12
ptm <- proc.time() #IGNORE: for computation time calculation;
objROC12_CDH_MultiCI <- tROC.multi(datGiven=datCredit[Sample=="Validation"], 
                                     Marker = datCredit[Sample=="Validation", Hazard_WriteOff_tree],
                                     month_End=predictTime, sLambda=0.05, numDigits=4, fld_ID="DefSpell_Key", 
                                     fld_Event="DefSpell_Event2", eventVal=1, fld_StartTime="Start", fld_EndTime="TimeInDefSpell",
                                     caseStudyName=paste0("tROC_", predictTime), numThreads=4, logPath=genPath, 
                                     Graph=T, genFigPathGiven=paste0(genFigPath, "tROC-Analyses/"),
                                     graphName = "Multi_CI_Tree")
proc.time() - ptm
objROC12_CDH_MultiCI$AUC; objROC12_CDH_MultiCI$ROC_graph
### RESULTS: AUC up to t: 98.49%, achieved in 24.79  secs


# --- 6.3 tROC analyses using the CD-approach | Time-window chosen as first 24 months in default
predictTime <- 24
ptm <- proc.time() #IGNORE: for computation time calculation;
objROC24_CDH_MultiCI <- tROC.multi(datGiven=datCredit[Sample=="Validation"], 
                                     Marker = datCredit[Sample=="Validation", Hazard_WriteOff_tree],
                                     month_End=predictTime, sLambda=0.05, numDigits=4, fld_ID="DefSpell_Key", 
                                     fld_Event="DefSpell_Event2", eventVal=1, fld_StartTime="Start", fld_EndTime="TimeInDefSpell",
                                     caseStudyName=paste0("tROC_", predictTime), numThreads=4, logPath=genPath, 
                                     Graph=T, genFigPathGiven=paste0(genFigPath, "tROC-Analyses/"),
                                     graphName = "Multi_CI_Tree")
proc.time() - ptm
objROC24_CDH_MultiCI$AUC; objROC24_CDH_MultiCI$ROC_graph
### RESULTS: AUC up to t: 96.99%, achieved in 27.62  secs


# --- 6.4 tROC analyses using the CD-approach | Time-window chosen as first 48 months in default
predictTime <- 48
ptm <- proc.time() #IGNORE: for computation time calculation;
objROC48_CDH_MultiCI <- tROC.multi(datGiven=datCredit[Sample=="Validation"], 
                                     Marker = datCredit[Sample=="Validation", Hazard_WriteOff_tree],
                                     month_End=predictTime, sLambda=0.05, numDigits=4, fld_ID="DefSpell_Key", 
                                     fld_Event="DefSpell_Event2", eventVal=1, fld_StartTime="Start", fld_EndTime="TimeInDefSpell",
                                     caseStudyName=paste0("tROC_", predictTime), numThreads=4, logPath=genPath, 
                                     Graph=T, genFigPathGiven=paste0(genFigPath, "tROC-Analyses/"),
                                     graphName = "Multi_CI_Tree")
proc.time() - ptm
objROC48_CDH_MultiCI$AUC; objROC48_CDH_MultiCI$ROC_graph
### RESULTS: AUC up to t: 0.93.44%, achieved in 30.89  secs


# --- 4.5 Store experimental objects | Memory optimisation
saveRDS(objROC6_CDH_MultiCI, paste0(genPath,"WOffSurvModel-CDH-MultiCI_Tree-ROC_Depedendence_06.rds"))
saveRDS(objROC12_CDH_MultiCI, paste0(genPath,"WOffSurvModel-CDH-MultiCI_Tree-ROC_Depedendence_12.rds"))
saveRDS(objROC24_CDH_MultiCI, paste0(genPath,"WOffSurvModel-CDH-MultiCI_Tree-ROC_Depedendence_24.rds"))
saveRDS(objROC48_CDH_MultiCI, paste0(genPath,"WOffSurvModel-CDH-MultiCI_Tree-ROC_Depedendence_48.rds"))





# ============ 7. Create combined ROC-graphs across multiple prediction times ============

# --- 7.1 Basic discrete-time hazard model
# - Ensure required objects exist in memory
if (!exists('objROC6_CDH_CoxDisc_bas')) objROC6_CDH_CoxDisc_bas <- readRDS(paste0(genPath,"WOffSurvModel-CoxDisc-CDH-ROC_Depedendence_06_bas.rds"))
if (!exists('objROC12_CDH_CoxDisc_bas')) objROC12_CDH_CoxDisc_bas <- readRDS(paste0(genPath,"WOffSurvModel-CoxDisc-CDH-ROC_Depedendence_12_bas.rds"))
if (!exists('objROC24_CDH_CoxDisc_bas')) objROC24_CDH_CoxDisc_bas <- readRDS(paste0(genPath,"WOffSurvModel-CoxDisc-CDH-ROC_Depedendence_24_bas.rds"))
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
datGraph[, FacetLabel:="Basic discrete-time hazard model"]
vCol <- brewer.pal(8,"Set1")
vLabels_F <- setNames(vLabels, paste0(letters[1:length(vecPercTimepoint)],"_", vecPercTimepoint))
chosenFont <- "Cambria"
# Create ROC-graph
(gg <- ggplot(datGraph, aes(x=x,y=y,group=PredictTime)) + theme_minimal() + 
    theme(text = element_text(family=chosenFont), legend.position="inside", 
          strip.background=element_rect(fill="snow2", colour="snow2"),
          strip.text=element_text(size=8, colour="gray50"), strip.text.y.right=element_text(angle=90),
          legend.position.inside=c(0.65,0.25),
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
dpi <- 250
ggsave(gg, file=paste0(paste0(genFigPath,"/tROC-Analyses/", "WOffSurvModel_CombinedROC_DtH_Basic.png")), 
       width=1200/dpi, height=1000/dpi, dpi=dpi, bg="white")


# --- 7.2 Advanced discrete-time hazard model
# - Ensure required objects exist in memory
if (!exists('objROC6_CDH_CoxDisc_adv')) objROC6_CDH_CoxDisc_adv <- readRDS(paste0(genPath,"WOffSurvModel-CoxDisc-CDH-ROC_Depedendence_06_adv.rds"))
if (!exists('objROC12_CDH_CoxDisc_adv')) objROC12_CDH_CoxDisc_adv <- readRDS(paste0(genPath,"WOffSurvModel-CoxDisc-CDH-ROC_Depedendence_12_adv.rds"))
if (!exists('objROC24_CDH_CoxDisc_adv')) objROC24_CDH_CoxDisc_adv <- readRDS(paste0(genPath,"WOffSurvModel-CoxDisc-CDH-ROC_Depedendence_24_adv.rds"))
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
datGraph[, FacetLabel := "Advanced discrete-time hazard model"]
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
dpi <- 250
ggsave(gg, file=paste0(paste0(genFigPath,"/tROC-Analyses/", "WOffSurvModel_CombinedROC_DtH_Advanced.png")), 
       width=1200/dpi, height=1000/dpi, dpi=dpi, bg="white")


# --- 7.3 Survival tree
# - Ensure required objects exist in memory
if (!exists('objROC6_SurvTree')) objROC6_CDH_st <- readRDS(paste0(genPath,"WOffSurvModel-CDH-SurvTree-ROC_Depedendence_06.rds"))
if (!exists('objROC12_SurvTree')) objROC12_CDH_st <- readRDS(paste0(genPath,"WOffSurvModel-CDH-SurvTree-ROC_Depedendence_12.rds"))
if (!exists('objROC24_SurvTree')) objROC24_CDH_st <- readRDS(paste0(genPath,"WOffSurvModel-CDH-SurvTree-ROC_Depedendence_24.rds"))
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
datGraph[, FacetLabel := "Conditional inference survival tree"]
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
dpi <- 250
ggsave(gg, file=paste0(paste0(genFigPath,"/tROC-Analyses/", "WOffSurvModel_CombinedROC_SurvTree.png")), 
       width=1200/dpi, height=1000/dpi, dpi=dpi, bg="white")


# --- 7.4 Multinomial discrete-time hazard model
# - Ensure required objects exist in memory
if (!exists('objROC6_CDH_DtH_Multi')) objROC6_CDH_CoxDisc_bas <- readRDS(paste0(genPath,"WOffSurvModel-CDH-DtHMulti-ROC_Depedendence_06.rds"))
if (!exists('objROC12_CDH_DtH_Multi')) objROC12_CDH_CoxDisc_bas <- readRDS(paste0(genPath,"WOffSurvModel-CDH-DtHMulti-ROC_Depedendence_12.rds"))
if (!exists('objROC24_CDH_DtH_Multi')) objROC24_CDH_CoxDisc_bas <- readRDS(paste0(genPath,"WOffSurvModel-CDH-DtHMulti-ROC_Depedendence_24.rds"))
if (!exists('objROC48_CDH_DtH_Multi')) objROC48_CDH_CoxDisc_bas <- readRDS(paste0(genPath,"WOffSurvModel-CDH-DtHMulti-ROC_Depedendence_48.rds")); gc()

# - Set ROC-parameters and initialize data structures
vecPercTimepoint <- c(6,12,24,48)
vecTROC <- list(objROC6_CDH_DtH_Multi, objROC12_CDH_DtH_Multi,
                objROC24_CDH_DtH_Multi, objROC48_CDH_DtH_Multi)
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
datGraph[, FacetLabel:="Multinomial discrete-time hazard model"]
vCol <- brewer.pal(8,"Set1")
vLabels_F <- setNames(vLabels, paste0(letters[1:length(vecPercTimepoint)],"_", vecPercTimepoint))
chosenFont <- "Cambria"
# Create ROC-graph
(gg <- ggplot(datGraph, aes(x=x,y=y,group=PredictTime)) + theme_minimal() + 
    theme(text = element_text(family=chosenFont), legend.position="inside", 
          strip.background=element_rect(fill="snow2", colour="snow2"),
          strip.text=element_text(size=8, colour="gray50"), strip.text.y.right=element_text(angle=90),
          legend.position.inside=c(0.65,0.25),
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
dpi <- 250
ggsave(gg, file=paste0(paste0(genFigPath,"/tROC-Analyses/", "WOffSurvModel_CombinedROC_DtH_Multinomial.png")), 
       width=1200/dpi, height=1000/dpi, dpi=dpi, bg="white")


# --- 7.5 Multinomial conditional inference tree
# - Ensure required objects exist in memory
if (!exists('objROC6_CDH_MultiCI')) objROC6_CDH_CoxDisc_bas <- readRDS(paste0(genPath,"WOffSurvModel-CDH-MultiCI_Tree-ROC_Depedendence_06.rds"))
if (!exists('objROC12_CDH_MultiCI')) objROC12_CDH_CoxDisc_bas <- readRDS(paste0(genPath,"WOffSurvModel-CDH-MultiCI_Tree-ROC_Depedendence_12.rds"))
if (!exists('objROC24_CDH_MultiCI')) objROC24_CDH_CoxDisc_bas <- readRDS(paste0(genPath,"WOffSurvModel-CDH-MultiCI_Tree-ROC_Depedendence_24.rds"))
if (!exists('objROC48_CDH_MultiCI')) objROC48_CDH_CoxDisc_bas <- readRDS(paste0(genPath,"WOffSurvModel-CDH-MultiCI_Tree-ROC_Depedendence_48.rds")); gc()

# - Set ROC-parameters and initialize data structures
vecPercTimepoint <- c(6,12,24,48)
vecTROC <- list(objROC6_CDH_MultiCI, objROC12_CDH_MultiCI,
                objROC24_CDH_MultiCI, objROC48_CDH_MultiCI)
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
datGraph[, FacetLabel:="Multinomial conditional inference tree"]
vCol <- brewer.pal(8,"Set1")
vLabels_F <- setNames(vLabels, paste0(letters[1:length(vecPercTimepoint)],"_", vecPercTimepoint))
chosenFont <- "Cambria"
# Create ROC-graph
(gg <- ggplot(datGraph, aes(x=x,y=y,group=PredictTime)) + theme_minimal() + 
    theme(text = element_text(family=chosenFont), legend.position="inside", 
          strip.background=element_rect(fill="snow2", colour="snow2"),
          strip.text=element_text(size=8, colour="gray50"), strip.text.y.right=element_text(angle=90),
          legend.position.inside=c(0.65,0.25),
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
dpi <- 250
ggsave(gg, file=paste0(paste0(genFigPath,"/tROC-Analyses/", "WOffSurvModel_CombinedROC_MultiCI_Tree.png")), 
       width=1200/dpi, height=1000/dpi, dpi=dpi, bg="white")





# ============ 8. Comparing ROC analyses at a specific spell time for all models ============
### NOTE: - Using custom tROC()-function from script 0b(iii) under the CD-approach with an NN-estimator and 0/1-kernel
###       - Uses the superior Nearest Neighbour Estimator (NNE) method for S(t) with a 0/1-kernel NNE-based for S(t)
###       - Assume dependence (by specifying ID-field) amongst certain observations clustered around ID-values

# --- 8.1 Classical logistic regression model
# - Create an event indicator
datCredit[, DefSpell_Event3 := ifelse(DefSpellResol_Type_Hist=="WOFF", 1, 0)]

# - Run ROC analyses on the subet data
ptm <- proc.time() #IGNORE: for computation time calculation
objROC_CSLR <- roc(response=datCredit[DefSpell_Counter==1, DefSpell_Event3], 
                 predictor=datCredit[DefSpell_Counter==1, Hazard_CSLR])
proc.time() - ptm
objROC_CSLR$auc; plot(objROC_CSLR)
### RESULTS: AUC up to t: 80.81%, achieved in 1 secs


# --- 8.2 Basic discrete-time hazard model
predictTime <- 41
ptm <- proc.time() #IGNORE: for computation time calculation
objROC41_CDH_CoxDisc_bas <- tROC.multi(datGiven=datCredit[Sample=="Validation"], 
                                       Marker = datCredit[Sample=="Validation", Hazard_bas],
                                       month_End=predictTime, sLambda=0.05, numDigits=4, fld_ID="DefSpell_Key", 
                                       fld_Event="DefSpell_Event2", eventVal=1, fld_StartTime="Start", fld_EndTime="TimeInDefSpell",
                                       caseStudyName=paste0("tROC_", predictTime), numThreads=4, logPath=genPath, 
                                       Graph=T, genFigPathGiven=paste0(genFigPath, "tROC-Analyses/"),
                                       graphName = "DtH_Basic")
proc.time() - ptm
objROC41_CDH_CoxDisc_bas$AUC; objROC41_CDH_CoxDisc_bas$ROC_graph
### RESULTS: AUC up to t: 61.93%, achieved in 360.73 secs


# --- 8.3 Advanced discrete-time hazard model
ptm <- proc.time() #IGNORE: for computation time calculation
objROC41_CDH_CoxDisc_adv <- tROC.multi(datGiven=datCredit[Sample=="Validation"], 
                                       Marker = datCredit[Sample=="Validation", Hazard_adv],
                                       month_End=predictTime, sLambda=0.05, numDigits=4, fld_ID="DefSpell_Key", 
                                       fld_Event="DefSpell_Event2", eventVal=1, fld_StartTime="Start", fld_EndTime="TimeInDefSpell",
                                       caseStudyName=paste0("tROC_", predictTime), numThreads=4, logPath=genPath, 
                                       Graph=T, genFigPathGiven=paste0(genFigPath, "tROC-Analyses/"),
                                       graphName = "DtH_Advanced")
proc.time() - ptm
objROC41_CDH_CoxDisc_adv$AUC; objROC41_CDH_CoxDisc_adv$ROC_graph
### RESULTS: AUC up to t: 97.15%, achieved in 16 785  secs


# --- 8.4 Conditional inference survival tree
ptm <- proc.time() #IGNORE: for computation time calculation;
objROC41_CDH_st <- tROC.multi(datGiven=datCredit[Sample=="Validation"], 
                              Marker = datCredit[Sample=="Validation", Hazard_survtree],
                              month_End=predictTime, sLambda=0.05, numDigits=4, fld_ID="DefSpell_Key", 
                              fld_Event="DefSpell_Event2", eventVal=1, fld_StartTime="Start", fld_EndTime="TimeInDefSpell",
                              caseStudyName=paste0("tROC_", predictTime), numThreads=4, logPath=genPath, 
                              Graph=T, genFigPathGiven=paste0(genFigPath, "tROC-Analyses/"),
                              graphName = "CI_Tree")
proc.time() - ptm
objROC41_CDH_st$AUC; objROC41_CDH_st$ROC_graph
### RESULTS: AUC up to t: 75.75%, achieved in 3049 secs


# --- 8.5 Multinomial discrete-time hazard model
ptm <- proc.time() #IGNORE: for computation time calculation
objROC41_CDH_CoxDisc_Multi <- tROC.multi(datGiven=datCredit[Sample=="Validation"], 
                                       Marker = datCredit[Sample=="Validation", Hazard_WriteOff],
                                       month_End=predictTime, sLambda=0.05, numDigits=4, fld_ID="DefSpell_Key", 
                                       fld_Event="DefSpell_Event2", eventVal=1, fld_StartTime="Start", fld_EndTime="TimeInDefSpell",
                                       caseStudyName=paste0("tROC_", predictTime), numThreads=4, logPath=genPath, 
                                       Graph=T, genFigPathGiven=paste0(genFigPath, "tROC-Analyses/"),
                                       graphName = "DtH_Multinomial")
proc.time() - ptm
objROC41_CDH_CoxDisc_Multi$AUC; objROC41_CDH_CoxDisc_Multi$ROC_graph
### RESULTS: AUC up to t: 97.15%, achieved in 16 785  secs


# --- 8.6 Multinomial Conditional inference tree
ptm <- proc.time() #IGNORE: for computation time calculation;
objROC41_CDH_Multi_CI <- tROC.multi(datGiven=datCredit[Sample=="Validation"], 
                              Marker = datCredit[Sample=="Validation", Hazard_WriteOff_tree],
                              month_End=predictTime, sLambda=0.05, numDigits=4, fld_ID="DefSpell_Key", 
                              fld_Event="DefSpell_Event2", eventVal=1, fld_StartTime="Start", fld_EndTime="TimeInDefSpell",
                              caseStudyName=paste0("tROC_", predictTime), numThreads=4, logPath=genPath, 
                              Graph=T, genFigPathGiven=paste0(genFigPath, "tROC-Analyses/"),
                              graphName = "Multi_CI_Tree")
proc.time() - ptm
objROC41_CDH_Multi_CI$AUC; objROC41_CDH_Multi_CI$ROC_graph
### RESULTS: AUC up to t: 75.75%, achieved in 3049 secs


# --- 6.5 Save objects
saveRDS(objROC_CSLR, paste0(genPath,"WOffSurvModel-CSLR-ROC.rds"))
saveRDS(objROC41_CDH_CoxDisc_bas, paste0(genPath,"WOffSurvModel-CoxDisc-CDH-ROC_Depedendence_41_bas.rds"))
saveRDS(objROC41_CDH_CoxDisc_adv, paste0(genPath,"WOffSurvModel-CoxDisc-CDH-ROC_Depedendence_41_adv.rds"))
saveRDS(objROC41_CDH_st, paste0(genPath,"WOffSurvModel-CDH-SurvTree-ROC_Depedendence_41.rds"))
saveRDS(objROC41_CDH_CoxDisc_Multi, paste0(genPath,"WOffSurvModel-CDH-DtH_Multi-ROC_Depedendence_41.rds"))
saveRDS(objROC41_CDH_Multi_CI, paste0(genPath,"WOffSurvModel-CDH-MultiCI_Tree-ROC_Depedendence_41.rds"))



# ============ 9. Create combined ROC-graphs across multiple prediction times ============

# --- 9.1 Ensure required objects exist in memory
if (!exists('objROC_CSLR')) objROC41_LR <- readRDS(paste0(genPath,"WOffSurvModel-CSLR-ROC.rds"))
if (!exists('objROC41_CDH_CoxDisc_bas')) objROC41_CDH_CoxDisc_bas <- readRDS(paste0(genPath,"WOffSurvModel-CoxDisc-CDH-ROC_Depedendence_41_bas.rds"))
if (!exists('objROC41_CDH_CoxDisc_adv')) objROC41_CDH_CoxDisc_adv <- readRDS(paste0(genPath,"WOffSurvModel-CoxDisc-CDH-ROC_Depedendence_41_adv.rds"))
if (!exists('objROC41_CDH_st')) objROC41_CDH_st <- readRDS(paste0(genPath,"WOffSurvModel-CDH-SurvTree-ROC_Depedendence_41.rds"))
if (!exists('objROC41_CDH_CoxDisc_Multi')) objROC41_CDH_CoxDisc_Multi <- readRDS(paste0(genPath,"WOffSurvModel-CDH-DtH_Multi-ROC_Depedendence_41.rds"))
if (!exists('objROC41_CDH_Multi_CI')) objROC41_CDH_Multi_CI <- readRDS(paste0(genPath,"WOffSurvModel-CDH-MultiCI_Tree-ROC_Depedendence_41.rds")); gc()

# --- 9.2 Compare ROCs of the A-series models
# - Recast ROC-object from CSLR-model into a common format in accordance with the tROC-objects
objROC_CSLR_recast <- data.table(Thresholds=objROC_CSLR$thresholds, TPR=(objROC_CSLR$sensitivities),
                           FPR=rev(objROC_CSLR$specificities), AUC=objROC_CSLR$auc)

# - Initialize data structure
vecTROC <- list(objROC_CSLR_recast, objROC41_CDH_CoxDisc_bas, objROC41_CDH_CoxDisc_adv, objROC41_CDH_st,
                objROC41_CDH_CoxDisc_Multi,objROC41_CDH_Multi_CI)
vLabels <- vector("list", length=length(vecPercTimepoint))
vModel <- c("CS-LR", "DtH-Basic","DtH-Advanced", "CI-tree", "DtH-Multinomial", "Multinomial CI-tree")

# - Create a combined data object for plotting purposes
for (i in 1:length(vecTROC)) {
  # i <- 1 # testing condition
  # datGraph <- data.frame(x = vFPR[-(nThresh+1)], y=vTPR[-1])
  
  # - Create a data object for the current prediction time
  if (i == 1) {
    datGraph <- data.table(PredictTime=paste0(letters[i], "_", predictTime), Threshold=vecTROC[[i]]$Thresholds, 
                           x=vecTROC[[i]]$FPR, y=vecTROC[[i]]$TPR, Model=paste0(letters[i],"_", predictTime),
                           ModelName=vModel[i])
  } else {
    datGraph <- rbind(datGraph, 
                      data.table(PredictTime= paste0(letters[i], "_", predictTime), Threshold=vecTROC[[i]]$Thresholds, 
                                 x=vecTROC[[i]]$FPR, y=vecTROC[[i]]$TPR, Model=paste0(letters[i],"_", predictTime),
                                 ModelName=vModel[i]))
  }
  vLabels[[i]] <- bquote(.(vModel[i])*" AUC: "*.(round(vecTROC[[i]]$AUC*100, digits=2))*"%")
}

# - Set graphing parameters
datGraph[, FacetLabel := paste0("ROC(italic(t))~' at prediction time of t = ", predictTime, "'")]
vCol <- brewer.pal(6,"Dark2")
vLabels_F <- setNames(vLabels, paste0(letters[1:length(vecTROC)],"_", predictTime))
chosenFont <- "Cambria"

# - Create ROC-graph
(gg <- ggplot(datGraph, aes(x=x,y=y,group=Model)) + theme_minimal() + 
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
    geom_step(aes(x=x, y=y, linetype=Model, colour=Model), linewidth=0.5) + 
    #geom_point(aes(x=x, y=y, shape=PredictTime, colour=PredictTime), size=0.25) +
    # Facets and scales
    facet_grid(FacetLabel ~ ., labeller = label_parsed) +  
    #facet_wrap(~FacetLabel, labeller = label_parsed, strip.position = "right") +
    scale_color_manual(name="", values=vCol, labels=vLabels_F) + 
    scale_linetype_discrete(name="", labels=vLabels_F) + 
    scale_shape_discrete(name="", labels=vLabels_F) + 
    scale_y_continuous(label=percent) + scale_x_continuous(label=percent))

# - Save graph
dpi <- 250
ggsave(gg, file=paste0(paste0(genFigPath,"/tROC-Analyses/", "WOffModel-Combined.png")), 
       width=1800/dpi, height=1500/dpi, dpi=dpi, bg="white")



# ============ 9. Housekeeping ============

cleanEnviron()
