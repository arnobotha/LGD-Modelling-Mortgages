# ========================= TIME-DEPENDENT ROC-ANALYSIS ==========================
# Compare various functions from various packages in conducting time-dependent 
# ROC-analyses on the same fitted Cox regression model, having used the 
# prepared credit data
# --------------------------------------------------------------------------------
# PROJECT TITLE: Default Survival Modelling
# SCRIPT AUTHOR(S): Dr Arno Botha (AB), Mohammed Gabru (MG)
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




# ----------------- 2. Fit a discrete-time hazard model on the resampled prepared data


# ------ Basic discrete-time hazard model
# - Initialize variables
vars_basic <- c("log(TimeInDefSpell)","DefSpell_Num_binned", "g0_Delinq_Lag_1",
                "M_Inflation_Growth_9","g0_Delinq_Any_Aggr_Prop_Lag_1")

# - Fit discrete-time hazard model with selected variables
modLR_basic <- glm( as.formula(paste("DefSpell_Event ~", paste(vars_basic, collapse = " + "))),
                    data=datCredit_train, family="binomial", weights = Weight)



# ------ Advanced discrete-time hazard model
# - Initialize variables
vars <- c("Time_Binned","log(TimeInDefSpell)*DefSpell_Num_binned", 
          "DefaultStatus1_Aggr_Prop_Lag_12","g0_Delinq_Ave",
          "InterestRate_Margin_Aggr_Med_9","NewLoans_Aggr_Prop","InterestRate_Nom",
          "Balance_adj_WOff","pmnt_method_grp","Principal","g0_Delinq_Lag_1",
          "M_RealIncome_Growth_9", "M_Inflation_Growth_12","M_DTI_Growth_12","M_Repo_Rate_12","g0_Delinq_Any_Aggr_Prop_Lag_1")

# - Fit discrete-time hazard model with selected variables
modLR <- glm( as.formula(paste("DefSpell_Event ~", paste(vars, collapse = " + "))),
              data=datCredit_train, family="binomial", weights = Weight)




# ------ Basic discrete-time hazard model

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

# -- Multi-threaded calculation of the # AUC from given start up to given prediction time 3 in following the CD-approach
# NOTE: Uses the superior Nearest Neighbour Estimator (NNE) method for S(t) with a 0/1-kernelNNE-kernel for S(t)
# NOTE2: Assume dependence (by specifying ID-field) amongst certain observations clustered around ID-values
ptm <- proc.time() #IGNORE: for computation time calculation;
predictTime <- 12
objROC2_CDH_CoxDisc_bas <- tROC.multi(datGiven=datCredit_valid, modGiven=modLR_basic, month_End=predictTime, sLambda=0.05, estMethod="NN-0/1", numDigits=4, 
                                        fld_ID="DefSpell_Key", fld_Event="DefSpell_Event", eventVal=1, fld_StartTime="Start", fld_EndTime="TimeInDefSpell",
                                        graphName="WOffSurvModel-ROC_CoxDisc_Basic_TimeVar", genFigPathGiven=paste0(genFigPath, "tROC-Analyses/"), 
                                        caseStudyName=paste0("CoxDisc_CDH_", predictTime), numThreads=12, logPath=genPath, 
                                        predType="response")
objROC2_CDH_CoxDisc_bas$AUC; objROC2_CDH_CoxDisc_bas$ROC_graph
proc.time() - ptm
### RESULTS: AUC up to t: 94.53%, achieved in  1566.49 secs


# -- Multi-threaded calculation of the # AUC from given start up to given prediction time 3 in following the CD-approach
# NOTE: Uses the superior Nearest Neighbour Estimator (NNE) method for S(t) with a 0/1-kernelNNE-kernel for S(t)
# NOTE2: Assume dependence (by specifying ID-field) amongst certain observations clustered around ID-values
ptm <- proc.time() #IGNORE: for computation time calculation;
predictTime <- 24
objROC3_CDH_CoxDisc_bas <- tROC.multi(datGiven=datCredit_valid, modGiven=modLR_basic, month_End=predictTime, sLambda=0.05, estMethod="NN-0/1", numDigits=4, 
                                        fld_ID="DefSpell_Key", fld_Event="DefSpell_Event", eventVal=1, fld_StartTime="Start", fld_EndTime="TimeInDefSpell",
                                        graphName="WOffSurvModel-ROC_CoxDisc_Basic_TimeVar", genFigPathGiven=paste0(genFigPath, "tROC-Analyses/"), 
                                        caseStudyName=paste0("CoxDisc_CDH_", predictTime), numThreads=12, logPath=genPath, 
                                        predType="response")
objROC3_CDH_CoxDisc_bas$AUC; objROC3_CDH_CoxDisc_bas$ROC_graph
proc.time() - ptm
### RESULTS: AUC up to t: 94.47%, achieved in 2667.72 secs


# -- Multi-threaded calculation of the # AUC from given start up to given prediction time 3 in following the CD-approach
# NOTE: Uses the superior Nearest Neighbour Estimator (NNE) method for S(t) with a 0/1-kernelNNE-kernel for S(t)
# NOTE2: Assume dependence (by specifying ID-field) amongst certain observations clustered around ID-values
ptm <- proc.time() #IGNORE: for computation time calculation;
predictTime <- 48
objROC4_CDH_CoxDisc_bas <- tROC.multi(datGiven=datCredit_valid, modGiven=modLR_basic, month_End=predictTime, sLambda=0.05, estMethod="NN-0/1", numDigits=4, 
                                        fld_ID="DefSpell_Key", fld_Event="DefSpell_Event", eventVal=1, fld_StartTime="Start", fld_EndTime="TimeInDefSpell",
                                        graphName="WOffSurvModel-ROC_CoxDisc_Basic_TimeVar", genFigPathGiven=paste0(genFigPath, "tROC-Analyses/"), 
                                        caseStudyName=paste0("CoxDisc_CDH_", predictTime), numThreads=12, logPath=genPath, 
                                        predType="response")
objROC4_CDH_CoxDisc_bas$AUC; objROC4_CDH_CoxDisc_bas$ROC_graph
proc.time() - ptm
### RESULTS: AUC up to t: 94.3%, achieved in 3748.25 secs


# -- Store experimental objects | Memory optimisation
# CDH-model: Basic Cox-regression model
pack.ffdf(paste0(genPath,"WOffSurvModel-CoxDisc-CDH-ROC_Depedendence_03_bas"), objROC1_CDH_CoxDisc_bas);
pack.ffdf(paste0(genPath,"WOffSurvModel-CoxDisc-CDH-ROC_Depedendence_12_bas"), objROC2_CDH_CoxDisc_bas);
pack.ffdf(paste0(genPath,"WOffSurvModel-CoxDisc-CDH-ROC_Depedendence_24_bas"), objROC3_CDH_CoxDisc_bas);
pack.ffdf(paste0(genPath,"WOffSurvModel-CoxDisc-CDH-ROC_Depedendence_36_bas"), objROC4_CDH_CoxDisc_bas);




# ------ Advanced discrete-time hazard model

# --- Package: tROCkit() | custom "package"/function
# NOTE: Using custom tROC()-function from script 0b(iii) under the CD-approach with an NN-estimator and 0/1-kernel

# -- Multi-threaded calculation of the # AUC from given start up to given prediction time 3 in following the CD-approach
# NOTE: Uses the superior Nearest Neighbour Estimator (NNE) method for S(t) with a 0/1-kernelNNE-kernel for S(t)
# NOTE2: Assume dependence (by specifying ID-field) amongst certain observations clustered around ID-values
ptm <- proc.time() #IGNORE: for computation time calculation;
predictTime <- 6
objROC1_CDH_CoxDisc_adv <- tROC.multi(datGiven=datCredit_valid, modGiven=modLR, month_End=predictTime, sLambda=0.05, estMethod="NN-0/1", numDigits=4, 
                                        fld_ID="DefSpell_Key", fld_Event="DefSpell_Event", eventVal=1, fld_StartTime="Start", fld_EndTime="TimeInDefSpell",
                                        graphName="WOffSurvModel-ROC_CoxDisc_Advanced_TimeVar", genFigPathGiven=paste0(genFigPath, "tROC-Analyses/"), 
                                        caseStudyName=paste0("CoxDisc_CDH_", predictTime), numThreads=12, logPath=genPath, 
                                        predType="response")
objROC1_CDH_CoxDisc_adv$AUC; objROC1_CDH_CoxDisc_adv$ROC_graph
proc.time() - ptm
### RESULTS: AUC up to t: 94.66%, achieved in 1659.32 secs


# -- Multi-threaded calculation of the # AUC from given start up to given prediction time 3 in following the CD-approach
# NOTE: Uses the superior Nearest Neighbour Estimator (NNE) method for S(t) with a 0/1-kernelNNE-kernel for S(t)
# NOTE2: Assume dependence (by specifying ID-field) amongst certain observations clustered around ID-values
ptm <- proc.time() #IGNORE: for computation time calculation;
predictTime <- 12
objROC2_CDH_CoxDisc_adv <- tROC.multi(datGiven=datCredit_valid, modGiven=modLR, month_End=predictTime, sLambda=0.05, estMethod="NN-0/1", numDigits=4, 
                                        fld_ID="DefSpell_Key", fld_Event="DefSpell_Event", eventVal=1, fld_StartTime="Start", fld_EndTime="TimeInDefSpell",
                                        graphName="WOffSurvModel-ROC_CoxDisc_Advanced_TimeVar", genFigPathGiven=paste0(genFigPath, "tROC-Analyses/"), 
                                        caseStudyName=paste0("CoxDisc_CDH_", predictTime), numThreads=12, logPath=genPath, 
                                        predType="response")
objROC2_CDH_CoxDisc_adv$AUC; objROC2_CDH_CoxDisc_adv$ROC_graph
proc.time() - ptm
### RESULTS: AUC up to t: 94.87%, achieved in 1954.01  secs


# -- Multi-threaded calculation of the # AUC from given start up to given prediction time 3 in following the CD-approach
# NOTE: Uses the superior Nearest Neighbour Estimator (NNE) method for S(t) with a 0/1-kernelNNE-kernel for S(t)
# NOTE2: Assume dependence (by specifying ID-field) amongst certain observations clustered around ID-values
ptm <- proc.time() #IGNORE: for computation time calculation;
predictTime <- 24
objROC3_CDH_CoxDisc_adv <- tROC.multi(datGiven=datCredit_valid, modGiven=modLR, month_End=predictTime, sLambda=0.05, estMethod="NN-0/1", numDigits=4, 
                                        fld_ID="DefSpell_Key", fld_Event="DefSpell_Event", eventVal=1, fld_StartTime="Start", fld_EndTime="TimeInDefSpell",
                                        graphName="WOffSurvModel-ROC_CoxDisc_Advanced_TimeVar", genFigPathGiven=paste0(genFigPath, "tROC-Analyses/"), 
                                        caseStudyName=paste0("CoxDisc_CDH_", predictTime), numThreads=12, logPath=genPath, 
                                        predType="response")
objROC3_CDH_CoxDisc_adv$AUC; objROC3_CDH_CoxDisc_adv$ROC_graph
proc.time() - ptm
### RESULTS: AUC up to t: 96.48%, achieved in 2596.92  secs


# -- Multi-threaded calculation of the # AUC from given start up to given prediction time 3 in following the CD-approach
# NOTE: Uses the superior Nearest Neighbour Estimator (NNE) method for S(t) with a 0/1-kernelNNE-kernel for S(t)
# NOTE2: Assume dependence (by specifying ID-field) amongst certain observations clustered around ID-values
ptm <- proc.time() #IGNORE: for computation time calculation;
predictTime <- 48
objROC4_CDH_CoxDisc_adv <- tROC.multi(datGiven=datCredit_valid, modGiven=modLR, month_End=predictTime, sLambda=0.05, estMethod="NN-0/1", numDigits=4, 
                                        fld_ID="DefSpell_Key", fld_Event="DefSpell_Event", eventVal=1, fld_StartTime="Start", fld_EndTime="TimeInDefSpell",
                                        graphName="WOffSurvModel-ROC_CoxDisc_Advanced_TimeVar", genFigPathGiven=paste0(genFigPath, "tROC-Analyses/"), 
                                        caseStudyName=paste0("CoxDisc_CDH_", predictTime), numThreads=12, logPath=genPath, 
                                        predType="response")
objROC4_CDH_CoxDisc_adv$AUC; objROC4_CDH_CoxDisc_adv$ROC_graph
proc.time() - ptm
### RESULTS: AUC up to t: 96.82%, achieved in 3847.32  secs


# -- Store experimental objects | Memory optimisation
# CDH-model: Basic Cox-regression model
pack.ffdf(paste0(genPath,"WOffSurvModel-CoxDisc-CDH-ROC_Depedendence_03_adv"), objROC1_CDH_CoxDisc_adv);
pack.ffdf(paste0(genPath,"WOffSurvModel-CoxDisc-CDH-ROC_Depedendence_12_adv"), objROC2_CDH_CoxDisc_adv);
pack.ffdf(paste0(genPath,"WOffSurvModel-CoxDisc-CDH-ROC_Depedendence_24_adv"), objROC3_CDH_CoxDisc_adv);
pack.ffdf(paste0(genPath,"WOffSurvModel-CoxDisc-CDH-ROC_Depedendence_36_adv"), objROC4_CDH_CoxDisc_adv);




# ----------------- 4. Create combined ROC-graph across multiple prediction times
# ------ Basic discrete-time hazard model

# - Ensure required objects exist in memory
if (!exists('objROC1_CDH_CoxDisc_bas')) unpack.ffdf(paste0(genPath,"WOffSurvModel-CoxDisc-CDH-ROC_Depedendence_03_bas"), tempPath);gc()
if (!exists('objROC2_CDH_CoxDisc_bas')) unpack.ffdf(paste0(genPath,"WOffSurvModel-CoxDisc-CDH-ROC_Depedendence_12_bas"), tempPath);gc()
if (!exists('objROC3_CDH_CoxDisc_bas')) unpack.ffdf(paste0(genPath,"WOffSurvModel-CoxDisc-CDH-ROC_Depedendence_24_bas"), tempPath);gc()
if (!exists('objROC4_CDH_CoxDisc_bas')) unpack.ffdf(paste0(genPath,"WOffSurvModel-CoxDisc-CDH-ROC_Depedendence_36_bas"), tempPath);gc()

# - Set ROC-parameters and initialize data structures
vecPercTimepoint <- c(6,12,24,48)
vecTROC <- list(objROC1_CDH_CoxDisc_bas, objROC2_CDH_CoxDisc_bas, objROC3_CDH_CoxDisc_bas, objROC4_CDH_CoxDisc_bas)
vLabels <- vector("list", length=length(vecPercTimepoint))

# -- Create a combined data object for plotting purposes
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


# -- Graph a combined ROC-graph across prediction times t
# - Aesthetic parameters
datGraph[, FacetLabel := "Basic discrete-time hazard model"]
vCol <- brewer.pal(8,"Set1")
vLabels_F <- setNames(vLabels, paste0(letters[1:length(vecPercTimepoint)],"_", vecPercTimepoint))
chosenFont <- "Cambria"

# - Create ROC-graph
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
ggsave(gg, file=paste0(paste0(genFigPath,"/tROC-Analyses/", "WOffSurvModel-CoxDisc-CDH-CombinedROC_Depedendence_bas.png")), 
       width=1800/dpi, height=1500/dpi, dpi=dpi, bg="white")




# ------ Advanced discrete-time hazard model

# - Ensure required objects exist in memory
if (!exists('objROC1_CDH_CoxDisc_adv')) unpack.ffdf(paste0(genPath,"WOffSurvModel-CoxDisc-CDH-ROC_Depedendence_03_adv"), tempPath);gc()
if (!exists('objROC2_CDH_CoxDisc_adv')) unpack.ffdf(paste0(genPath,"WOffSurvModel-CoxDisc-CDH-ROC_Depedendence_12_adv"), tempPath);gc()
if (!exists('objROC3_CDH_CoxDisc_adv')) unpack.ffdf(paste0(genPath,"WOffSurvModel-CoxDisc-CDH-ROC_Depedendence_24_adv"), tempPath);gc()
if (!exists('objROC4_CDH_CoxDisc_adv')) unpack.ffdf(paste0(genPath,"WOffSurvModel-CoxDisc-CDH-ROC_Depedendence_36_adv"), tempPath);gc()

# - Set ROC-parameters and initialize data structures
vecPercTimepoint <- c(6,12,24,48)
vecTROC <- list(objROC1_CDH_CoxDisc_adv, objROC2_CDH_CoxDisc_adv, objROC3_CDH_CoxDisc_adv, objROC4_CDH_CoxDisc_adv)
vLabels <- vector("list", length=length(vecPercTimepoint))

# -- Create a combined data object for plotting purposes
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


# -- Graph a combined ROC-graph across prediction times t
# - Aesthetic parameters
datGraph[, FacetLabel := "Advanced discrete-time hazard model"]
vCol <- brewer.pal(8,"Set1")
vLabels_F <- setNames(vLabels, paste0(letters[1:length(vecPercTimepoint)],"_", vecPercTimepoint))
chosenFont <- "Cambria"

# - Create ROC-graph
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
                     cox_CDH_adv, cox_CDH_basic, modLR, modLR_basic, 
                     objROC1_CDH_CoxDisc_adv, objROC2_CDH_CoxDisc_adv, objROC3_CDH_CoxDisc_adv, objROC4_CDH_CoxDisc_adv,
                     objROC1_CDH_CoxDisc_bas, objROC2_CDH_CoxDisc_bas, objROC3_CDH_CoxDisc_bas, objROC4_CDH_CoxDisc_bas,
                     datCredit_train_CDH, datCredit_valid_CDH, datCredit_train, datCredit_valid
) )





# ------ Combined model

# --- Package: tROCkit() | custom "package"/function
# NOTE: Using custom tROC()-function from script 0b(iii) under the CD-approach with an NN-estimator and 0/1-kernel

# -- Multi-threaded calculation of the # AUC from given start up to given prediction time 3 in following the CD-approach
# NOTE: Uses the superior Nearest Neighbour Estimator (NNE) method for S(t) with a 0/1-kernelNNE-kernel for S(t)
# NOTE2: Assume dependence (by specifying ID-field) amongst certain observations clustered around ID-values
ptm <- proc.time() #IGNORE: for computation time calculation;
predictTime <- 44
objROC1_LR <- tROC.multi(datGiven=datValid_classic, modGiven=modLR_classic, month_End=predictTime, sLambda=0.05, estMethod="NN-0/1", numDigits=4, 
                                      fld_ID="DefSpell_Key", fld_Event="DefSpell_Event", eventVal=1, fld_StartTime="Start", fld_EndTime="DefSpell_Age",
                                      graphName="WOffLRModel-ROC_TimeVar", genFigPathGiven=paste0(genFigPath, "tROC-Analyses/"), 
                                      caseStudyName=paste0("LR_", predictTime), numThreads=12, logPath=genPath, 
                                      predType="response")
objROC1_LR$AUC; objROC1_LR$ROC_graph
proc.time() - ptm
### RESULTS: AUC up to t: 94.66%, achieved in 1659.32 secs
### MM: Need


ptm <- proc.time() #IGNORE: for computation time calculation;

predictTime <- 44
objROC5_CDH_CoxDisc_adv <- tROC.multi(datGiven=datCredit_valid, modGiven=modLR, month_End=predictTime, sLambda=0.05, estMethod="NN-0/1", numDigits=4, 
                                      fld_ID="DefSpell_Key", fld_Event="DefSpell_Event", eventVal=1, fld_StartTime="Start", fld_EndTime="TimeInDefSpell",
                                      graphName="WOffSurvModel-ROC_CoxDisc_Advanced_TimeVar", genFigPathGiven=paste0(genFigPath, "tROC-Analyses/"), 
                                      caseStudyName=paste0("CoxDisc_CDH_", predictTime), numThreads=12, logPath=genPath, 
                                      predType="response")
objROC5_CDH_CoxDisc_adv$AUC; objROC5_CDH_CoxDisc_adv$ROC_graph
proc.time() - ptm
### RESULTS: AUC up to t: 96.82%, achieved in 3847.32  secs

ptm <- proc.time() #IGNORE: for computation time calculation;
predictTime <- 44
objROC5_CDH_CoxDisc_bas <- tROC.multi(datGiven=datCredit_valid, modGiven=modLR_basic, month_End=predictTime, sLambda=0.05, estMethod="NN-0/1", numDigits=4, 
                                      fld_ID="DefSpell_Key", fld_Event="DefSpell_Event", eventVal=1, fld_StartTime="Start", fld_EndTime="TimeInDefSpell",
                                      graphName="WOffSurvModel-ROC_CoxDisc_Basic_TimeVar", genFigPathGiven=paste0(genFigPath, "tROC-Analyses/"), 
                                      caseStudyName=paste0("CoxDisc_CDH_", predictTime), numThreads=12, logPath=genPath, 
                                      predType="response")
objROC5_CDH_CoxDisc_bas$AUC; objROC5_CDH_CoxDisc_bas$ROC_graph
proc.time() - ptm



# - Set ROC-parameters and initialize data structures
vecPercTimepoint <- c(44,44,44)
vecTROC <- list(objROC5_CDH_CoxDisc_adv, objROC5_CDH_CoxDisc_bas ,objROC1_LR)
vLabels <- vector("list", length=length(vecPercTimepoint))
vmodel <- c("DtH-Advanced A","DtH-Basic A", "Logistic Regression A")
# -- Store experimental objects | Memory optimisation
# CDH-model: Basic Cox-regression model
pack.ffdf(paste0(genPath,"WOffSurvModel-CoxDisc-bas"), objROC5_CDH_CoxDisc_bas);
pack.ffdf(paste0(genPath,"WOffSurvModel-CoxDisc-adv"), objROC5_CDH_CoxDisc_adv);
pack.ffdf(paste0(genPath,"WOffSurvModel-CoxDisc-LR"), objROC1_LR );
### MM: Need another version of this for the B-series models

# ------ Advanced discrete-time hazard model

# - Ensure required objects exist in memory
if (!exists('objROC5_CDH_CoxDisc_bas')) unpack.ffdf(paste0(genPath,"WOffSurvModel-CoxDisc-bas"), tempPath);gc()
if (!exists('objROC5_CDH_CoxDisc_adv')) unpack.ffdf(paste0(genPath,"WOffSurvModel-CoxDisc-adv"), tempPath);gc()
if (!exists('objROC1_LR')) unpack.ffdf(paste0(genPath,"WOffSurvModel-CoxDisc-LR"), tempPath);gc()


# -- Create a combined data object for plotting purposes
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
  vLabels[[i]] <- bquote(.(vmodel[i]) * "; AUC: " * .(formatC(vecTROC[[i]]$AUC * 100, format = "f", digits = 2)) * "%")
  
}


# -- Graph a combined ROC-graph across prediction times t
# - Aesthetic parameters
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












