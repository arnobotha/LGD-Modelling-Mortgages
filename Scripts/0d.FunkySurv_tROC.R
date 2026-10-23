# ============================== SURVIVAL FUNCTIONS ==============================
# Defining various custom functions relating to the estimation, analysis, and
# graphing of ROC-graphs and their summary statistics (AUC), as part of testing
# the discrimination power (prediction accuracy) of a given Cox regression model,
# or marker values arising from such a model.
# --------------------------------------------------------------------------------
# PROJECT TITLE: Loss Modelling (LGD) for Residential Mortgages
# SCRIPT AUTHOR(S): Dr Arno Botha (AB), Bernard Scheepers (BS)
# VERSION: 3.0 (Sept-2026)
# ================================================================================





# ----------------- 1. Main functions for conducting time-Dependent ROC-analysis

# --- Multi-threaded function to calculate the clustered/classical tROC-diagnostic over a given time interval
# This quantity is adjusted for right-censoring by estimating both the overall and marker-conditional 
# survivor functions. For a given/fitted Cox regression model (or given Marker vector), this function 
# chiefly implements the Nearest Neighbours estimator from Heagerty2000 
# (DOI: https://doi.org/10.1111/j.0006-341x.2000.00337.x)
# This function then culminates in producing both the associated ROC-graph, itself constructed using 
# the trapezoidal rule from Mason2002 (DOI: https://doi.org/10.1256/003590002320603584),
# and the AUC-statistic in summarising the ROC-graph.
# Input:  [datGiven]: A validation dataset containing the variables of interest for testing prediction accuracy.
#         [cox]: A fitted cox model used to obtain marker values (theoretically either the risk scores exp(\beta.X)
#               or simply just the linear combination \beta.X.
#         [Markers]: a vector of marker values obtained from a Cox model's predictions
#         [month_Start]: The prediction starting period of the time range over which prediction accuracy is tested.
#         [month_End], The last prediction period of the time range over which prediction accuracy is tested. Evaluation time
#         [sLambda]: A smoothing parameter representing the %-valued neighborhood size,
#                  symmetrically calculated around each unique marker.
#         [numDigits]: The number of digits to which unique marker values are rounded, as an algorithmic efficiency boost.
#         [fld_ID]: An optional field name that designates whether to group certain observations together by subject/spell ID.
#         [fld_Event]: A required field name that designates the main event indicator.
#         [eventVal]: A required field that denotes the main event value against which [fld_Event] is tested.
#         [fld_StartTime]: An optional field name that denotes the entry times of observations becoming at risk of the main event.
#         [fld_EndTime]: A required field name that designates the stop or raw event times.
#         [Graph]: A boolean-valued toggle to produce the ROC-graph as a ggplot-object.
#         [graphName]: The base name under which the produced ggplot graph will be saved in the given path directory.
#         [genFigPathGiven], A given path directory in which the ROC-graph (if produced) will be saved.
#         [numThreads]: The number of threads to register when iterating independently on different parts of the population
#                       in a multi-threaded fashion.
#         [caseStudyName]: An optional name to assign the assessment log in tracking the multithreaded performance whilst hte
#                           function is running.
#         [reportFlag]: An indicator whether thread-specific results should be reported within a communal text file in 
#                       tracking the overall progress of the function whilst running on larger datasets
#         [logPath]: A given path directory in which the log file is stored in tracking the performance of the multithreaded loop
#         [implementFix]: A flag signifying whether the latent error in survivalROC() regarding unordered start times should be fixed or not
# Output: [AUC]: The time-dependent Area under the curve (AUC) in summarising the corresponding time-dependent ROC-graph
#         [ROC_graph]: The associated ROC-graph as a ggplot-object
tROC.multi <- function(datGiven, cox=NA, Markers=NA, month_Start=0, month_End, sLambda=0.05, numDigits=2, 
                       fld_ID=NA, fld_Event="MainEvent_Ind", eventVal=1, fld_StartTime="Start", fld_EndTime="Stop",
                       Graph=TRUE, graphName="timedROC-Graph", genFigPathGiven=paste0(getwd(),"/"), numThreads=4, 
                       caseStudyName="Main",reportFlag=T, logPath=paste0(getwd(),"/"), predType="exp",
                       implementFix=T) {
  
  # ------ Preliminaries 
  # -- Testing Conditions
  # datGiven = copy(dat); cox=coxExample; month_End=203; numDigits=2; Graph=TRUE; month_Start=0; estMethod="NN-0/1";sLambda=0.05;
  # fld_ID="ID"; fld_Event="Event_Ind"; fld_StartTime="Start"; fld_EndTime="End"; caseStudyName="Main"
  # graphName="coxExample_cgd"; eventVal=1; genFigPathGiven=paste0(genFigPath, "/TFD"); numThreads=6; reportFlag=T; 
  # logPath=paste0(getwd(),"/")
  # -- Testing conditions 2 (simulated data) | Classical tROC
  # datGiven = datCreditSim; Markers=datCreditSim$M; month_End=t_eval; numDigits=2; Graph=TRUE; 
  # month_Start=0; estMethod="NN-0/1";sLambda=0.05; cox<-NA
  # fld_ID=NA; fld_Event="Event_Ind"; eventVal=1; fld_StartTime="Start"; fld_EndTime="TimeInSpell";
  # graphName="SimStudy_Truth"; genFigPathGiven=paste0(genFigPath, "Proof of Concepts/"); 
  # numThreads=12; reportFlag=T; caseStudyName=paste0("SimStudy_", predictTime); ogPath=genPath
  # -- Testing conditions 3 (real-world data)
  # datGiven = copy(datCredit_valid_TFD); cox=cox_TFD; month_End=12; numDigits=0; Graph=TRUE; month_Start=0; estMethod="NN-0/1";sLambda=0.05;
  # fld_ID="PerfSpell_Key"; fld_Event="PerfSpell_Event"; eventVal=1; fld_StartTime="Start"; fld_EndTime="End";
  # graphName="DefaultSurvModel-Cox1_Depedendence"; genFigPathGiven=paste0(genFigPath, "/TFD"); numThreads=6; reportFlag=T; caseStudyName="Main"
  # logPath=genPath
  
  # -- Preliminaries
  require(doParallel)
  require(data.table)
  require(ggplot2)
  
  # - Create copy so as not to add columns by reference to the given dataset
  dt <- copy(datGiven)
  
  
  # -- Error handling
  hasCox <- inherits(cox, "coxph")
  hasMarkers <- !all(is.na(Markers))
  
  if (!is.data.table(dt)) {
    stop("[datGiven] must be a data table.\n")
  }# Test whether datGiven is a data table
  if (hasCox && hasMarkers) {
    stop("Specify either [cox] or [Markers], not both.\n")
  } # Test whether correct object has been given
  if (!hasCox && !hasMarkers) {
    stop("Either [cox] or [Markers] must be supplied.\n")
  } # Test whether correct object has been given
  if (hasCox) {
    if (!all(all.vars(formula(cox)) %in% colnames(dt))) {
      stop("[datGiven] does not contain the variables required within the [cox] object.\n")
    }
  } # Test whether dataset contains necessary variables in order to use Cox-object for prediction
  if (hasMarkers && length(Markers) != NROW(dt)) {
    stop("[Markers] must have the same length as the number of rows in [datGiven].\n")
  } # Test whether the rows in datGiven correspond to the length of the markers (if given)
  if (!is.numeric(month_Start) || !is.numeric(month_End)) {
    stop("[month_Start] and [month_End] must be numeric.\n")
  }# Test whether [month_Start] and [month_End] are numerical
  if (month_Start < 0 || month_End < 0) {
    stop("[month_Start] and [month_End] must be non-negative.\n")
  }# Test whether [month_Start] and [month_End] are positive
  if (month_Start > month_End) {
    stop("[month_Start] cannot be greater or equal to [month_End].\n")
  }# Test whether [month_Start] is less than [month_End]
  if (anyNA(c(fld_Event, eventVal, fld_StartTime, fld_EndTime))) {
    stop("The arguments [fld_Event], [eventVal], [fld_StartTime], [fld_EndTime], and [lfd_Marker] cannot be missing and must be specified. \n")
  }
  if ((Graph & is.na(graphName)) | (Graph & is.na(genFigPathGiven))) {
    stop("The graphing arguments [graphName] and [genFigPathGiven] cannot be missing and must be specified when desiring an ROC-graph. \n")
  }
  
  
  # -- Obtain Markers/prediction scores M_i for i=1,...,n cases (not necessarily subjects) and assign as thresholds
  # - Score the given dataset using the given Cox regression model towards obtaining marker
  # values (option: linear predictors), but only if not available
  if (hasCox) {
    dt[, Marker := round(predict(cox, newdata=dt, type=predType),numDigits)] 
  } else{
    dt[, Marker := round(Markers, numDigits)]
  }
  
  # - Let the unique marker values represent our threshold space, which is standard practice in ROC-analysis
  thresholds <- dt$Marker %>% unique() %>% sort()
  nThresh <- length(thresholds) # number of such unique thresholds for iteration purposes
  
  # -- Reassign given field names to standardised naming conventions, if only within this function
  if (!is.na(fld_ID) & !is.na(fld_StartTime)){
    setnames(dt, old=c(c(fld_StartTime, fld_EndTime, fld_ID, fld_Event)),
             new=c("StartTime", "EndTime", "ID", "Event_Ind"))
    # - Set indicators according to which the rest of the program will function conditionally
    Grouped_Ind <- T
    HasStarting_Ind <- T
  } else if (is.na(fld_ID) & !is.na(fld_StartTime)) {
    setnames(dt, old=c(c(fld_StartTime, fld_EndTime, fld_Event)),
             new=c("StartTime", "EndTime", "Event_Ind"))
    # - Set indicators according to which the rest of the program will function conditionally
    Grouped_Ind <- F
    HasStarting_Ind <- T
  } else if (is.na(fld_ID) & is.na(fld_StartTime)) {
    setnames(dt, old=c(c(fld_EndTime, fld_Event)),
             new=c("EndTime", "Event_Ind"))
    # - Set indicators according to which the rest of the program will function conditionally
    Grouped_Ind <- F
    HasStarting_Ind <- F
  } else if (!is.na(fld_ID) & is.na(fld_StartTime)) {
    setnames(dt, old=c(c(fld_EndTime, fld_ID, fld_Event)),
             new=c("EndTime", "ID", "Event_Ind"))
    # - Set indicators according to which the rest of the program will function conditionally
    Grouped_Ind <- T
    HasStarting_Ind <- F
  }
  
  # - Check if any end points fall within the given range
  if (dt[EndTime >= month_Start & EndTime <= month_End, .N] == 0) {
    stop(paste0("The observed endpoints exceed the bounds of the given prediction time interval, [",
                month_Start, ",", month_End, "]"))
  }
  
  
  # -- Obtain various quantities towards implementing the Nearest Neighbour Estimator (NNE) method
  nRows <- NROW(dt) # Total number of markers or rows (not necessarily subjects)
  
  # - Obtain the rank order of the observations when sorted by ascending EndTime.
  vOrder <- order(dt$EndTime)
  
  # - Re-sort the raw end points according to these particular rank-order indices, whereafter
  # the same is performed to the vectors of event indicators and marker-values
  vEventTimes <- dt$EndTime[vOrder]
  vEventInds <- dt$Event_Ind[vOrder]
  vMarkers <- dt$Marker[vOrder]
  # - Check for missingness in the starting times vector and, if found, treat by assigning a zero vector
  if ("StartTime" %in% names(dt)) {
    # - NB: The survivalROC() package did not order start times, so if parirty is wanted, then
    # set implementFix=F; otherwise the default is to order these starting times by [ord].
    if (implementFix) vStartTimes <- dt$StartTime[vOrder] else vStartTimes <- dt$StartTime
  } else {
    vStartTimes <- rep(0, nRows)
  }
  
  
  # ------ Estimation procedure for constructing an ROC-graph
  
  # ------ In calculating the ROC-graph, the following fundamental quantities must be estimated:
  # 1) the conditional survivor function S(t | M > c), used in estimating S(t) at given evaluation time t (month_end)
  # 2) the respective True Positive Rates (TPR) and False Positive Rates (FPR) across all thresholds at month_end
  
  
  # --- 1. Estimate the marker-conditional average survival probability S(t | M=c) using the NN-related kernal function
  # from Akritas (1994)
  
  # -- Preliminaries
  
  # - Obtain unique Markers (predictions from the given Cox-model), and order them as before according to 
  # the indices of the ordered unique event times
  vMarkers_unique <- unique(dt$Marker) 
  vMarkers_unique <- vMarkers_unique[order(vMarkers_unique)]
  
  # - Obtain those unique event times at which the main event occurred
  vEventTimes_Main <- unique(vEventTimes[vEventInds==eventVal])
  vEventTimes_Main <- vEventTimes_Main[order(vEventTimes_Main)]
  
  # - Using the filtered event times, retrieve only those for which they preclude the given upper bound of the
  # prediction time t, though also exceed the lower bound (if specified)
  # NOTE: Its dimensions will inform those of the eventual survival probability vector
  vEventTimes_Filtered <- vEventTimes_Main[vEventTimes_Main <= month_End & vEventTimes_Main >= month_Start]
  
  
  # -- Iterate across thresholds using a multi-threaded setup
  ptm <- proc.time() #IGNORE: for computation time calculation
  cl.port <- makeCluster(round(numThreads)); registerDoParallel(cl.port) # multi-threading setup
  cat("New Job: Estimating average survival probability across each given threshold ..",
      file=paste0(logPath,"assesslog_",caseStudyName,".txt"), append=F)
  datS_t <- foreach(j=1:nThresh, .combine='rbind', .verbose=F, .inorder=T,
                    .packages=c('data.table'), .export=c('S_t.estimator')) %dopar%
    { # ----------------- Start of Outer Loop -----------------
      # j <- 1 # testing condition
      prepDat <- S_t.estimator(vMarkers=vMarkers, vMarkers_unique=vMarkers_unique, 
                               vStartTimes=vStartTimes, vEventTimes=vEventTimes, vEventTimes_Filtered=vEventTimes_Filtered, 
                               vEventInds=vEventInds, eventVal=eventVal, threshold=thresholds[j], 
                               sLambda=sLambda, nRows=nRows, reportFlag=reportFlag, 
                               iteration=j, nThresh=nThresh, caseStudyName=caseStudyName, logPath=logPath)
      
    } # ----------------- End of Outer Loop -----------------
  stopCluster(cl.port); proc.time() - ptm
  
  # - Create an index that maps each observation’s marker to its position in the unique-marker vector
  # (used to retrieve the corresponding S(t | m) value)
  vMatched <- match(vMarkers, vMarkers_unique)
  
  # - Allocate the survival probability estimates across subjects over prediction times t (same as unique
  # event times), given a specific marker value under which that particularly S(t) was estimated. 
  # This allocation is achieved using the previously-created map vector.
  vSurvProb <- datS_t$S_t[vMatched]
  
  
  
  # --- 2. Estimate bivariate survivor function S(c,t) for given threshold c and evaluation time t (month_end)
  # NOTE: Estimation hereof depends on whether independence is assumed or not amongst rows in the provided data
  if (Grouped_Ind==F) { # Independence amongst all observations
    
    # -- Calculate the mean survival probability across scored cases, given an -\infty marker value
    S_mean <- mean(vSurvProb, na.rm=T)  
    
  } else { # Dependence amongst observations, clustered by a given ID-value | clustered tROC-extension
    
    # NOTE: This operation will need to use the grouping functionality of data.table, and therefore
    # the operations will occur outside of vectors and inside the given data.table object
    # - As such, first obtain an index mapping between the sorted vMarkers and the original unsorted vector in dt
    vMatched2 <- match(dt$Marker, vMarkers)
    # - Merge the [vSurvProb] vector unto this datatset
    dt[, Surv_prob := vSurvProb[vMatched2]]
    
    # -- Calculate the overall survival probability S(c, t) at prediction time t and cut-off c, 
    # First, calculate the average [Surv_prob] for each "id" and average these "id" specific averages across the portfolio
    # NOTE: This estimator is the grand mean of the average [Surv_prob]-values per ID
    S_mean <- mean(dt[,list(S_Marg = mean(Surv_prob,na.rm=T), na.rm=T), by=list(ID)]$S_Marg, na.rm=T) 
  }
  
  
  
  # --- 3. Calculate TPR and FPR by iterating across each threshold (unique marker)
  # - Preliminaries
  vTPR <- c(rep(NA, nThresh-1),0) # Initialise vector and set threshold boundaries
  vFPR <- c(rep(NA, nThresh-1),1) # Initialise vector and set threshold boundaries
  
  # -- Iterate across thresholds and calculate TPR and FPR given that each threshold conditions the risk set
  # but do so across a multithreaded setup
  ptm <- proc.time() #IGNORE: for computation time calculation
  cat("\n\n\n Estimating vector of confusion matrix elements (TPR, FPR) across each given threshold ..",
      file=paste0(logPath,"assesslog_",caseStudyName,".txt"), append=T)
  cl.port <- makeCluster(numThreads); registerDoParallel(cl.port) # multi-threading setup
  datROC <- foreach(c=1:nThresh, .combine='rbind', .verbose=F, .inorder=T,
                    .packages=c('data.table'), .export=c('ROC_quants.estimator')) %dopar%
    
    { # ----------------- Start of Outer Loop -----------------
      # c <- 2 # testing condition    
      prepData <- ROC_quants.estimator(vMarkers=vMarkers, threshold=thresholds[c], vSurvProb=vSurvProb,
                                       S_mean=S_mean, nRows=nRows, datGiven=dt, Grouped_Ind=Grouped_Ind,
                                       iteration=c, nThresh=nThresh, reportFlag=reportFlag,
                                       caseStudyName=caseStudyName, logPath=logPath)
    } # ----------------- End of Outer Loop -----------------
  stopCluster(cl.port); proc.time() - ptm
  
  
  # -- Constructing the ROC-graph itself using the Trapezoidal Rule from numerical integration practices
  # NOTE: We apply the trapezoidal rule as in survivalROC() from Heagerty2000, implicitly following 
  # DeLong1988 and Fawcett2008 but not exactly
  # - Attach the lower boundary to the vectors of TPR and FPR (cannot be done earlier)
  # Note that we are increasing the dimensions by 1 and will need to use (nThresh+1)
  vTPR <- c(1, datROC$TPR)
  vFPR <- c(1, datROC$FPR)
  # - Bar width = difference between consecutive points, having removed outer boundaries
  vWidth <- vFPR[-(nThresh+1)] - vFPR[-1] 
  # - Calculate Bar height = Average between two consecutive points, having removed outer boundaries
  vMidpoints <- (vTPR[-(nThresh+1)] + vTPR[-1])/2
  # - Finally, calculate the area under the ROC-curve as the summed product
  sArea <- sum(vWidth * vMidpoints) # AUC
  
  
  # -- If toggled, produce an ROC-graph with annotations
  if(Graph){
    
    # - Create a data object for plotting purposes
    datGraph <- data.frame(x = vFPR[-(nThresh+1)], y=vTPR[-1])
    datSegment <- data.frame(x = 0, y = 0, xend = 1, yend = 1)
    # - Annotate with concordance (Harrell's C)
    if (hasCox) {
      conc <- percent(as.numeric(concordance(cox,newdata=datGiven)[1]),accuracy=0.001)  
      vAnnotate <- c(paste0("AUC(",month_Start,",", month_End,"): ", percent(sArea, accuracy=0.01)),
                     paste0("Harrell's c-statistic: ", conc))
      vY <- c(0.375,0.125)
    } else {
      vAnnotate <- paste0("AUC(",month_Start,",", month_End,"): ", percent(sArea, accuracy=0.01))
      vY <- 0.375
    }
    
    # - Aesthetic parameters
    vCol <- brewer.pal(8,"Set1")[c(2)]
    chosenFont <- "Cambria"
    
    # - Create ROC-graph
    gg <- ggplot(datGraph,aes(x=x,y=y,group=1)) + theme_minimal() + 
      theme(text = element_text(family=chosenFont), legend.position="inside",
            legend.background = element_rect(fill="snow2", color="black",
                                             linetype="solid")) +
      labs(x = bquote("False Positive Rate "*italic(F^"+")), y = 
             bquote("True Positive Rate "*italic(T^"+"))) + geom_step(color=vCol) +
      geom_segment(data = datSegment,aes(x = x, y = y, xend = xend, yend = yend),
                   color = "grey", linewidth=1) +
      annotate("label", x = c(0.75,0.75), y = vY,label = vAnnotate, 
               fill="grey", family=chosenFont) + 
      scale_y_continuous(label=percent) + scale_x_continuous(label=percent)
    
    # Save graph
    dpi <- 200
    ggsave(gg, file=paste0(genFigPathGiven, graphName,"(",month_Start,",",month_End,").png"), 
           width=1200/dpi, height=1000/dpi, dpi=dpi, bg="white")
    
    # - Prepare return object
    retObj <- list(AUC = sArea, ROC_graph=gg, Thresholds=c(-Inf, thresholds), TPR=vTPR, 
                   FPR=vFPR, SurvivalProb_Mean = S_mean, S_t = vSurvProb)
    
  } else{ retObj <- list(AUC = sArea, Thresholds=c(-Inf, thresholds), TPR=vTPR, FPR=vFPR, 
                         SurvivalProb_Mean = S_mean, S_t = vSurvProb) }
  
  # - Conclude program and return results
  return(retObj)
  
  
  # - Generic cleanup (should parts of this function be run in debug/interactive mode)
  suppressWarnings( rm(datGiven, thresholds, nThresh, Grouped_Ind, HasStarting_Ind, nRows, vOrder, vEventTimes, vEventInds, 
                       vMarkers, vStartTimes,S_t, vMarkers_unique, vEventTimes_Main, vEventTimes_Filtered, vDiff, 
                       Neigh_Mid, Neigh_Mid2, Neigh_UpperB, Neigh_LowerB, Neigh_UpperB_ind, Neigh_LowerB_ind, 
                       weights, S_0, n_values, d_values, vMatched, vMatch2, vSurvProb, S_mean, vTPR, vFPR, 
                       cumulMark, S_tc, vWidth, vMidpoints, sArea, datGraph, datSegment, conc, gg, retObj,
                       datS_t, datROC, cl.port) )
}





# ------Helper functions for enabling a multithreading environment

# - Function for estimating marker-conditional average survival probability S(t | M=c)
# Used in tROC.multi() within a multi-threaded loop in estimating the survival probability at a given
# prediction time (implicit within vEventTimes_Filtered) and, more importantly, at a given threshold c
# for markers (beyond which a positive is predicted, and below which a negative is predicted)
S_t.estimator <- function(vMarkers, vMarkers_unique, vStartTimes, vEventTimes, vEventTimes_Filtered,
                          vEventInds, eventVal, threshold, sLambda, nRows, reportFlag=T, 
                          iteration, nThresh, caseStudyName="Main", logPath="C:/") {
  # -- Testing conditions
  #  iteration <- 49; threshold <- thresholds[iteration]; reportFlag=T;
  
  if (reportFlag) {
    cat(paste0("\n 1)[", iteration, " of ", nThresh, "] Estimating average survivor probability S(t,c) given a marker threshold [c=",
               threshold, "] at and beyownd which a cases' Marker (or risk score) will be predicted as a positive event, and below which",
               " the case will be predicted as a negative event."), 
        file=paste0(logPath, "assesslog_",caseStudyName,".txt"), append=T);
  }
  
  # threshold = thresholds[j]   # testing condition
  # -- Create a distance vector between each marker and the current threshold
  vDiff <- vMarkers - threshold # will always be positive, but unordered
  vDiff_ord <- sort(vDiff)
  #vDiff <- vDiff[order(vDiff)]  # Sort in ascending order
  
  # --- Differentiate execution by kernel choice | 0/1 Kernel from Akritas1994
  # Establish a symmetrical neighbourhood around unique marker values that are in
  # close proximity to one another
  
  # - Upper bound calculation of neighbourhood
  # Find an index in the ordered difference vector [vDiff] beyond which point all markers are positively 
  # differenced wrt the current threshold
  Neigh_Mid <- sum(vDiff_ord <  0) + 1L
  # Find corresponding index for upper bound of neighbourhood
  upper_idx <- min(Neigh_Mid + trunc(nRows * sLambda + 0.5), nRows)
  # Retrieve associated neighbourhood bounds (upper + lower)
  Neigh_UpperB <- vDiff_ord[upper_idx]
  
  # - Lower bound calculation of neighbourhood
  # The classical symmetric window uses the same absolute distance on either side of the current marker
  Neigh_LowerB <- -abs(Neigh_UpperB)
  
  # - Apply chosen kernel function: 1 if the Marker is within the neighbourhood, 0 otherwise
  #weights <- as.numeric( (vMarkers - m_j) <= Neigh_UpperB & (vMarkers - m_j) >= Neigh_LowerB )
  weights <- as.numeric( (vDiff <= Neigh_UpperB) & (vDiff >= Neigh_LowerB) )
  
  # -- S(t) can now be estimated by calculating the event and at-risk populations across filtered event times s,
  # as weighted by the kernel function [weights], using a Kaplan-Meier style estimator
  
  S_0 <- 1.0 # Initialise the starting survival probability to 100%
  
  # Iterate over event times
  for (jj in 1:length(vEventTimes_Filtered)) {
    # jj <- 2   # - At each filtered event time s, calculate the number of at-risk cases within the current neighbourhood
    n_values <- sum(weights * (vStartTimes <= vEventTimes_Filtered[jj]) &
                      (vEventTimes >= vEventTimes_Filtered[jj]), na.rm=T)
    # - At each filtered event time s, calculate the number of cases within the current neighbourhood
    # that experienced the event exactly at the current s
    d_values <- sum(weights * (vStartTimes <= vEventTimes_Filtered[jj]) &
                      (vEventTimes == vEventTimes_Filtered[jj])*(vEventInds==eventVal),na.rm=T)
    
    # - Calculate the well-known KM-based survival factor vector given the current filtered event time s
    if (n_values>0) S_0 <- S_0 * (1 - (d_values / n_values)) else 1
  }
  
  return(data.table(Threshold=threshold, S_t=S_0))
}


# - Function for estimating threshold-specific confusion matrix elements for time-dependent ROC-analyses
# Used in tROC.multi() within a multi-threaded loop in estimating the True and False Positive Rates at
# a particular prediction time (implicit in the estimated Survival probability vector), and, more importantly, 
# at a given threshold for markers (beyond which a positive is predicted, and below which a negative is predicted)
ROC_quants.estimator <- function(vMarkers, threshold, vSurvProb, S_mean, nRows, datGiven=NA, Grouped_Ind, 
                                 logPath="C:/",reportFlag=T, iteration, nThresh, caseStudyName="Main") {
  
  if (reportFlag) {
    cat(paste0("\n 1)[", iteration, " of ", nThresh, "] Estimating confusion matrix elements given a marker threshold [c=",
               threshold, "] at and beyownd which a cases' Marker (or risk score) will be predicted as a positive event, and below which",
               " the case will be predicted as a negative event."), 
        file=paste0(logPath, "assesslog_",caseStudyName,".txt"), append=T);
  }
  
  # NOTE: Estimation hereof depends on whether independence is assumed or not amongst rows in the provided data
  if (Grouped_Ind==F) { # Independence amongst all observations
    
    # - Empirical distribution of markers being less than the current threshold p_c
    cumulMark <- mean(vMarkers <= threshold, na.rm=T)
    
    # - Calculate mean survival probability given current threshold p_c, within the subset of marker values exceeding p_c
    # NOTE: The inner operation amounts to the indicator function in the equation of S_{lambda_n}(p_c,t)
    S_tc <- sum(vSurvProb[vMarkers > threshold], na.rm=T)/nRows
    
  } else { # Dependence amongst observations, clustered by a given ID-value
    
    # - Empirical distribution of markers being less than the current threshold p_c
    # First, we obtain the proportion amongst all cases with markers less than or equal to p_c
    # Then, we calculate the grand mean amongst all of these ID-level proportions.
    cumulMark = mean(datGiven[,list(Prop = sum(Marker <= threshold)/.N), by=list(ID)]$Prop, na.rm=T)
    
    # - Calculate mean survival probability given current threshold p_c, within the subset of marker values exceeding p_c
    # NOTE: The inner operation amounts to the indicator function in the equation of S_{lambda_n}(p_c,t)
    # NOTE2: Firstly, we obtain the mean survival probability amongst markers associated with an ID, conditioned on
    # these markers exceeding the threshold.
    #       Secondly, we calculate the grand mean amongst all of these ID-level mean survival probabilities given p_c
    S_tc <- mean(datGiven[,list(S_tc_ID=sum(ifelse(Marker > threshold,Surv_prob,0),na.rm=T)/.N), 
                          by=list(ID)]$S_tc_ID, na.rm=T)
  }
  
  # - Calculate TPR and FPR according to Heagerty2000
  sTPR <- ((1-cumulMark) - S_tc)/(1 - S_mean) # TPR
  sFPR <- S_tc/S_mean # FPR
  
  return(data.table(Threhsold=threshold, TPR=sTPR, FPR=sFPR))
}





# --- Compact multi-threaded function to estimate a clustered time-dependent ROC-analysis
tROC.Clustered_compact <- function(datGiven, t_eval, sLambda = 0.05, numDigits = 2, eventVal = 1, 
                                   numThreads = 8) {
  
  # -----------------------------------------------------------------
  # 0. Preparation
  # -----------------------------------------------------------------
  if (!is.data.table(datGiven)) stop("datGiven must be a data.table")
  req <- c("ObsID", "SpellNum", "TimeInSpell", "M", "Event_Ind", "Start")
  if (!all(req %in% names(datGiven)))
    stop("Missing columns: ", paste(setdiff(req, names(datGiven)), collapse = ", "))
  
  # - Create copy of data to avoid referencing errors in creating columns in datGiven, and by 
  # extension, the given dataset outside of function scope
  dt <- copy(datGiven)
  dt[, EndTime := TimeInSpell]
  dt[, KeyID := paste0(ObsID, "_", SpellNum)]
  dt[, Marker := round(M, numDigits)]
  
  nRows <- nrow(dt)
  
  # -----------------------------------------------------------------
  # 1. Order by event time
  # -----------------------------------------------------------------
  ord <- order(dt$EndTime)
  vEventTimes <- dt$EndTime[ord]
  vEventInds <- dt$Event_Ind[ord]
  vMarkers <- dt$Marker[ord]
  vStartTimes <- dt$Start[ord]
  vKeyID <- dt$KeyID[ord]
  
  vEventTimes_Filtered <- sort(unique(
    vEventTimes[vEventInds == eventVal & vEventTimes <= t_eval]
  ))
  nTimes <- length(vEventTimes_Filtered)
  
  # Full unique-marker grid
  vMarkers_unique <- sort(unique(vMarkers))
  nThresh <- length(vMarkers_unique)
  
  # -----------------------------------------------------------------
  # 2. Multithreaded NN + weighted KM for S(t | M = m)
  # -----------------------------------------------------------------
  compute_S <- function(m_j, vMarkers, nRows, sLambda) {
    
    # -- Create a distance vector between each marker and the current threshold
    vDiff <- vMarkers - m_j
    vDiff_ord <- sort(vDiff)
    
    # -- Establish a symmetrical neighbourhood around unique marker values that are in
    # close proximity to one another
    
    # - Upper bound calculation of neighbourhood
    # Find an index in the ordered difference vector [vDiff] beyond which point all markers are positively 
    # differenced wrt the current threshold
    Neigh_Mid <- sum(vDiff_ord <  0) + 1L
    # Find corresponding index for upper bound of neighbourhood
    upper_idx <- min(Neigh_Mid + trunc(nRows * sLambda + 0.5), nRows)
    # Retrieve associated neighbourhood bounds (upper + lower)
    Neigh_UpperB <- vDiff_ord[upper_idx]
    
    # - Lower bound calculation of neighbourhood
    # The classical symmetric window uses the same absolute distance on either side of the current marker
    Neigh_LowerB <- -abs(Neigh_UpperB)
    
    # - Apply chosen kernel function: 1 if the Marker is within the neighbourhood, 0 otherwise
    #weights <- as.numeric( (vMarkers - m_j) <= Neigh_UpperB & (vMarkers - m_j) >= Neigh_LowerB )
    weights <- as.numeric( (vDiff <= Neigh_UpperB) & (vDiff >= Neigh_LowerB) )
    
    # - S(t) can now be estimated by calculating the event and at-risk populations across filtered event times s,
    # as weighted by the kernel function [weights]
    S0 <- 1.0
    for (k in seq_len(nTimes)) {
      tt <- vEventTimes_Filtered[k]
      
      # - At each filtered event time tt, calculate the number of at-risk cases within the current neighbourhood
      at_risk <- sum(weights * (vStartTimes <= tt & vEventTimes >= tt), na.rm = T)
      
      # - At each filtered event time s, calculate the number of cases within the current neighbourhood
      # that experienced the event exactly at the current tt event time
      events  <- sum(weights * (vStartTimes <= tt & vEventTimes == tt & vEventInds == eventVal), na.rm = T)
      
      # - Calculate the well-known KM-based survival factor vector given the current filtered event time s
      if (at_risk > 0) S0 <- S0 * (1 - events / at_risk) else 1.0
    }
    return(S0)
  }
  
  if (numThreads > 1) {
    # Create cluster with numThreads (workers)
    cl <- parallel::makeCluster(numThreads)
    doParallel::registerDoParallel(cl)
    # Multithreaded loop over thresholds
    S_t_vec <- foreach(j = seq_len(nThresh), .combine = c) %dopar% {
      compute_S(vMarkers_unique[j], vMarkers, nRows, sLambda)
    }
    parallel::stopCluster(cl)
  } else {
    S_t_vec <- vapply(vMarkers_unique, compute_S, numeric(1))
  }
  
  # Index that maps each observation’s marker to its position in the unique-marker vector
  # (used to retrieve the corresponding S(t | m) value)
  vMatched  <- match(vMarkers, vMarkers_unique)
  
  # - Allocate the estimated S(t)-values across subjects over prediction times t (same as unique event times), 
  # given a specific marker value under which that particularly S(t) was estimated. This allocation is 
  # achieved using the previously-created map vector.
  vSurvProb <- S_t_vec[vMatched]
  
  # -----------------------------------------------------------------
  # 3. Prepare data for the second (also multithreaded) loop
  # -----------------------------------------------------------------
  dt_ord <- data.table(KeyID = vKeyID, Marker = vMarkers, Surv_prob = vSurvProb)
  
  # - Calculate the overall survival probability S(t) at prediction time t, i.e., given an -\infty marker value
  # Calculate the average [Surv_prob] for each "id" and average these "id" specific averages across the portfolio
  # NOTE: This estimator is the grand mean of the average [Surv_prob]-values per ID
  S_mean <- mean( dt_ord[, mean(Surv_prob, na.rm = T), by = KeyID]$V1, na.rm = T)
  
  # -----------------------------------------------------------------
  # 4. Multithreaded computation of S_tc and F_M for every threshold c
  # -----------------------------------------------------------------
  compute_ROC_quants <- function(c) {
    tmp <- dt_ord[, .(
      S_tc_spell = mean(ifelse(Marker > c, Surv_prob, 0), na.rm = T),
      F_spell    = mean(Marker <= c, na.rm = T)
    ), by = KeyID]
    return( c(S_tc = mean(tmp$S_tc_spell, na.rm = T),
              F_M = mean(tmp$F_spell,   na.rm = T)) )
  }
  
  if (numThreads > 1) {
    # Create cluster with numThreads (workers)
    cl <- parallel::makeCluster(numThreads)
    doParallel::registerDoParallel(cl)
    # Export the data table to the workers
    parallel::clusterExport(cl, varlist = "dt_ord", envir = environment())
    # Multithreaded loop over thresholds
    roc_mat <- foreach(j = seq_len(nThresh), .combine = rbind, .packages = "data.table") %dopar% {
      compute_ROC_quants(vMarkers_unique[j])
    }
    parallel::stopCluster(cl)
  } else {
    roc_mat <- t(vapply(vMarkers_unique, compute_ROC_quants, numeric(2)))
  }
  
  datROC <- data.table( Threshold = vMarkers_unique, S_tc = roc_mat[, "S_tc"], F_M = roc_mat[, "F_M"] )
  
  # -----------------------------------------------------------------
  # 5. TPR / FPR (Heagerty et al. [2000]) + trapezoidal AUC
  # -----------------------------------------------------------------
  datROC[, TPR := ((1 - F_M) - S_tc) / (1 - S_mean)]
  datROC[, FPR := S_tc / S_mean]
  
  # Add boundaries
  datROC <- rbind(
    data.table(Threshold =  Inf, S_tc = 0, F_M = 1, TPR = 0, FPR = 0),
    datROC,
    data.table(Threshold = -Inf, S_tc = S_mean, F_M = 0, TPR = 1, FPR = 1)
  )
  setorder(datROC, FPR)
  
  # - Calculate final tAUC value
  AUC <- sum(diff(datROC$FPR) * (datROC$TPR[-1] + datROC$TPR[-nrow(datROC)]) / 2)
  
  return(list( AUC = AUC, S_mean = S_mean, ROC_table = datROC[, .(Threshold, TPR, FPR)],
               sLambda = sLambda, numDigits = numDigits, nThresh = nThresh
  ))
}





# --- Compact multi-threaded function to estimate a classical time-dependent ROC-analysis
tROC.Classical_compact <- function(datGiven, t_eval, sLambda = 0.05, numDigits = 2, eventVal = 1, 
                                   numThreads = 8, implementFix=T) {
  # -- Testing conditions
  # datGiven <- datCreditSim
  
  # -----------------------------------------------------------------
  # 0. Preparation
  # -----------------------------------------------------------------
  if (!is.data.table(datGiven)) stop("datGiven must be a data.table")
  req <- c("TimeInSpell", "M", "Event_Ind")
  if (!all(req %in% names(datGiven)))
    stop("Missing columns: ", paste(setdiff(req, names(datGiven)), collapse = ", "))
  
  # - Create copy of data to avoid referencing errors in creating columns in datGiven, and by 
  # extension, the given dataset outside of function scope
  dt <- copy(datGiven)
  if (!"Start" %in% names(dt)) dt[, Start := TimeInSpell - 1L]
  dt[, EndTime := TimeInSpell]
  dt[, Marker  := round(M, numDigits)]
  
  nRows <- nrow(dt)
  
  # -----------------------------------------------------------------
  # 1. Order by event time
  # -----------------------------------------------------------------
  ord <- order(dt$EndTime)
  vEventTimes <- dt$EndTime[ord]
  vEventInds <- dt$Event_Ind[ord]
  vMarkers <- dt$Marker[ord]
  vKeyID <- dt$KeyID[ord]
  # - NB: The survivalROC() package did not order start times, so if parirty is wanted, then
  # set implementFix=F; otherwise the default is to order these starting times by [ord].
  if (implementFix) vStartTimes <- dt$Start[ord] else vStartTimes <- dt$Start
  
  vEventTimes_Filtered <- sort(unique(
    vEventTimes[vEventInds == eventVal & vEventTimes <= t_eval]
  ))
  nTimes <- length(vEventTimes_Filtered)
  
  # Full unique-marker grid
  vMarkers_unique <- sort(unique(vMarkers))
  nThresh <- length(vMarkers_unique)
  
  # -----------------------------------------------------------------
  # 2. Multithreaded NN + weighted KM for S(t | M = m)
  # -----------------------------------------------------------------
  compute_S <- function(m_j, vMarkers, nRows, sLambda) {
    
    # -- Create a distance vector between each marker and the current threshold
    vDiff <- vMarkers - m_j
    vDiff_ord <- sort(vDiff)
    
    # -- Establish a symmetrical neighbourhood around unique marker values that are in
    # close proximity to one another
    
    # - Upper bound calculation of neighbourhood
    # Find an index in the ordered difference vector [vDiff] beyond which point all markers are positively 
    # differenced wrt the current threshold
    Neigh_Mid <- sum(vDiff_ord <  0) + 1L
    # Find corresponding index for upper bound of neighbourhood
    upper_idx <- min(Neigh_Mid + trunc(nRows * sLambda + 0.5), nRows)
    # Retrieve associated neighbourhood bounds (upper + lower)
    Neigh_UpperB <- vDiff_ord[upper_idx]
    
    # - Lower bound calculation of neighbourhood
    # The classical symmetric window uses the same absolute distance on either side of the current marker
    Neigh_LowerB <- -abs(Neigh_UpperB)
    
    # - Apply chosen kernel function: 1 if the Marker is within the neighbourhood, 0 otherwise
    #weights <- as.numeric( (vMarkers - m_j) <= Neigh_UpperB & (vMarkers - m_j) >= Neigh_LowerB )
    weights <- as.numeric( (vDiff <= Neigh_UpperB) & (vDiff >= Neigh_LowerB) )
    
    # - S(t) can now be estimated by calculating the event and at-risk populations across filtered event times s,
    # as weighted by the kernel function [weights]
    S0 <- 1.0
    for (k in seq_len(nTimes)) {
      tt <- vEventTimes_Filtered[k]
      
      # - At each filtered event time tt, calculate the number of at-risk cases within the current neighbourhood
      at_risk <- sum(weights * (vStartTimes <= tt & vEventTimes >= tt), na.rm = T)
      
      # - At each filtered event time s, calculate the number of cases within the current neighbourhood
      # that experienced the event exactly at the current tt event time
      events  <- sum(weights * (vStartTimes <= tt & vEventTimes == tt & vEventInds == eventVal), na.rm = T)
      
      # - Calculate the well-known KM-based survival factor vector given the current filtered event time s
      if (at_risk > 0) S0 <- S0 * (1 - events / at_risk) else 1.0
    }
    return(S0)
  }
  
  if (numThreads > 1) {
    # Create cluster with numThreads (workers)
    cl <- parallel::makeCluster(numThreads)
    doParallel::registerDoParallel(cl)
    # Multithreaded loop over thresholds
    S_t_vec <- foreach(j = seq_len(nThresh), .combine = c) %dopar% {
      compute_S(vMarkers_unique[j], vMarkers, nRows, sLambda)
    }
    parallel::stopCluster(cl)
  } else {
    S_t_vec <- vapply(vMarkers_unique, compute_S, numeric(1))
  }
  
  # Index that maps each observation’s marker to its position in the unique-marker vector
  # (used to retrieve the corresponding S(t | m) value)
  vMatched  <- match(vMarkers, vMarkers_unique)
  
  # - Allocate the estimated S(t)-values across subjects over prediction times t (same as unique event times), 
  # given a specific marker value under which that particularly S(t) was estimated. This allocation is 
  # achieved using the previously-created map vector.
  vSurvProb <- S_t_vec[vMatched]
  
  # -----------------------------------------------------------------
  # 3. Prepare data for the second (also multithreaded) loop
  # -----------------------------------------------------------------
  dt_ord <- data.table(Marker = vMarkers, Surv_prob = vSurvProb)
  
  # - Classical estimator: grand mean across observations
  S_mean <- mean(dt_ord$Surv_prob, na.rm = T)
  
  # -----------------------------------------------------------------
  # 4. Multithreaded computation of S_tc and F_M for every threshold c
  # -----------------------------------------------------------------
  compute_ROC_quants <- function(c) {
    # - Calculate the marginal survival probability contribution from observations
    # whose marker value exceeds the current threshold c.
    # NOTE: The zero contribution from observations with Marker <= c is retained
    # in the mean, thereby implementing the indicator function I(M > c).
    S_tc <- mean( ifelse(dt_ord$Marker > c, dt_ord$Surv_prob, 0), na.rm = T)
    # - Calculate marker distribution.
    F_M <- mean( dt_ord$Marker <= c, na.rm = T)
    return( c(S_tc = S_tc,F_M  = F_M))
  }
  
  if (numThreads > 1) {
    # Create cluster with numThreads (workers)
    cl <- parallel::makeCluster(numThreads)
    doParallel::registerDoParallel(cl)
    # Export the data table to the workers
    parallel::clusterExport(cl, varlist = "dt_ord", envir = environment())
    # Multithreaded loop over thresholds
    roc_mat <- foreach(j = seq_len(nThresh), .combine = rbind, .packages = "data.table") %dopar% {
      compute_ROC_quants(vMarkers_unique[j])
    }
    parallel::stopCluster(cl)
  } else {
    roc_mat <- t(vapply(vMarkers_unique, compute_ROC_quants, numeric(2)))
  }
  
  datROC <- data.table( Threshold = vMarkers_unique, S_tc = roc_mat[, "S_tc"], F_M = roc_mat[, "F_M"] )
  
  # -----------------------------------------------------------------
  # 5. TPR / FPR (Heagerty et al. [2000]) + trapezoidal AUC
  # -----------------------------------------------------------------
  datROC[, TPR := ((1 - F_M) - S_tc) / (1 - S_mean)]
  datROC[, FPR := S_tc / S_mean]
  
  # Add boundaries
  datROC <- rbind(
    data.table(Threshold =  Inf, S_tc = 0, F_M = 1, TPR = 0, FPR = 0),
    datROC,
    data.table(Threshold = -Inf, S_tc = S_mean, F_M = 0, TPR = 1, FPR = 1)
  )
  setorder(datROC, FPR)
  
  # - Calculate final tAUC value
  AUC <- sum(diff(datROC$FPR) * (datROC$TPR[-1] + datROC$TPR[-nrow(datROC)]) / 2)
  
  return(list( AUC = AUC, S_mean = S_mean, ROC_table = datROC[, .(Threshold, TPR, FPR)],
               sLambda = sLambda, numDigits = numDigits, nThresh = nThresh
  ))
}
