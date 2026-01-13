# ============================== SURVIVAL FUNCTIONS ==============================
# Defining various custom functions relating to the estimation, analysis, and
# graphing of ROC-graphs and their summary statistics (AUC), as part of testing
# the discrimination power (prediction accuracy) of a given Cox regression model
# --------------------------------------------------------------------------------
# PROJECT TITLE: Loss Modelling (LGD) for Residential Mortgages
# SCRIPT AUTHOR(S): Dr Arno Botha (AB), Bernard Scheepers (BS), Mohammed Gabru (MG)
# VERSION: 2.0 (Apr-2025)
# ================================================================================





# ----------------- 1. Main functions for conducting time-Dependent ROC-analysis

# --- Single-threaded function to calculate an ROC-graph (True vs false positive rates) for a given prediction time interval,
# as adjusted for right-censoring by estimating both the overall and marker-conditional survivor functions.
# For a given/fitted Cox regression model, this function chiefly implements the Nearest Neighbours estimator from 
# Heagerty2000 (DOI: https://doi.org/10.1111/j.0006-341x.2000.00337.x) in estimating the 
# aforementioned survivor functions. This function then culminates in producing both the associated
# ROC-graph, itself constructed using the trapezoidal rule from Mason2002 (DOI: https://doi.org/10.1256/003590002320603584),
# and the AUC-statistic in summarising the ROC-graph.
# Input:  [datGiven]: A validation dataset containing the variables of interest for testing prediction accuracy.
#         [cox]: A fitted cox model used to obtain marker values (theoretically either the risk scores exp(\beta.X)
#               or simply just the linear combination \beta.X.
#         [month_Start]: The prediction starting period of the time range over which prediction accuracy is tested.
#         [month_End], The last prediction period of the time range over which prediction accuracy is tested.
#         [sLambda]: A smoothing parameter representing the %-valued neighborhood size,
#                  symmetrically calculated around each unique marker.
#         [estMethod]: The estimation method by which True Positive Rates (TPR) and False Positive Rates (FPR) are calculated
#         [numDigits]: The number of digits to which unique marker values are rounded, as an algorithmic efficiency boost
#         [fld_ID]: An optional field name that designates whether to group certain observations together by subject/spell ID
#         [fld_Event]: A required field name that designates the main event indicator
#         [eventVal]: A required field that denotes the main event value against which [fld_Event] is tested
#         [fld_StartTime]: An optional field name that denotes the entry times of observations becoming at risk of the main event
#         [fld_EndTime]: A required field name that designates the stop or raw event times
#         [Graph]: A boolean-valued toggle to produce the ROC-graph as a ggplot-object.
#         [graphName]: The base name under which the produced ggplot graph will be saved in the given path directory
#         [genFigPathGiven], A given path directory in which the ROC-graph (if produced) will be saved
# Output: [AUC]: The time-dependent Area under the curve (AUC) in summarising the corresponding time-dependent ROC-graph
#         [ROC_graph]: The associated ROC-graph as a ggplot-object
tROC <- function(datGiven, cox, month_Start=0, month_End, sLambda=0.05, estMethod="NN-0/1", numDigits=2, 
                 fld_ID=NA, fld_Event="MainEvent_Ind", eventVal=1, fld_StartTime="Start", fld_EndTime="Stop",
                 Graph=TRUE, graphName="timedROC-Graph", genFigPathGiven=paste0(getwd(),"/")){
  
  # ------ Preliminaries 
  # -- Testing Conditions
  #datGiven = copy(dat); cox=coxExample; month_End=203; numDigits=2; Graph=TRUE; month_Start=0; estMethod="NN-0/1";sLambda=0.05;
  #fld_ID="ID"; fld_Event="Event_Ind"; fld_StartTime="Start"; fld_EndTime="End";
  #graphName="coxExample_cgd"; eventVal=1; genFigPathGiven=genFigPath
  
  # -- Error handling
  if (!is.data.table(datGiven)) {
    stop("[datGiven] must be a data table.\n")
  }# Test whether datGiven is a data table
  if (!inherits(cox, "coxph")) {
    stop("[cox] must be a valid 'coxph' model object.\n")
  }# Test whether [cox] is a coxph model
  if (!all(all.vars(formula(cox)) %in% colnames(datGiven))){
    stop("[datGiven] does not contain the variables required within the [cox] object.\n")
  }# Test whether [datGiven] contains the variables on which [cox] was built
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
  # values (option: linear predictors)
  datGiven[, Marker := round(predict(cox, newdata=datGiven, type="lp"),numDigits)]
  # - Let the unique marker values represent our threshold space, which is standard practice in ROC-analysis
  thresholds <- datGiven$Marker %>% unique() %>% sort()
  nThresh <- length(thresholds) # number of such unique thresholds for iteration purposes
  
  # -- Reassign given field names to standardised naming conventions, if only within this function
  # NOTE: This will be reverted towards the end of the function
  if (!is.na(fld_ID) & !is.na(fld_StartTime)){
    setnames(datGiven, old=c(c(fld_StartTime, fld_EndTime, fld_ID, fld_Event)),
             new=c("StartTime", "EndTime", "ID", "Event_Ind"))
    # - Set indicators according to which the rest of the program will function conditionally
    Grouped_Ind <- T
    HasStarting_Ind <- T
  } else if (is.na(fld_ID) & !is.na(fld_StartTime)) {
    setnames(datGiven, old=c(c(fld_StartTime, fld_EndTime, fld_Event)),
             new=c("StartTime", "EndTime", "Event_Ind"))
    # - Set indicators according to which the rest of the program will function conditionally
    Grouped_Ind <- F
    HasStarting_Ind <- T
  } else if (is.na(fld_ID) & is.na(fld_StartTime)) {
    setnames(datGiven, old=c(c(fld_EndTime, fld_Event)),
             new=c("EndTime", "Event_Ind"))
    # - Set indicators according to which the rest of the program will function conditionally
    Grouped_Ind <- F
    HasStarting_Ind <- F
  } else if (!is.na(fld_ID) & is.na(fld_StartTime)) {
    setnames(datGiven, old=c(c(fld_EndTime, fld_ID, fld_Event)),
             new=c("EndTime", "ID", "Event_Ind"))
    # - Set indicators according to which the rest of the program will function conditionally
    Grouped_Ind <- T
    HasStarting_Ind <- F
  }
  
  # - Check if any end points fall within the given range
  if (datGiven[EndTime >= month_Start & EndTime <= month_End, .N] == 0) {
    stop(paste0("The observed endpoints exceed the bounds of the given prediction time interval, [",
                month_Start, ",", month_End, "]"))
  }
  
  
  # -- Obtain various quantities towards implementing the Nearest Neighbour Estimator (NNE) method
  nRows <- NROW(datGiven) # Total number of markers or rows (not necessarily subjects)
  
  # - Get the corresponding rank order of the raw end times when sorted ascendantly
  vOrder <- order(datGiven$EndTime)
  # Re-sort the raw end points according to these particular  rank-order indices, whereafter
  # the same is performed to the vectors of event indicators, marker-values, and starting times
  vEventTimes <- datGiven$EndTime[vOrder]
  vEventInds <- datGiven$Event_Ind[vOrder]
  vMarkers <- datGiven$Marker[vOrder]
  # - Check for missingness in the starting times vector and, if found, treat by assigning a zero vector
  if ("StartTime" %in% names(datGiven)) {
    vStartTimes <- datGiven$StartTime
  } else {
    vStartTimes <- rep(0, nRows)
  }
  
  
  # ------ Estimation procedure for constructing an ROC-graph
  # Use a tryCatch-block to handle deliberate errors towards more concise programming
  tryCatch({ # ----------------- Start of tryCatch-block -----------------
    
    # ------ In calculating the ROC-graph, 3 fundamental quantities must be estimated:
    # 1) the classical survivor function S(t) irrespective of Marker values
    # 2) the conditional survivor function S(t| M > c), used in estimating S(t)
    # 3) the respective True Positive Rates (TPR) and False Positive Rates (FPR) across all thresholds
    
    
    # ----------------- Start of Program ----------------------
    
    # --- 1. Estimating the classical S(t) given each threshold
    # Implement the chosen estimator for S(t) and the choice of kernel (if Nearest Neighbour)
    if(substr(estMethod,1,2)=="NN"){ # Nearest Neighbour (NN) estimator
      
      # -- Preliminaries
      
      # - Initialize empty data structures for implementing the current method
      S_t <- numeric(nThresh) #Initialise the eventual survival probability vector across unique ordered failure times
      
      # - Obtain unique Markers (predictions from the given Cox-model), and order them as before according to 
      # the indices of the ordered unique event times
      vMarkers_unique <- unique(datGiven$Marker) 
      vMarkers_unique <- vMarkers_unique[order(vMarkers_unique)]
      
      # - Obtain those unique event times at which the main event occurred
      vEventTimes_Main <- unique(vEventTimes[vEventInds==eventVal])
      vEventTimes_Main <- vEventTimes_Main[order(vEventTimes_Main)]
      
      # - Using the filtered event times, retrieve only those for which they preclude the given upper bound of the
      # prediction time t, though also exceed the lower bound (if specified)
      # NOTE: Achieves parity with the previous version of this function that computed [DTimes]
      # NOTE2: Its dimensions will inform those of the eventual survival probability vector
      vEventTimes_Filtered <- vEventTimes_Main[vEventTimes_Main <= month_End & vEventTimes_Main >= month_Start]
      
      
      # -- Estimation procedure for S(t), based on the combined works of Heagerty2000, and elements of the survivalROC-package
      # Outer loop: Calculate the 0/1-kernel function by iterating across thresholds, and
      #             finding a symmetrical (fixed-size) neighbourhood of markers within each iteration
      # Inner loop: Using the estimated kernels [weights], calculate the constituents of the KM-estimator,
      #             as weighed by the kernels in following Heagerty2000
      # NOTE: nThresh is identical to length(vMarkers_unique)
      for (j in 1:nThresh) { # ----------------- Start of Outer Loop -----------------
        #
        
        # -- Create a distance vector between each marker and the current threshold
        vDiff <- vMarkers - thresholds[j] # will always be positive, but unordered
        vDiff <- vDiff[order(vDiff)]  # Sort in ascending order
        
        # --- Differentiate execution by kernel choice
        if (estMethod == "NN-0/1") { # 0/1 Kernel from Akritas1994
          
          # -- Establish a symmetrical neighbourhood around unique marker values that are in
          # close proximity to one another
          
          # - Find an index in the ordered difference vector [sDiff] beyond which point all markers are positively 
          # differenced wrt the current threshold
          Neigh_Mid <- sum(vDiff < 0) +1
          # In catering for cases where the midpoint must be decided between two identical Marker-values,
          # we shall take the next index
          Neigh_Mid2 <- sum(vDiff <= 0) # catering for cases where the midpoint 
          # - Find an index for upper bound of neighbourhood bounded by the index of largest marker
          Neigh_UpperB_ind <- min(Neigh_Mid + trunc(nRows * sLambda + 0.5), nRows)
          # - Find an index for lower bound of neighbourhood bounded by the index of smallest marker
          Neigh_LowerB_ind <- max(Neigh_Mid2 - trunc(nRows * sLambda /2), 1)
          # - Retrieve associated neighbourhood bounds (upper + lower)
          Neigh_UpperB <- vDiff[Neigh_UpperB_ind]
          Neigh_LowerB <- vDiff[Neigh_LowerB_ind]
          
          # - Apply chosen kernel function: 1 if the Marker is within the neighbourhood, 0 otherwise
          # NOTE: thresholds[j] and vMarkers_unique[j] will always be equal
          weights <- ifelse(((vMarkers - vMarkers_unique[j]) <= Neigh_UpperB) & 
                              ((vMarkers - vMarkers_unique[j]) >= Neigh_LowerB),  1,0)
        } else{ stop("Unknown kernel choice.")}
        
        # -- S(t) can now be estimated by calculating the event and at-risk populations across filtered event times s,
        # as weighted by the kernel function [weights]
        S_0 <- 1 # Initialise the starting survival probability to 100%
        for (jj in 1:length(vEventTimes_Filtered)) { 
          
          # - At each filtered event time s, calculate the number of at-risk cases within the current neighbourhood
          n_values <- sum(weights * (vStartTimes <= vEventTimes_Filtered[jj]) & 
                            (vEventTimes >= vEventTimes_Filtered[jj]),na.rm=T) 
          # - At each filtered event time s, calculate the number of cases within the current neighbourhood 
          # that experienced the event exactly at the current s
          d_values <- sum(weights * (vStartTimes <= vEventTimes_Filtered[jj]) & 
                            (vEventTimes == vEventTimes_Filtered[jj])*(vEventInds==eventVal),na.rm=T) 
          
          # - Calculate the well-known KM-based survival factor vector given the current filtered event time s
          if (n_values>0) S_0 <- S_0 * (1 - (d_values / n_values))
        }
        
        # - Save the pooled survival probability (at last ordered event time) as the main result for the current threshold
        S_t[j] <- S_0
      } # ----------------- End of Outer Loop -----------------
      
      
      
      # ------------------------------------- END OF NN-Method (0/1 Kernel)
    } else{ stop("Unknown estimation method.") }
    
    # - Obtain an index mapping between the unique marker vector x' and the raw (non-distinct) marker vector x such that each element
    # in this mapping (corresponding to each value in x, i.e., the mapping has the same dimensions as x) denotes the index in x' that 
    # represents the unique value's position and/or rank in x'.
    vMatched <- match(vMarkers, vMarkers_unique)
    
    # - Allocate the estimated S(t)-values across subjects over prediction times t (same as unique event times), given
    # a specific marker value under which that particularly S(t) was estimated. This allocation is achieved using the 
    # previously-created map vector
    vSurvProb <- S_t[vMatched]
    
    
    
    # --- 2. Estimate the marker-conditional survivor function S(t| M > c)  by simply filtering for certain marker/scores 
    # NOTE: Estimation hereof depends on whether independence is assumed or not amongst rows in the provided data
    if (Grouped_Ind==F) { # Independence amongst all observations
      
      # -- Calculate the mean survival probability across scored cases, given an -\infty marker value
      S_mean <- mean(vSurvProb, na.rm=T)  
      
    } else { # Dependence amongst observations, clustered by a given ID-value
      
      # NOTE: This operation will need to use the grouping functionality of data.table, and therefore
      # the operations will occur outside of vectors and inside the given data.table object
      # - As such, first obtain an index mapping between the sorted vMarkers and the original unsorted vector in datGiven
      vMatched2 <- match(datGiven$Marker, vMarkers)
      # - Merge the [vSurvProb] vector unto this datatset
      datGiven[, Surv_prob := vSurvProb[vMatched2]]
      
      # -- Calculate the overall survival probability at prediction time t, i.e., given an -\infty marker value
      # Calculate the average [Surv_prob] for each "id" and averaging these "id" specific averages across the portfolio
      # NOTE: This estimator is the grand mean of the average [Surv_prob]-values per ID
      S_mean <- mean(datGiven[,list(S_Marg = sum(Surv_prob,na.rm=T)/.N), by=list(ID)]$S_Marg) 
    }
    
    
    
    # --- 3. Calculate TPR and FPR by iterating across each threshold (unique marker)
    # - Preliminaries
    vTPR <- c(rep(NA, nThresh-1),0) # Initialise vector and set threshold boundaries
    vFPR <- c(rep(NA, nThresh-1),1) # Initialise vector and set threshold boundaries
    
    
    # -- Iterate across thresholds and calculate TPR and FPR given that each threshold conditions the risk set
    for (c in 1:(nThresh )) {
      
      # NOTE: Estimation hereof depends on whether independence is assumed or not amongst rows in the provided data
      if (Grouped_Ind==F) { # Independence amongst all observations
        
        # - Empirical distribution of markers being less than the current threshold p_c
        cumulMark <- mean(vMarkers <= thresholds[c], na.rm=T)
        
        # - Calculate mean survival probability given current threshold p_c, within the subset of marker values exceeding p_c
        # NOTE: The inner operation amounts to the indicator function in the equation of S_{lambda_n}(p_c,t)
        S_tc <- sum(vSurvProb[vMarkers > thresholds[c]], na.rm=T)/nRows
        
        # - Calculate TPR and FPR according to Heagerty2000
        vTPR[c] <- ((1-cumulMark) - S_tc)/(1 - S_mean) # TPR
        vFPR[c] <- S_tc/S_mean # FPR
        
      } else { # Dependence amongst observations, clustered by a given ID-value
        
        # - Empirical distribution of markers being less than the current threshold p_c
        # First, we obtain the proportion amongst all cases with markers less than or equal to p_c
        # Then, we calculate the grand mean amongst all of these ID-level proportions.
        cumulMark = mean(datGiven[,list(Prop = sum(Marker <= thresholds[c])/.N), by=list(ID)]$Prop, na.rm=T)
        
        #S_lam <- sum(datGiven$Surv_prob[datGiven$Marker > thresholds[c]])/nRows # Sum of survival probabilities for Marker values greater than threshold
        
        # - Calculate mean survival probability given current threshold p_c, within the subset of marker values exceeding p_c
        # NOTE: The inner operation amounts to the indicator function in the equation of S_{lambda_n}(p_c,t)
        # NOTE2: Firstly, we obtain the mean survival probability amongst markers associated with an ID, conditioned on
        # these markers exceeding the threshold.
        #       Secondly, we calculate the grand mean amongst all of these ID-level mean survival probabilities given p_c
        S_tc <- mean(datGiven[,list(S_tc_ID=sum(ifelse(Marker > thresholds[c],Surv_prob,0),na.rm=T)/.N), 
                              by=list(ID)]$S_tc_ID, na.rm=T)
        
        # - Calculate TPR and FPR according to Heagerty2000
        vTPR[c] <- ((1-cumulMark) - S_tc)/(1 - S_mean) # TPR
        vFPR[c] <- S_tc/S_mean # FPR
      }
    }
    
    
    # --- Constructing the ROC-graph itself using the Trapezoidal Rule from numerical integration practices
    # NOTE: We apply the trapezoidal rule as in survivalROC() from Heagerty2000, implicitly following 
    # DeLong1988 and Fawcett2008 but not exactly
    # - Attach the lower boundary to the vectors of TPR and FPR (cannot be done earlier)
    # Note that we are increasing the dimensions by 1 and will need to use (nThresh+1)
    vTPR <- c(1, vTPR)
    vFPR <- c(1, vFPR)
    # - Bar width = difference between consecutive points, having removed outer boundaries
    vWidth <- vFPR[-(nThresh+1)] - vFPR[-1] 
    # - Calculate Bar height = Average between two consecutive points, having removed outer boundaries
    vMidpoints <- (vTPR[-(nThresh+1)] + vTPR[-1])/2
    # - Finally, calculate the area under the ROC-curve as the summed product
    sArea <- sum(vWidth * vMidpoints) # AUC
    
    
    # -- Revert name changes 
    if (Grouped_Ind & HasStarting_Ind){
      setnames(datGiven, new=c(c(fld_StartTime, fld_EndTime, fld_ID, fld_Event)),
               old=c("StartTime", "EndTime", "ID", "Event_Ind"))
    } else if (!Grouped_Ind & HasStarting_Ind) {
      setnames(datGiven, new=c(c(fld_StartTime, fld_EndTime, fld_Event)),
               old=c("StartTime", "EndTime", "Event_Ind"))
      # - Set indicators according to which the rest of the program will function conditionally
      Grouped_Ind <- F
      HasStarting_Ind <- T
    } else if (!Grouped_Ind & !HasStarting_Ind) {
      setnames(datGiven, new=c(c(fld_EndTime, fld_Event)),
               old=c("EndTime", "Event_Ind"))
    } else if (Grouped_Ind & !HasStarting_Ind) {
      setnames(datGiven, new=c(c(fld_EndTime, fld_ID, fld_Event)),
               old=c("EndTime", "ID", "Event_Ind"))
    }
    
    # -- If toggled, produce an ROC-graph with annotations
    if(Graph){
      
      # - Create a data object for plotting purposes
      datGraph <- data.frame(x = vFPR[-(nThresh+1)], y=vTPR[-1])
      datSegment <- data.frame(x = 0, y = 0, xend = 1, yend = 1)
      # - Annotate with concordance (Harrell's C)
      conc=percent(as.numeric(concordance(cox,newdata=datGiven)[1]))
      
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
        annotate("label", x = c(0.75,0.75), y = c(0.375,0.125),label = 
                   c(paste0("AUC(",month_Start,",", month_End,"): ", percent(sArea, accuracy=0.01)),
                     paste0("Harrell's c-statistic: ", conc)), fill="grey", family=chosenFont) + 
        scale_y_continuous(label=percent) + scale_x_continuous(label=percent)
      
      # Save graph
      dpi <- 200
      ggsave(gg, file=paste0(genFigPathGiven, graphName,"(",month_Start,",",month_End,").png"), width=1200/dpi, height=1000/dpi, dpi=dpi, bg="white")
      
      retObj <- list(AUC = sArea, ROC_graph=gg, Thresholds=c(-Inf, thresholds), TPR=vTPR, FPR=vFPR, SurvivalProb_Mean = S_mean)
    } else{ retObj <- list(AUC = sArea, Thresholds=c(-Inf, thresholds), TPR=vTPR, FPR=vFPR, SurvivalProb_Mean = S_mean) } 
    
    # - Conclude program and return results
    return(retObj)
    
    # ----------------- End of Program ------------------------
    
  }, error = function(e){
    print(e)
    cat("Reverting necessary name changes in given dataset and exiting .. \n")
    
    # - Revert name changes 
    if (Grouped_Ind & HasStarting_Ind){
      setnames(datGiven, new=c(c(fld_StartTime, fld_EndTime, fld_ID, fld_Event)),
               old=c("StartTime", "EndTime", "ID", "Event_Ind"))
    } else if (!Grouped_Ind & HasStarting_Ind) {
      setnames(datGiven, new=c(c(fld_StartTime, fld_EndTime, fld_Event)),
               old=c("StartTime", "EndTime", "Event_Ind"))
      # - Set indicators according to which the rest of the program will function conditionally
      Grouped_Ind <- F
      HasStarting_Ind <- T
    } else if (!Grouped_Ind & !HasStarting_Ind) {
      setnames(datGiven, new=c(c(fld_EndTime, fld_Event)),
               old=c("EndTime", "Event_Ind"))
    } else if (Grouped_Ind & !HasStarting_Ind) {
      setnames(datGiven, new=c(c(fld_EndTime, fld_ID, fld_Event)),
               old=c("EndTime", "ID", "Event_Ind"))
    }  
    
  }) # ----------------- End of tryCatch-block -----------------
  
  # - Generic cleanup (should parts of this function be run in debug/interactive mode)
  suppressWarnings( rm(datGiven, thresholds, nThresh, Grouped_Ind, HasStarting_Ind, nRows, vOrder, vEventTimes, vEventInds, 
                       vMarkers, vStartTimes,S_t, vMarkers_unique, vEventTimes_Main, vEventTimes_Filtered, vDiff, 
                       Neigh_Mid, Neigh_Mid2, Neigh_UpperB, Neigh_LowerB, Neigh_UpperB_ind, Neigh_LowerB_ind, 
                       weights, S_0, n_values, d_values, vMatched, vMatch2, vSurvProb, S_mean, vTPR, vFPR, 
                       cumulMark, S_tc, vWidth, vMidpoints, sArea, datGraph, datSegment, conc, gg, retObj) )
}

# NOTE: Unit tests for tROC() are performed in a proof of concept script called Comparison_tROC()





# --- Multi-threaded function to calculate an ROC-graph (True vs false positive rates) for a given prediction time interval,
# as adjusted for right-censoring by estimating both the overall and marker-conditional survivor functions.
# For a given/fitted Cox regression model, this function chiefly implements the Nearest Neighbours estimator from 
# Heagerty2000 (DOI: https://doi.org/10.1111/j.0006-341x.2000.00337.x) in estimating the 
# aforementioned survivor functions. This function then culminates in producing both the associated
# ROC-graph, itself constructed using the trapezoidal rule from Mason2002 (DOI: https://doi.org/10.1256/003590002320603584),
# and the AUC-statistic in summarising the ROC-graph.
# Input:  [datGiven]: A validation dataset containing the variables of interest for testing prediction accuracy.
#         [modGiven]: A fitted cox/GLM model used to obtain marker values (theoretically either the risk scores exp(\beta.X)
#               or simply just the linear combination \beta.X.
#         [month_Start]: The prediction starting period of the time range over which prediction accuracy is tested.
#         [month_End], The last prediction period of the time range over which prediction accuracy is tested.
#         [sLambda]: A smoothing parameter representing the %-valued neighborhood size,
#                  symmetrically calculated around each unique marker.
#         [estMethod]: The estimation method by which True Positive Rates (TPR) and False Positive Rates (FPR) are calculated.
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
#         [MarkerGiven]: A given existing marker value
#         [threshold]: A given threshold to dichotomise marker values
# Output: [AUC]: The time-dependent Area under the curve (AUC) in summarising the corresponding time-dependent ROC-graph
#         [ROC_graph]: The associated ROC-graph as a ggplot-object
tROC.multi <- function(datGiven, modGiven, month_Start=0, month_End, sLambda=0.05, estMethod="NN-0/1", numDigits=2, 
                       fld_ID=NA, fld_Event="MainEvent_Ind", eventVal=1, fld_StartTime="Start", fld_EndTime="Stop",
                       Graph=TRUE, graphName="timedROC-Graph", genFigPathGiven=paste0(getwd(),"/"), numThreads=4, 
                       caseStudyName="Main",reportFlag=T, logPath=paste0(getwd(),"/"), predType="exp",
                       MarkerGiven=NA, threshold=NA) {
  
  # ------ Preliminaries 
  # -- Testing Conditions
  # datGiven = copy(dat); cox=coxExample; month_End=203; numDigits=2; Graph=TRUE; month_Start=0; estMethod="NN-0/1";sLambda=0.05;
  # fld_ID="ID"; fld_Event="Event_Ind"; fld_StartTime="Start"; fld_EndTime="End"; caseStudyName="Main"
  # graphName="coxExample_cgd"; eventVal=1; genFigPathGiven=paste0(genFigPath, "/TFD"); numThreads=6; reportFlag=T; 
  # logPath=paste0(getwd(),"/"); predType="exp"
  # -- Testing conditions 2 (real-world data)
  # datGiven = copy(datCredit_valid_TFD); cox=cox_TFD; month_End=12; numDigits=0; Graph=TRUE; month_Start=0; estMethod="NN-0/1";sLambda=0.05;
  # fld_ID="PerfSpell_Key"; fld_Event="PerfSpell_Event"; eventVal=1; fld_StartTime="Start"; fld_EndTime="End";
  # graphName="DefaultSurvModel-Cox1_Depedendence"; genFigPathGiven=paste0(genFigPath, "/TFD"); numThreads=6; reportFlag=T; caseStudyName="Main"
  # logPath=genPath; predType="exp"
  # -- Testing conditions 3 (real-world Cox Proportional Odds / Discrete-time model)
  # datGiven=datCredit_valid; modGiven=modLR; month_End=predictTime; sLambda=0.05; estMethod="NN-0/1"; numDigits=4 
  # fld_ID="PerfSpell_Key"; fld_Event="PerfSpell_Event"; eventVal=1; fld_StartTime="Start"; fld_EndTime="TimeInPerfSpell";
  # graphName="ROC_CoxDisc_Basic_TimeVar"; genFigPathGiven=paste0(genFigPath, "tROC-Analyses/");
  # caseStudyName=paste0("CoxDisc_PWPST_", predictTime); numThreads=12; logPath=genPath
  # month_Start=0; Graph=T; predType="response"; reportFlag=T
  # MarkerGiven=EventRate_bas ; threshold=0.5
  datGiven <- datCredit; modGiven <- NA; month_End <- predictTime; sLambda <- 0.05; estMethod <- "NN-0/1"; numDigits <- 4; 
  fld_ID <- "DefSpell_Key"; fld_Event <- "WOff_Ind"; eventVal <- 1; fld_StartTime <- "Start"; fld_EndTime <- "TimeInDefSpell";
  caseStudyName <- paste0("Cond_SurvTree_", predictTime); numThreads <- 12; logPath <- genPath; 
  predType <- "response"; MarkerGiven <- "Hazard"; Graph <- F
  
  
  # -- Error handling
  if (!is.data.table(datGiven)) {
    stop("[datGiven] must be a data table.\n")
  }# Test whether datGiven is a data table
  if (!any(class(modGiven) %in% c("coxph","lm","glm", "logical"))) {
    stop("[modGiven] must be a valid 'coxph' or 'glm' model object.\n")
  }# Test whether [cox] is a coxph model
  if (class(modGiven)!="logical"){
    if (!all(all.vars(formula(modGiven)) %in% colnames(datGiven))){
      stop("[datGiven] does not contain the variables required within the [modGiven] object.\n")
    }
  }# Test whether [datGiven] contains the variables on which [cox] was built
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
  
  # - Create shallow copy
  datGiven <- copy(datGiven)
  
  # -- Obtain Markers/prediction scores M_i for i=1,...,n cases (not necessarily subjects) and assign as thresholds
  # - Score the given dataset using the given Cox regression model towards obtaining marker 
  # values (option: linear predictors)
  if(is.na(MarkerGiven)){
    datGiven[, Marker := round(predict(modGiven, newdata=datGiven, type=predType),numDigits)]
  } else if (!is.na(MarkerGiven)) {
    datGiven[, Marker := get(MarkerGiven)]  
  }
  
  # - Dichotomise marker values given a specific threshold
  if (!is.na(threshold)){
    datGiven[, Marker:=ifelse(Marker>threshold,1,0)]
  }
  
  # - Let the unique marker values represent our threshold space, which is standard practice in ROC-analysis
  thresholds <- datGiven$Marker %>% unique() %>% sort()
  nThresh <- length(thresholds) # number of such unique thresholds for iteration purposes
  
  # -- Reassign given field names to standardised naming conventions, if only within this function
  # NOTE: This will be reverted towards the end of the function
  if (!is.na(fld_ID) & !is.na(fld_StartTime)){
    setnames(datGiven, old=c(c(fld_StartTime, fld_EndTime, fld_ID, fld_Event)),
             new=c("StartTime", "EndTime", "ID", "Event_Ind"))
    # - Set indicators according to which the rest of the program will function conditionally
    Grouped_Ind <- T
    HasStarting_Ind <- T
  } else if (is.na(fld_ID) & !is.na(fld_StartTime)) {
    setnames(datGiven, old=c(c(fld_StartTime, fld_EndTime, fld_Event)),
             new=c("StartTime", "EndTime", "Event_Ind"))
    # - Set indicators according to which the rest of the program will function conditionally
    Grouped_Ind <- F
    HasStarting_Ind <- T
  } else if (is.na(fld_ID) & is.na(fld_StartTime)) {
    setnames(datGiven, old=c(c(fld_EndTime, fld_Event)),
             new=c("EndTime", "Event_Ind"))
    # - Set indicators according to which the rest of the program will function conditionally
    Grouped_Ind <- F
    HasStarting_Ind <- F
  } else if (!is.na(fld_ID) & is.na(fld_StartTime)) {
    setnames(datGiven, old=c(c(fld_EndTime, fld_ID, fld_Event)),
             new=c("EndTime", "ID", "Event_Ind"))
    # - Set indicators according to which the rest of the program will function conditionally
    Grouped_Ind <- T
    HasStarting_Ind <- F
  }
  
  # - Check if any end points fall within the given range
  if (datGiven[EndTime >= month_Start & EndTime <= month_End, .N] == 0) {
    stop(paste0("The observed endpoints exceed the bounds of the given prediction time interval, [",
                month_Start, ",", month_End, "]"))
  }
  
  
  # -- Obtain various quantities towards implementing the Nearest Neighbour Estimator (NNE) method
  nRows <- NROW(datGiven) # Total number of markers or rows (not necessarily subjects)
  
  # - Get the corresponding rank order of the raw end times when sorted ascendantly
  vOrder <- order(datGiven$EndTime)
  # Re-sort the raw end points according to these particular  rank-order indices, whereafter
  # the same is performed to the vectors of event indicators, marker-values, and starting times
  vEventTimes <- datGiven$EndTime[vOrder]
  vEventInds <- datGiven$Event_Ind[vOrder]
  vMarkers <- datGiven$Marker[vOrder]
  # - Check for missingness in the starting times vector and, if found, treat by assigning a zero vector
  if ("StartTime" %in% names(datGiven)) {
    vStartTimes <- datGiven$StartTime
  } else {
    vStartTimes <- rep(0, nRows)
  }
  
  
  # ------ Estimation procedure for constructing an ROC-graph
  # Use a tryCatch-block to handle deliberate errors towards more concise programming
  tryCatch({ # ----------------- Start of tryCatch-block -----------------
    
    # ------ In calculating the ROC-graph, 3 fundamental quantities must be estimated:
    # 1) the classical survivor function S(t) irrespective of Marker values
    # 2) the conditional survivor function S(t| M > c), used in estimating S(t)
    # 3) the respective True Positive Rates (TPR) and False Positive Rates (FPR) across all thresholds
    
    
    # ----------------- Start of Program ----------------------
    
    # --- 1. Estimating the classical S(t) given each threshold
    # Implement the chosen estimator for S(t) and the choice of kernel (if Nearest Neighbour)
    if(substr(estMethod,1,2)=="NN"){ # Nearest Neighbour (NN) estimator
      
      # -- Preliminaries
      
      # - Initialize empty data structures for implementing the current method
      #S_t <- numeric(nThresh) #Initialise the eventual survival probability vector across unique ordered failure times
      
      # - Obtain unique Markers (predictions from the given Cox/GLM-model), and order them as before according to 
      # the indices of the ordered unique event times
      vMarkers_unique <- unique(datGiven$Marker) 
      vMarkers_unique <- vMarkers_unique[order(vMarkers_unique)]
      
      # - Obtain those unique event times at which the main event occurred
      vEventTimes_Main <- unique(vEventTimes[vEventInds==eventVal])
      vEventTimes_Main <- vEventTimes_Main[order(vEventTimes_Main)]
      
      # - Using the filtered event times, retrieve only those for which they preclude the given upper bound of the
      # prediction time t, though also exceed the lower bound (if specified)
      # NOTE: Achieves parity with the previous version of this function that computed [DTimes]
      # NOTE2: Its dimensions will inform those of the eventual survival probability vector
      vEventTimes_Filtered <- vEventTimes_Main[vEventTimes_Main <= month_End & vEventTimes_Main >= month_Start]
      
      
      # -- Estimation procedure for S(t), based on the combined works of Heagerty2000, and elements of the survivalROC-package
      # Outer loop: Calculate the 0/1-kernel function by iterating across thresholds, and
      #             finding a symmetrical (fixed-size) neighbourhood of markers within each iteration
      # Inner loop: Using the estimated kernels [weights], calculate the constituents of the KM-estimator,
      #             as weighed by the kernels in following Heagerty2000
      # NOTE: nThresh is identical to length(vMarkers_unique)
      
      # - Iterate across loan space using a multi-threaded setup
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
                                   estMethod=estMethod, sLambda=sLambda, nRows=nRows, reportFlag=reportFlag, 
                                   iteration=j, nThresh=nThresh, caseStudyName=caseStudyName, logPath=logPath,
                                   defChoice="ForLoop2")
          
        } # ----------------- End of Outer Loop -----------------
      stopCluster(cl.port); proc.time() - ptm
      
    } else{ stop("Unknown estimation method.") }
    
    # - Obtain an index mapping between the unique marker vector x' and the raw (non-distinct) marker vector x such that each element
    # in this mapping (corresponding to each value in x, i.e., the mapping has the same dimensions as x) denotes the index in x' that 
    # represents the unique value's position and/or rank in x'.
    vMatched <- match(vMarkers, vMarkers_unique)
    
    # - Allocate the estimated S(t)-values across subjects over prediction times t (same as unique event times), given
    # a specific marker value under which that particularly S(t) was estimated. This allocation is achieved using the 
    # previously-created map vector
    vSurvProb <- datS_t$S_t[vMatched]
    
    
    
    # --- 2. Estimate the marker-conditional survivor function S(t| M > c)  by simply filtering for certain marker/scores 
    # NOTE: Estimation hereof depends on whether independence is assumed or not amongst rows in the provided data
    if (Grouped_Ind==F) { # Independence amongst all observations
      
      # -- Calculate the mean survival probability across scored cases, given an -\infty marker value
      S_mean <- mean(vSurvProb, na.rm=T)  
      
    } else { # Dependence amongst observations, clustered by a given ID-value
      
      # NOTE: This operation will need to use the grouping functionality of data.table, and therefore
      # the operations will occur outside of vectors and inside the given data.table object
      # - As such, first obtain an index mapping between the sorted vMarkers and the original unsorted vector in datGiven
      vMatched2 <- match(datGiven$Marker, vMarkers)
      # - Merge the [vSurvProb] vector unto this datatset
      datGiven[, Surv_prob := vSurvProb[vMatched2]]
      
      # -- Calculate the overall survival probability at prediction time t, i.e., given an -\infty marker value
      # Calculate the average [Surv_prob] for each "id" and averaging these "id" specific averages across the portfolio
      # NOTE: This estimator is the grand mean of the average [Surv_prob]-values per ID
      S_mean <- mean(datGiven[,list(S_Marg = sum(Surv_prob,na.rm=T)/.N), by=list(ID)]$S_Marg) 
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
                                         S_mean=S_mean, nRows=nRows, datGiven=datGiven, Grouped_Ind=Grouped_Ind,
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
    
    
    # -- Revert name changes 
    if (Grouped_Ind & HasStarting_Ind){
      setnames(datGiven, new=c(c(fld_StartTime, fld_EndTime, fld_ID, fld_Event)),
               old=c("StartTime", "EndTime", "ID", "Event_Ind"))
    } else if (!Grouped_Ind & HasStarting_Ind) {
      setnames(datGiven, new=c(c(fld_StartTime, fld_EndTime, fld_Event)),
               old=c("StartTime", "EndTime", "Event_Ind"))
      # - Set indicators according to which the rest of the program will function conditionally
      Grouped_Ind <- F
      HasStarting_Ind <- T
    } else if (!Grouped_Ind & !HasStarting_Ind) {
      setnames(datGiven, new=c(c(fld_EndTime, fld_Event)),
               old=c("EndTime", "Event_Ind"))
    } else if (Grouped_Ind & !HasStarting_Ind) {
      setnames(datGiven, new=c(c(fld_EndTime, fld_ID, fld_Event)),
               old=c("EndTime", "ID", "Event_Ind"))
    }
    
    # -- If toggled, produce an ROC-graph with annotations
    if(Graph){
      
      # - Create a data object for plotting purposes
      datGraph <- data.frame(x = vFPR[-(nThresh+1)], y=vTPR[-1])
      datSegment <- data.frame(x = 0, y = 0, xend = 1, yend = 1)
      # - Annotate with concordance (Harrell's C)
      conc=percent(as.numeric(concordance(modGiven,newdata=datGiven)[1]),accuracy=0.001)
      
      
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
        annotate("label", x = c(0.75,0.75), y = c(0.375,0.125),label = 
                   c(paste0("AUC(",month_Start,",", month_End,"): ", percent(sArea, accuracy=0.01)),
                     paste0("Concordance-statistic: ", conc)), fill="grey", family=chosenFont) + 
        scale_y_continuous(label=percent) + scale_x_continuous(label=percent)
      
      # Save graph
      dpi <- 200
      ggsave(gg, file=paste0(genFigPathGiven, graphName,"(",month_Start,",",month_End,").png"), 
             width=1200/dpi, height=1000/dpi, dpi=dpi, bg="white")
      
      retObj <- list(AUC = sArea, ROC_graph=gg, Thresholds=c(-Inf, thresholds), TPR=vTPR, 
                     FPR=vFPR, SurvivalProb_Mean = S_mean)
      
    } else{ retObj <- list(AUC = sArea, Thresholds=c(-Inf, thresholds), TPR=vTPR, FPR=vFPR, SurvivalProb_Mean = S_mean) }
    
    
    # - Conclude program and return results
    return(retObj)
    
    # ----------------- End of Program ------------------------
    
  }, error = function(e){
    print(e)
    cat("Reverting necessary name changes in given dataset and exiting .. \n")
    # - Revert name changes 
    if (Grouped_Ind & HasStarting_Ind){
      setnames(datGiven, new=c(c(fld_StartTime, fld_EndTime, fld_ID, fld_Event)),
               old=c("StartTime", "EndTime", "ID", "Event_Ind"))
    } else if (!Grouped_Ind & HasStarting_Ind) {
      setnames(datGiven, new=c(c(fld_StartTime, fld_EndTime, fld_Event)),
               old=c("StartTime", "EndTime", "Event_Ind"))
      # - Set indicators according to which the rest of the program will function conditionally
      Grouped_Ind <- F
      HasStarting_Ind <- T
    } else if (!Grouped_Ind & !HasStarting_Ind) {
      setnames(datGiven, new=c(c(fld_EndTime, fld_Event)),
               old=c("EndTime", "Event_Ind"))
    } else if (Grouped_Ind & !HasStarting_Ind) {
      setnames(datGiven, new=c(c(fld_EndTime, fld_ID, fld_Event)),
               old=c("EndTime", "ID", "Event_Ind"))
    }
  }) # ----------------- End of tryCatch-block -----------------
  
  # - Generic cleanup (should parts of this function be run in debug/interactive mode)
  suppressWarnings( rm(datGiven, thresholds, nThresh, Grouped_Ind, HasStarting_Ind, nRows, vOrder, vEventTimes, vEventInds, 
                       vMarkers, vStartTimes,S_t, vMarkers_unique, vEventTimes_Main, vEventTimes_Filtered, vDiff, 
                       Neigh_Mid, Neigh_Mid2, Neigh_UpperB, Neigh_LowerB, Neigh_UpperB_ind, Neigh_LowerB_ind, 
                       weights, S_0, n_values, d_values, vMatched, vMatch2, vSurvProb, S_mean, vTPR, vFPR, 
                       cumulMark, S_tc, vWidth, vMidpoints, sArea, datGraph, datSegment, conc, gg, retObj,
                       datS_t, datROC, cl.port) )
}





# ------Helper functions for enabling a multithreading environment

# - Function for estimating marker-conditional average survival probability S(t,c)
# Used in tROC.multi() within a multi-threaded loop in estimating the survival probability at a given
# prediction time (implicit within vEventTimes_Filtered) and, more importantly, at a given threshold
# for markers (beyond which a positive is predicted, and below which a negative is predicted)
S_t.estimator <- function(vMarkers, vMarkers_unique, vStartTimes, vEventTimes, vEventTimes_Filtered,
                          vEventInds, eventVal, threshold, estMethod, sLambda, nRows, reportFlag=T, 
                          iteration, nThresh, caseStudyName="Main", defChoice="ForLoop", logPath="C:/") {
  # -- Testing conditions
  #  iteration <- 49; threshold <- thresholds[iteration]; reportFlag=T; defChoice="ForLoop"
  
  if (reportFlag) {
    cat(paste0("\n 1)[", iteration, " of ", nThresh, "] Estimating average survivor probability S(t,c) given a marker threshold [c=",
               threshold, "] at and beyownd which a cases' Marker (or risk score) will be predicted as a positive event, and below which",
               " the case will be predicted as a negative event."), 
        file=paste0(logPath, "assesslog_",caseStudyName,".txt"), append=T);
  }
  
  # threshold = thresholds[j]   # testing condition
  # -- Create a distance vector between each marker and the current threshold
  vDiff <- vMarkers - threshold # will always be positive, but unordered
  vDiff <- vDiff[order(vDiff)]  # Sort in ascending order
  
  # --- Differentiate execution by kernel choice
  if (estMethod == "NN-0/1") { # 0/1 Kernel from Akritas1994
    
    # -- Establish a symmetrical neighbourhood around unique marker values that are in
    # close proximity to one another
    
    # - Find an index in the ordered difference vector [sDiff] beyond which point all markers are positively 
    # differenced wrt the current threshold
    Neigh_Mid <- sum(vDiff < 0) +1
    # In catering for cases where the midpoint must be decided between two identical Marker-values,
    # we shall take the next index
    Neigh_Mid2 <- sum(vDiff <= 0) # catering for cases where the midpoint 
    # - Find an index for upper bound of neighbourhood bounded by the index of largest marker
    Neigh_UpperB_ind <- min(Neigh_Mid + trunc(nRows * sLambda + 0.5), nRows)
    # - Find an index for lower bound of neighbourhood bounded by the index of smallest marker
    Neigh_LowerB_ind <- max(Neigh_Mid2 - trunc(nRows * sLambda /2), 1)
    # - Retrieve associated neighbourhood bounds (upper + lower)
    Neigh_UpperB <- vDiff[Neigh_UpperB_ind]
    Neigh_LowerB <- vDiff[Neigh_LowerB_ind]
    
    # - Apply chosen kernel function: 1 if the Marker is within the neighbourhood, 0 otherwise
    # NOTE: thresholds[j] and vMarkers_unique[j] will always be equal
    weights <- ifelse(((vMarkers - vMarkers_unique[iteration]) <= Neigh_UpperB) & 
                        ((vMarkers - vMarkers_unique[iteration]) >= Neigh_LowerB),  1,0)
  } else{ stop("Unknown kernel choice.")}
  
  # -- S(t) can now be estimated by calculating the event and at-risk populations across filtered event times s,
  # as weighted by the kernel function [weights]
  
  # 2 different methods by which S(t) can be calculated; kept for experimentation purposes
  if (defChoice=="ForLoop") {
    S_0 <- 1 # Initialise the starting survival probability to 100%
    for (jj in 1:length(vEventTimes_Filtered)) {
      # jj <- 2   # - At each filtered event time s, calculate the number of at-risk cases within the current neighbourhood
      n_values <- sum(weights * (vStartTimes <= vEventTimes_Filtered[jj]) &
                        (vEventTimes >= vEventTimes_Filtered[jj]),na.rm=T)
      # - At each filtered event time s, calculate the number of cases within the current neighbourhood
      # that experienced the event exactly at the current s
      d_values <- sum(weights * (vStartTimes <= vEventTimes_Filtered[jj]) &
                        (vEventTimes == vEventTimes_Filtered[jj])*(vEventInds==eventVal),na.rm=T)
      
      # - Calculate the well-known KM-based survival factor vector given the current filtered event time s
      if (n_values>0) S_0 <- S_0 * (1 - (d_values / n_values)) else 1
    }    
  } else {
    
    # Calculate the number of at-risk cases for all event times
    n_values <- sapply(vEventTimes_Filtered, function(t) {
      sum(weights * (vStartTimes <= t) & (vEventTimes >= t), na.rm = TRUE)
    })
    
    # Calculate the number of events occurring exactly at each event time
    d_values <- sapply(vEventTimes_Filtered, function(t) {
      sum(weights * (vStartTimes <= t) & 
            (vEventTimes == t) * (vEventInds == eventVal), na.rm = TRUE)
    })
    
    # Compute survival factors for all event times
    survival_factors <- ifelse(n_values > 0, 1 - (d_values / n_values), 1)
    
    # Overall survival probability
    S_0 <- prod(survival_factors)
  }
  
  
  # - Save the pooled survival probability (at last ordered event time) as the main result for the current threshold
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
    cumulMark <- mean(vMarkers <= thresholds[c], na.rm=T)
    
    # - Calculate mean survival probability given current threshold p_c, within the subset of marker values exceeding p_c
    # NOTE: The inner operation amounts to the indicator function in the equation of S_{lambda_n}(p_c,t)
    S_tc <- sum(vSurvProb[vMarkers > thresholds[c]], na.rm=T)/nRows
    
    # - Calculate TPR and FPR according to Heagerty2000
    sTPR <- ((1-cumulMark) - S_tc)/(1 - S_mean) # TPR
    sFPR <- S_tc/S_mean # FPR
    
  } else { # Dependence amongst observations, clustered by a given ID-value
    
    # - Empirical distribution of markers being less than the current threshold p_c
    # First, we obtain the proportion amongst all cases with markers less than or equal to p_c
    # Then, we calculate the grand mean amongst all of these ID-level proportions.
    cumulMark = mean(datGiven[,list(Prop = sum(Marker <= thresholds[c])/.N), by=list(ID)]$Prop, na.rm=T)
    
    #S_lam <- sum(datGiven$Surv_prob[datGiven$Marker > thresholds[c]])/nRows # Sum of survival probabilities for Marker values greater than threshold
    
    # - Calculate mean survival probability given current threshold p_c, within the subset of marker values exceeding p_c
    # NOTE: The inner operation amounts to the indicator function in the equation of S_{lambda_n}(p_c,t)
    # NOTE2: Firstly, we obtain the mean survival probability amongst markers associated with an ID, conditioned on
    # these markers exceeding the threshold.
    #       Secondly, we calculate the grand mean amongst all of these ID-level mean survival probabilities given p_c
    S_tc <- mean(datGiven[,list(S_tc_ID=sum(ifelse(Marker > thresholds[c],Surv_prob,0),na.rm=T)/.N), 
                          by=list(ID)]$S_tc_ID, na.rm=T)
    
    # - Calculate TPR and FPR according to Heagerty2000
    sTPR <- ((1-cumulMark) - S_tc)/(1 - S_mean) # TPR
    sFPR <- S_tc/S_mean # FPR
  }
  
  return(data.table(Threhsold=threshold, TPR=sTPR, FPR=sFPR))
}
