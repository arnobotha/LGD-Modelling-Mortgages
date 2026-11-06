# ============================== SURVIVAL FUNCTIONS ==============================
# Defining bespoke time-specific Brier score function, capable of working with
# time-to-event left-truncated data in the counting process style
# --------------------------------------------------------------------------------
# PROJECT TITLE: Loss Modelling (LGD) for Residential Mortgages
# SCRIPT AUTHOR(S): Dr Arno Botha
# VERSION: 2.0 (Nov-2026)
# ================================================================================



# --- Function to calculate time-dependent Brier Score (tBS) for a given object's predictions
# Crafted using Graff1999 (DOI: 10.1002/(sici)1097-0258(19990915/30)18:17/18<2529::aid-sim274>3.0.co;2-5)
# Input:    [datGiven]: given loan history
#           [spellPeriodMax]: maximum period to impose upon spell ages
#           <fldNames>: Various field names for quantities of interest
#           [threshold]: Threshold against which to dichotomise the model predictions
#           [empiricalType]: Specify which estimator to use to calculate the overall survival probability:
#                         1) "KM": Kaplan-Meier for single-risks survival model
#                         2) "AJ": Aalen-Johansen for competing risks survival model

# Output:   Vector of tBS-values; Integrated Brier Score (IBS)
tBrierScore <- function(datGiven, spellPeriodMax=300, fldKey="DefSpell_Key", fldStart = "Start", 
                        fldStop="TimeInDefSpell", fldCounter="Counter", fldPrediction="Survival_bas",
                        fldEvent="DefSpell_Event", fldCensored="DefSpell_Censored", fldSpellAge="DefSpell_Age",
                        fldSpellOutcome="DefSpellResol_Type_Hist", fldSpellOutcomeVal="WOFF",
                        empiricalType="KM") {
  # Testing conditions
  # datGiven <- datCredit; spellPeriodMax <- 84; empiricalType="KM"; fldSpellOutcomeVal<- "WOFF"
  # fldKey <- "DefSpell_Key"; fldStart <- "Start"; fldStop<-"TimeInDefSpell"; fldCounter<-"Counter"
  # fldCensored<-"DefSpell_Censored"; fldSpellAge<-"DefSpell_Age"; fldSpellOutcome<-"DefSpellResol_Type_Hist"
  # fldEvent="DefSpell_Event"; brierType<-"Survival"; fldPrediction<-"Survival_bas";
  
  # --- Prepare dataset
  # Create copy of given dataset and select only relevant fields
  dat <- copy(subset(datGiven, select=c(fldKey, fldStart, fldStop, fldCounter, fldPrediction,
                                        fldEvent, fldCensored, fldSpellAge, fldSpellOutcome)))
  
  # Remove artificial observations, if not genuine risk intervals
  dat <- dat[get(fldCounter) > 0]
  
  # Verify uniqueness of spell-period observations
  stopifnot(!anyDuplicated(dat, by=c(fldKey, fldStop)),
    all(is.finite(dat[[fldPrediction]])),
    all(dat[[fldPrediction]] >= 0 & dat[[fldPrediction]] <= 1) )
  
  # Evaluation times
  datTimes <- data.table(Time=sort(unique(dat[get(fldStop) <= spellPeriodMax, get(fldStop)])))
  
  # --- Estimate survival rate of the censoring event G(t) = P(C >= t) for time-to-censoring variable C
  # This prepares for implementing the Inverse Probability of Censoring Weighting (IPCW) scheme,
  # as an adjustment to event rates (discrete density), as well as for the time-dependent Brier score
  
  # - Compute Kaplan-Meier survival estimates (product-limit) for censoring-event | Spell-level with right-censoring & left-truncation
  cat(paste0("\nFitting Kaplan-Meier estimator towards deriving the empirical survival probability of",
              "the censoring event for IPC-weighting.. "))
  formCens <- as.formula(sprintf("Surv(%s, %s, %s == 1, type='counting') ~ 1",fldStart, fldStop, fldCensored))
  km_Censoring <- survfit(formCens, id=get(fldKey), data=dat)
  # summary(km_Censoring)$table # overall summary statistics
  # plot(km_Censoring, conf.int = T) # survival curve
  
  # - Include all required evaluation and terminal ages
  evalTime <- sort(unique(c(0, dat[[fldStop]],dat[[fldSpellAge]])))
  
  # - Right-continuous and left-limit indices for handling tied events
  # Get positions of most recent KM times less than or equal to evaluation time
  idxR <- findInterval(evalTime, km_Censoring$time)
  # Get positions of most recent KM times strictly less than evaluation time
  idxL <- findInterval(evalTime, km_Censoring$time, left.open=TRUE)
  
  # Consolidate censoring survival probabilities
  datG <- data.table(Time = evalTime,
    G_t = c(1, km_Censoring$surv)[idxR + 1L],
    G_Ti_left = c(1, km_Censoring$surv)[idxL + 1L])
  
  # Merge quqntities with main dataset
  dat <- merge(dat, datG[, .(Time, G_t)], by.x=fldStop, by.y="Time",
    all.x=TRUE, sort=FALSE)
  dat <- merge(dat, datG[, .(Time, G_Ti_left)], by.x=fldSpellAge, by.y="Time",
                    all.x=TRUE, sort=FALSE)
  
  
  # --- Estimate survival rate of the main event S(t) = P(T >= t) for time-to-event variable T
  # This serves as a baseline model towards calculating the pseudo R^2-measure
  
  if (empiricalType=="KM") {
    cat("\nFitting Kaplan-Meier estimator towards deriving the empirical survival probability .. ")
    
    # - Compute Kaplan-Meier survival estimates (product-limit) for main-event | Spell-level with right-censoring & left-truncation
    formKM <- as.formula(sprintf("Surv(%s, %s, %s == 1, type='counting') ~ 1",fldStart, fldStop, fldEvent))
    km_main <- survfit(formKM, id=get(fldKey), data=dat)

    # - Extract survival probabilities and collate into central object over all times
    datT <- data.table(Time = c(0, km_main$time), T_t = c(1, km_main$surv))
    
  } else if (empiricalType=="AJ") {
    cat(paste0("\nFitting Aalen-Johansen estimator towards deriving the overall event-free empirical",
                "survival probability amidst competing risks .. "))
    
    # - Fit Aalen-Johansen estimator for S(t)amidst competing risks | Spell-level with right-censoring & left-truncation
    formAJ <- as.formula(sprintf("Surv(%s, %s, %s, type='mstate') ~ 1", fldStart, fldStop, fldEvent))
    aj_All <- survfit(formAJ, id=get(fldKey), data=dat)
    
    # - Extract survival probabilities and collate into central object over all times
    datT <- data.table(Time = c(0, aj_All$time), T_t = c(1, aj_All$pstate[, "(s0)"]))

  } else stop("Uknown estimator specified for estimating the overall empirical survival probability")
  
  
  # --- Map empirical survival onto evaluation periods
  # Include zero to initialise the survival step function
  datT_eval <- merge( data.table(Time=sort(unique(c(0, datTimes$Time)))), 
                      datT, by="Time", all.x=TRUE, sort=TRUE )
  
  datT_eval[, T_t := nafill(T_t, type="locf")] # fill NAs with last non-NA value in vector
  setnames(datT_eval, "T_t", "Survival_0")
  
  # - Merge main event / event-free survival probabilities unto main set
  dat <- merge(dat, datT_eval, by.x=fldStop, by.y="Time", all.x=TRUE,sort=FALSE)
  
  
  # --- Retrieve predictions from given field
  dat[, Survival := get(fldPrediction)]

  
  # --- Compute constituent quantities of time-specific Brier score (tBS)
  # - Compute binary event indicator at time t: y(t) = I(T>t)
  dat[, y_t := as.integer(get(fldSpellAge) > get(fldStop))]
  
  # - Terminal event Boolean indicator for the chosen target
  dat[, EventTerminal := (get(fldSpellOutcome) == fldSpellOutcomeVal)]  
  
  # - Compute weight/contribution to the Brier score at each time point,
  # according to the 3 categories of Graff1999:
  # 1. Observed event at or before t
  # 2. 
  dat[, Weight_w := fifelse( 
    # Category 1: T_i <= t^* & \delta_i =1; should get weight 1/G(T_i) since subject experienced the event
    get(fldSpellAge) <= get(fldStop) & EventTerminal, 1/G_Ti_left,
    # Category 2: T_i > t^*; should get weight 1/G(t) since subject has survived  (still at-risk)
    fifelse( get(fldSpellAge) > get(fldStop), 1/G_t, 
             # Category 3: T_i <= t^* & \delta_i=0; should get weight 0 for censored subject
             0))]  
  
  # -- Compute squared error loss per spell period
  dat[, SquaredError := (y_t - Survival)^2]
  dat[, SquaredError_0 := (y_t - Survival_0)^2]
  
  
  # --- Restrict scoring to requested evaluation horizon
  datScore <- dat[get(fldStop) <= spellPeriodMax]

  # Check IPCW positivity for contributing records
  if (any(!is.finite(datScore$Weight_w))) {
    stop("Non-finite IPCW weights: check G(t) positivity.")
  }
  
  if (anyNA(datScore$Survival_0)) {
    stop("Missing empirical baseline survival probabilities.")
  }
  
  # --- Aggregate and compute weighted Brier score per time point
  datTBS <- datScore[, .(Brier = mean(Weight_w * SquaredError),
    Brier_0 = mean(Weight_w * SquaredError_0),
    N= .N, N_Events = sum(EventTerminal & get(fldSpellAge) == get(fldStop)),
    Weight_Mean = mean(Weight_w)
  ), by=.(get(fldStop))]
  setnames(datTBS, "get", "Time") # change field names ("get)" to given stop-time field
  setorder(datTBS, Time)
  
  # --- Calculate Integrated Brier Score assuming uniform weights
  
  # - Check for completeness
  stopifnot( identical(datTBS$Time, seq_len(spellPeriodMax)), all(is.finite(datTBS$Brier)) )
  
  # - Calculate IBS assuming equal/uniform weights
  IBS <- mean(datTBS$Brier, na.rm=T) # Emanating from fitted survival model
  IBS_0 <- mean(datTBS$Brier_0) # Emanating from empirical baseline survival model (KM or AJ)
  
  # Calculate optional relative performance measure
  IBS_Skill <- if (IBS_0 > 0) {
    1 - IBS / IBS_0
  } else {
    NA_real_
  }
  
  return(list(tBS=datTBS, IBS=IBS, IBS_0=IBS_0, IBS_Skill=IBS_Skill))
  
  # - Cleanup (useful during interactive debugging of function)
  rm(datGiven, dat, fldKey, fldStart, fldStop, fldCensored, fldSpellAge, fldPrediction,
     km_Censoring, datTBS, datG, datG_ti, km_main, aj_All, empiricalType, fldCounter,
     fldSpellOutcomeVal, datTimes, IBS, IBS_0, IBS_Skill)
}
