# ============================== SURVIVAL FUNCTIONS ==============================
# Defining bespoke time-dependent Brier score function, capable of working with
# time-to-event left-truncated data in the counting process style
# --------------------------------------------------------------------------------
# PROJECT TITLE: Default Survival Modelling
# SCRIPT AUTHOR(S): Dr Arno Botha
# VERSION: 1.0 (Jul-2025)
# ================================================================================



# --- Function to calculate time-dependent Brier Score (tBS) for a given object's predictions
# Crafted using Graff1999 (DOI: 10.1002/(sici)1097-0258(19990915/30)18:17/18<2529::aid-sim274>3.0.co;2-5)
# Input:    [datGiven]: given loan history; [modGiven]: fitted cox PH or discrete-time hazard model;
#           [predType]: option to pass to predict(); [spellPeriodMax]: maximum period to impose upon spell ages
#           <fldNames>: Various field names for quantities of interest
# Output:   Vector of tBS-values; Integrated Brier Score (IBS)
tBrierScore <- function(datGiven, modGiven, predType="response", spellPeriodMax=300,
                        fldKey="DefSpell_Key", fldStart = "Start", fldStop="TimeInDefSpell",
                        fldEvent="DefSpell_Event", fldCensored="DefSpell_Censored", fldSpellAge="DefSpell_Age",
                        fldSpellOutcome="DefSpellResol_Type_Hist") {
  # Testing conditions
  # datGiven <- datCredit; modGiven <- cox_PWPST_basic ; predType <- "exp"; spellPeriodMax <- 300
  # fldKey <- "PerfSpell_Key"; fldStart <- "Start"; fldStop<-"TimeInPerfSpell";
  # fldCensored<-"PerfSpell_Censored"; fldSpellAge<-"PerfSpell_Age"; fldSpellOutcome<-"PerfSpellResol_Type_Hist"
  # fldEvent="PerfSpell_Event"
  
  
  # --- Estimate survival rate of the censoring event G(t) = P(C >= t) for time-to-censoring variable C
  # This prepares for implementing the Inverse Probability of Censoring Weighting (IPCW) scheme,
  # as an adjustment to event rates (discrete density), as well as for the time-dependent Brier score
  
  # - Compute Kaplan-Meier survival estimates (product-limit) for censoring-event | Spell-level with right-censoring & left-truncation
  km_Censoring <- survfit(
    as.formula(paste0("Surv(time=",fldStart,", time2=", fldStop, ", event=", fldCensored, "==1, type='counting') ~ 1")), 
    id=get(fldKey), data=datGiven)
  # summary(km_Censoring)$table # overall summary statistics
  # plot(km_Censoring, conf.int = T) # survival curve
  datG <- data.table(summary(km_Censoring)$time, summary(km_Censoring)$surv)
  setnames(datG, c(fldStop, "G_t"))
  datG_ti <- data.table(EventTime = summary(km_Censoring)$time, G_Ti = summary(km_Censoring)$surv)
  
  
  # --- Estimate survival rate of the main event S(t) = P(T >= t) for time-to-event variable T
  # This serves as a baseline model towards calculating the pseudo R^2-measure
  
  # - Compute Kaplan-Meier survival estimates (product-limit) for censoring-event | Spell-level with right-censoring & left-truncation
  km_main <- survfit(
    as.formula(paste0("Surv(time=",fldStart,", time2=", fldStop, ", event=", fldEvent, "==1, type='counting') ~ 1")), 
    id=get(fldKey), data=datGiven)
  # summary(km_main)$table # overall summary statistics
  # plot(km_main, conf.int = T) # survival curve
  datT <- data.table(summary(km_main)$time, summary(km_main)$surv)
  setnames(datT, c(fldStop, "Survival_0"))
  
  
  # --- Calculate survival quantities of interest
  # Predict hazard h(t) = P(T=t | T>= t) in discrete-time
  datGiven[, Hazard := predict(modGiven, newdata=datGiven, type = predType)]
  if (predType=="response") {
    # Derive survival probability S(t) = \prod ( 1- hazard), based on output of predict()
    datGiven[, Survival := cumprod(1-Hazard), by=list(get(fldKey))] 
  } else if (predType=="exp") {
    # Calculate survival probability S(t,x)=exp(-H(t,x)), based on output of predict()
    # NOTE: Hazard is actually the cumulative hazard in this context
    datGiven[, Survival := exp(-Hazard), by=list(get(fldKey))] 
  }
  # - Merge censoring survival probability unto main set
  datGiven <- merge(datGiven, datG, by=fldStop, all.x=T)
  datGiven[is.na(G_t), G_t := 1] # Fill any missing G_t with 1 (no censoring information)
  # - Merge event survival probability unto main set
  datGiven <- merge(datGiven, datT, by=fldStop, all.x=T)
  datGiven[is.na(Survival_0), Survival_0 := 1] # Fill any missing G_t with 1 (no censoring information)
  
  
  # --- Compute Censoring survival probability at age T_i for each subject (i,j), i..e, G_hat(T_i)
  # This requires mapping each subject's observed event time (Spell age) to g_hat(T_i)
  datGiven <- merge(datGiven, datG_ti, by.x=fldSpellAge, by.y="EventTime", all.x=T)
  datGiven[is.na(G_Ti), G_Ti := 1]
  
  
  # --- Compute constituent quantities of time-dependent Brier score (tBS)
  # - Compute binary event indicator at time t: y(t) = I(T>t)
  datGiven[, y_t := as.integer(get(fldSpellAge) > get(fldStop))]
  # - Compute weight/contribution to the Brier score at each time point, according to the 3 categories of Graff1999
  datGiven[, Weight_w := fifelse( 
    # Category 1: T_i <= t^* & \delta_i =1; should get weight 1/G(T_i) since subject experienced the event
    get(fldSpellAge) <= get(fldStop) & get(fldSpellOutcome)=="Defaulted", 1/G_Ti,
    # Category 2: T_i > t^*; should get weight 1/G(t) since subject has survived  (still at-risk)
    fifelse( get(fldSpellAge) > get(fldStop), 1/G_t, 
             # Category 3: T_i <= t^* & \delta_i=0; should get weight 0 for censored subject
             0)), by=list(get(fldKey))]  
  
  # -- Compute squared error loss per row ---
  datGiven[, SquaredError := (y_t - Survival)^2]
  datGiven[, SquaredError_0 := (y_t - Survival_0)^2]
  
  
  # --- Aggregate and compute weighted Brier score per time point
  datTBS <- datGiven[Weight_w > 0 & get(fldStop)<=spellPeriodMax, .(
    mean(Weight_w * SquaredError, na.rm = TRUE),
    mean(Weight_w * SquaredError_0, na.rm = TRUE)
  ), by=list(get(fldStop))]
  setnames(datTBS, c(fldStop, "Brier", "Brier_0"))
  
  # --- Calculate pseudo R^2
  datTBS[, RSquared := 1 - Brier/Brier_0]
  #plot(datTBS$RSquared)
  
  # --- Calculate Integrated Brier Score assuming uniform weights
  IBS <- mean(datTBS$Brier, na.rm=T)
  
  return(list(tBS=datTBS, IBS=IBS))
  
  # - Cleanup (useful during interactive debugging of function)
  rm(datGiven, modGiven, fldKey, fldStart, fldStop, fldCensored, fldSpellAge, predType,
     km_Censoring, datTBS,datG, datG_ti)
}