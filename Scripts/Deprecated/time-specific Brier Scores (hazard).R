

tBrierScore_hazard <- function(datGiven, spellPeriodMax=84,
                        fldKey="DefSpell_Key", fldStop="TimeInDefSpell", fldCounter="Counter",
                        fldPrediction="Hazard_bas", fldEvent="DefSpell_Event", fldBaseline=NULL) {
  # Testing conditions
  # fldPrediction<-"Hazard_bas"; fldKey<-"DefSpell_Key"; fldStop<-"TimeInDefSpell"; fldCounter<-"Counter"
  # spellPeriodMax<-84; fldEvent<-"DefSpell_Event"; fldBaseline <- NULL; datGiven <- datCredit
  
  # --- 1. Prepare person-period dataset and retain only necessary fields as memory optimisation
  dat <- copy(subset(datGiven, select=c(fldKey, fldCounter, fldStop, fldEvent, fldPrediction,fldBaseline)))
  
  # Remove artificial observations ordinarily inserted for handling left-truncation prior to first spell period > 1
  dat <- dat[get(fldCounter) > 0 & get(fldStop) <= spellPeriodMax]
  
  # Check that only one record exists per spell-period
  stopifnot(!anyDuplicated(dat, by=c(fldKey, fldStop)),
    all(!is.na(dat[[fldEvent]])),
    all(dat[[fldEvent]] %in% c(0, 1)),
    all(is.finite(dat[[fldPrediction]])),
    all(dat[[fldPrediction]] >= 0 &
          dat[[fldPrediction]] <= 1))
  
  # --- 2. Retrieve observed event indicator and predicted hazards
  dat[, Event := get(fldEvent)]
  dat[, Hazard := get(fldPrediction)]
  
  # --- 3. Compute squared prediction errors and aggregate them by time
  dat[, SquaredError := (Event - Hazard)^2]
  # - Compute time-specific Brier Scores
  datTBS <- dat[, .(Brier = mean(SquaredError), N = .N,
    Events = sum(Event), Hazard_Obs = mean(Event), Hazard_Exp = mean(Hazard)
  ), by=.(Time = get(fldStop))]
  
  
  # --- 5. Calculate baseline Brier score
  if (is.null(fldBaseline)) {
    
    # Empirical period-specific write-off hazard
    datTBS[, Brier_0 := Hazard_Obs * (1 - Hazard_Obs)]
    
  } else {
    
    stopifnot(all(is.finite(dat[[fldBaseline]])),
      all(dat[[fldBaseline]] >= 0 & dat[[fldBaseline]] <= 1) )
    
    dat[, SquaredError_0 := (Event - get(fldBaseline))^2]
    
    datBaseline <- dat[, .(Brier_0 = mean(SquaredError_0)), by=.(Time = get(fldStop))]
    
    datTBS <- merge( datTBS, datBaseline, by="Time", all.x=TRUE, sort=FALSE)
  }
  
  # --- 6. Order and validate evaluation periods
  setorder(datTBS, Time)
  
  evalTimes <- seq_len(spellPeriodMax)
  if (!identical(datTBS$Time, evalTimes)) {
    warning("Some evaluation periods have no eligible records.")
  }
  
  # --- 7. Calculate integrated Brier scores
  # Uniform weighting over available evaluation periods
  IBS <- mean(datTBS$Brier)
  IBS_0 <- mean(datTBS$Brier_0)
  
  # Skill score relative to baseline
  IBS_Skill <- if (IBS_0 > 0) {
    1 - IBS / IBS_0
  } else NA_real_
  
  return(list(
    tBS = datTBS,
    IBS = IBS,
    IBS_0 = IBS_0,
    IBS_Skill = IBS_Skill
  ))
  
  rm(datGiven, fldKey, fldStop, fldCensored, fldPrediction, datTBS, fldCounter, fldBaseline)
}





objCoxDisc_bas <- tBrierScore_hazard(
  datCredit,
  spellPeriodMax=84,
  fldKey="DefSpell_Key",
  fldStop="TimeInDefSpell",
  fldCounter="Counter",
  fldEvent="DefSpell_Event",
  fldPrediction="Hazard_bas"
)

objCoxDisc_adv <- tBrierScore_hazard(
  datCredit,
  spellPeriodMax=84,
  fldKey="DefSpell_Key",
  fldStop="TimeInDefSpell",
  fldCounter="Counter",
  fldEvent="DefSpell_Event",
  fldPrediction="Hazard_adv"
)

plot(objCoxDisc_bas$tBS$Brier, type="b")
lines(objCoxDisc_adv$tBS$Brier, type="b", col="blue")
