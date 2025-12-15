
if (!exists('weights_check')) unpack.ffdf(paste0(genPath,"MAE_Weights"), tempPath);gc()

weights <- seq(0,20,1)
mae_basic <- c()
mae_adv <- c()

for (i in seq_along(weights)){
  # - Weigh default cases heavier. as determined interactively based on calibration success (script 6e)
  datCredit_train[, Weight := ifelse(DefSpell_Event==1,weights[i],1)]
  datCredit_valid[, Weight := ifelse(DefSpell_Event==1,weights[i],1)] # for merging purposes
  
  # ---  Basic discrete-time hazard model
  # - Initialize variables
  vars_basic <- c("log(TimeInDefSpell):DefSpell_Num_binned",
                  "InterestRate_Nom", "M_Inflation_Growth_6")
  
  # - Fit discrete-time hazard model with selected variables
  modLR_basic <- glm( as.formula(paste("DefSpell_Event ~", paste(vars_basic, collapse = " + "))),
                      data=datCredit_train, family="binomial", weights= Weight)
  
  
  
  # ---  Advanced discrete-time hazard model
  # - Initialize variables
  vars <- c("Time_Binned*DefSpell_Num_binned", "DefaultStatus1_Aggr_Prop_Lag_12","slc_acct_arr_dir_3", 
            "g0_Delinq_Any_Aggr_Prop_Lag_1",
            "BalanceToPrincipal", "InterestRate_Nom","InterestRate_Margin_Aggr_Med_9"
            ,"M_RealIncome_Growth_9", "M_Inflation_Growth","M_DTI_Growth_12","M_Repo_Rate")
  modLR <- glm( as.formula(paste("DefSpell_Event ~", paste(vars, collapse = " + "))),
                data=datCredit_train, family="binomial", weights= Weight)
  
  # - Create pointer to the appropriate data object 
  datCredit <- rbind(datCredit_train, datCredit_valid)
  
  

  km_Default <- survfit(Surv(time=TimeInDefSpell-1, time2=TimeInDefSpell, event=DefSpell_Event==1, type="counting") ~ 1, 
                        id=DefSpell_Key, data=datCredit)
  
  # - Create survival table using surv_summary(), from the subsampled set
  (datSurv_sub <- surv_summary(km_Default)) # Survival table
  datSurv_sub <- datSurv_sub %>% rename(Time=time, AtRisk_n=`n.risk`, Event_n=`n.event`, Censored_n=`n.censor`, SurvivalProb_KM=`surv`) %>%
    mutate(Hazard_Actual = Event_n/AtRisk_n) %>% 
    mutate(CHaz = cumsum(Hazard_Actual)) %>% # Created as a sanity check
    mutate(EventRate = Hazard_Actual*shift(SurvivalProb_KM, n=1, fill=1)) %>%  # probability mass function f(t)=h(t).S(t-1)
    filter(Event_n > 0 | Censored_n >0) %>% as.data.table()
  setorder(datSurv_sub, Time)
  datSurv_sub[,AtRisk_perc := AtRisk_n / max(AtRisk_n, na.rm=T)]
  
  

  
  # - Compute Kaplan-Meier survival estimates (product-limit) for censoring-event | Spell-level with right-censoring & left-truncation
  km_Censoring <- survfit(Surv(time=TimeInDefSpell-1, time2=TimeInDefSpell, event=DefSpell_Censored==1, type="counting") ~ 1, 
                          id=DefSpell_Key, data=datCredit)
  summary(km_Censoring)$table # overall summary statistics
  # plot(km_Censoring, conf.int = T) # survival curve
  
  # - Extract quantities of interest
  datSurv_censoring <- data.table(TimeInDefSpell=summary(km_Censoring)$time,
                                  Survival_censoring = summary(km_Censoring)$surv)
  
  # - Merge Censoring survival probability back to main dataset
  datCredit <- merge(datCredit, datSurv_censoring, by="TimeInDefSpell", all.x=T)
  
  
  
  

  datAdd <- subset(datCredit, Counter == 1 & TimeInDefSpell > 1)
  datAdd[, Start := Start-1]
  datAdd[, TimeInDefSpell := TimeInDefSpell-1]
  datAdd[, Counter := 0]
  datCredit <- rbind(datCredit, datAdd); setorder(datCredit, DefSpell_Key, TimeInDefSpell)
  
  
  # --- Calculate survival quantities of interest
  # Compute IPCW-scheme weight
  datCredit[, Weight := 1/Survival_censoring]
  # Predict hazard h(t) = P(T=t | T>= t) in discrete-time
  datCredit[, Hazard_adv := predict(modLR, newdata=datCredit, type = "response")]
  datCredit[, Hazard_bas := predict(modLR_basic, newdata=datCredit, type = "response")]
  # Derive survival probability S(t) = prod ( 1- hazard)
  datCredit[, Survival_adv := cumprod(1-Hazard_adv), by=list(DefSpell_Key)]
  datCredit[, Survival_bas := cumprod(1-Hazard_bas), by=list(DefSpell_Key)]
  # Derive discrete density, or event probability f(t) = S(t-1) . h(t)
  datCredit[, EventRate_adv := shift(Survival_adv, type="lag", n=1, fill=1) - Survival_adv, by=list(DefSpell_Key)]
  datCredit[, EventRate_bas := shift(Survival_bas, type="lag", n=1, fill=1) - Survival_bas, by=list(DefSpell_Key)]
  
  # - Remove added rows
  datCredit <- subset(datCredit, Counter > 0)
  
  
  # --- Period-level aggregation
  
  # - Aggregate to period-level towards plotting key survival quantities
  datSurv_exp <- datCredit[,.(EventRate_bas = mean(EventRate_bas, na.rm=T),
                              EventRate_adv = mean(EventRate_adv, na.rm=T), EventRate_Emp = sum(DefSpell_Event)/.N,
                              EventRate_IPCW = sum(Weight*DefSpell_Event)/sum(Weight)),
                           by=list(TimeInDefSpell)]
  setorder(datSurv_exp, TimeInDefSpell)
  datSurv_exp[, Survival_bas := 1 - cumsum(coalesce(EventRate_bas,0))]
  datSurv_exp[, Survival_adv := 1 - cumsum(coalesce(EventRate_adv,0))]
  datSurv_exp[, Survival_IPCW := 1 - cumsum(coalesce(EventRate_IPCW,0))]
  
  

  
  
  
  
  # ------ 4. Graphing the event density / probability mass function f(t)
  
  # - General parameters
  sMaxSpellAge <- 120 # max for [DefSpell_Age], as determined in earlier analyses (script 3c
  sMaxSpellAge_graph <- 120 # max for [DefSpell_Age] for graphing purposes
  

  
  
  # - Fitting natural cubic regression splines
  sDf_Act <- 12; sDf_Exp <- 12
  smthEventRate_Act <- lm(EventRate ~ ns(Time, df=sDf_Act), data=datSurv_sub[Time <= sMaxSpellAge,])
  smthEventRate_Exp_bas <- lm(EventRate_bas ~ ns(TimeInDefSpell, df=sDf_Exp), data=datSurv_exp[TimeInDefSpell <= sMaxSpellAge])
  smthEventRate_Exp_adv <- lm(EventRate_adv ~ ns(TimeInDefSpell, df=sDf_Exp), data=datSurv_exp[TimeInDefSpell <= sMaxSpellAge])
  summary(smthEventRate_Act); summary(smthEventRate_Exp_bas); summary(smthEventRate_Exp_adv)
  
  # - Render predictions based on fitted smoother, with standard errors for confidence intervals
  vPredSmth_Act <- predict(smthEventRate_Act, newdata=datSurv_sub, se=T)
  vPredSmth_Exp_bas <- predict(smthEventRate_Exp_bas, newdata=datSurv_exp, se=T)
  vPredSmth_Exp_adv <- predict(smthEventRate_Exp_adv, newdata=datSurv_exp, se=T)
  
  # - Add smoothed estimate to graphing object
  datSurv_sub[, EventRate_spline := vPredSmth_Act$fit]
  datSurv_exp[, EventRate_spline_bas := vPredSmth_Exp_bas$fit]
  datSurv_exp[, EventRate_spline_adv := vPredSmth_Exp_adv$fit]
  
  # - Create graphing data object
  datGraph <- rbind(datSurv_sub[,list(Time, EventRate, Type="a_Actual")],
                    datSurv_sub[,list(Time, EventRate=EventRate_spline, Type="b_Actual_spline")],
                    datSurv_exp[, list(Time=TimeInDefSpell, EventRate=EventRate_bas, Type="c_Expected_bas")],
                    datSurv_exp[, list(Time=TimeInDefSpell, EventRate=EventRate_spline_bas, Type="d_Expected_spline_bas")],
                    datSurv_exp[, list(Time=TimeInDefSpell, EventRate=EventRate_adv, Type="e_Expected_adv")],
                    datSurv_exp[, list(Time=TimeInDefSpell, EventRate=EventRate_spline_adv, Type="f_Expected_spline_adv")]
  )
  
  # - Create different groupings for graphing purposes
  datGraph[Type %in% c("a_Actual","c_Expected_bas", "e_Expected_adv"), EventRatePoint := EventRate ]
  datGraph[Type %in% c("b_Actual_spline","d_Expected_spline_bas", "f_Expected_spline_adv"), EventRateLine := EventRate ]
  datGraph[, FacetLabel := "Prentice-Williams-Peterson (PWP) spell-time"]
  
  # - Aesthetic engineering
  chosenFont <- "Cambria"
  mainEventName <- "Default"
  
  # - Calculate MAE between event rates
  datFusion <- merge(datSurv_sub[Time <= sMaxSpellAge], 
                     datSurv_exp[TimeInDefSpell <= sMaxSpellAge,list(Time=TimeInDefSpell, EventRate_bas, EventRate_adv)], by="Time")
  mae_basic [i]<- mean(abs(datFusion$EventRate - datFusion$EventRate_bas), na.rm=T)
  mae_adv[i] <- mean(abs(datFusion$EventRate - datFusion$EventRate_adv), na.rm=T)
}

weights_check <- data.frame(weights,mae_basic, mae_adv)

weights_check <- weights_check %>%
  mutate(mae_basic = mae_basic * 100,
         mae_adv = mae_adv * 100)
weights_long <- pivot_longer(weights_check,
                             cols = c(mae_basic, mae_adv),
                             names_to = "Model",
                             values_to = "MAE")

plot<- (ggplot(weights_long, aes(x = weights, y = MAE, color = Model)) +
  geom_line(size = 1) +
  labs( x = "Weights",
       y = "Mean Absolute Error (%)",
       color = "Model Type") +
  theme_minimal())
ggsave(plot, file=paste0(genFigPath,"MAE_Check.png"),
       width=1200/dpi, height=1000/dpi,dpi=dpi, bg="white")
pack.ffdf(paste0(genPath, "MAE_Weights"), weights_check)
