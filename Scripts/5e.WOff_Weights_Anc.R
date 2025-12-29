# ================== DETERMINING WEIGHTS FOR WRITE-OFFS ========================
# Determining optimal weights to assign to write-off cases to enhance model 
# performance
# ------------------------------------------------------------------------------
# PROJECT TITLE: Loss Modelling (LGD) for Residential Mortgages
# SCRIPT AUTHOR(S): Mohammed Gabru (MG), Marcel Muller (MM)
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
#   - 4a(i).InputSpace_DiscreteCox.R
#   - 4a(ii).InputSpace_DiscreteCox_Basic.R

# -- Inputs:
#   - datCredit_train_CDH | Prepared from script 2g
#   - datCredit_valid_CDH | Prepared from script 2g
# -- Outputs:
#   - <Analytics> | Graphs
# ------------------------------------------------------------------------------


# ------ 1. Preliminaries
# --- 1.1 Load and prepare data for discrete time hazard models
# if (!exists('weights_check')) unpack.ffdf(paste0(genPath,"MAE_Weights"), tempPath);gc()
# - Confirm prepared datasets are loaded into memory
if (!exists('datCredit_train_CDH')) unpack.ffdf(paste0(genPath,"creditdata_train_CDH"), tempPath);gc()
if (!exists('datCredit_valid_CDH')) unpack.ffdf(paste0(genPath,"creditdata_valid_CDH"), tempPath);gc()

# - Use only default spells
datCredit_train <- datCredit_train_CDH[!is.na(DefSpell_Key),]
datCredit_valid <- datCredit_valid_CDH[!is.na(DefSpell_Key),]


# --- 1.2 Load and prepare data for classical logit model
# - Use only default spells and first default observations
datCredit_train_classic <- datCredit_train_CDH[!is.na(DefSpell_Key) & DefSpell_Counter==1,]
datCredit_valid_classic <- datCredit_valid_CDH[!is.na(DefSpell_Key) & DefSpell_Counter==1,]

# - Set [DefaultSpell_Age]=[TimeInDefSpell] to enable the application of the classical model within the full datasets
datCredit_train[,DefSpell_Age:=TimeInDefSpell]
datCredit_valid[,DefSpell_Age:=TimeInDefSpell]

# - Remove previous objects from memory
rm(datCredit_train_CDH, datCredit_valid_CDH); gc()


# --- 1.3 Load models
# Basic discrete-time hazard model
modLR_bas <- readRDS(paste0(genObjPath,"CoxDisc_Basic_Model.rds"))
# Advanced discrete-time hazard model
modLR_adv <- readRDS(paste0(genObjPath,"CoxDisc_Advanced_Model.rds"))
# Classic logit model
modLR_classic <- readRDS(paste0(genObjPath,"LR_Model.rds"))


# --- 1.2 Initialise parameters
# - Looping conditions
weights <- seq(0,20,1)

# - Get input spaces
# Basic model
form_bas <- formula(modLR_bas)
# Advanced model
form_adv <- formula(modLR_adv)
# Classical model
form_classic <- formula(modLR_classic)

# - Other variables
mae_bas <- rep(NA,length(weights)); names(mae_bas) <- weights
mae_adv <- rep(NA,length(weights)); names(mae_adv) <- weights
mae_classic <- rep(NA,length(weights)); names(mae_classic) <- weights

# - Spell age cut-offs
# Analyses cut-off
sMaxSpellAge <- 120
# Graphing cut-off
sMaxSpellAge_graph <- 120

# - Number of knots for splines
# Splines for acutals
sDf_Act <- 12
# Splines for expecteds
sDf_Exp <- 12

# - Graphing parameters
chosenFont <- "Cambria"
mainEventName <- "WOff"


# --- 1.3 Estimate actual term-structure
# - Fit a Kaplan-Meier estimator
km_WOff <- survfit(Surv(time=TimeInDefSpell-1, time2=TimeInDefSpell, event=DefSpell_Event==1, type="counting") ~ 1, 
                   id=DefSpell_Key, data=datCredit_valid)

# - Create survival table using surv_summary(), from the subsampled set
(datSurv_act <- surv_summary(km_WOff)) # Survival table
datSurv_act <- datSurv_act %>% rename(Time=time, AtRisk_n=`n.risk`, Event_n=`n.event`, Censored_n=`n.censor`, SurvivalProb_KM=`surv`) %>%
  # Estimate hazard
  mutate(Hazard_Actual = Event_n/AtRisk_n) %>% 
  # Estimate cummulative hazard
  mutate(CHaz = cumsum(Hazard_Actual)) %>%
  # Estimate event rate as f(t)=h(t).S(t-1)
  mutate(EventRate = Hazard_Actual*shift(SurvivalProb_KM, n=1, fill=1)) %>%
  # Filter for observations with either censosred or write-off cases present
  filter(Event_n > 0 | Censored_n >0) %>% as.data.table()

# - Set dataset order
setorder(datSurv_act, Time)

# - Calculate the at-risk population portion at each time point
datSurv_act[,AtRisk_perc:=AtRisk_n/max(AtRisk_n, na.rm=T)]




# ------ 2. Determine the optimal weighting scheme via a grid search

for (weight_i in weights){
  # weight_i <- weights[1]
  
  # --- 2.1 Set weights of write-off cases
  # - Training set
  datCredit_train[, Weight:=ifelse(DefSpell_Event==1,weight_i,1)]
  datCredit_train_classic[, Weight:=ifelse(DefSpell_Event==1,weight_i,1)]
  
  # - Validation set
  datCredit_valid[, Weight:=ifelse(DefSpell_Event==1,weight_i,1)]
  datCredit_valid_classic[, Weight:=ifelse(DefSpell_Event==1,weight_i,1)]
  
  
  # --- 2.2 Fit models
  # - Basic model
  modLR_bas <- glm(form_bas, data=datCredit_train, family="binomial", weights=Weight)
  
  # - Advanced model
  modLR_adv <- glm(form_adv, data=datCredit_train, family="binomial", weights=Weight)
  
  # - Classical model
  modLR_classic <- glm(form_classic, data=datCredit_train_classic, family="binomial")


  # --- 2.3 Add an observation for left-truncated spells to enable sensible application of models
  # - Create additional record
  datAdd <- subset(datCredit_valid, Counter == 1 & TimeInDefSpell > 1)
  datAdd[, Start:=Start-1]
  datAdd[, TimeInDefSpell:=TimeInDefSpell-1]
  datAdd[, Counter:=0]
  
  # - Add additional record to credit data
  datCredit_valid <- rbind(datCredit_valid, datAdd)
  
  # - Set order of credit data
  setorder(datCredit_valid, DefSpell_Key, TimeInDefSpell)
  
  
  # --- 2.4 Calculate survival quantities of interest
  # - Predict hazard h(t) = P(T=t | T>= t) in discrete-time
  datCredit_valid[, Hazard_bas:=predict(modLR_bas, newdata=datCredit_valid, type = "response")]
  datCredit_valid[, Hazard_adv:=predict(modLR_adv, newdata=datCredit_valid, type = "response")]
  datCredit_valid[, Hazard_classic:=predict(modLR_classic, newdata=datCredit_valid, type = "response")]
  
  # - Derive survival probability S(t) = prod ( 1- hazard)
  datCredit_valid[, Survival_bas:=cumprod(1-Hazard_bas), by=list(DefSpell_Key)]
  datCredit_valid[, Survival_adv:=cumprod(1-Hazard_adv), by=list(DefSpell_Key)]
  datCredit_valid[, Survival_classic:=cumprod(1-Hazard_classic), by=list(DefSpell_Key)]
  
  # - Derive discrete density, or event probability f(t) = S(t-1) . h(t)
  datCredit_valid[, EventRate_bas:=shift(Survival_bas, type="lag", n=1, fill=1)-Survival_bas, by=list(DefSpell_Key)]
  datCredit_valid[, EventRate_adv:=shift(Survival_adv, type="lag", n=1, fill=1)-Survival_adv, by=list(DefSpell_Key)]
  datCredit_valid[, EventRate_classic:=shift(Survival_classic, type="lag", n=1, fill=1)-Survival_classic, by=list(DefSpell_Key)]
  
  # - Remove added rows
  datCredit_valid <- subset(datCredit_valid, Counter>0)
  
  
  # --- 2.5 Period-level aggregation
  # - Aggregate to period-level towards plotting key survival quantities
  datSurv_exp <- datCredit_valid[,.(EventRate_bas = mean(EventRate_bas, na.rm=T),
                                    EventRate_adv = mean(EventRate_adv, na.rm=T),
                                    EventRate_classic = mean(EventRate_classic, na.rm=T),
                                    EventRate_Emp = sum(DefSpell_Event)/.N), ### MM: This is not how we calculate the empirical event rate; this is the hazard rate
                                 by=list(TimeInDefSpell)]
  
  # - Set dataset order
  setorder(datSurv_exp, TimeInDefSpell)
  
  # - Calculate 
  datSurv_exp[, Survival_bas := 1 - cumsum(coalesce(EventRate_bas,0))]
  datSurv_exp[, Survival_adv := 1 - cumsum(coalesce(EventRate_adv,0))]
  
  
  
  
  # ------ 4. Comparing actual and expected event density / probability mass function f(t)
  
  # --- 4.1 Fitting natural cubic regression splines
  # - Fit splines to event rates
  smthEventRate_Act <- lm(EventRate ~ ns(Time, df=sDf_Act), data=datSurv_act[Time <= sMaxSpellAge,])
  smthEventRate_Exp_bas <- lm(EventRate_bas ~ ns(TimeInDefSpell, df=sDf_Exp), data=datSurv_exp[TimeInDefSpell<=sMaxSpellAge])
  smthEventRate_Exp_adv <- lm(EventRate_adv ~ ns(TimeInDefSpell, df=sDf_Exp), data=datSurv_exp[TimeInDefSpell<=sMaxSpellAge])
  smthEventRate_Exp_classic <- lm(EventRate_classic ~ ns(TimeInDefSpell, df=sDf_Exp), data=datSurv_exp[TimeInDefSpell<=sMaxSpellAge])
  
  # - [SANITY CHECK] Check the reasonability of the splines
  # summary(smthEventRate_Act); summary(smthEventRate_Exp_bas); summary(smthEventRate_Exp_adv); summary(smthEventRate_Exp_classic)
  
  # - Render predictions based on fitted smoother, with standard errors for confidence intervals
  vPredSmth_Act <- predict(smthEventRate_Act, newdata=datSurv_act, se=T)
  vPredSmth_Exp_bas <- predict(smthEventRate_Exp_bas, newdata=datSurv_exp, se=T)
  vPredSmth_Exp_adv <- predict(smthEventRate_Exp_adv, newdata=datSurv_exp, se=T)
  vPredSmth_Exp_classic <- predict(smthEventRate_Exp_classic, newdata=datSurv_exp, se=T)
  
  # - Add smoothed estimate to graphing object
  datSurv_act[, EventRate_spline := vPredSmth_Act$fit]
  datSurv_exp[, EventRate_spline_bas := vPredSmth_Exp_bas$fit]
  datSurv_exp[, EventRate_spline_adv := vPredSmth_Exp_adv$fit]
  datSurv_exp[, EventRate_spline_classic := vPredSmth_Exp_classic$fit]
  
  
  # --- 4.2 Combine and analyse all event rates
  # - Create data object to compare event rates
  datComp <- rbind(datSurv_act[, list(Time, EventRate, Type="a_Actual")],
                   datSurv_act[, list(Time, EventRate=EventRate_spline, Type="b_Actual_spline")],
                   datSurv_exp[, list(Time=TimeInDefSpell, EventRate=EventRate_bas, Type="c_Expected_bas")],
                   datSurv_exp[, list(Time=TimeInDefSpell, EventRate=EventRate_spline_bas, Type="d_Expected_spline_bas")],
                   datSurv_exp[, list(Time=TimeInDefSpell, EventRate=EventRate_adv, Type="e_Expected_adv")],
                   datSurv_exp[, list(Time=TimeInDefSpell, EventRate=EventRate_spline_adv, Type="f_Expected_spline_adv")],
                   datSurv_exp[, list(Time=TimeInDefSpell, EventRate=EventRate_classic, Type="g_Expected_classic")],
                   datSurv_exp[, list(Time=TimeInDefSpell, EventRate=EventRate_spline_classic, Type="h_Expected_spline_classic")]
  )
  
  # - Create different groupings for graphing purposes
  datComp[Type %in% c("a_Actual","c_Expected_bas", "e_Expected_adv", "g_Expected_classic"),
          EventRatePoint:=EventRate]
  datComp[Type %in% c("b_Actual_spline","d_Expected_spline_bas", "f_Expected_spline_adv", "h_Expected_spline_classic"),
          EventRateLine:=EventRate]
  datComp[, FacetLabel:=""]
  
  # - Calculate MAE between actual and expected event rates
  # Merge actual and expected event rate data sets
  datFusion <- merge(datSurv_act[Time<=sMaxSpellAge], 
                     datSurv_exp[TimeInDefSpell<=sMaxSpellAge,list(Time=TimeInDefSpell,
                                                                   EventRate_spline_bas,
                                                                   EventRate_spline_adv,
                                                                   EventRate_spline_classic)], by="Time")
  # Calculate MAEs
  mae_bas[as.character(weight_i)]<- mean(abs(datFusion$EventRate - datFusion$EventRate_spline_bas), na.rm=T)
  mae_adv[as.character(weight_i)] <- mean(abs(datFusion$EventRate - datFusion$EventRate_spline_adv), na.rm=T)
  mae_classic[as.character(weight_i)] <- mean(abs(datFusion$EventRate - datFusion$EventRate_spline_classic), na.rm=T)
  
  
}




# ------ 5. Analysing event rate MAEs in slecting appropriate weights

weights_check <- data.frame(weights,mae_bas, mae_adv, mae_classic)

weights_check <- weights_check %>%
  mutate(mae_bas = mae_bas * 100,
         mae_adv = mae_adv * 100,
         mae_classic = mae_classic * 100)
weights_long <- pivot_longer(weights_check,
                             cols = c(mae_bas, mae_adv),
                             names_to = "Model",
                             values_to = "MAE")

(gWeights <- ggplot(weights_long, aes(x = weights, y = MAE, color = Model)) +
  geom_line(size = 1) +
  labs(x = "Weights",
       y = "Mean Absolute Error (%)",
       color = "Model Type") +
  theme_minimal())

ggsave(plot, file=paste0(genFigPath,"MAE_Check.png"),
       width=1200/dpi, height=1000/dpi,dpi=dpi, bg="white")
pack.ffdf(paste0(genPath, "MAE_Weights"), weights_check)
