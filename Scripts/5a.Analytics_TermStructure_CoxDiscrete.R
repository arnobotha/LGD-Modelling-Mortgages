# ============================== TERM-STUCTURE: Cox Discrete-time Hazard ===============================
# Construct and compare empirical and expected term-structures of default risk
# ------------------------------------------------------------------------------------------------------
# PROJECT TITLE: Loss Modelling (LGD) for FNB Mortgages
# SCRIPT AUTHOR(S): Dr Arno Botha (AB), Mohammed Gabru (MG)
# ------------------------------------------------------------------------------------------------------
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
#
# -- Outputs:
#   - <Analytics> | Graphs
# ------------------------------------------------------------------------------------------------------



# ------ 1. Model fitting

# - Confirm prepared datasets are loaded into memory
if (!exists('datCredit_train_CDH')) unpack.ffdf(paste0(genPath,"creditdata_train_CDH"), tempPath);gc()
if (!exists('datCredit_valid_CDH')) unpack.ffdf(paste0(genPath,"creditdata_valid_CDH"), tempPath);gc()

# - Use only default spells
datCredit_train <- datCredit_train_CDH[!is.na(DefSpell_Key),]
datCredit_valid <- datCredit_valid_CDH[!is.na(DefSpell_Key),]
# remove previous objects from memory
rm(datCredit_train_CDH, datCredit_valid_CDH); gc()

# - Weigh default cases heavier. as determined interactively based on calibration success (script 6e)
datCredit_train[, Weight := ifelse(DefSpell_Event==1,1,1)]
datCredit_valid[, Weight := ifelse(DefSpell_Event==1,1,1)] # for merging purposes

# ---  Basic discrete-time hazard model
# - Initialize variables
vars_basic <- c("log(TimeInDefSpell)","DefSpell_Num_binned", "g0_Delinq_Lag_1",
                "slc_acct_arr_dir_3","M_Inflation_Growth_9","g0_Delinq_Any_Aggr_Prop_Lag_1")

# - Fit discrete-time hazard model with selected variables
modLR_basic <- glm( as.formula(paste("DefSpell_Event ~", paste(vars_basic, collapse = " + "))),
                    data=datCredit_train, family="binomial", weights= Weight)



# ---  Advanced discrete-time hazard model
# - Initialize variables
vars <- c("Time_Binned","log(TimeInDefSpell)*DefSpell_Num_binned", 
           "DefaultStatus1_Aggr_Prop_Lag_12","g0_Delinq_Ave", "slc_acct_arr_dir_3",
          "InterestRate_Margin_Aggr_Med_9","NewLoans_Aggr_Prop","InterestRate_Nom",
          "Balance_adj_WOff","pmnt_method_grp","Principal","g0_Delinq_Lag_1",
          "M_RealIncome_Growth_9", "M_Inflation_Growth_12","M_DTI_Growth_12","M_Repo_Rate_12","g0_Delinq_Any_Aggr_Prop_Lag_1")
modLR <- glm( as.formula(paste("DefSpell_Event ~", paste(vars, collapse = " + "))),
              data=datCredit_train, family="binomial", weights= Weight)

# ------ 2. Kaplan-Meier estimation towards constructing empirical term-structure of default risk

# --- Preliminaries
# - Create pointer to the appropriate data object 
datCredit <- rbind(datCredit_train, datCredit_valid)


# --- Estimate survival rate of the main event S(t) = P(T >=t) for time-to-event variable T
# Compute Kaplan-Meier survival estimates (product-limit) for main-event | Spell-level with right-censoring & left-truncation
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



# --- Estimate survival rate of the censoring event G(t) = P(C >= t) for time-to-censoring variable C
# This prepares for implementing the Inverse Probability of Censoring Weighting (IPCW) scheme,
# as an adjustment to event rates (discrete density), as well as for the time-dependent Brier score

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





# ------ 3. Constructing expected term-structure of write-off risk

# --- Handle left-truncated spells by adding a starting record 
# This is necessary for calculating certain survival quantities later
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


# -- Graphing survival quantities
# - Confirm prepared datasets are loaded into memory : actual term-structure of full-sample
#if (!exists('datSurv')) unpack.ffdf(paste0(genPath,"datSurv_KM_MultiSpell"), tempPath);gc()




# ------ 4. Graphing the event density / probability mass function f(t)

# - General parameters
sMaxSpellAge <- 120 # max for [DefSpell_Age], as determined in earlier analyses (script 3c
sMaxSpellAge_graph <- 120 # max for [DefSpell_Age] for graphing purposes

# - Determine population average survival and event rate across loans per time period
# Event probability ("term-structure"): Actual vs Expected
#plot(datSurv_sub[Time <= 300, Time], datSurv_sub[Time <= 300, EventRate], type="b")
#lines(datSurv_exp[TimeInDefSpell <= 300, TimeInDefSpell], datSurv_exp[TimeInDefSpell <= 300, EventRate_adv], type="b", col="red")
#lines(datSurv_exp[TimeInDefSpell <= 300, TimeInDefSpell], datSurv_exp[TimeInDefSpell <= 300, EventRate_Emp], type="b", col="blue")
#lines(datSurv_exp[TimeInDefSpell <= 300, TimeInDefSpell], datSurv_exp[TimeInDefSpell <= 300, EventRate_IPCW], type="b", col="gray")


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
datGraph[, FacetLabel := "Term-structure of write-off risk"]

# - Aesthetic engineering
chosenFont <- "Cambria"
mainEventName <- "Write-off"

# - Calculate MAE between event rates
datFusion <- merge(datSurv_sub[Time <= sMaxSpellAge], 
                   datSurv_exp[TimeInDefSpell <= sMaxSpellAge,list(Time=TimeInDefSpell, EventRate_bas, EventRate_adv)], by="Time")
MAE_eventProb_bas <- mean(abs(datFusion$EventRate - datFusion$EventRate_bas), na.rm=T)
MAE_eventProb_adv <- mean(abs(datFusion$EventRate - datFusion$EventRate_adv), na.rm=T)

# - Graphing parameters
vCol <- brewer.pal(10, "Paired")[c(3,4,5,6,1,2)]
vLabel2 <- c("b_Actual_spline"=paste0("Actual spline"), 
             "d_Expected_spline_bas"=paste0("Exp spline: Basic"),
             "f_Expected_spline_adv"=paste0("Exp spline: Advanced"),
             "a_Actual"="Actual", "c_Expected_bas"="Exp: Basic", "e_Expected_adv"="Exp: Advanced")
vSize <- c(0.2,0.3,0.2,0.3,0.2,0.3)
vLineType <- c("dashed", "solid", "dashed", "solid", "dashed", "solid")

# - Create main graph 
(gsurv_ft <- ggplot(datGraph[Time <= sMaxSpellAge_graph,], aes(x=Time, y=EventRate, group=Type)) + theme_minimal() +
    labs(y=bquote(plain(Event~probability~~italic(w(t))*" ["*.(mainEventName)*"]"*"")), 
         x=bquote("Default spell age (months)"*~italic(t))
         #subtitle="Term-structures of default risk: Discrete-time hazard models"
    ) + 
    theme(text=element_text(family=chosenFont),legend.position = "bottom",
          strip.background=element_rect(fill="snow2", colour="snow2"),
          strip.text=element_text(size=8, colour="gray50"), strip.text.y.right=element_text(angle=90)) + 
    # Main graph
    geom_point(aes(y=EventRatePoint, colour=Type, shape=Type), size=0.6) + 
    geom_line(aes(y=EventRate, colour=Type, linetype=Type, linewidth=Type)) + 
    # Annotations
    annotate("text", y=0.015,x=100, label=paste0("MAE (basic): ", percent(MAE_eventProb_bas, accuracy=0.0001)), family=chosenFont,
             size = 3) + 
    annotate("text", y=0.0125,x=100, label=paste0("MAE (advanced): ", percent(MAE_eventProb_adv, accuracy=0.0001)), family=chosenFont,
             size = 3) + 
    # Scales and options
    facet_grid(FacetLabel ~ .) + 
    scale_colour_manual(name="", values=vCol, labels=vLabel2) + 
    scale_linewidth_manual(name="", values=vSize, labels=vLabel2) + 
    scale_linetype_manual(name="", values=vLineType, labels=vLabel2) + 
    scale_shape_discrete(name="", labels=vLabel2) + 
    scale_y_continuous(breaks=breaks_pretty(), label=percent) + 
    scale_x_continuous(breaks=breaks_pretty(n=8), label=comma) + 
    guides(color=guide_legend(nrow=2))
)

# - Save plot
dpi <- 280 # reset
ggsave(gsurv_ft, file=paste0(genFigPath, "EventProb_", mainEventName,"_ActVsExp_CoxDisc.png"),
       width=1200/dpi, height=1000/dpi,dpi=dpi, bg="white")


# --- Cleanup
rm(gsurv_ft, km_Censoring, km_Default, datSurv_censoring, datSurv_exp, datSurv_sub,
   datGraph, datFusion, smthEventRate_Act, smthEventRate_Exp_bas, smthEventRate_Exp_adv,
   vPredSmth_Act, vPredSmth_Exp_bas, vPredSmth_Exp_adv,
   modLR, modLR_basic, datAdd)