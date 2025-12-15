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



# ------ 1. Preliminaries

# --- 1.1 Load and prepare data
# - Confirm prepared datasets are loaded into memory
if (!exists('datCredit_train_CDH')) unpack.ffdf(paste0(genPath,"creditdata_train_CDH"), tempPath);gc()
if (!exists('datCredit_valid_CDH')) unpack.ffdf(paste0(genPath,"creditdata_valid_CDH"), tempPath);gc()

# - Use only default spells
datCredit_train <- datCredit_train_CDH[!is.na(DefSpell_Key),]
datCredit_valid <- datCredit_valid_CDH[!is.na(DefSpell_Key),]

# - Remove previous objects from memory
rm(datCredit_train_CDH, datCredit_valid_CDH); gc()

# - Weigh write-off cases
datCredit_train[, Weight:=ifelse(DefSpell_Event==1,1,1)]
datCredit_valid[, Weight:=ifelse(DefSpell_Event==1,1,1)]


# --- 1.2 Load models
# - Basic discrete-time hazard model
modLR_Bas <- readRDS(paste0(genObjPath,"CoxDisc_Basic_Model.rds"))

# - Advanced discrete-time hazard model
modLR_Adv <- readRDS(paste0(genObjPath,"CoxDisc_Advanced_Model.rds"))

# - classic logit model
modLR_Classic <- readRDS(paste0(genObjPath,"LR_Model.rds"))


# --- 1.3 Additional parameters
# - Youden Index cut-offs
# Basic discrete-time model
thresh_dth <-0.1037777
# Advanced discrete-time model
thresh_dth_bas <- 0.00795927
# Classical logit model
thres_classic <- 0.2352999




# ------ 2. Constructing the empirical term-structure of write-off using a Kaplan-Meier estimator

# --- 2.1 Preliminaries
# - Create pointer to the appropriate data object 
datCredit <- rbind(datCredit_train, datCredit_valid)


# --- 2.2 Estimate survival rate of the main event S(t) = P(T >=t) for time-to-event variable T
# - Compute Kaplan-Meier survival estimates (product-limit) for main-event | Spell-level with right-censoring & left-truncation
km_Default <- survfit(Surv(time=TimeInDefSpell-1, time2=TimeInDefSpell, event=DefSpell_Event==1, type="counting") ~ 1, 
                      id=DefSpell_Key, data=datCredit)

# - Create survival table
(datSurv_act <- surv_summary(km_Default))

# - Enrich data
datSurv_act <- datSurv_act %>%
  # Rename some fields
  rename(Time=time, AtRisk_n=`n.risk`, Event_n=`n.event`, Censored_n=`n.censor`, SurvivalProb_KM=`surv`) %>%
  # Calculate hazard rate
  mutate(Hazard_Actual=Event_n/AtRisk_n) %>% 
  # Calculate cumulative hazard rate
  mutate(CHaz=cumsum(Hazard_Actual)) %>%
  # Probability mass function f(t)=h(t)*S(t-1)
  mutate(EventRate=Hazard_Actual*shift(SurvivalProb_KM, n=1, fill=1)) %>%
  # Filter for observations with events and censoring
  filter(Event_n > 0 | Censored_n >0) %>% as.data.table()

# - Order dataset
setorder(datSurv_act, Time)

# - Calculate the at-risk population proportion
datSurv_act[,AtRisk_perc:=AtRisk_n/max(AtRisk_n, na.rm=T)]


# --- 2.3 Estimate survival rate of the censoring event G(t) = P(C >= t) for time-to-censoring variable C
### NOTE: This prepares for implementing the Inverse Probability of Censoring Weighting (IPCW) scheme,
###       as an adjustment to event rates (discrete density), as well as for the time-dependent Brier score

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




# ------ 3. Constructing expected term-structures of write-off

# --- 3.1 Handle left-truncated spells by adding a starting record 
### NOTE:  This is necessary for calculating certain survival quantities later

# - Create an additional record for each default spell
datAdd <- subset(datCredit, Counter == 1 & TimeInDefSpell > 1)
datAdd[, Start := Start-1]
datAdd[, TimeInDefSpell := TimeInDefSpell-1]
datAdd[, Counter := 0]

# - Add record to main dataset
datCredit <- rbind(datCredit, datAdd); setorder(datCredit, DefSpell_Key, TimeInDefSpell)


# --- 3.2 Calculate account-level survival quantities of interest | Discrete-time models
# - Compute IPCW-scheme weight
datCredit[, Weight:= 1/Survival_censoring]

# - Predict hazard h(t) = P(T=t | T>= t) in discrete-time
datCredit[, Hazard_adv:=predict(modLR_Adv, newdata=datCredit, type = "response")]
datCredit[, Hazard_bas:=predict(modLR_Bas, newdata=datCredit, type = "response")]

# - Derive survival probability S(t) = prod( 1- hazard)
datCredit[, Survival_adv:=cumprod(1-Hazard_adv), by=list(DefSpell_Key)]
datCredit[, Survival_bas:=cumprod(1-Hazard_bas), by=list(DefSpell_Key)]

# - Derive discrete density, or event probability f(t) = S(t-1) - S(t)
datCredit[, EventRate_adv:=shift(Survival_adv, type="lag", n=1, fill=1) - Survival_adv, by=list(DefSpell_Key)]
datCredit[, EventRate_bas:=shift(Survival_bas, type="lag", n=1, fill=1) - Survival_bas, by=list(DefSpell_Key)]

# - Remove added rows
datCredit <- subset(datCredit, Counter > 0)


# --- 3.3 Calculate account-level survival quantities of interest | Classic model
# - Score using classic model for each instance of [TimeInDefSpell] as [DefSpell_Age]
datCredit[, DefSpell_Age:=TimeInDefSpell]

# - Derive hazard rate
datCredit[!is.na(DefSpell_Num), Hazard_classic:=predict(modLR_Classic, newdata=.SD[], type="response")]

# - Derive survival probability S(t) = prod( 1- hazard)
datCredit[!is.na(DefSpell_Num), Survival_classic:=cumprod(1-Hazard_classic), by=list(DefSpell_Key)]

# - Derive discrete density, or event probability f(t) = S(t-1) - S(t)
datCredit[!is.na(DefSpell_Num), EventRate_classic:=ashift(Survival_classic, type="lag", n=1, fill=1) - Survival_classic, by=list(DefSpell_Key)]


# --- 3.4 Dichotomise model predictions
datCredit <- datCredit[,Youden_bas:=ifelse(EventRate_bas>thresh_dth_bas,1,0)]
datCredit <- datCredit[,Youden_adv:=ifelse(EventRate_adv>thresh_dth,1,0)]
datCredit <- datCredit[,Youden_classic:=ifelse(EventRate_classic>thres_classic,1,0)]


# --- 3.5 Aggregate event probabilities to period-level
datSurv_exp <- datCredit[,.(# Basic model
                            EventRate_bas = mean(EventRate_bas, na.rm=T),
                            EventRate_bas_Youden = mean(Youden_bas, na.rm=T),
                            # Advanced model
                            EventRate_adv = mean(EventRate_adv, na.rm=T),
                            EventRate_adv_Youden = mean(Youden_adv, na.rm=T),
                            # Classical model
                            EventRate_classic = mean(EventRate_classic, na.rm=T),
                            EventRate_classic_Youden = mean(Youden_classic, na.rm=T),
                            # Empirical estimates
                            EventRate_Emp = sum(DefSpell_Event)/.N,
                            EventRate_IPCW = sum(Weight*DefSpell_Event)/sum(Weight)),
                         by=list(TimeInDefSpell)]


# --- 3.6 [SANITY CHECK] Graphing survival quantities
plot(datSurv_act[Time <= 120, EventRate], type="b")
lines(datSurv_exp[TimeInDefSpell <= 120, EventRate_adv], type="b", col="red")
lines(datSurv_exp[TimeInDefSpell <= 120, EventRate_adv_Youden], type="b", col="green")
lines(datSurv_exp[TimeInDefSpell <= 120, EventRate_bas], type="b", col="blue")
lines(datSurv_exp[TimeInDefSpell <= 120, EventRate_bas_Youden], type="b", col="grey")
lines(datSurv_exp[TimeInDefSpell <= 120, EventRate_classic], type="b", col="cyan")
lines(datSurv_exp[TimeInDefSpell <= 120, EventRate_classic_Youden], type="b", col="orange")




# ------ 4. Graphing the event density / probability mass function f(t) | Main graph

# --- 4.1 Preliminaries
# - Cut-offs for analysis and graphs 
sMaxSpellAge <- 120 # Max for [DefSpell_Age], as determined in earlier analyses (script 3c)
sMaxSpellAge_graph <- 120 # Max for [DefSpell_Age] for graphing purposes

# - Fit natural cubic regression splines
# Knots
sDf_Act <- 12; sDf_Exp <- 12
# Actuals
smthEventRate_Act <- lm(EventRate~ns(Time, df=sDf_Act), data=datSurv_act[Time<=sMaxSpellAge,])
# Basic model
smthEventRate_Exp_bas <- lm(EventRate_bas~ns(TimeInDefSpell, df=sDf_Exp), data=datSurv_exp[TimeInDefSpell<=sMaxSpellAge])
smthEventRate_Exp_bas_Youden <- lm(EventRate_bas_Youden~ns(TimeInDefSpell, df=sDf_Exp), data=datSurv_exp[TimeInDefSpell<=sMaxSpellAge])
# Advanced model
smthEventRate_Exp_adv <- lm(EventRate_adv~ns(TimeInDefSpell, df=sDf_Exp), data=datSurv_exp[TimeInDefSpell<=sMaxSpellAge])
smthEventRate_Exp_adv_Youden <- lm(EventRate_adv_Youden~ns(TimeInDefSpell, df=sDf_Exp), data=datSurv_exp[TimeInDefSpell<=sMaxSpellAge])
# Classical model
smthEventRate_Exp_classic <- lm(EventRate_classic~ns(TimeInDefSpell, df=sDf_Exp), data=datSurv_exp[TimeInDefSpell<=sMaxSpellAge])
smthEventRate_Exp_classic_Youden <- lm(EventRate_classic_Youden~ns(TimeInDefSpell, df=sDf_Exp), data=datSurv_exp[TimeInDefSpell<=sMaxSpellAge])

# - [SANITY CHECK] Evaluate splines
# summary(smthEventRate_Act); summary(smthEventRate_Exp_bas); summary(smthEventRate_Exp_adv); summary(smthEventRate_Exp_classic)
# summary(smthEventRate_Exp_adv_Youden);summary(smthEventRate_Exp_bas_Youden); summary(smthEventRate_Exp_bas_Youden)

# - Render predictions based on fitted smoother, with standard errors for confidence intervals
# Actuals
vPredSmth_Act <- predict(smthEventRate_Act, newdata=datSurv_act, se=T)
# Basic model
vPredSmth_Exp_bas <- predict(smthEventRate_Exp_bas, newdata=datSurv_exp, se=T)
vPredSmth_Exp_bas_Youden <- predict(smthEventRate_Exp_bas_Youden, newdata=datSurv_exp, se=T)
# Advanced model
vPredSmth_Exp_adv <- predict(smthEventRate_Exp_adv, newdata=datSurv_exp, se=T)
vPredSmth_Exp_adv_Youden <- predict(smthEventRate_Exp_adv_Youden, newdata=datSurv_exp, se=T)
# Classical model
vPredSmth_Exp_classic <- predict(smthEventRate_Exp_classic, newdata=datSurv_exp, se=T)
vPredSmth_Exp_classic_Youden <- predict(smthEventRate_Exp_classic_Youden, newdata=datSurv_exp, se=T)

# - Add smoothed estimate to graphing object
# Actuals
datSurv_act[, EventRate_spline:=vPredSmth_Act$fit]
# Basic model
datSurv_exp[, EventRate_spline_bas:=vPredSmth_Exp_bas$fit]
datSurv_exp[, EventRate_spline_bas_Youden:=vPredSmth_Exp_bas_Youden$fit]
# Advanced model
datSurv_exp[, EventRate_spline_adv:=vPredSmth_Exp_adv$fit]
datSurv_exp[, EventRate_spline_adv_Youden:=vPredSmth_Exp_adv_Youden$fit]
# Classical model
datSurv_exp[, EventRate_spline_classic:=vPredSmth_Exp_classic$fit]
datSurv_exp[, EventRate_spline_classic_Youden:=vPredSmth_Exp_classic_Youden$fit]


# --- 4.2 Create graphing data
# - Initialise graphing data
datGraph <- rbind(datSurv_act[,list(Time, EventRate, Type="a_Actual")],
                  datSurv_act[,list(Time, EventRate=EventRate_spline, Type="b_Actual_spline")],
                  datSurv_exp[,list(Time=TimeInDefSpell, EventRate=EventRate_bas, Type="c_Expected_bas")],
                  datSurv_exp[,list(Time=TimeInDefSpell, EventRate=EventRate_spline_bas, Type="d_Expected_spline_bas")],
                  datSurv_exp[,list(Time=TimeInDefSpell, EventRate=EventRate_adv, Type="e_Expected_adv")],
                  datSurv_exp[,list(Time=TimeInDefSpell, EventRate=EventRate_spline_adv, Type="f_Expected_spline_adv")],
                  datSurv_exp[,list(Time=TimeInDefSpell, EventRate=EventRate_adv_Youden, Type="i_Expected_Youden_adv")],
                  datSurv_exp[,list(Time=TimeInDefSpell, EventRate=EventRate_spline_adv_Youden, Type="j_Expected_spline_Youden_adv")],
                  datSurv_exp[,list(Time=TimeInDefSpell, EventRate=EventRate_classic, Type="g_Expected_classic")],
                  datSurv_exp[,list(Time=TimeInDefSpell, EventRate=EventRate_spline_classic, Type="h_Expected_spline_classic")]
)

# - Create different groupings for graphing purposes
datGraph[Type %in% c("a_Actual","c_Expected_bas", "e_Expected_adv", "g_Expected_classic","i_Expected_Youden_adv",
                     "k_Expected_Youden_classic"), EventRatePoint:=EventRate]
datGraph[Type %in% c("b_Actual_spline","d_Expected_spline_bas", "f_Expected_spline_adv", "h_Expected_spline_classic",
                     "j_Expected_spline_Youden_adv"), EventRateLine:=EventRate]
datGraph[, FacetLabel:="Term-structure of write-off risk"]

# - Calculate MAE between event rates
# Compile a dataset to facilitate calculations
datMAE <- merge(datSurv_act[Time <= sMaxSpellAge], 
                datSurv_exp[TimeInDefSpell<=sMaxSpellAge,list(Time=TimeInDefSpell,
                                                              EventRate_bas, EventRate_bas_Youden,
                                                              EventRate_adv, EventRate_adv_Youden,
                                                              EventRate_classic, EventRate_classic_Youden)], by="Time")
# Basic model
MAE_eventProb_bas <- mean(abs(datMAE$EventRate - datMAE$EventRate_bas), na.rm=T)
MAE_eventProb_bas_Youden <- mean(abs(datMAE$EventRate - datMAE$EventRate_bas_Youden), na.rm=T)
# Advanced model
MAE_eventProb_adv <- mean(abs(datMAE$EventRate - datMAE$EventRate_adv), na.rm=T)
MAE_eventProb_adv_Youden <- mean(abs(datMAE$EventRate - datMAE$EventRate_adv_Youden), na.rm=T)
# Classical model
MAE_eventProb_classic <- mean(abs(datMAE$EventRate - datMAE$EventRate_classic), na.rm=T)
MAE_eventProb_classic_Youden <- mean(abs(datMAE$EventRate - datMAE$EventRate_classic_Youden), na.rm=T)


# --- 4.3 Main graph
# - Graphing parameters
vCol <- brewer.pal(12, "Paired")[c(3,4,5,6,1,2,7,8,9,10,11,12)]
length(vCol)
vLabel2 <- c("b_Actual_spline"=paste0("Empirical Spline"), 
             "d_Expected_spline_bas"=paste0("Exp spline: DtH-Basic A"),
             "f_Expected_spline_adv"=paste0("Exp spline: DtH-Advanced A"),
             "h_Expected_spline_classic"=paste0("Exp spline: LR A"),
             "j_Expected_spline_Youden_adv"=paste0("Exp spline: DtH-Advanced B"),
             "a_Actual"="Emprirical", "c_Expected_bas"="Exp: DtH-Basic A", "e_Expected_adv"="Exp: DtH-Advanced A",
             "g_Expected_classic"="Exp: LR A",
             "i_Expected_Youden_adv"="Exp: DtH-Advanced B")
vSize <- c(0.2,0.3,0.2,0.3,0.2,0.3, 0.2, 0.3, 0.2, 0.3,0.2,0.3)
vLineType <- c("dashed", "solid", "dashed", "solid", "dashed", "solid","dashed", "solid","dashed", "solid","dashed", "solid")
chosenFont <- "Cambria"
mainEventName <- "Write-off"

# - Create main graph 
(gsurv_ft <- ggplot(datGraph[Time <= sMaxSpellAge_graph,], aes(x=Time, y=EventRate, group=Type)) + theme_minimal() +
    labs(y=bquote(plain(Event~probability~~italic(w(t))*" ["*.(mainEventName)*"]"*"")), 
         x=bquote("Spell time (months)"*~italic(t))) + 
    theme(text=element_text(family=chosenFont),legend.position = "bottom",
          strip.background=element_rect(fill="snow2", colour="snow2"),
          strip.text=element_text(size=8, colour="gray50"), strip.text.y.right=element_text(angle=90)) + 
    coord_cartesian(ylim=c(0,0.0325)) +
    # Main graph
    geom_point(aes(y=EventRatePoint, colour=Type, shape=Type), size=0.6) + 
    geom_line(aes(y=EventRate, colour=Type, linetype=Type, linewidth=Type)) + 
    # Annotations
    annotate("text", y=0.03,x=100, label=paste0("MAE (DtH-Basic A): ", percent(MAE_eventProb_bas, accuracy=0.0001)), family=chosenFont,
             size = 3) + 
    annotate("text", y=0.0275,x=100, label=paste0("MAE (DtH-Advanced A): ", percent(MAE_eventProb_adv, accuracy=0.0001)), family=chosenFont,
             size = 3) + 
    annotate("text", y=0.025,x=100, label=paste0("MAE (LR A): ", percent(MAE_eventProb_classic, accuracy=0.0001)), family=chosenFont,
             size = 3)+
    annotate("text", y=0.0225,x=100, label=paste0("MAE (DtH-Advanced B): ", percent(MAE_eventProb_adv_Youden, accuracy=0.0001)), family=chosenFont,
             size = 3)+
    # Scales and options
    facet_grid(FacetLabel ~ .) + 
    scale_colour_manual(name="", values=vCol, labels=vLabel2) + 
    scale_linewidth_manual(name="", values=vSize, labels=vLabel2) + 
    scale_linetype_manual(name="", values=vLineType, labels=vLabel2) + 
    scale_shape_discrete(name="", labels=vLabel2) + 
    scale_y_continuous(breaks=breaks_pretty(), label=percent)+ 
    scale_x_continuous(breaks=breaks_pretty(n=8), label=comma) + 
    guides(color=guide_legend(nrow=2))
)

# - Save plot
dpi <- 280 # reset
ggsave(gsurv_ft, file=paste0(genFigPath, "EventProb_", mainEventName,"_ActVsExp_CoxDisc.png"),
       width=2400/dpi, height=1800/dpi,dpi=dpi, bg="white")




# ------ 5. Graphing the event density / probability mass function f(t) | Main graph
# --- 5.1 Create graphing object
# - Create graphing data object for the two that are out of range
datGraph_OOB <- rbind(
                  datSurv_act[,list(Time, EventRate, Type="a_Actual")],
                  datSurv_act[,list(Time, EventRate=EventRate_spline, Type="b_Actual_spline")],
                  datSurv_exp[, list(Time=TimeInDefSpell, EventRate=EventRate_classic_Youden, Type="c_Expected_Youden_classic")],
                  datSurv_exp[, list(Time=TimeInDefSpell, EventRate=EventRate_spline_classic_Youden, Type="d_Expected_spline_Youden_classic")],
                  datSurv_exp[, list(Time=TimeInDefSpell, EventRate=EventRate_bas_Youden, Type="e_Expected_Youden_bas")],
                  datSurv_exp[, list(Time=TimeInDefSpell, EventRate=EventRate_spline_bas_Youden, Type="f_Expected_spline_Youden_bas")]
)

# - Create different groupings for graphing purposes
datGraph_OOB[Type %in% c("a_Actual","c_Expected_Youden_classic", "e_Expected_Youden_bas"), EventRatePoint:=EventRate ]
datGraph_OOB[Type %in% c("b_Actual_spline","d_Expected_spline_Youden_classic", "f_Expected_spline_Youden_bas"), EventRateLine:=EventRate ]
datGraph_OOB[, FacetLabel:="Term-structure of write-off risk"]

# - Calculate MAE between event rates
datFusion_OOB <- merge(datSurv_act[Time <= sMaxSpellAge], 
                   datSurv_exp[TimeInDefSpell <= sMaxSpellAge,list(Time=TimeInDefSpell,EventRate_bas_Youden,EventRate_classic_Youden)], by="Time")
MAE_eventProb_classic_Youden <- mean(abs(datFusion_OOB$EventRate - datFusion_OOB$EventRate_classic_Youden), na.rm=T)
MAE_eventProb_bas_Youden <- mean(abs(datFusion_OOB$EventRate - datFusion_OOB$EventRate_bas_Youden), na.rm=T)


# --- 5.2 Ancillary graph
# - Graphing parameters
vCol <- brewer.pal(12, "Paired")[c(3,4,5,6,1,2)]
vLabel2 <- c("b_Actual_spline"=paste0("Empirical spline"), 
             "d_Expected_spline_Youden_classic"=paste0("Exp spline: LR B"),
             "f_Expected_spline_Youden_bas"=paste0("Exp spline: DtH-Basic B"),
             "a_Actual"="Empirical", "c_Expected_Youden_classic"="Exp: LR B", "e_Expected_Youden_bas"="Exp: DtH-Basic B")
vSize <- c(0.2,0.3,0.2,0.3,0.2,0.3)
vLineType <- c("dashed", "solid", "dashed", "solid", "dashed", "solid")
chosenFont <- "Cambria"
mainEventName <- "Write-off"

# - Create main graph 
(gsurv_ft <- ggplot(datGraph_OOB[Time <= sMaxSpellAge_graph,], aes(x=Time, y=EventRate, group=Type)) + theme_minimal() +
    labs(y=bquote(plain(Event~probability~~italic(w(t))*" ["*.(mainEventName)*"]"*"")), 
         x=bquote("Spelll time (months)"*~italic(t))
         #subtitle="Term-structures of default risk: Discrete-time hazard models"
    ) + 
    theme(text=element_text(family=chosenFont),legend.position = "bottom",
          strip.background=element_rect(fill="snow2", colour="snow2"),
          strip.text=element_text(size=8, colour="gray50"), strip.text.y.right=element_text(angle=90)) + 
    # Main graph
    geom_point(aes(y=EventRatePoint, colour=Type, shape=Type), size=0.6) + 
    geom_line(aes(y=EventRate, colour=Type, linetype=Type, linewidth=Type)) + 
    # Annotations
    annotate("text", y=0.2,x=100, label=paste0("MAE (LR B): ", percent(MAE_eventProb_classic_Youden, accuracy=0.0001)), family=chosenFont,
             size = 3) + 
    annotate("text", y=0.18,x=100, label=paste0("MAE (DtH-Basic B): ", percent(MAE_eventProb_bas_Youden, accuracy=0.0001)), family=chosenFont,
             size = 3) +
    # Scales and options
    facet_grid(FacetLabel ~ .) + 
    scale_colour_manual(name="", values=vCol, labels=vLabel2) + 
    scale_linewidth_manual(name="", values=vSize, labels=vLabel2) + 
    scale_linetype_manual(name="", values=vLineType, labels=vLabel2) + 
    scale_shape_discrete(name="", labels=vLabel2) + 
    scale_y_continuous(breaks=breaks_pretty(), label=percent)+ 
    scale_x_continuous(breaks=breaks_pretty(n=8), label=comma) + 
    guides(color=guide_legend(nrow=2))
)

# - Save plot
dpi <- 280 # reset
ggsave(gsurv_ft, file=paste0(genFigPath, "EventProb_", mainEventName,"_ActVsExp_CoxDisc_OOB.png"),
       width=2400/dpi, height=1800/dpi,dpi=dpi, bg="white")


# --- 5.3 Cleanup
rm(gsurv_ft, km_Censoring, km_Default, datSurv_censoring, datSurv_exp, datSurv_act,
   datGraph, datMAE, smthEventRate_Act, smthEventRate_Exp_bas, smthEventRate_Exp_adv,
   vPredSmth_Act, vPredSmth_Exp_bas, vPredSmth_Exp_adv,
   modLR_Classic, modLR_Bas, modLR_Adv, datAdd)
gc()




