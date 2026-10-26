# ============================== HAZARD & EVENT RATE ANALYSIS =========================================
# Analysis performed on the hazard rate of different default spells. The 
# analysis aids in the decision-making about a suitable cut-off point beyond 
# which all default spells are grouped together.
# # Refer to LifetimePD-TermStructure-RecurrentEvents codebase on Github for varieties of KM-analysis
# across including/excluding left-truncation and first-spell vs multi-spell.
# ------------------------------------------------------------------------------------------------------
# PROJECT TITLE: Loss Modelling (LGD) for Residential Mortgages
# SCRIPT AUTHOR(S): Dr Arno Botha (AB), Bernard Scheepers (BS), Mohammed Gabru (MG)
# ------------------------------------------------------------------------------------------------------
# -- Script dependencies:
#   - 0.Setup.R
#   - 1.Data_Import.R
#   - 2a.Data_Prepare_Credit_Basic.R
#   - 2b.Data_Prepare_Credit_Advanced.R
#   - 2c.Data_Prepare_Credit_Advanced2.R
#   - 2d.Data_Enrich.R
#   - 2e.Data_Prepare_Macro.R
#   - 2f.Data_Fusion1.R

# -- Inputs:
#   - datCredit_real | Prepared from script 2f.
#
# -- Outputs:
#   - Cumulative baseline hazard rates by default spells (2 different groupings)
#   - Event probabilities
#   - Hazard rates
#   - Kaplan-Meyer analysis by performance spells
#   - datSurv objects | Respective to each setting, containing survival, cumulative hazard, & event probabilities
# ------------------------------------------------------------------------------------------------------


# -------- 0 Preliminaries
# - Generic parameters
mainEventName <- "Write-off"

# - Confirm prepared datasets are loaded into memory
if (!exists('datCredit_real')) unpack.ffdf(paste0(genPath,"creditdata_final4a"), tempPath)




# -------- 1. Kaplan-Meier analysis across all default spells | Left-truncation included

# --- Fit Kaplan-Meier (KM) nonparametric model
# Compute Kaplan-Meier survival estimates (product-limit) for main-event | Spell-level with right-censoring & left-truncation
km_All <- survfit(Surv(time=TimeInDefSpell-1, time2=TimeInDefSpell, event=WOff_Ind==1, type="counting") ~ 1, 
                  id=DefSpell_Key, data=datCredit_real[!is.na(DefSpell_Key),])
summary(km_All)$table # overall summary statistics
plot(km_All, conf.int = T) # survival curve

# -- Calculate various summary statistics
# - Chosen percentile of survival times, used later as a prediction time, e.g., 75%
# NOTE: Median would also work, though sample sizes typically dwindle at that point, which can complicate analyses
survPercentile <- 0.75
cat(paste0("Kaplan-Meier: Survival time at the xth percentile of the distribution of unique survival times: \n\t",
           percTimepoint <- min(km_All$time[km_All$surv <= survPercentile],na.rm=T)))

# - Calculate the Truncated Mean (or "Restricted Mean Survival Time" [RMST]) of survival times
# NOTE: The classical mean survival time is only defined when S(t) can reach 0
# NOTE2: Calculating RMST involves integrating the KM-curve up to the last (hence truncated) observed unique event time
#         I.e., sum the survival probabilities, as weighted by the time difference between two successive survival times
# NOTE3: RMST: What is the average survival time (time-to-event) over a window lf length \tau?
#       Up to how long does an individual survive during a time span of \tau units?
tau <- max(km_All$time) # if max, then RMST = "mean follow-up time"
rmst <- calcRMST(km_All, tau)
cat(paste0("Kaplan-Meier: Restricted Mean Survival Time [RMST] during window of length tau = ", tau, ": \t", round(rmst,digits=2),
           " months. \nWhat is the average survival time during study window?"))
### RESULTS: 61.18 months
tau <- 60
rmst <- calcRMST(km_All, tau)
cat(paste0("Kaplan-Meier: Restricted Mean Survival Time [RMST] during window of length tau = ", tau, ": \t", round(rmst,digits=2),
           " months. \nWhat is the average survival time during study window?"))
### RESULTS: 18.89 months

# - Calculate naive estimator of time-to-event by taking mean of spell ages
round(mean(datCredit_real[DefSpell_Counter==1, DefSpell_Age], na.rm=T), digits=2)
round(mean(pmin(datCredit_real[DefSpell_Counter==1, DefSpell_Age], tau), na.rm=T), digits=2)
### RESULTS: 20.95 months vs 18 months

# - Retrieve the mean survival probability at the previously found percentile-based prediction point
cat(paste0("Kaplan-Meier: Retrieved mean survival probability at a given time point (t=", percTimepoint, "): \n\t",
           percent(km_All$surv[max(which(km_All$time <= percTimepoint))], accuracy=0.01)))

# - Retrieve the mean survival probability at the last unique event time as prediction time, i.e., 
# the "pooled mean survival probability"
cat(paste0("Kaplan-Meier: Retrieved mean survival probability at last unique event point as time point (t=", max(km_All$time), "): \n\t",
           percent(km_All$surv[max(which(km_All$time <= max(km_All$time)))], accuracy=0.01)))




# --- Cumulative Lifetime Distribution F(t)=1-S(t)

# - Graphing parameters
vCol <- brewer.pal(8, "Set1")[c(1)] # for F(t)
medLife <- summary(km_All)$table["median"]
median_survival <- data.frame(x = medLife, y = 0.5)
maxTime <- max(km_All$time)
chosenFont <- "Cambria"

# - Graphing logic: Entire domain of time to event
gsurv_Ft <- ggsurvplot(km_All, fun="event", conf.int=T, surv.scale = "percent", legend="none", 
                       break.time.by=100, palette=vCol,
                       xlab = bquote(Discrete~time~italic(t)*" (months) in spell: Multi-spell"),
                       ylab = bquote(CLD~"["*.(mainEventName)*"]"*~italic(F(t))*": Kaplan-Meier (incl. left-truncation)"),
                       xlim=c(0, maxTime+1), censor=F, 
                       ggtheme = theme_bw(base_family=chosenFont), tables.theme = theme_cleantable(),
                       tables.height=0.10, tables.y.text=F, tables.y.text.col=T, risk.table = "abs_pct", risk.table.pos = "out",
                       cumevents=T, cumevents.title="Cumulative number of events", 
                       cumcensor=T, cumcensor.title="Cumulative number of censored observations (incl. competing risks)",
                       risk.table.title = "Number in (% of) sample at risk of main event",
                       font.family=chosenFont, fontsize=2.5,
                       surv.median.line = "hv", size=0.5)

# Add median annotation for illustrative purposes
gsurv_Ft$plot <- gsurv_Ft$plot  +    
  annotate("text", x = medLife, y = 0.5,
           label = paste0("50th Percentile: ", medLife, " months"),
           vjust=-0.5, hjust=-0.1, color = "black", size = 2.5, family = chosenFont) + 
  annotate("text", x=medLife*5, y=0.55,
           label = paste0("RMST (during window of ", tau, " months): ", round(rmst, digits=1), " months"),
           vjust=-0.5, hjust=1, color = "black", size = 2.5, family = chosenFont)
# Add Custom percentile line segment for illustrative purposes   
gsurv_Ft$plot <- gsurv_Ft$plot  +    
  geom_segment(x = 0, xend=percTimepoint, y=1-survPercentile, yend = 1-survPercentile, linetype = "dashed", color = "black") +
  geom_segment(x = percTimepoint, xend=percTimepoint, y=0, yend = 1-survPercentile, linetype = "dashed", color = "black") +
  annotate("text", x = percTimepoint, y = 1 - survPercentile, label = paste0(comma((1-survPercentile)*100), "th Percentile: ", round(percTimepoint, 2), " months"),
           vjust=-0.5, hjust=-0.1, color = "black", size = 2.5, family = chosenFont)
### RESULTS: Based on survival analysis, about 100% of the dataset was consumed at t=120; We shall henceforth remove outlines purely
# for graphing purposes
maxPeriod <- 120

# - Save graph
dpi <- 190
ggsave(gsurv_Ft$plot, file=paste0(genFigPath, "Kaplan-Meier/CumulLifetimeProb_", mainEventName,"_SpellLevel_MultiSpell_LatentComp_InclLeftTrunc.png"), width=1200/dpi, height=1000/dpi, dpi=dpi, bg="white")
dpi <- 185 # need to decrease size for risk tables' text
ggsave(print(gsurv_Ft,newpage=F), file=paste0(genFigPath,"Kaplan-Meier/CumulLifetimeProb_", mainEventName,"_SpellLevel_MultiSpell_LatentComp_InclLeftTrunc_RiskTable.png"),width=1200/dpi, height=1200/dpi,dpi=dpi, bg="white")






# - Graphing logic: Limited domain of time to event [maxPeriod]
gsurv_Ft2 <- ggsurvplot(km_All, fun="event", conf.int=T, surv.scale = "percent", legend="none", 
                        break.time.by=round(maxPeriod)/8, palette=vCol,
                        xlab = bquote(Discrete~time~italic(t)*" (months) in spell: Multi-spell"),
                        ylab = bquote(CLD~"["*.(mainEventName)*"]"*~italic(F(t))*": Kaplan-Meier (incl. left-truncation)"),
                        xlim=c(0, maxPeriod), censor=F, 
                        ggtheme = theme_bw(base_family=chosenFont), tables.theme = theme_cleantable(),
                        tables.height=0.10, tables.y.text=F, tables.y.text.col=T, risk.table = "abs_pct", risk.table.pos = "out",
                        cumevents=T, cumevents.title="Cumulative number of events", 
                        cumcensor=T, cumcensor.title="Cumulative number of censored observations (incl. competing risks)",
                        risk.table.title = "Number in (% of) sample at risk of main event",
                        font.family=chosenFont, fontsize=2.5,
                        surv.median.line = "hv", size=0.5)
# Add median annotation for illustrative purposes
gsurv_Ft2$plot <- gsurv_Ft2$plot  +    
  annotate("text", x = medLife, y = 0.51,
           label = paste0("50th Percentile: ", medLife, " months"),
           vjust=-0.5, hjust=1, color = "black", size = 2.5, family = chosenFont) + 
  annotate("text", x=medLife*1.5, y=0.55,
           label = paste0("RMST (during window of ", tau, " months): ", round(rmst, digits=1), " months"),
           vjust=-0.5, hjust=1, color = "black", size = 2.5, family = chosenFont)
# Add Custom percentile line segment for illustrative purposes   
gsurv_Ft2$plot <- gsurv_Ft2$plot  +    
  geom_segment(x = 0, xend=percTimepoint, y=1-survPercentile, yend = 1-survPercentile, linetype = "dashed", color = "black") +
  geom_segment(x = percTimepoint, xend=percTimepoint, y=0, yend = 1-survPercentile, linetype = "dashed", color = "black") +
  annotate("text", x = percTimepoint, y = 1 - survPercentile+0.01, label = paste0(comma((1-survPercentile)*100), "th Percentile: ", round(percTimepoint, 2), " months"),
           vjust=-0.5, hjust=1, color = "black", size = 2.5, family = chosenFont)

# - Save graph
dpi <- 190
ggsave(gsurv_Ft2$plot, file=paste0(genFigPath,"Kaplan-Meier/CumulLifetimeProb_", mainEventName,"_SpellLevel_MultiSpell_LatentComp_InclLeftTrunc_LimPeriod.png"),width=1200/dpi, height=1200/dpi,dpi=dpi, bg="white")
dpi <- 185
ggsave(print(gsurv_Ft2,newpage=F), file=paste0(genFigPath,"Kaplan-Meier/CumulLifetimeProb_", mainEventName,"_SpellLevel_MultiSpell_LatentComp_InclLeftTrunc_RiskTable_LimPeriod.png"),width=1200/dpi, height=1200/dpi,dpi=dpi, bg="white")




# --- Create data and graphing objects
# - Create survival table using surv_summary()
(datSurv <- surv_summary(km_All)) # Survival table
datSurv <- datSurv %>% rename(Time=time, AtRisk_n=`n.risk`, Event_n=`n.event`, Censored_n=`n.censor`, SurvivalProb_KM=`surv`) %>%
  mutate(Hazard_Actual = Event_n/AtRisk_n, Hazard_Actual2 = 1 - SurvivalProb_KM/shift(SurvivalProb_KM,n=1,fill=1)) %>% 
  mutate(Hazard_Actual2 = ifelse(is.na(Hazard_Actual2), 0, Hazard_Actual2)) %>% # Handle NaN-values
  mutate(CHaz = cumsum(Hazard_Actual),
         CHaz2 = -log(SurvivalProb_KM),# Created as a sanity check
         SurvivalProb_KM_Disc = cumprod(1-Hazard_Actual)) %>% # Created as a sanity check
  mutate(EventRate = Hazard_Actual*shift(SurvivalProb_KM, n=1, fill=1)) %>%  # probability mass function f(t)=h(t).S(t-1)
  filter(Event_n > 0 | Censored_n >0) %>% as.data.table()

# - conduct sanity checks
all.equal(datSurv$Hazard_Actual, datSurv$Hazard_Actual2) # Should be TRUE
all.equal(datSurv$SurvivalProb_KM, datSurv$SurvivalProb_KM_Disc) # Should be TRUE
all.equal(km_All$cumhaz, datSurv$CHaz) # Should be TRUE
all.equal(datSurv$CHaz, datSurv$CHaz2)# Mean relative difference: 0.01859292
plot(datSurv$CHaz - datSurv$CHaz2, type="b")
# No real difference here only as the x-axis values increase it is seen that this difference becomes greater
# So less attention should be placed on these values
# - Remove sanity checks
datSurv[, c("SurvivalProb_KM_Disc","CHaz2", "Hazard_Actual2") := NULL]

# - Distributional analyses
# Hazard rate
describe(datSurv$Hazard_Actual); hist(datSurv$Hazard_Actual, breaks="FD")
plot(datSurv$Hazard_Actual, type="b")
# No real difference here only as the x-axis values increase it is seen that this difference becomes greater
# So less attention should be placed on these values
# Event rate
describe(datSurv$EventRate); hist(datSurv$EventRate, breaks="FD")
plot(datSurv$EventRate, type="b")
### RESULTS: Somewhat of a U-shape is seen
### the values increase till roughly 24 months and from that they decrease
### This is intuitive and what is to be expected

# - Save snapshots to disk (zip) for quick disk-based retrieval later
pack.ffdf(paste0(genPath,"datSurv_KM_MultiSpell"), datSurv)



# --- Graphing the hazard rate h(t)

# Fitting Locally Estimated Scatterplot Smoothing (LOESS)
sSpan <- 0.2
smthHazard_Act <- loess(datSurv$Hazard_Actual ~ datSurv[,list(x=1:.N)]$x, span=sSpan)
summary(smthHazard_Act)

# - Render predictions based on fitted smoother, with standard errors for confidence intervals
vPredSmth <- predict(smthHazard_Act, newdata=datSurv, se=T)

# - Add smoothed hazard to graphing object
datSurv[, Hazard_spline := vPredSmth$fit]
alpha <- 0.05 # significance level for confidence intervals
critVal <- qt(1-alpha/2, df=vPredSmth$df) # use t-distribution for small sample sizes (<30)
datSurv[, Hazard_spline_upper := Hazard_spline + critVal*vPredSmth$se.fit]
datSurv[, Hazard_spline_lower := Hazard_spline - critVal*vPredSmth$se.fit]

# - Graphing parameters
vCol2 <- brewer.pal(10, "Paired")[c(10,9)]
vlabel <- c(bquote('LOESS-smoothed [span: '*.(sSpan)*'] hazard'*~italic(h(t))*' with 95% CI'))
datSurv[, Group := "1"] # only necessary for aesthetics when using geom_smooth() during plotting

# - Create main graph 
(gsurv_ht <- ggplot(datSurv[Time <= maxPeriod, ], aes(x = Time)) + theme_minimal() +
    geom_point(aes(y=Hazard_Actual), colour=vCol2[2])+
    geom_line(aes(y = Hazard_Actual, colour = "Empirical", linetype = "Empirical")) +
    geom_line(aes(y = Hazard_spline, colour = "Spline", linetype = "Spline")) +
    geom_ribbon(aes(ymin = Hazard_spline_lower, ymax = Hazard_spline_upper, fill = "Spline 95% CI"),
      alpha = 0.25, colour = NA) +
    labs(y = bquote(plain(Discrete~hazard~~italic(h(t))*" ["*.(mainEventName)*"]"*":  Kaplan-Meier (incl. left-truncation)")),
      x = bquote(Discrete~time~italic(t)*" (months) in spell: Multi-spell")) +
    theme(text = element_text(family = chosenFont), legend.position = "bottom") +
    scale_colour_manual(name = "", values = c("Empirical" = vCol2[2], "Spline" = "black"),
                        breaks = c("Empirical", "Spline")) +
    scale_linetype_manual(name = "", values = c("Empirical" = "solid", "Spline" = "dotted"),
      breaks = c("Empirical", "Spline")) +
    scale_fill_manual(name = "", values = c("Spline 95% CI" = vCol2[1]),
      breaks = c("Spline 95% CI")) +
    guides(fill = guide_legend(override.aes = list(linetype = 0, shape = NA))) +
    scale_y_continuous(breaks = scales::breaks_pretty(), labels = scales::comma) +
    scale_x_continuous(breaks = scales::breaks_pretty(n = 8), labels = scales::comma))
### RESULTS: The hazard appears as expected somewhat of a U-shape being produced here

# - Save plot
dpi <- 180 # reset
ggsave(gsurv_ht, file=paste0(genFigPath, "Kaplan-Meier/DiscreteHazard_", mainEventName,"_SpellLevel_MultiSpell-LatentComp_InclLeftTrunc-AG.png"),
       width=1200/dpi, height=1000/dpi,dpi=dpi, bg="white")



# --- Graphing the event density / probability mass function f(t)

# Fitting Locally Estimated Scatterplot Smoothing (LOESS)
sSpan <- 0.175
smthEventRate_Act <- loess(datSurv$EventRate ~ datSurv[,list(x=1:.N)]$x, span=sSpan)
summary(smthEventRate_Act)

# - Render predictions based on fitted smoother, with standard errors for confidence intervals
vPredSmth <- predict(smthEventRate_Act, newdata=datSurv, se=T)

# - Add smoothed estimate to graphing object
datSurv[, EventRate_spline := vPredSmth$fit]
alpha <- 0.05 # significance level for confidence intervals
critVal <- qt(1-alpha/2, df=vPredSmth$df) # use t-distribution for small sample sizes (<30)
datSurv[, EventRate_spline_upper := EventRate_spline + critVal*vPredSmth$se.fit]
datSurv[, EventRate_spline_lower := EventRate_spline - critVal*vPredSmth$se.fit]

# - Graphing parameters
vCol3 <- brewer.pal(10, "Paired")[c(4,3)]
vlabel <- c(bquote('LOESS-smoothed [span: '*.(sSpan)*'] estimate'*~italic(f(t))*' with 95% CI'))

# - Create main graph 
(gsurv_ft <- ggplot(datSurv[Time <= maxPeriod,], aes(x = Time, y = EventRate)) + theme_minimal() +
    geom_point(aes(colour = "Empirical"), size = 1, show.legend = FALSE) +
    geom_line(aes(colour = "Empirical", linetype = "Empirical")) +
    geom_line(aes(y = EventRate_spline, colour = "Spline", linetype = "Spline")) +
    geom_ribbon(aes(ymin = EventRate_spline_lower, ymax = EventRate_spline_upper, fill = "Spline 95% CI"),
      alpha = 0.15, colour = NA) +
    labs(y = bquote(plain(Event~probability~~italic(f(t))*" ["*.(mainEventName)*"]"*":  Kaplan-Meier (incl. left-truncation)")),
      x = bquote(Discrete~time~italic(t)*" (months) in spell: Multi-spell")) +
    theme(text = element_text(family = chosenFont),legend.position = "bottom") +
    scale_colour_manual(name = "",values = c("Empirical" = vCol3[2], "Spline" = "black"),
      breaks = c("Empirical", "Spline")) +
    scale_linetype_manual(name = "", values = c("Empirical" = "solid", "Spline" = "dotted"),
      breaks = c("Empirical", "Spline")) +
    scale_fill_manual(name = "", values = c("Spline 95% CI" = vCol3[1]),
      breaks = c("Spline 95% CI")) +
    guides(fill = guide_legend(override.aes = list(linetype = 0, shape = NA))) +
    scale_y_continuous(breaks = scales::breaks_pretty(), labels = scales::percent) +
    scale_x_continuous(breaks = scales::breaks_pretty(n = 8), labels = scales::comma)
)

# - Save plot
dpi <- 180 # reset
ggsave(gsurv_ft, file=paste0(genFigPath, "Kaplan-Meier/EventProb-", mainEventName,"_SpellLevel_MultiSpell_LatentComp_InclLeftTrunc.png"),
       width=1200/dpi, height=1000/dpi,dpi=dpi, bg="white")





# -------- 2. Fit competing risks non-parametric survival model using Aalen-Johansen estimator of S(t)

# - Create multinomial event type variable
### NOTE: Semantically, the spell periods preceding the last record are not "censored", even though they are
# labelled as such. This is merely to fit the paradigm of the counting-process of the Surv() function 
# within a multistate setup, which requires the event indicator at the end of each interval, where 0 (or "censored")
# indicates zero events during said interval.
datCredit_real[, DefSpell_EventType := 
                 case_when(TimeInDefSpell==DefSpell_Age & DefSpellResol_Type_Hist == "WOFF" ~ "Write-off",
                           TimeInDefSpell==DefSpell_Age & DefSpellResol_Type_Hist == "Cured" ~ "Cured",
                           TimeInDefSpell==DefSpell_Age & DefSpellResol_Type_Hist == "Censored" ~ "Censored",
                           TimeInDefSpell<DefSpell_Age | DefSpellResol_Type_Hist == "Censored" ~ "Censored",
                           T ~ NA), by=.(DefSpell_Key)]
describe(datCredit_real[!is.na(DefSpell_Key), DefSpell_EventType])

# - Recreate event type as a factor in preparation for Surv() function
datCredit_real[, DefSpell_EventType := 
                 factor(DefSpell_EventType, levels=c("Censored", "Write-off", "Cured"))]

# - Fit Aalen-Johansen estimator for S(t) amidst competing risks
aj_All <- survfit(Surv(time=TimeInDefSpell - 1, time2=TimeInDefSpell, event=DefSpell_EventType) ~ 1,
                  id = DefSpell_Key, data = datCredit_real[!is.na(DefSpell_Key),])
summary(aj_All)

# - Cast state probabilities over time into data object and merge with main object
# Note that these probabilities are equivalent to the cause-specific cumulative incidence functions (CIFs)
datSurv2 <- data.table(Time = aj_All$time, AtRisk_n = aj_All$n.risk[,"(s0)"],
           Event_n_WriteOff = aj_All$n.event[,"Write-off"], Event_n_Cured = aj_All$n.event[,"Cured"],
           Censored_n = aj_All$n.censor[,"(s0)"], SurvivalProb_AJ = aj_All$pstate[,"(s0)"], 
           CIF_WriteOff = aj_All$pstate[,"Write-off"], # Write-off CIF
           CIF_Cured = aj_All$pstate[,"Cured"]) # Cure CIF

# - Create event probabilities for cause g as f_g(t) = F_g(t) - F_g(t-1)
datSurv2[, EventRate_WriteOff := CIF_WriteOff - shift(CIF_WriteOff, n=1, fill=0) ]
datSurv2[, EventRate_Cured := CIF_Cured - shift(CIF_Cured, n=1, fill=0) ]

# - Create cause-specific sub-hazard functions h_1(t) and h_2(t)
datSurv2[, Hazard_WriteOff := Event_n_WriteOff / AtRisk_n]
datSurv2[, Hazard_Cured := Event_n_Cured / AtRisk_n]
datSurv2[, SurvivalProb_Overall := cumprod(1 - Hazard_WriteOff - Hazard_Cured)] # S(t) = 1 - h_1(t) - h_2(t)

# - Create cause-specific survival quantities for comparative purposes
datSurv2[, SurvivalProb_WriteOff := exp(-cumsum(CIF_WriteOff))]
datSurv2[, SurvivalProb_Cured := exp(-cumsum(CIF_Cured))]

# - Recreate event probabilities for cause g as f_g(t) = S(t-1).h_g(t), as a sanity check
datSurv2[, EventRate_WriteOff2 := shift(SurvivalProb_Overall, n=1, fill=1)*Hazard_WriteOff]
datSurv2[, EventRate_Cured2 := shift(SurvivalProb_Overall, n=1, fill=1)*Hazard_Cured]
datSurv2[, EventRate_Any_AJ := shift(SurvivalProb_AJ, n = 1, fill = 1) - SurvivalProb_AJ]

# - Sanity checks
all.equal(datSurv2$EventRate_WriteOff, datSurv2$EventRate_WriteOff2)
all.equal(datSurv2$SurvivalProb_AJ, datSurv2$SurvivalProb_Overall)
all.equal(datSurv2$EventRate_WriteOff + datSurv2$EventRate_Cured, datSurv2$EventRate_Any_AJ)

# - Diagnostic plots
# Write-off event rate
plot(datSurv2$EventRate_WriteOff, type="b")
# Overall survival probability vs cause-specific survival probabilities
plot(datSurv2$SurvivalProb_AJ,type="b")
lines(datSurv2$SurvivalProb_WriteOff, type="b", col="red")
lines(datSurv2$SurvivalProb_Cured, type="b", col="blue")
# Overall event rate vs cause-specific event rate (write-off)
plot(datSurv$EventRate, type="b")
lines(datSurv2$EventRate_WriteOff, type="b", col="red")

# - Save snapshots to disk (zip) for quick disk-based retrieval later
pack.ffdf(paste0(genPath,"datSurv_KM_MultiSpell_CompRisks"), datSurv2)



# --- Graphing the event density / probability mass function f(t)
# Comparison between KM-based classical write-off probability f(t) and f_g(t) for cause g (write-off)

# - Compile data for graphing purposes
datGraph <- rbind(data.table(Time=datSurv$Time, EventRate=datSurv$EventRate, Type="Classical"),
                  data.table(Time=datSurv2$Time, EventRate=datSurv2$EventRate_WriteOff, Type="CompRisks"))

# - Calculate MAE between event rates
datMAE <- datGraph %>%
  subset(Time<=sMaxSpellAge, select=c("Time", "Type", "EventRate")) %>%
  pivot_wider(names_from=Type, values_from=EventRate) %>% as.data.table()
(MAE_eventProbs <- mean(abs(datMAE$Classical - datMAE$CompRisks), na.rm=T))

# - Graphing parameters
vCol4 <- brewer.pal(10, "Paired")[c(4,6)]
vLabel <- c("Classical"=bquote("Write-off Probability (Kaplan-Meier) "*italic(f(t))),
            "CompRisks"=bquote("Write-off Probability (Aalen-Johansen) "*italic(f[1](t))))

# - Create main graph 
(gsurv_ft_1 <- ggplot(datGraph[Time <= maxPeriod,], aes(x = Time, y = EventRate)) + theme_minimal() +
    theme(text = element_text(family = chosenFont),legend.position = "bottom") +
    labs(y = paste0("Write-off probabilities (incl. left-truncation)"),
         x = bquote(Discrete~time~italic(t)*" (months) in spell: Multi-spell")) +
    geom_point(aes(colour = Type, shape=Type), size = 1) +
    geom_line(aes(colour = Type, linetype = Type)) +  
    # Annotations
    annotate("text", y=0.008,x=75, label=paste0("MAE: ", percent(MAE_eventProbs, accuracy=0.0001)), 
             family=chosenFont, size=3) + 
    scale_colour_manual(name="", values=vCol4, labels=vLabel) + 
    scale_linetype_discrete(name="", labels=vLabel) + 
    scale_shape_discrete(name="", labels=vLabel) + 
    scale_y_continuous(breaks = scales::breaks_pretty(), labels = scales::percent) +
    scale_x_continuous(breaks = scales::breaks_pretty(n = 8), labels = scales::comma)
)

# - Save plot
dpi <- 175 # reset
ggsave(gsurv_ft_1, file=paste0(genFigPath, "Kaplan-Meier/EventProb-", mainEventName,"_SpellLevel_MultiSpell_LatentComp_InclLeftTrunc_CompRisks.png"),
       width=1200/dpi, height=1000/dpi,dpi=dpi, bg="white")

# - Housekeeping
rm(gsurv_Ft, gsurv_Ft2, gsurv_ht, gsurv_ft, km_All, median_survival, datSurv, vlabel, 
   smthEventRate_Act, smthHazard_Act, vPredSmth, datCredit_real, datGraph, datSurv2, gsurv_ft_1,
   vLabel, aj_All)
