### Assume script 5d has been interactively run up to before all graphs

# ---- 1 Random thoughts
# At each reporting date, we note that a spectrum of spell periods exist
hist(datCredit[Date==as.Date("2016-01-31"),TimeInDefSpell])

# -- Idea 1: Re-use survival quantities from KM-analyses as empirical target
# Load KM-estimator of survival probability and related quantities
if (!exists('datSurv')) unpack.ffdf(paste0(genPath,"datSurv_KM_MultiSpell"), tempPath); gc()

# Plot again the hazard and event rates for inspiration
plot(datSurv[Time<=84, EventRate], type="b", col="red")
plot(datSurv[Time<=84, Hazard_Actual], type="b", col="blue")

# Merge empirical survival quantities back onto credit dataset
datCredit <- merge(datCredit, datSurv[,.(TimeInDefSpell=Time, EventRate_emp=EventRate,
                                         Survival_KM = SurvivalProb_KM, Hazard_KM = Hazard_Actual)],
                   by="TimeInDefSpell", all.x=T)


# -- Idea 2: Calculate a 3-month rolling future write-off probability based on CIFs
# - Calculate empirical CIF (KM-based)
datCredit[, CIF_KM := cumsum(EventRate_emp), by=.(DefSpell_Key)]

# Create a lookup on which to develop the idea
lookup <- subset(datCredit, DefSpell_Key == unique(datCredit[DefSpellResol_Type_Hist=="WOFF", DefSpell_Key])[1],
                 select=c("DefSpell_Key", "Date", "DefSpell_Counter", "WOff_Ind",
                          "Survival_KM", "Survival_adv", "Survival_bas",
                          "EventRate_emp", "EventRate_adv", "EventRate_bas",
                          "CIF_adv", "CIF_bas", "CIF_KM"))

sW <- 3 # w-month window over which the write-off rate is calculated
lookup[, WOffProb_3mo_adv := condFutureProb(DefSpell_Counter, Survival_adv, CIF_adv, n=sW), by=.(DefSpell_Key)]
lookup[, WOffProb_3mo_bas := condFutureProb(DefSpell_Counter, Survival_bas, CIF_bas, n=sW), by=.(DefSpell_Key)]
lookup[, WOffProb_3mo_KM := condFutureProb(DefSpell_Counter, Survival_KM, CIF_KM, n=sW), by=.(DefSpell_Key)]

# - Apply idea onto greater dataset
datCredit[, WOffProb_3mo_adv := condFutureProb(DefSpell_Counter, Survival_adv, CIF_adv, n=sW), by=.(DefSpell_Key)]
datCredit[, WOffProb_3mo_bas := condFutureProb(DefSpell_Counter, Survival_bas, CIF_bas, n=sW), by=.(DefSpell_Key)]
datCredit[, WOffProb_3mo_KM := condFutureProb(DefSpell_Counter, Survival_KM, CIF_KM, n=sW), by=.(DefSpell_Key)]


# ---- 2 Graphing
# - Aggregate data by reporting date
datPlot <- datCredit[DefSpellResol_Type_Hist!="Censored",.(Empirical=mean(WOffProb_3mo_KM, na.rm=T),
                        Basic_DtH=mean(WOffProb_3mo_bas, na.rm=T),
                        Advanced_DtH=mean(WOffProb_3mo_adv, na.rm=T)),
                     by=.(Date)]

# - Pivot data to facilitate grapging
datPlot <- datPlot %>% pivot_longer(cols=c("Empirical", "Basic_DtH", "Advanced_DtH"),
                                    values_to="Rate", names_to="Model") %>% data.table() %>% setorder(Date)

# - Refactor levels
datPlot[,Model:=case_when(Model=="Empirical"~"a_Empirical", Model=="Basic_DtH"~"b_Basic_DtH",
                          Model=="Advanced_DtH"~"c_Advanced_DtH", Model=="SurvTree"~"d_SurvTree",
                          Model=="Mult"~"e_Mult", Model=="MultSurvTree"~"f_MultSurvTree")]

# - Graphing parameters
chosenFont <- "Cambria"; dpi <- 180
vCol <- brewer.pal(8, "Dark2")[c(2, 1, 3, 5, 7, 4)]
vLabel <- c("a_Empirical"=bquote(italic(A[t])~": Empirical"), "b_Basic_DtH"=bquote(italic(B[t])~": DtH-Basic"),
            "c_Advanced_DtH"=bquote(italic(C[t])~": DtH-Advanced"), "d_SurvTree"=bquote(italic(D[t])~": CI-tree"),
            "e_Mult"=bquote(italic(E[t])~": DtH-Multinomial"), 
            "f_MultSurvTree"=bquote(italic(F[t])~": Multinomial CI-tree"))
date_forecast_start <- max(datCredit[Sample=="Train",Date])

# - Plot
(g <- ggplot(datPlot, aes(x=Date, y=Rate, group=Model)) + theme_minimal() +
  labs(y=bquote("3mo write-off rate (%) "), x=bquote("Reporting date (mmmccyy)")) + 
  theme(text=element_text(family=chosenFont), legend.position="bottom",
        axis.text.x=element_text(angle=90), 
        strip.background=element_rect(fill="snow2", colour="snow2"),
        strip.text=element_text(size=11, colour="gray50"), strip.text.y.right=element_text(angle=90)) + 
  # Main graph
  geom_line(aes(colour=Model, linetype=Model)) +
  geom_point(aes(colour=Model, shape=Model)) + 
  # Forecasting period
  geom_vline(aes(xintercept=max(datCredit[Sample=="Train",Date])), linetype="dashed", colour="black") +
  annotate(geom="text", x=date_forecast_start, y=Inf, label="Forecast start", angle=90, vjust=-0.5, hjust=1,
           size=3, alpha=0.5, family=chosenFont) +
  # Facets & scale options
  scale_colour_manual(name="Model", values=vCol, labels=vLabel) +
  scale_fill_manual(name="Model", values=vCol, labels=vLabel) +
  scale_shape_discrete(name="Model", labels=vLabel) +
  scale_linetype_discrete(name="Model", labels=vLabel) +
  scale_x_date(date_breaks="12 month", date_labels="%b %Y") +
  scale_y_continuous(breaks=pretty_breaks(), labels=percent) + 
  guides(colour=guide_legend(nrow=2, byrow=T)))


dpi <-180
ggsave(g, file=paste0(genFigPath, "3Month-Future-WriteOffRate_ReportingDate_ExCensored.png"),
       width=1600/dpi, height=1000/dpi, dpi=dpi, bg="white")


### LOG:
# 1) Graphing the average event rate (KM-based) seemed feasible, and the rate reacted somewhat to 
#    macroeconomic events. However, a very large divergence exists between the empirical and the DtH-Advanced's
#    line, which disagrees with the narrative supported by the other diagnostics that it is an excellent model.
# 2) Slotting in the survival probabilities further worsened the view, with the DtH-Basic seeimingly outperforming
#    DtH-Advanced massively, which cannot be true.
# 3) Slotting in the hazard gave similar view than the event rate, which is to be expected. But still infeasible since
#    similarly shows DtH-Basic to be the better model. Slightly worse view than using event rates
# 4) Since prior analysis showed the distribution of event rates to be heavily skewed, I opted for the median.
#    But this worsened the view even more, with the DtH-Advanced's output staying near zero.
# 5) Calculating the 3-month future write-off probability (q hazard-type quantity used in LGD-distributions)
#    gave a feasible empirical rate. However, and as before, the DtH-Advanced model performed significantly
#    poorer than the DtH-Basic model, which doesn't make sense.