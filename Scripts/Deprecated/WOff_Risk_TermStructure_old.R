# ============================== DEPRECATED GRAPHING LOGIC ==============================
# Legacy code from script 5a(i) "WOff_Risk_TermStructure"


# ============ 3. Preparing the graphing datasets ============

# --- 3.1 Preliminaries
# - Cut-offs for analysis and graphs 
sMaxSpellAge <- 120 # Max for [DefSpell_Age], as determined in earlier analyses (script 3c)
sMaxSpellAge_graph <- 120 # Max for [DefSpell_Age] for graphing purposes

# - Fit natural cubic regression splines
# Knots
sDf_Act <- 12; sDf_Bas <- 12; 
sDf_Adv <- 12; sDf_Clas <- 2; 
sDf_SurvTree <- 12
# Actuals
smthEventRate_act <- lm(EventRate~ns(Time, df=sDf_Act), data=datSurv[Time<=sMaxSpellAge])
# Basic model
smthEventRate_Exp_bas <- lm(EventRate_bas~ns(TimeInDefSpell, df=sDf_Bas), data=datSurv_exp[TimeInDefSpell<=sMaxSpellAge])
# Advanced model
smthEventRate_Exp_adv <- lm(EventRate_adv~ns(TimeInDefSpell, df=sDf_Adv), data=datSurv_exp[TimeInDefSpell<=sMaxSpellAge])
# Classical model
smthEventRate_Exp_classic <- lm(EventRate_classic~ns(TimeInDefSpell, df=sDf_Clas), data=datSurv_exp[TimeInDefSpell<=sMaxSpellAge])
# Survival tree
smthEventRate_Exp_SurvTree <- lm(EventRate_survtree~ns(TimeInDefSpell, df=sDf_SurvTree), data=datSurv_exp[TimeInDefSpell<=sMaxSpellAge])

# - Render predictions based on fitted smoother, with standard errors for confidence intervals
# Actuals
vPredSmth_act <- predict(smthEventRate_act, newdata=datSurv, se=T)
# Basic model
vPredSmth_Exp_bas <- predict(smthEventRate_Exp_bas, newdata=datSurv_exp, se=T)
# Advanced model
vPredSmth_Exp_adv <- predict(smthEventRate_Exp_adv, newdata=datSurv_exp, se=T)
# Classical model
vPredSmth_Exp_classic <- predict(smthEventRate_Exp_classic, newdata=datSurv_exp, se=T)
# Survival tree
vPredSmth_Exp_SurvTree <- predict(smthEventRate_Exp_SurvTree, newdata=datSurv_exp, se=T)

# - Add smoothed estimate to graphing object
# Actuals
datSurv[, EventRate_spline:=vPredSmth_act$fit]
# Basic model
datSurv_exp[, EventRate_spline_bas:=vPredSmth_Exp_bas$fit]
# Advanced model
datSurv_exp[, EventRate_spline_adv:=vPredSmth_Exp_adv$fit]
# Classical model
datSurv_exp[, EventRate_spline_classic:=vPredSmth_Exp_classic$fit]
# Classical model
datSurv_exp[, EventRate_spline_SurvTree:=vPredSmth_Exp_SurvTree$fit]


# --- 3.2 Create graphing data
# - Initialise graphing data
datGraph <- rbind(datSurv[,list(Time=Time, EventRate=EventRate, Type="a_Actual")],
                  datSurv[,list(Time=Time, EventRate=EventRate_spline, Type="b_Actual_spline")],
                  datSurv_exp[,list(Time=TimeInDefSpell, EventRate=EventRate_bas, Type="c_Expected_bas")],
                  datSurv_exp[,list(Time=TimeInDefSpell, EventRate=EventRate_spline_bas, Type="d_Expected_spline_bas")],
                  datSurv_exp[,list(Time=TimeInDefSpell, EventRate=EventRate_adv, Type="e_Expected_adv")],
                  datSurv_exp[,list(Time=TimeInDefSpell, EventRate=EventRate_spline_adv, Type="f_Expected_spline_adv")],
                  datSurv_exp[,list(Time=TimeInDefSpell, EventRate=EventRate_classic, Type="g_Expected_classic")],
                  datSurv_exp[,list(Time=TimeInDefSpell, EventRate=EventRate_spline_classic, Type="h_Expected_spline_classic")],
                  datSurv_exp[,list(Time=TimeInDefSpell, EventRate=EventRate_survtree, Type="i_Expected_SurvTree")],
                  datSurv_exp[,list(Time=TimeInDefSpell, EventRate=EventRate_spline_SurvTree, Type="j_Expected_spline_SurvTree")]
)

# - Create different groupings for graphing purposes
# Non-splines
datGraph[Type %in% c("a_Actual","c_Expected_bas", "e_Expected_adv", "g_Expected_classic", "i_Expected_SurvTree"),
         EventRatePoint:=EventRate]
# Splines
datGraph[Type %in% c("b_Actual_spline", "d_Expected_spline_bas", "f_Expected_spline_adv", "h_Expected_spline_classic",
                     "j_Expected_spline_SurvTree"),
         EventRateLine:=EventRate]

# - Set facet label
datGraph[, FacetLabel:="Classical term-structure of write-off risk"]

# - Calculate MAE between event rates
datMAE <- datGraph %>%
  subset(Time<=sMaxSpellAge, select=c("Time", "Type", "EventRatePoint")) %>%
  pivot_wider(names_from=Type, values_from=EventRatePoint) %>%
  as.data.table()
# Basic model
(MAE_eventProb_bas <- mean(abs(datMAE$a_Actual-datMAE$c_Expected_bas), na.rm=T))
# Advanced model
(MAE_eventProb_adv <- mean(abs(datMAE$a_Actual-datMAE$e_Expected_adv), na.rm=T))
# Classical model
(MAE_eventProb_classic <- mean(abs(datMAE$a_Actual-datMAE$g_Expected_classic), na.rm=T))
# Survival tree
(MAE_eventProb_SurvTree <- mean(abs(datMAE$a_Actual-datMAE$i_Expected_SurvTree), na.rm=T))




# ============ 4. Graphing the event density / probability mass function f(t) | Best fitting model ============

# --- 4.1 Preliminaries 
# - Subset data
datGraph_main <- subset(datGraph, Type %in% c("a_Actual", "b_Actual_spline", 
                                              "c_Expected_bas", "d_Expected_spline_bas", 
                                              "e_Expected_adv", "f_Expected_spline_adv",
                                              "i_Expected_SurvTree", "j_Expected_spline_SurvTree"))

# - Graphing parameters
vCol <- brewer.pal(12, "Paired")[c(3,4,5,6,1,2,9,10)]
vLabel2 <- c("a_Actual"="Empirical (KM)", "c_Expected_bas"="DtH-Basic", "e_Expected_adv"="DtH-Advanced",
             "i_Expected_SurvTree"="CI-tree",
             "b_Actual_spline"="Spline: Empirical", "d_Expected_spline_bas"="Spline: DtH-Basic",
             "f_Expected_spline_adv"="Spline: DtH-Advanced",
             "j_Expected_spline_SurvTree"="Spline: CI-tree")
vSize <- rep(c(0.2,0.3), length(vLabel2)/2)
vLineType <- rep(c("dashed", "solid"), length(vLabel2)/2)
vShapeType <- c(15,NA,16,NA,17,NA,0,NA,1,NA)
chosenFont <- "Cambria"

# --- 4.2 Create & save graph
# - Graph
(gsurv_ft <- ggplot(datGraph_main[Time <= sMaxSpellAge_graph,], aes(x=Time, y=EventRate, group=Type)) + theme_minimal() +
    labs(y=bquote(plain("Write-off "*probability~~italic(w(t)))), 
         x=bquote("Spell time (months)"*~italic(t))) + 
    theme(text=element_text(family=chosenFont),legend.position = "bottom",
          strip.background=element_rect(fill="snow2", colour="snow2"),
          strip.text=element_text(size=8, colour="gray50"), strip.text.y.right=element_text(angle=90)) + 
    coord_cartesian(ylim=c(0,0.018)) +
    # Main graph
    geom_point(aes(y=EventRatePoint, colour=Type, shape=Type), size=0.6) + 
    geom_line(aes(y=EventRate, colour=Type, linetype=Type, linewidth=Type)) + 
    # Annotations
    annotate("text", y=0.017,x=75, label=paste0("MAE (DtH-Basic): ", percent(MAE_eventProb_bas, accuracy=0.0001)), family=chosenFont, size=3) + 
    annotate("text", y=0.0155,x=75, label=paste0("MAE (DtH-Advanced): ", percent(MAE_eventProb_adv, accuracy=0.0001)), family=chosenFont, size=3) + 
    annotate("text", y=0.0140,x=75, label=paste0("MAE (ST): ", percent(MAE_eventProb_SurvTree, accuracy=0.0001)), family=chosenFont, size=3) +    
    # Scales and options
    facet_grid(FacetLabel~.) + 
    scale_colour_manual(name="", values=vCol, labels=vLabel2) + 
    scale_linewidth_manual(name="", values=vSize, labels=vLabel2) + 
    scale_linetype_manual(name="", values=vLineType, labels=vLabel2) + 
    scale_shape_manual(name="", values=vShapeType, labels=vLabel2) + 
    scale_y_continuous(breaks=breaks_pretty(), label=percent)+ 
    scale_x_continuous(breaks=breaks_pretty(n=8), label=comma) + 
    guides(color=guide_legend(nrow=2))
)

# - Save plot
dpi <- 335 # reset
ggsave(gsurv_ft, file=paste0(genFigPath, "EventProb_Write-Off_ActVsExp_CoxDisc.png"),
       width=2400/dpi, height=1800/dpi,dpi=dpi, bg="white")




# ============ 5. Graphing the event density / probability mass function f(t) | Worst fitting models graph ============

# --- 5.1 Preliminaries
# - Create graphing data object for the two that are out of range
datGraph_OOB <- datGraph %>% subset(Type %in% c("a_Actual", "b_Actual_spline", "g_Expected_classic"))

# - Facet label
datGraph_OOB[, FacetLabel:="Classical term-structure of write-off risk"]


# --- 5.2 Graph of worst fitting models
# - Graphing parameters
vCol <- brewer.pal(12, "Paired")[c(3,4,7)]
vLabel2 <- c("b_Actual_spline"="Spline: Empirical", "a_Actual"="Empirical (KM)",
             "g_Expected_classic"="CS-LR")
vSize <- c(0.2, 0.3, 0.2)
vLineType <- c("dashed", "solid", "dashed")
vShapeType <- c(15, NA, 16)
chosenFont <- "Cambria"
mainEventName <- "Write-off"

# - Create main graph 
(gsurv_ft <- ggplot(datGraph_OOB[Time<=sMaxSpellAge_graph,], aes(x=Time, y=EventRate, group=Type)) + theme_minimal() +
    labs(y=bquote(plain("Write-off probability "*italic(w(t)))), 
         x=bquote("Spelll time (months)"*~italic(t))) + 
    theme(text=element_text(family=chosenFont),legend.position="bottom",
          strip.background=element_rect(fill="snow2", colour="snow2"),
          strip.text=element_text(size=8, colour="gray50"), strip.text.y.right=element_text(angle=90)) + 
    # Main graph
    geom_point(aes(y=EventRatePoint, colour=Type, shape=Type), size=0.6) + 
    geom_line(aes(y=EventRate, colour=Type, linetype=Type, linewidth=Type)) + 
    # Annotations
    annotate("text", y=0.1, x=110, label=paste0("MAE (LR): ", percent(MAE_eventProb_classic, accuracy=0.0001)), family=chosenFont, size=3) +
    # Scales and options
    facet_grid(FacetLabel ~ .) +
    scale_colour_manual(name="", values=vCol, labels=vLabel2) +
    scale_linewidth_manual(name="", values=vSize, labels=vLabel2) +
    scale_linetype_manual(name="", values=vLineType, labels=vLabel2) +
    scale_shape_manual(name="", values=vShapeType, labels=vLabel2) +
    scale_y_continuous(breaks=breaks_pretty(), label=percent) +
    scale_x_continuous(breaks=breaks_pretty(n=8), label=comma) +
    guides(color=guide_legend(nrow=1))
)

# - Create zoomed-in inset graph
(gsurv_ft2 <- ggplot(datGraph_OOB[Time<=sMaxSpellAge_graph & Type %in% c("a_Actual","b_Actual_spline", "g_Expected_classic"),],
                     aes(x=Time, y=EventRate, group=Type)) + theme_minimal() +
    theme_bw() +
    theme(legend.position="none", text=element_text(size=12, family="Cambria"),
          #specific for plot-in-plot
          axis.text.y=element_text(margin=unit(c(0,0,0,0), "mm"), size=9),
          axis.text.x=element_text(margin=unit(c(0,0,0,0), "mm"), size=9),
          axis.ticks=element_blank(), axis.title.x=element_blank(), #axis.title.y=element_blank(),
          panel.grid.major=element_blank(), panel.grid.minor = element_blank(),
          panel.background=element_rect(color="black", fill="white"),
          plot.background=element_rect(color="white"), plot.margin = unit(c(0,0,0,0),"mm"),
          plot.title=element_text(hjust=0.55,vjust=-10)) +
    labs(x=NULL, y=NULL) +
    # Main graph
    geom_point(aes(y=EventRatePoint, colour=Type, shape=Type), size=0.6) + 
    geom_line(aes(y=EventRate, colour=Type, linetype=Type, linewidth=Type)) + 
    # Scales and options
    scale_colour_manual(name="", values=vCol, labels=vLabel2) + 
    scale_linewidth_manual(name="", values=vSize, labels=vLabel2) + 
    scale_linetype_manual(name="", values=vLineType, labels=vLabel2) + 
    scale_shape_manual(name="", values=vShapeType, labels=vLabel2) + 
    scale_y_continuous(breaks=breaks_pretty(), label=percent)+ 
    scale_x_continuous(breaks=breaks_pretty(n=8), label=comma) + 
    guides(color=guide_legend(nrow=2)) +
    coord_cartesian(xlim=c(0,80), ylim=c(0,0.018))
)

# - Combine graphs
(plot.full <- gsurv_ft + annotation_custom(grob = ggplotGrob(gsurv_ft2), xmin=10, xmax=90, 
                                           ymin=0.05, ymax=0.125))

# - Save plot
dpi <- 335 # reset
ggsave(plot.full, file=paste0(genFigPath, "EventProb_Write-off_ActVsExp_CoxDisc_OOB.png"),
       width=2400/dpi, height=1800/dpi,dpi=dpi, bg="white")