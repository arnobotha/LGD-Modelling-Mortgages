# ================================= DEFAULT SPELL ANALYSES =========================================
# Analysing the default spells in various ways, including the maximum spell number (histogram),
# the failure time histogram/densities per 
# resolution type, and censoring present over time in default spell
# Histogram and density of resolution over default spell age
# ------------------------------------------------------------------------------------------------------
# PROJECT TITLE: Loss Modelling (LGD) for FNB Mortgages
# SCRIPT AUTHOR(S): Dr Arno Botha (AB), Bernard Scheepers (BS), Marcel Muller (MM) , Mohammed Gabru (MG)
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
#   - Bar chart of proportion of number fo default spells
#   - Line char of write-off rate per spell number
#   - Histogram of censoring rate per spell age
#   - Histogram and density of resolution over default spell age
# ------------------------------------------------------------------------------------------------------




# ----------------- 1. Max spell number ---
# - Confirm prepared datasets are loaded into memory
if (!exists('datCredit_real')) unpack.ffdf(paste0(genPath,"creditdata_final4a"), tempPath)
# - Aggregation to account-level
# NOTE: Assign max conditionally since there are loans that are forever in performing spell and hence will have no 

# information on default spells
datAggr <- datCredit_real[, list(MaxDefNum = ifelse(all(is.na(DefSpell_Num)), 0, 
                                                     max(DefSpell_Num, na.rm=T)) ), by=list(LoanID)]
# - Analysis on Maximum default spell number
describe(datAggr$MaxDefNum); hist(datAggr$MaxDefNum)

# - Rebin
datAggr[, MaxDefNum_Binned := ifelse(MaxDefNum >= 4, 4, MaxDefNum)]
describe(datAggr$MaxDefNum_Binned); hist(datAggr$MaxDefNum_Binned)

# - Aesthetic engineering
chosenFont <- "Cambria"; colPalette <- "BrBG"
datAggr[MaxDefNum_Binned > 0, MaxDefNum_Binned_Total := .N]
totFreq <- datAggr[MaxDefNum_Binned > 0, .N]
datAggr2 <- unique(datAggr[MaxDefNum_Binned > 0, list(MaxDefNum_Binned_Pc = .N / MaxDefNum_Binned_Total,
                                                       MaxDefNum_Binned_Freq = .N), by=list(MaxDefNum_Binned)])
datAggr2[, MaxDefNum_Binned_Pc_labelY := MaxDefNum_Binned_Freq + totFreq*0.005]
vLabelX <- c("1"="1", "2"="2", "3"="3", "4"="4+")
vBreaks <- 1:length(vLabelX)
vCol <- brewer.pal(10, "Paired")

# - Graph
(g1 <- ggplot(datAggr[MaxDefNum_Binned > 0,], aes(x=MaxDefNum_Binned)) + theme_minimal() + 
    theme(text=element_text(family=chosenFont)) + 
    labs(y="Frequency", x="Maximum Number of Default Spells (pre-binned)") + 
    geom_bar(fill = vCol[2]) +
    geom_label(data=datAggr2, aes(y=MaxDefNum_Binned_Pc_labelY, label=paste(percent(MaxDefNum_Binned_Pc, accuracy=0.1))), 
               family=chosenFont, fill = vCol[1]) +
    scale_y_continuous(label=comma) +
    scale_x_continuous(labels=vLabelX, breaks=vBreaks) )

# Save graph
dpi <- 220
ggsave(g1, file=paste0(genFigPath, "DefResolRate_SpellNumber.png"), width=1200/dpi, height=1000/dpi, dpi=dpi, bg="white")

# House keeping
rm(datAggr,datAggr2,g1);gc()




# ----------------- 2. Default resolution rate over time per numbered spell | Cohort-end

# --- Merge datasets together for graphing purposes, subset necessary fields, and rename columns for graphing ease
datGraph <- rbind(datCredit_real[DefSpell_Num==1, list(Date, LoanID, DefSpell_Key, DefSpell_Num, 
                                                        DefSpellResol_Type_Hist, Sample="Spell 1")],
                  datCredit_real[DefSpell_Num==2, list(Date, LoanID, DefSpell_Key, DefSpell_Num, 
                                                        DefSpellResol_Type_Hist, Sample="Spell 2")],
                  datCredit_real[DefSpell_Num==3, list(Date, LoanID, DefSpell_Key, DefSpell_Num, 
                                                        DefSpellResol_Type_Hist, Sample="Spell 3")],
                  datCredit_real[DefSpell_Num>=4, list(Date, LoanID, DefSpell_Key, DefSpell_Num, 
                                                      DefSpellResol_Type_Hist, Sample="Spell 4+")])

# - Creating spell-level min/max date variables as stratifiers
datGraph[, DefSpellDate_End := Date[.N], by=list(Sample, DefSpell_Key)]

# - Setting some aggregation parameters, purely to facilitate graphing aesthetics
StartDte <- min(datCredit_real$Date, na.rm=T)
EndDte <- max(datCredit_real$Date, na.rm=T)
maxDate <- EndDte %m-% months(1)# A post-hoc filter, used for graphing purposes - left as the end of the sampling window
minDate <- StartDte  #+ month(1) # A post-hoc filter, used for graphing purposes - set as one month after the sampling window

# - Fixing to spell entry-time, we aggregate to monthly level and observe the time series up to given point
datAggr_cohorts <- merge(datGraph[Date==DefSpellDate_End, list(Sum_Total = .N), by=list(Sample,Date)],
                         datGraph[Date==DefSpellDate_End, list(Sum_Resol = .N), by=list(Sample,Date,DefSpellResol_Type_Hist)],
                         by=c("Sample", "Date"))[Date >= minDate & Date <= maxDate,]
datAggr_cohorts[, Prop := Sum_Resol/Sum_Total]
datAggr_wo <- datAggr_cohorts[DefSpellResol_Type_Hist == "WOFF"]
datMean <- datAggr_wo[, .(meanProp = mean(Prop, na.rm = TRUE)), by = Sample]
datMean <- datMean[, label:=paste0("TTC mean: ",paste0(round(meanProp*100,2),"%"))]
                                                
# - Graphing parameters
chosenFont <- "Cambria"
vCol <- brewer.pal(9, "Set1")

(g1 <- ggplot(datAggr_cohorts[DefSpellResol_Type_Hist=="WOFF",], aes(x=Date, y=Prop)) + theme_minimal() + 
    labs(x=bquote("Default spell cohorts (ccyymm): stop time "*italic(t[s])), y="Write-off rate (%)") +
    theme(text=element_text(family=chosenFont),legend.position = "bottom",
          axis.text.x=element_text(angle=90), #legend.text=element_text(family=chosenFont), 
          strip.background=element_rect(fill="snow2", colour="snow2"),
          strip.text=element_text(size=8, colour="gray50"), strip.text.y.right=element_text(angle=90)) + 
    # main line graph with overlaid points
    geom_line(aes(colour=Sample, linetype=Sample)) + 
    geom_point(aes(colour=Sample, shape=Sample), size=1) + 
    geom_hline(data = datMean, aes(yintercept = meanProp),
               linetype = "solid", colour = "black") +
    facet_wrap(Sample~., scales = "free", nrow=4, strip.position="right") + 
    # scale options
    scale_colour_manual(name="", values=vCol) + 
    scale_shape_discrete(name="") + scale_linetype_discrete(name="") + 
    scale_y_continuous(breaks=pretty_breaks(), label=percent) + 
    scale_x_date(date_breaks=paste0(6, " month"), date_labels = "%b %Y"))


# - Save graph
dpi <- 200
ggsave(g1, file=paste0(genFigPath, "Write-offRate_SpellNumber.png"), width=1200/dpi, height=1800/dpi, dpi=dpi, bg="white")

# - Cleanup
rm(datAggr_cohorts, datGraph)





# ----------------- 3. Extent of censored spells over spell age

datAggr <- datCredit_real[, list(CensorRate = mean(ifelse(DefSpellResol_Type_Hist=="Censored", 1, 0), na.rm=T)), 
                          by=list(DefSpell_Age)]
datAggr[, Resol_Type := "Right-censoring rate"]

describe(datAggr[DefSpell_Age<=120, CensorRate]); hist(datAggr[DefSpell_Age<=120, CensorRate], breaks="FD")

# - Annotations
sMeanCensor <- mean(datAggr[DefSpell_Age<=120, CensorRate], na.rm=T)

# - Graphing Parameters
chosenFont <- "Cambria"
vCol <- brewer.pal(10, "Paired")[c(2,1)]

(ggOuter <- ggplot(datAggr[DefSpell_Age<=120, ], aes(x=CensorRate)) + theme_minimal() +
    labs(y="Histogram and empirical density", 
         x="Censoring rate per unique spell age") + 
    theme(text=element_text(family=chosenFont),legend.position="inside", 
          legend.position.inside = c(0.2,0.4), 
          strip.background=element_rect(fill="snow2", colour="snow2"),
          strip.text = element_text(size=8, colour="gray50"), strip.text.y.right = element_text(angle=90)) + 
    # Graphs
    geom_histogram(aes(y=after_stat(density), colour=Resol_Type, fill=Resol_Type), position="identity",
                   alpha=0.75, size=0.2, show.legend = F) + 
    geom_density(aes(colour=Resol_Type), linewidth=0.8, show.legend = F) + 
    scale_colour_manual(name="", values=vCol[1]) + 
    scale_fill_manual(name="", values=vCol[2]) + 
    scale_y_continuous(label=comma) + scale_x_continuous(label=percent))


(ggInner <- ggplot(datAggr[DefSpell_Age<=120,], aes(x=DefSpell_Age, y=CensorRate)) + theme_bw() +
    labs(y="Mean censoring rate (%)", x=" Default spell age") + 
    theme(text=element_text(size=12, family="Cambria"),
          #specific for plot-in-plot
          axis.text.y=element_text(margin=unit(c(0,0,0,0), "mm"), size=9),
          axis.text.x=element_text(margin=unit(c(0,0,0,0), "mm"), size=9),
          axis.ticks=element_blank(), #axis.title.x=element_blank(), #axis.title.y=element_blank(),
          panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
          panel.background = element_rect(color="black", fill="white"),
          plot.background = element_rect(color="white"), plot.margin = unit(c(0,0,0,0),"mm"),
          plot.title = element_text(hjust=0.55,vjust=-10,margin=margin(t=-12))) + 
    geom_line(aes(colour=Resol_Type), show.legend = F) + 
    geom_point(aes(colour=Resol_Type), show.legend=F, size= 0.5) + 
    geom_hline(aes(colour=Resol_Type), linewidth=0.6, yintercept=sMeanCensor) +
    annotate(geom="text", label=paste0("Mean: ", percent(sMeanCensor,accuracy=0.01)),
             x=75, y=0.6,family=chosenFont, size=4) + 
    scale_colour_manual(name="", values=vCol[1]) + 
    scale_y_continuous(label=percent))


# - Combining the two above plots onto a single graph
ymin <- diff(ggplot_build(ggOuter)$layout$panel_params[[1]]$y.range) * 0.45
ymax <- max(ggplot_build(ggOuter)$layout$panel_params[[1]]$y.range) * 0.975
(plot.full <- ggOuter + annotation_custom(grob = ggplotGrob(ggInner), xmin=0.45, xmax=0.875, ymin=ymin, ymax=ymax))

# - Save plot
dpi <- 170
ggsave(plot.full, file=paste0(genFigPath,"CensoringRate_DefaultSpells-HistogramDensity.png"),width=1350/dpi, height=1000/dpi,dpi=dpi, bg="white")




# ----------------- 4. Failure time histogram & densities per resolution type


# ------Preliminaries

# - Select one record per default spell
datSurv <- subset(datCredit_real, DefSpell_Counter==1 & DefSpell_Age < 500,
                  select=c("LoanID","DefSpell_Key","DefSpell_Age", "DefSpellResol_Type_Hist", "DefSpell_Num", "TimeInDefSpell")); gc()

# - Group competing risks together in Resol_Type
datSurv[,Resol_Type := case_when(DefSpellResol_Type_Hist == "WOFF" ~ "a_Written-off",
                                 DefSpellResol_Type_Hist == "Censored" ~ "b_Right-censored",
                                 DefSpellResol_Type_Hist == "Cured" ~ "c_Cured")]
# [SANITY CHECK] Frequency analysis
datSurv$DefSpellResol_Type_Hist %>% table() %>% prop.table()
(Resol_Type.props <- datSurv$Resol_Type %>% table() %>% prop.table())


# - Graphing Parameters
chosenFont <- "Cambria"
vCol <- brewer.pal(10, "Paired")[c(10,6,4, 2,1,8)]
vLabels <- c(paste0("a_Written-off"="Written-off (", round(Resol_Type.props[1]*100, digits=1), "%)"), # Need to round to the first decimal place to ensure that the prior add up to one
             paste0("b_Right-censored"="Right-censored (", round(Resol_Type.props[2]*100, digits=1), "%)"),
             paste0("c_Cured"="Cured (", round(Resol_Type.props[3]*100, digits=1), "%)"))

# - Densities of resolution types overlaid
(g1_Densities_Resol_Type <- ggplot(datSurv[DefSpell_Age<=120,], aes(x=DefSpell_Age, group=Resol_Type)) + theme_minimal() + 
    labs(y=bquote(plain('Empirical histogram & density ')), 
         x=bquote("Default spell age (months)"*~italic(t))) + 
    theme(text=element_text(family=chosenFont),legend.position.inside=c(0.785,0.2), 
          strip.background=element_rect(fill="snow2", colour="snow2"),
          strip.text = element_text(size=8, colour="gray50"), strip.text.y.right = element_text(angle=90),
          legend.position=c(0.75,0.20)) + 
    # Graphs
    geom_histogram(aes(y=after_stat(density), colour=Resol_Type, fill=Resol_Type), position="identity",
                   alpha=0.75, linewidth=0.2) + 
    geom_density(aes(colour=Resol_Type, linetype=Resol_Type), linewidth=0.8) + 
    # facets & scale options
    scale_colour_manual(name=bquote("Resolution Type"*~italic(R)), values=vCol, labels=vLabels) + 
    scale_fill_manual(name=bquote("Resolution Type"*~italic(R)), values=vCol, labels=vLabels) + 
    scale_linetype_manual(name=bquote("Resolution Type"*~italic(R)), values=c("solid","dashed", "dotted"), labels=vLabels) + 
    scale_y_continuous(breaks=breaks_pretty(), label=comma) + 
    scale_x_continuous(breaks=breaks_pretty(), label=comma)
)

# - Save plot
dpi <- 170
ggsave(g1_Densities_Resol_Type, file=paste0(genFigPath,"WOff-FailureTime-Densities.png"),width=1350/dpi, height=1000/dpi,dpi=dpi, bg="white")

# - Clean-up
rm(datSurv, g1_Densities_Resol_Type)







