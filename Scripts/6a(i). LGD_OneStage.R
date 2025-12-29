# ================= LGD DISTRIBUTION ANALYSIS - ONE-STAGE MODELS ===============
# Comparing actual LGDs with expected LGDs from the fitted one-stage models
# ------------------------------------------------------------------------------
# PROJECT TITLE: Loss Modelling (LGD) for Residential Mortgages
# SCRIPT AUTHOR(S): Mohammed Gabru (MG), Marcel Muller (MM), Dr Arno Botha (AB)
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
#   - 4a(i).InputSpace_SingleStage_Gaussian.R
#   - 4a(ii).InputSpace_SingleStage_CPG.R
# -- Inputs:
#   - datCredit_train_CDH | Prepared from script 2g
#   - datCredit_valid_CDH | Prepared from script 2g
#   - modGLM_OneStage_Gaus | Single stage GLM model with a Gaussian link function as fitted in script 4a(i)
#   - modGLM_OneStage_CPG | Single stage GLM model with a Tweedie link function as fitted in script 4a(ii)
# -- Outputs:
#   - <Analytics> | Graphs
# ==============================================================================


# ------ 1. Preliminaries

# --- 1.1 Load and prepare datasets
# - Confirm prepared datasets are loaded into memory
if (!exists('datCredit_train_CDH')) unpack.ffdf(paste0(genPath,"creditdata_train_CDH"), tempPath);gc()
if (!exists('datCredit_valid_CDH')) unpack.ffdf(paste0(genPath,"creditdata_valid_CDH"), tempPath);gc()

# - Filter to maximum spell counter
datCredit_train <- datCredit_train_CDH[, .SD[which.max(DefSpell_Counter)], by=LoanID]
datCredit_valid <- datCredit_valid_CDH[, .SD[which.max(DefSpell_Counter)], by=LoanID]

# - Identify where the loss rate is out of bounds and not feasible
datCredit_train <- datCredit_train[, OOB_Ind:=ifelse(LossRate_Real<0 | LossRate_Real>1,1,0)]
datCredit_valid <- datCredit_valid[, OOB_Ind:=ifelse(LossRate_Real<0 | LossRate_Real>1,1,0)]

# - Subset to include only relevant data
datCredit_train <- subset(datCredit_train, OOB_Ind==0)
datCredit_valid <- subset(datCredit_valid, OOB_Ind==0)

# - Remove unfiltered data
rm(datCredit_train_CDH, datCredit_valid_CDH); gc()

# - Combine training and validation datasets to facilitate "better" (smooth) graphs
datCredit <- rbind(datCredit_train, datCredit_valid)

# - Subset for write-offs only to create inset plots
datCredit_WOFFs <- subset(datCredit, DefSpellResol_Type_Hist=="WOFF")


# --- 1.2 Load models
# - GLM with Gaussian link function
modGLM_OneStage_Gaus <- readRDS(paste0(genObjPath,"OneStage_Gaus_Model.rds"))

# - GLM with Tweedie (Compound Poisson Gaussian) link function
modGLM_OneStage_CPG <- readRDS(paste0(genObjPath,"OneStage_CPH_Model.rds"))



# ------ 2. Actual LGDs

# --- 2.1 Create graphs
# - Graphing parameters
chosenFont <- "Cambria"
vCol <- brewer.pal(10, "Paired")[c(8,6)]

# - Aesthetic engineering: Statistical Summaries
(meanLoss_TruEnd <- mean(datCredit$LossRate_Real, na.rm=T))
(MeanLoss_TruEnd_W <- mean(datCredit_WOFFs$LossRate_Real, na.rm=T))
(mix_WC_TruEnd <- datCredit_WOFFs[, .N] / datCredit[, .N] )# overall write-off probability given default of 18%

# - Overall LGD distribution
(g1 <- ggplot(datCredit, aes(x=LossRate_Real)) + theme_bw() +
    geom_histogram(aes(y=after_stat(density)), alpha=0.4, bins=round(2*datCredit[,.N]^(1/3)),
                   position="identity", fill=vCol[1], colour=vCol[1]) + 
    geom_density(linewidth=1, colour=vCol[1], linetype="dotted") + 
    geom_vline(xintercept=meanLoss_TruEnd, linewidth=0.6, colour=vCol[1], linetype="dashed") + 
    annotate(geom="text", x=meanLoss_TruEnd*0.8, y=20, family=chosenFont,
             label = paste0("Mean Loss: ", sprintf("%.1f", meanLoss_TruEnd*100), "%"), size=3, colour=vCol[1], angle=90) +     
    # facets & scale options
    labs(x=bquote({Realised~loss~rate~italic(L)}), 
         y="Histogram and density of resolved defaults [cures/write-offs]") + 
    theme(text=element_text(family=chosenFont),legend.position="bottom",
          strip.background=element_rect(fill="snow2", colour="snow2"),
          strip.text = element_text(size=8, colour="gray50"), 
          strip.text.y.right = element_text(angle=90)) + 
    scale_x_continuous(breaks=pretty_breaks(), label=percent)
)

# - Inset graph for write-offs only
(g2 <- ggplot(datCredit_WOFFs, aes(x=LossRate_Real)) + theme_bw() +
    geom_histogram(aes(y=after_stat(density)), alpha=0.4, bins=round(2*datCredit_WOFFs[,.N]^(1/3)), 
                   position="identity", fill=vCol[2], colour=vCol[2]) + 
    geom_density(linewidth=1, colour=vCol[2], linetype="dotted") + 
    geom_vline(xintercept=MeanLoss_TruEnd_W, linewidth=0.6, colour=vCol[2], linetype="dashed") + 
    annotate(geom="text", x=MeanLoss_TruEnd_W*0.93,  y=2.6, family=chosenFont,
             label = paste0("Mean Loss: ", sprintf("%.1f", MeanLoss_TruEnd_W*100), "%"), size=3, colour=vCol[2], angle=90) +     
    # facets & scale options
    labs(x="", y="", title=paste0("Write-offs only (", sprintf("%.0f", mix_WC_TruEnd*100), "%)")) + 
    theme(legend.position="none", text=element_text(size=12, family="Cambria"),
          #specific for plot-in-plot
          axis.text.y=element_text(margin=unit(c(0,0,0,0), "mm"), size=9),
          axis.text.x=element_text(margin=unit(c(0,0,0,0), "mm"), size=9),
          axis.ticks=element_blank(), axis.title.x=element_blank(), #axis.title.y=element_blank(),
          panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
          panel.background = element_rect(color="black", fill="white"),
          plot.background = element_rect(color="white"), plot.margin = unit(c(0,0,0,0),"mm"),
          plot.title = element_text(hjust=0.55,vjust=-10,margin=margin(t=-12))
    ) + 
    scale_x_continuous(breaks=pretty_breaks(), label=percent)
)


# --- 2.2 Combine and save graphs
# - Combine graphs
ymin <- diff(ggplot_build(g1)$layout$panel_params[[1]]$y.range) * 0.2
ymax <- max(ggplot_build(g1)$layout$panel_params[[1]]$y.range) * 0.95
(plot.full <- g1 + annotation_custom(grob = ggplotGrob(g2), xmin=0.1, xmax=0.9, ymin=ymin, ymax=ymax))

# - Save plot
dpi <- 180
ggsave(plot.full, file=paste0(genFigPath,"/Actual_LGD.png"),width=1200/dpi, height=1000/dpi,dpi=dpi, bg="white")




# ------ 3. LGDs from Gaussian model

# --- 3.1 Gaussian LGDs only
# - Render predictions with model
datCredit[, LossRate_Gaussian:=predict(modGLM_OneStage_Gaus, newdata=datCredit,type="response")]

# - Impose a logical floor and ceiling of 0 and 1 to the predicted loss rates
datCredit_WOFFs[, LossRate_Gaussian:=ifelse(LossRate_Gaussian>1,1,LossRate_Gaussian)]
datCredit_WOFFs[, LossRate_Gaussian:=ifelse(LossRate_Gaussian<0,0,LossRate_Gaussian)]

# - Estimate mean expected loss rate
meanLoss_TruEnd_gaussian <- mean(datCredit$LossRate_Gaussian, na.rm=T)
### RESULTS: Mean = 

# - Plot
(g1b <- ggplot(datCredit, aes(x=LossRate_Gaussian)) + theme_bw() +
    geom_histogram(aes(y=after_stat(density)), alpha=0.4, bins=round(2*datCredit[,.N]^(1/3)),
                   position="identity", fill=vCol[1], colour=vCol[1]) + 
    geom_density(linewidth=1, colour=vCol[1], linetype="dotted") + 
    geom_vline(xintercept=meanLoss_TruEnd_gaussian, linewidth=0.6, colour=vCol[1], linetype="dashed") + 
    annotate(geom="text", x=meanLoss_TruEnd_gaussian*0.8, y=5, family=chosenFont,
             label = paste0("Mean Loss: ", sprintf("%.1f", meanLoss_TruEnd_gaussian*100), "%"), size=3, colour=vCol[1], angle=90) +     
    # facets & scale options
    labs(x=bquote({Realised~loss~rate~italic(L)}), 
         y="Histogram and density of resolved defaults [cures/write-offs]") + 
    theme(text=element_text(family=chosenFont),legend.position="bottom",
          strip.background=element_rect(fill="snow2", colour="snow2"),
          strip.text = element_text(size=8, colour="gray50"), 
          strip.text.y.right = element_text(angle=90)) + 
    scale_x_continuous(breaks=pretty_breaks(), label=percent)
)


# --- 3.2 Compare overall expected LGDs with actuals
# - Estimate statistics on distributional diffirences
metrics <- evalModel_onestage(datCredit,"LossRate_Real", "LossRate_Gaussian", "gaussian", modGLM_OneStage_Gaus)

# - Combine statistics
stats_text <- paste("KS: ", sprintf("%.1f%%", metrics$KS * 100), "\n",
                    "KL: ", sprintf("%.4f", metrics$KL), "\n",
                    "JS: ", sprintf("%.4f", metrics$JS), 
                    sep="")

# - Create plotting data
plotData <- melt(datCredit, measure.vars=c("LossRate_Real", "LossRate_Gaussian"),
                 variable.name="Type",value.name="LossRate")
plotData[, Type:=factor(Type,levels=c("LossRate_Real", "LossRate_Gaussian"),
                        labels=c("Empirical", "Gaussian GLM"))]
plotData[, FacetLabel:="Resolved defaults [cures/write-offs]"]

# - Plot
(gOverlay <- ggplot(plotData, aes(x=LossRate)) + 
    theme_bw() + geom_histogram(
      aes(y=after_stat(density), fill=Type, colour=Type),
      alpha=0.35, bins=round(2*datCredit[, .N]^(1/3)), position="identity") +
    labs(x=bquote({Realised~loss~rate~italic(L)}), y="Histogram of loss rates" ) +
    theme(text=element_text(family=chosenFont),legend.position="bottom",
          strip.background=element_rect(fill="snow2", colour="snow2"),
          strip.text=element_text(size=8, colour="gray50"),
          strip.placement="outside",        
          strip.text.y.right=element_text(angle=90)) +
    scale_x_continuous(breaks=pretty_breaks(), labels=scales::percent) +
    scale_colour_manual(values=c(vCol[1], vCol[2])) +
    scale_fill_manual(values=c(vCol[1], vCol[2])) +
    facet_grid(FacetLabel ~., scales="free")+
    guides(fill=guide_legend(title=NULL),
           colour=guide_legend(title=NULL)))


# --- 3.3 Compare expected write-off LGDs with actuals
# - Render predictions with model
datCredit_WOFFs[, LossRate_Gaussian:=predict(modGLM_OneStage_Gaus, newdata=datCredit_WOFFs,type="response")]

# - Impose a logical floor and ceiling of 0 and 1 to the predicted loss rates
datCredit_WOFFs[, LossRate_Gaussian:=ifelse(LossRate_Gaussian>1,1,LossRate_Gaussian)]
datCredit_WOFFs[, LossRate_Gaussian:=ifelse(LossRate_Gaussian<0,0,LossRate_Gaussian)]

# - Create plotting data
plotData <- melt(datCredit_WOFFs, measure.vars=c("LossRate_Real", "LossRate_Gaussian"),
                 variable.name="Type", value.name="LossRate")
plotData[, Type:=factor(Type, levels=c("LossRate_Real", "LossRate_Gaussian"),
                        labels=c("Actual loss rate", "Gaussian GLM"))]
plotData[, FacetLabel:="Resolved defaults [cures/write-offs]"]

# - Plot
((gOverlay_WOFF <- ggplot(plotData, aes(x=LossRate)) + 
    theme_bw() +
    geom_histogram(aes(y=after_stat(density), fill=Type, colour=Type),alpha = 0.35,
                   bins=round(2*datCredit_WOFFs[,.N]^(1/3)), position="identity") +
    theme(legend.position="none",text = element_text(size = 12, family = chosenFont),
          axis.text.y=element_text(size=9, margin=unit(c(0,0,0,0),"mm")),
          axis.text.x=element_text(size=9, margin=unit(c(0,0,0,0),"mm")),
          axis.title.x=element_blank(), axis.title.y=element_blank(),
          axis.ticks=element_blank(), panel.grid.major=element_blank(),
          panel.grid.minor=element_blank(), panel.background=element_rect(color="black", fill="white"),
          plot.background=element_rect(color="white"), plot.margin=unit(c(0,0,0,0),"mm"),
          strip.background=element_rect(fill="snow2", colour="snow2"),
          strip.text = element_text(size=8, colour="gray50"), strip.text.y.right = element_text(angle=90)) +
    annotate("label", x=0.7, y=30 , label=stats_text,
             hjust=0, vjust=1, family=chosenFont,
             size=4, fill="white", colour="black", label.size = 0.5) +
    labs(x="", y="", title=paste0("Write-offs only")) +
    scale_x_continuous(breaks=pretty_breaks(), labels=scales::percent) +
    scale_colour_manual(values=c(vCol[1], vCol[2])) +
    scale_fill_manual(values=c(vCol[1], vCol[2]))))


# --- 3.4 Combine and save graphs
# - Combine graphs
ymin <- diff(ggplot_build(gOverlay)$layout$panel_params[[1]]$y.range) * 0.2
ymax <- max(ggplot_build(gOverlay)$layout$panel_params[[1]]$y.range) * 0.95
(plot.full <- gOverlay + annotation_custom(grob = ggplotGrob(gOverlay_WOFF), xmin=0.2, xmax=1, ymin=ymin, ymax=ymax))

# - Save plot
dpi <- 180
ggsave(plot.full, file=paste0(genFigPath,"/ActvsExp_onestage_gaussian.png"),width=1200/dpi, height=1000/dpi,dpi=dpi, bg="white")




# ------ 4. LGDs from Tweedie model

# --- 4.1 Tweedie LGDs only
# - Render predictions with model
datCredit[, LossRate_Tweedie:=predict(modGLM_OneStage_CPG, newdata=datCredit,type="response")]

# - Impose a logical floor and ceiling of 0 and 1 to the predicted loss rates
datCredit[, LossRate_Tweedie:=ifelse(LossRate_Tweedie>1,1,LossRate_Tweedie)]
datCredit[, LossRate_Tweedie:=ifelse(LossRate_Tweedie<0,0,LossRate_Tweedie)]

# - Estimate mean expected loss rate
(meanLoss_TruEnd_tweedie <- mean(datCredit$LossRate_Tweedie, na.rm=T))
### RESULTS: Mean=0.08285599

# - Plot
(g1a <- ggplot(datCredit, aes(x=LossRate_Tweedie)) + theme_bw() +
    geom_histogram(aes(y=after_stat(density)), alpha=0.4, bins=round(2*datCredit[,.N]^(1/3)),
                   position="identity", fill=vCol[1], colour=vCol[1]) + 
    geom_density(linewidth=1, colour=vCol[1], linetype="dotted") + 
    geom_vline(xintercept=meanLoss_TruEnd_tweedie, linewidth=0.6, colour=vCol[1], linetype="dashed") + 
    annotate(geom="text", x=meanLoss_TruEnd_tweedie*0.8, y=20, family=chosenFont,
             label = paste0("Mean Loss: ", sprintf("%.1f", meanLoss_TruEnd_tweedie*100), "%"), size=3, colour=vCol[1], angle=90) +     
    # facets & scale options
    labs(x=bquote({Realised~loss~rate~italic(L)}), 
         y="Histogram and density of resolved defaults [cures/write-offs]") + 
    theme(text=element_text(family=chosenFont),legend.position="bottom",
          strip.background=element_rect(fill="snow2", colour="snow2"),
          strip.text = element_text(size=8, colour="gray50"), 
          strip.text.y.right = element_text(angle=90)) + 
    scale_x_continuous(breaks=pretty_breaks(), label=percent)
)


# --- 4.2 Compare overall expected LGDs with actuals
# - Estimate statistics on distributional differences
metrics <- evalModel_onestage(datCredit,"LossRate_Real","LossRate_Tweedie","tweedie",modGLM_OneStage_CPG)

# - Combine statistics
stats_text <- paste("KS: ", sprintf("%.1f%%", metrics$KS * 100), "\n",
                    "KL: ", sprintf("%.4f", metrics$KL), "\n",
                    "JS: ", sprintf("%.4f", metrics$JS),
                    sep = "")

# - Create plotting data
plotData <- melt(datCredit, measure.vars=c("LossRate_Real", "LossRate_Tweedie"),
                 variable.name="Type",value.name="LossRate")
plotData[, Type:=factor(Type,levels=c("LossRate_Real", "LossRate_Tweedie"),
                        labels=c("Empirical", "Compound Poisson GLM"))]
plotData[, FacetLabel:="Resolved defaults [cures/write-offs]"]

# - Overall LGD distribution
(gOverlay <- ggplot(plotData, aes(x=LossRate)) + 
    theme_bw() + geom_histogram(
      aes(y=after_stat(density), fill=Type, colour=Type),
      alpha=0.35, bins=round(2 * datCredit[, .N]^(1/3)), position="identity") +
    labs(x=bquote({Realised~loss~rate~italic(L)}), y="Histogram of loss rates" ) +
    theme(text=element_text(family=chosenFont),legend.position="bottom",
          strip.background=element_rect(fill="snow2", colour="snow2"),
          strip.text=element_text(size = 8, colour = "gray50"),
          strip.placement="outside",        
          strip.text.y.right=element_text(angle = 90)) +
    scale_x_continuous(breaks=pretty_breaks(), labels = scales::percent) +
  scale_colour_manual(values=c(vCol[1], vCol[2])) +
  scale_fill_manual(values=c(vCol[1], vCol[2])) +
  facet_grid(FacetLabel ~., scales="free")+
  guides(fill=guide_legend(title = NULL),
         colour=guide_legend(title = NULL)))


# --- 4.3 Compare expected write-off LGDs with actuals
# - Render predictions with model
datCredit_WOFFs[, LossRate_Tweedie:=predict(modGLM_OneStage_CPG, newdata=datCredit_WOFFs,type="response")]

# - Impose a logical floor and ceiling of 0 and 1 to the predicted loss rates
datCredit_WOFFs[, LossRate_Tweedie:=ifelse(LossRate_Tweedie>1,1,LossRate_Tweedie)]
datCredit_WOFFs[, LossRate_Tweedie:=ifelse(LossRate_Tweedie<0,0,LossRate_Tweedie)]

# - Create plotting data
plotData <- melt(datCredit_WOFFs,measure.vars=c("LossRate_Real", "LossRate_Tweedie"),
                 variable.name="Type", value.name="LossRate")
plotData[, Type:=factor(Type, levels=c("LossRate_Real", "LossRate_Tweedie"),
                        labels=c("Actual loss rate", "Compound Poisson GLM"))]
plotData[, FacetLabel:="Resolved defaults [cures/write-offs]"]

# - Plot
(gOverlay_WOFF <- ggplot(plotData, aes(x=LossRate)) + 
    theme_bw() +
    geom_histogram(
      aes(y=after_stat(density), fill=Type, colour=Type), alpha=0.35,
      bins=round(2 * datCredit_WOFFs[,.N]^(1/3)), position="identity") +
    theme(legend.position = "none",text=element_text(size=12, family=chosenFont),
      axis.text.y=element_text(size=9, margin=unit(c(0,0,0,0),"mm")),
      axis.text.x=element_text(size=9, margin=unit(c(0,0,0,0),"mm")),
      axis.title.x=element_blank(), axis.title.y=element_blank(),
      axis.ticks=element_blank(), panel.grid.major=element_blank(),
      panel.grid.minor=element_blank(), panel.background=element_rect(color="black", fill="white"),
      plot.background=element_rect(color="white"), plot.margin=unit(c(0,0,0,0),"mm"),
      strip.background=element_rect(fill="snow2", colour="snow2"),
      strip.text=element_text(size=8, colour="gray50"),
      strip.text.y.right=element_text(angle=90)) +
    annotate("label", x=0.7, y=7, label=stats_text,
      hjust=0, vjust=1, family=chosenFont,
      size=4, fill="white", colour="black", label.size=0.5) +
    labs(x="", y="", title=paste0("Write-offs only")) +
    scale_x_continuous(breaks=pretty_breaks(), labels=scales::percent) +
    scale_colour_manual(values=c(vCol[1], vCol[2])) +
    scale_fill_manual(values=c(vCol[1], vCol[2])))
    

# --- 4.4 Combine and save graphs
# - Combine graphs
ymin <- diff(ggplot_build(gOverlay)$layout$panel_params[[1]]$y.range) * 0.2
ymax <- max(ggplot_build(gOverlay)$layout$panel_params[[1]]$y.range) * 0.95
(plot.full <- gOverlay + annotation_custom(grob = ggplotGrob(gOverlay_WOFF), xmin=0.2, xmax=1, ymin=ymin, ymax=ymax))

# - Save plot
dpi <- 180
ggsave(plot.full, file=paste0(genFigPath,"/ActvsExp_onestage_tweedie.png"),width=1200/dpi, height=1000/dpi,dpi=dpi, bg="white")



