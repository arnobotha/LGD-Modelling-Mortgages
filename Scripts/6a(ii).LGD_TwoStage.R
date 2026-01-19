# ================= LGD DISTRIBUTION ANALYSIS - ONE-STAGE MODELS ===============
# Comparing actual LGDs with expected LGDs from the fitted probability and 
# severity models
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
#   - 4b(i).InputSpace_DiscreteCox.R
#   - 4b(ii).InputSpace_DiscreteCox_Basic.R
#   - 4c.InputSpace_LogisticRegression.R
#   - 4d.InputSpace_TwoStage_LossSeverity.R
# -- Inputs:
#   - datCredit_train_CDH | Prepared from script 2g
#   - datCredit_valid_CDH | Prepared from script 2g
#   - modLR_Bas | Basic discrete time model as fitted in script 4b(ii)
#   - modLR_Adv | Advanced discrete time model as fitted in script 4b(i)
#   - modLR_Classic | Classical logistic regression model as fitted in script 4c
#   - modGLM_Severity_CPG | Single stage GLM model with a Tweedie link function
#                           as fitted in script 4d
# -- Outputs:
#   - <Analytics> | Graphs
# ==============================================================================


# ------ 1. Preliminaries

# --- 1.1 Load and prepare datasets
# - Confirm prepared datasets are loaded into memory
if (!exists('datCredit_train_CDH')) unpack.ffdf(paste0(genPath,"creditdata_train_CDH"), tempPath);gc()
if (!exists('datCredit_valid_CDH')) unpack.ffdf(paste0(genPath,"creditdata_valid_CDH"), tempPath);gc()

# - Filter for default spells
datCredit_train <- subset(datCredit_train_CDH, !is.na(DefSpell_Key))
datCredit_valid <- subset(datCredit_valid_CDH, !is.na(DefSpell_Key))

# - Score data using classic model for each instance of [TimeInDefSpell] as [DefSpell_Age]
datCredit_train[, DefSpell_Age2:=DefSpell_Age]; datCredit_train[, DefSpell_Age:=TimeInDefSpell]
datCredit_valid[, DefSpell_Age2:=DefSpell_Age]; datCredit_valid[, DefSpell_Age:=TimeInDefSpell]

# - Weigh default cases, as determined interactively based on calibration success (script 6e)
datCredit_train[, Weight:=ifelse(DefSpell_Event==1,1,1)]
datCredit_valid[, Weight:=ifelse(DefSpell_Event==1,1,1)]

# - Transform categorical variables to factor
# Payment method
datCredit_train[,pmnt_method_grp_fac:=as.factor(pmnt_method_grp)]
datCredit_valid[,pmnt_method_grp_fac:=as.factor(pmnt_method_grp)]
# Previous defaults
datCredit_train[,PrevDefaults_fac:=as.factor(PrevDefaults)]
datCredit_valid[,PrevDefaults_fac:=as.factor(PrevDefaults)]

# - Combine training and validation datasets to facilitate "better" (smooth) graphs
datCredit <- rbind(datCredit_train, datCredit_valid)

# - Handle left-truncated spells by adding a starting record 
### NOTE:  This is necessary for calculating certain survival quantities later
# Create an additional record for each default spell
datAdd <- subset(datCredit, Counter==1 & TimeInDefSpell>1)
datAdd[, TimeInDefSpell:=TimeInDefSpell-1]
datAdd[, Counter:=0]
# Add record to main dataset
datCredit <- rbind(datCredit, datAdd); setorder(datCredit, DefSpell_Key, TimeInDefSpell)

# - Remove objects
rm(datCredit_train_CDH, datCredit_valid_CDH); gc()


# --- 1.2 Load models
# - Basic discrete-time hazard model
modLR_Bas <- readRDS(paste0(genObjPath,"CoxDisc_Basic_Model.rds"))

# - Advanced discrete-time hazard model
modLR_Adv <- readRDS(paste0(genObjPath,"CoxDisc_Advanced_Model.rds"))

# - Classical logit model
modLR_Classic <- readRDS(paste0(genObjPath,"LR_Model.rds"))

# - CPG severity model
modGLM_Severity_CPG <- readRDS(paste0(genObjPath,"Severity_CPH_Model.rds"))

# - Tree model
SurvTree_CTree <- readRDS(paste0(genObjPath,"SurvTree_CTree.rds"))


# --- 1.3 Estimate event rates to facilitate the application of dichotomisation
# - Predict hazard h(t) = P(T=t | T>= t) in discrete-time
datCredit[, Hazard_adv:=predict(modLR_Adv, newdata=datCredit, type = "response")]
datCredit[, Hazard_bas:=predict(modLR_Bas, newdata=datCredit, type = "response")]
datCredit[, Hazard_classic:=predict(modLR_Classic, newdata=.SD[], type="response")]

# - Derive survival probability S(t) = prod(1 - hazard)
datCredit[, Survival_adv:=cumprod(1-Hazard_adv), by=list(DefSpell_Key)]
datCredit[, Survival_bas:=cumprod(1-Hazard_bas), by=list(DefSpell_Key)]
datCredit[, Survival_classic:=cumprod(1-Hazard_classic), by=list(DefSpell_Key)]

# - Derive discrete density, or event probability f(t) = S(t-1) - S(t)
datCredit[, EventRate_adv:=shift(Survival_adv, type="lag", n=1, fill=1) - Survival_adv, by=list(DefSpell_Key)]
datCredit[, EventRate_bas:=shift(Survival_bas, type="lag", n=1, fill=1) - Survival_bas, by=list(DefSpell_Key)]
datCredit[, EventRate_classic:=shift(Survival_classic, type="lag", n=1, fill=1) - Survival_classic, by=list(DefSpell_Key)]

# - Remove added rows
datCredit <- subset(datCredit, Counter>0)


# --- 1.4 Predict survival quantities using survival tree
# - Get training data from survival tree
datCredit_train_smp_cross <- SurvTree_CTree$datTrain

# - Get node predictions from tree
datCredit[,Node:=predict(SurvTree_CTree$survTree, datCredit, type="node")]
datCredit_train_smp_cross[,Node:=predict(SurvTree_CTree$survTree, datCredit_train_smp_cross, type="node")]

# - Fit a survival object for each node | Using the training data only to align with how the tree was fitted
datHaz <- predSurv(survTree=SurvTree_CTree$survTree, datGiven=datCredit_train_smp_cross,
                   fld_DefSpell_Age="DefSpell_Age", fld_DefSpell_Event="DefSpell_Event")

# - Join hazards back to main dataset
datCredit <- merge(datCredit, datHaz[, list(Node, Time, EventRate_survtree=EventRate)],
                   by.x=c("Node","TimeInDefSpell"), by.y=c("Node","Time"), all.x=T)


# --- 1.5 Estimate severities and interact them with the probabilities
# - Forecast severities
datCredit[, LossSeverity:=predict(modGLM_Severity_CPG, newdata=datCredit, type="response")]

# - Interact severities and probabilities
# Basic discrete time hazard model
datCredit[, LossRate_est_bas:=LossSeverity*EventRate_bas]
# Advanced discrete time hazard model
datCredit[, LossRate_est_adv:=LossSeverity*EventRate_adv]
# Classical logistic regression model
datCredit[, LossRate_est_classic:=LossSeverity*EventRate_classic]
# Survival tree
datCredit[, LossRate_est_survtree:=LossSeverity*EventRate_survtree]


# --- 1.6 Subset data
# - Filter to maximum spell counter
# datCredit <- subset(datCredit, DefSpell_Counter==1)
# datCredit <- datCredit[, .SD[which.max(DefSpell_Counter)], by=DefSpell_Key]

# - Identify where the loss rate is out of bounds and not feasible
datCredit[, OOB_Ind:=ifelse(LossRate_Real<0 | LossRate_Real>1,1,0)]

# - Subset to include only relevant data
datCredit <- subset(datCredit, OOB_Ind==0)
# datCredit_a <- datCredit[,.SD(max(DefSpell_Counter)), by=list(DefSpell_Counter)]

# - Subset for write-offs only to create inset plots
datCredit_WOFFs <- subset(datCredit, DefSpellResol_Type_Hist=="WOFF")




# ------ 2. Comparing actual and expected severities

# --- 2.1 Prepare data for plotting
# - Filter for the last default spell observations
datCredit_Last <- datCredit[, .SD[which.max(DefSpell_Counter)], by=DefSpell_Key]

# - Filter for written-off spells only
datCredit_Last_WOFFs <- datCredit_Last[DefSpellResol_Type_Hist=="WOFF"]

# - Filter for non-sensical loss rates
datCredit_Last_WOFFs[, LossSeverity:=ifelse(LossSeverity>=0 & LossSeverity<=1, LossSeverity, NA)]

# - Estimate statistics on distributional differences
metrics<-evalModel_twostage(data_train=datCredit_Last_WOFFs, actField="LossRate_Real", estField="LossSeverity")

# - Create plotting data
stats_text <- paste("KS: ", sprintf("%.1f%%", metrics$KS * 100), "\n",
                    "KL: ", sprintf("%.4f", metrics$KL), "\n",
                    "JS: ", sprintf("%.4f", metrics$JS),
                    sep = "")


# --- 2.2 Plot
# - Mean expected loss
(MeanLoss_exp_W <- mean(datCredit_Last_WOFFs$LossSeverity, na.rm=T))

# - Create plotting data
plotData <- melt(datCredit_Last_WOFFs, measure.vars=c("LossRate_Real", "LossSeverity"),
                 variable.name="Type", value.name="LossRate")
plotData[, Type:=factor(Type,levels=c("LossRate_Real", "LossSeverity"),
                        labels=c("Actual loss rate", "Compound Poisson GLM"))]
plotData[, FacetLabel:="Resolved defaults [cures/write-offs]"]

# - Graphing parameters
chosenFont <-"Cambria"
vCol <- brewer.pal(10, "Paired")[c(8,6)]

# - Plot
(g_WOFFs <- ggplot(plotData, aes(x=LossRate)) + 
    theme_bw() +
    theme(text=element_text(family=chosenFont),legend.position="bottom",
          strip.background=element_rect(fill="snow2", colour="snow2"),
          strip.text=element_text(size=8, colour="gray50"),
          strip.placement="outside",        
          strip.text.y.right=element_text(angle = 90)) +
    labs(x=bquote({Realised~loss~rate~italic(L)}), y="Histogram of loss rates" ) +
    geom_histogram(aes(y=after_stat(density), fill=Type, colour=Type), alpha=0.35,
                   bins=50, position="identity") +
    geom_vline(xintercept=MeanLoss_exp_W, linewidth=0.6, colour=vCol[2], linetype="dashed") + 
    annotate(geom="text", x=MeanLoss_exp_W*1.05,  y=4, family=chosenFont,
             label = paste0(sprintf("%.1f", MeanLoss_exp_W*100), "%"), size=3, colour=vCol[2], angle=90) +       
    annotate("label", x=0.7, y=3 , label = stats_text,
             hjust=0, vjust =1, family = chosenFont,
             size=4, fill="white", colour="black", label.size=0.5) +
    scale_x_continuous(breaks=pretty_breaks(), labels=scales::percent) +
    scale_colour_manual(values=c(vCol[1], vCol[2])) +
    scale_fill_manual(values=c(vCol[1], vCol[2])))

# - Save plot
dpi <- 240
ggsave(g_WOFFs, file=paste0(genFigPath,"/ActvsExp_twostage_LossSeverities.png"), width=1200/dpi, height=1000/dpi,dpi=dpi, bg="white")




# ------ 3. LGDs from basic discrete time hazard model and the severity model

# --- 3.1 Compare overall expected LGDs with actuals
# - Filter for non-sensical loss rates
datCredit[, LossRate_est_bas:=ifelse(LossRate_est_bas<=1 & LossRate_est_bas>=0, LossRate_est_bas, NA)]

# - Estimate statistics on distributional differences
metrics<-evalModel_twostage(data_train=datCredit, actField="LossRate_Real", estField="LossRate_est_bas")

# - Create plotting data
stats_text <- paste("KS: ", sprintf("%.1f%%", metrics$KS * 100), "\n",
                    "KL: ", sprintf("%.4f", metrics$KL), "\n",
                    "JS: ", sprintf("%.4f", metrics$JS),
                    sep = "")

# - Mean expected loss
(MeanLoss_exp <- mean(datCredit$LossRate_est_bas, na.rm=T))

# - Create plotting dataset
plotData <- melt(datCredit, measure.vars=c("LossRate_Real", "LossRate_est_bas"),
                 variable.name="Type",value.name="LossRate")
plotData[, Type:=factor(Type,levels=c("LossRate_Real", "LossRate_est_bas"),
                          labels=c("Empirical", "DtH-Basic A"))]
plotData[, FacetLabel:="Resolved defaults [cures/write-offs]"]

# - Graphing parameters
chosenFont <-"Cambria"
vCol <- brewer.pal(10, "Paired")[c(8,6)]

# - Plot
(gOverlay <- ggplot(plotData, aes(x=LossRate)) + 
  theme_bw() +
  geom_histogram(aes(y=after_stat(density), fill=Type, colour=Type),
                 alpha=0.35,bins=50, position="identity") +
  geom_vline(xintercept=MeanLoss_exp, linewidth=0.6, colour=vCol[2], linetype="dashed") + 
  annotate(geom="text", x=MeanLoss_exp*4,  y=25, family=chosenFont,
           label = paste0(sprintf("%.1f", MeanLoss_exp*100), "%"), size=3, colour=vCol[2], angle=90) +  
  labs(x=bquote({Realised~loss~rate~italic(L)}), y="Histogram of loss rates" ) +
  theme(text=element_text(family=chosenFont),legend.position="bottom",
        strip.background=element_rect(fill="snow2", colour="snow2"),
        strip.text=element_text(size=8, colour="gray50"),
        strip.placement="outside",        
        strip.text.y.right=element_text(angle = 90)) +
  scale_x_continuous(breaks=pretty_breaks(), labels = scales::percent) +
  scale_colour_manual(values=c(vCol[1], vCol[2])) +
  scale_fill_manual(values=c(vCol[1], vCol[2])) +
  facet_grid(FacetLabel ~., scales="free")+
  guides(fill=guide_legend(title=NULL), colour=guide_legend(title=NULL)))


# --- 3.2 Compare expected write-off LGDs with actuals
# - Filter for non-sensical loss rates
datCredit_WOFFs[, LossRate_est_bas:=ifelse(LossRate_est_bas<=1 & LossRate_est_bas>=0, LossRate_est_bas, NA)]

# - Mean expected loss
(MeanLoss_exp_W <- mean(datCredit_WOFFs$LossRate_est_bas, na.rm=T))

# - Create plotting data
plotData <- melt(datCredit_WOFFs, measure.vars=c("LossRate_Real", "LossRate_est_bas"),
                 variable.name="Type", value.name="LossRate")
plotData[, Type:=factor(Type,levels=c("LossRate_Real", "LossRate_est_bas"),
                          labels=c("Actual loss rate", "DtH-Basic A"))]
plotData[, FacetLabel:="Resolved defaults [cures/write-offs]"]

# - Plot
(gOverlay_WOFFs <- ggplot(plotData, aes(x=LossRate)) + 
    theme_bw() +
    geom_histogram(aes(y=after_stat(density), fill=Type, colour=Type), alpha=0.35,
                   bins=50, position="identity") +
    geom_vline(xintercept=MeanLoss_exp_W, linewidth=0.6, colour=vCol[2], linetype="dashed") + 
    annotate(geom="text", x=MeanLoss_exp_W*5,  y=6.5, family=chosenFont,
             label = paste0(sprintf("%.1f", MeanLoss_exp_W*100), "%"), size=3, colour=vCol[2], angle=90) +       
    theme(legend.position="none",text = element_text(size = 12, family = chosenFont),
          axis.text.y=element_text(size=9, margin=unit(c(0,0,0,0),"mm")),
          axis.text.x=element_text(size=9, margin=unit(c(0,0,0,0),"mm")),
          axis.title.x=element_blank(), axis.title.y=element_blank(),
          axis.ticks=element_blank(), panel.grid.major=element_blank(),
          panel.grid.minor=element_blank(),
          panel.background=element_rect(color="black", fill="white"),
          plot.background=element_rect(color="white"),
          plot.margin=unit(c(0,0,0,0),"mm"),
          strip.background=element_rect(fill="snow2", colour="snow2"),
          strip.text=element_text(size=8, colour="gray50"),
          strip.text.y.right=element_text(angle=90)) +
    annotate("label", x=0.6, y=20 , label = stats_text,
             hjust=0, vjust =1, family = chosenFont,
             size=4, fill="white", colour="black", label.size=0.5) +
    labs(x="", y="", title=paste0("Write-offs only")) +
    scale_x_continuous(breaks=pretty_breaks(), labels=scales::percent) +
    scale_colour_manual(values=c(vCol[1], vCol[2])) +
    scale_fill_manual(values=c(vCol[1], vCol[2])))


# --- 3.3 Combine and save graphs
# - Combine graphs
ymin <- diff(ggplot_build(gOverlay)$layout$panel_params[[1]]$y.range) * 0.2
ymax <- max(ggplot_build(gOverlay)$layout$panel_params[[1]]$y.range) * 0.95
(plot.full <- gOverlay + annotation_custom(grob=ggplotGrob(gOverlay_WOFFs), xmin=0.2, xmax=1, ymin=ymin, ymax=ymax))

# - Save plot
dpi <- 240
ggsave(plot.full, file=paste0(genFigPath,"/ActvsExp_twostage_DtH_Bas_A.png"),width=1200/dpi, height=1000/dpi,dpi=dpi, bg="white")




# ------ 4. LGDs from advanced discrete time hazard model and the severity model

# --- 4.1 Compare overall expected LGDs with actuals
# - Filter for non-sensical loss rates
datCredit[, LossRate_est_adv := ifelse(LossRate_est_adv<=1 & LossRate_est_adv>=0, LossRate_est_adv, NA)]

# - Estimate statistics on distributional differences
metrics<-evalModel_twostage(data_train=datCredit, actField="LossRate_Real", estField="LossRate_est_adv")

# Mean expected loss
(MeanLoss_exp <- mean(datCredit$LossRate_est_adv, na.rm=T))

# - Create plotting data
stats_text <- paste("KS: ", sprintf("%.1f%%", metrics$KS * 100), "\n",
                    "KL: ", sprintf("%.4f", metrics$KL), "\n",
                    "JS: ", sprintf("%.4f", metrics$JS),
                    sep = "")

# - Create plotting dataset
plotData <- melt(datCredit, measure.vars=c("LossRate_Real", "LossRate_est_adv"),
                 variable.name="Type",value.name="LossRate")
plotData[, Type:=factor(Type,levels=c("LossRate_Real", "LossRate_est_adv"),
                          labels=c("Empirical", "DtH-Advanced A"))]
plotData[, FacetLabel:="Resolved defaults [cures/write-offs]"]

# - Graphing parameters
chosenFont <-"Cambria"
vCol <- brewer.pal(10, "Paired")[c(8,6)]

# - Plot
(gOverlay <- ggplot(plotData, aes(x=LossRate)) + 
  theme_bw() +
  geom_histogram(aes(y=after_stat(density), fill=Type, colour=Type),
                 alpha=0.35,bins=50, position="identity") +
  geom_vline(xintercept=MeanLoss_exp, linewidth=0.6, colour=vCol[2], linetype="dashed") + 
  annotate(geom="text", x=MeanLoss_exp*6,  y=25, family=chosenFont,
           label = paste0(sprintf("%.1f", MeanLoss_exp*100), "%"), size=3, colour=vCol[2], angle=90) +
  labs(x=bquote({Realised~loss~rate~italic(L)}), y="Histogram of loss rates" ) +
  theme(text=element_text(family=chosenFont),legend.position="bottom",
        strip.background=element_rect(fill="snow2", colour="snow2"),
        strip.text=element_text(size=8, colour="gray50"),
        strip.placement="outside",        
        strip.text.y.right=element_text(angle = 90)) +
  scale_x_continuous(breaks=pretty_breaks(), labels = scales::percent) +
  scale_colour_manual(values=c(vCol[1], vCol[2])) +
  scale_fill_manual(values=c(vCol[1], vCol[2])) +
  facet_grid(FacetLabel ~., scales="free")+
  guides(fill=guide_legend(title = NULL), colour=guide_legend(title = NULL)))


# --- 4.2 Compare expected write-off LGDs with actuals
# - Filter for non-sensical loss rates
datCredit_WOFFs[, LossRate_est_adv := ifelse(LossRate_est_adv<=1 & LossRate_est_adv>=0, LossRate_est_adv, NA)]

# Mean expected loss
(MeanLoss_exp_W <- mean(datCredit_WOFFs$LossRate_est_adv, na.rm=T))

# - Create plotting data
plotData <- melt(datCredit_WOFFs, measure.vars = c("LossRate_Real", "LossRate_est_adv"),
                 variable.name = "Type",value.name = "LossRate")
plotData[, Type := factor(Type,levels = c("LossRate_Real", "LossRate_est_adv"),
                          labels = c("Actual loss rate", "DtH-Advanced A"))]
plotData[, FacetLabel := "Resolved defaults [cures/write-offs]"]

# - Plot
(gOverlay_WOFFs <- ggplot(plotData, aes(x=LossRate)) + 
    theme_bw() +
    geom_histogram(aes(y=after_stat(density), fill=Type, colour=Type), alpha=0.35,
                   bins=50, position="identity") +
    geom_vline(xintercept=MeanLoss_exp_W, linewidth=0.6, colour=vCol[2], linetype="dashed") + 
    annotate(geom="text", x=MeanLoss_exp_W*4,  y=6.5, family=chosenFont,
             label = paste0(sprintf("%.1f", MeanLoss_exp_W*100), "%"), size=3, colour=vCol[2], angle=90) +     
    theme(legend.position="none",text=element_text(size=12, family=chosenFont),
          axis.text.y=element_text(size=9, margin=unit(c(0,0,0,0),"mm")),
          axis.text.x=element_text(size=9, margin=unit(c(0,0,0,0),"mm")),
          axis.title.x=element_blank(), axis.title.y=element_blank(),
          axis.ticks=element_blank(), panel.grid.major=element_blank(),
          panel.grid.minor=element_blank(),
          panel.background=element_rect(color="black", fill="white"),
          plot.background=element_rect(color="white"),
          plot.margin=unit(c(0,0,0,0),"mm"),
          strip.background=element_rect(fill="snow2", colour="snow2"),
          strip.text=element_text(size=8, colour="gray50"),
          strip.text.y.right=element_text(angle=90)) +
    annotate("label", x=0.6, y=40, label=stats_text,
             hjust=0, vjust=1, family=chosenFont,
             size=4, fill="white", colour="black", label.size=0.5) +
    labs(x="", y="", title=paste0("Write-offs only")) +
    scale_x_continuous(breaks=pretty_breaks(), labels=scales::percent) +
    scale_colour_manual(values=c(vCol[1], vCol[2])) +
    scale_fill_manual(values   = c(vCol[1], vCol[2])))


# --- 4.3 Combine and save graphs
# - Combine graphs
ymin <- diff(ggplot_build(gOverlay)$layout$panel_params[[1]]$y.range) * 0.2
ymax <- max(ggplot_build(gOverlay)$layout$panel_params[[1]]$y.range) * 0.95
(plot.full <- gOverlay + annotation_custom(grob = ggplotGrob(gOverlay_WOFFs), xmin=0.2, xmax=1, ymin=ymin, ymax=ymax))

# - Save plot
dpi <- 240
ggsave(plot.full, file=paste0(genFigPath,"/ActvsExp_twostage_DtH_Adv_A.png"),width=1200/dpi, height=1000/dpi,dpi=dpi, bg="white")




# ------ 5. LGDs from classical logistic regression model and the severity model

# --- 5.1 Compare overall expected LGDs with actuals
# - Filter for non-sensical loss rates
datCredit[, LossRate_est_classic := ifelse(LossRate_est_classic<=1 & LossRate_est_classic>=0, LossRate_est_classic, NA)]

# Mean expected loss
(MeanLoss_exp <- mean(datCredit$LossRate_est_classic, na.rm=T))

# - Estimate statistics on distributional differences
metrics<-evalModel_twostage(data_train=datCredit, actField="LossRate_Real", estField="LossRate_est_classic")

# - Create plotting data
stats_text <- paste("KS: ", sprintf("%.1f%%", metrics$KS * 100), "\n",
                    "KL: ", sprintf("%.4f", metrics$KL), "\n",
                    "JS: ", sprintf("%.4f", metrics$JS),
                    sep = "")

# - Create plotting dataset
plotData <- melt(datCredit, measure.vars = c("LossRate_Real", "LossRate_est_classic"),
                 variable.name="Type", value.name="LossRate")
plotData[, Type := factor(Type,levels = c("LossRate_Real", "LossRate_est_classic"),
                          labels = c("Empirical", "Logistic Regression A"))]
plotData[, FacetLabel := "Resolved defaults [cures/write-offs]"]

# - Graphing parameters
chosenFont <-"Cambria"
vCol <- brewer.pal(10, "Paired")[c(8,6)]

# - Plot
(gOverlay <- ggplot(plotData, aes(x=LossRate)) + 
  theme_bw() +
  geom_histogram(aes(y=after_stat(density), fill=Type, colour=Type),
                 alpha=0.35,bins=50, position="identity") +
  geom_vline(xintercept=MeanLoss_exp, linewidth=0.6, colour=vCol[2], linetype="dashed") + 
  annotate(geom="text", x=MeanLoss_exp*4,  y=25, family=chosenFont,
           label = paste0(sprintf("%.1f", MeanLoss_exp*100), "%"), size=3, colour=vCol[2], angle=90) +  
  labs(x=bquote({Realised~loss~rate~italic(L)}), y="Histogram of loss rates" ) +
  theme(text=element_text(family=chosenFont),legend.position="bottom",
        strip.background=element_rect(fill="snow2", colour="snow2"),
        strip.text=element_text(size=8, colour="gray50"),
        strip.placement="outside",        
        strip.text.y.right=element_text(angle = 90)) +
  scale_x_continuous(breaks=pretty_breaks(), labels = scales::percent) +
  scale_colour_manual(values=c(vCol[1], vCol[2])) +
  scale_fill_manual(values=c(vCol[1], vCol[2])) +
  facet_grid(FacetLabel ~., scales="free")+
  guides(fill=guide_legend(title = NULL), colour=guide_legend(title = NULL)))


# --- 5.2 Compare expected write-off LGDs with actuals
# - Filter for non-sensical loss rates
datCredit_WOFFs[, LossRate_est_classic := ifelse(LossRate_est_classic<=1 & LossRate_est_classic>=0, LossRate_est_classic, NA)]

# Mean expected loss
(MeanLoss_exp_W <- mean(datCredit_WOFFs$LossRate_est_classic, na.rm=T))

# - Create plotting data
plotData <- melt(datCredit_WOFFs, measure.vars = c("LossRate_Real", "LossRate_est_classic"), variable.name = "Type",value.name = "LossRate")
plotData[, Type:=factor(Type,levels=c("LossRate_Real", "LossRate_est_classic"),
                          labels=c("Actual loss rate", "Logistic Regression A"))]
plotData[, FacetLabel:="Resolved defaults [cures/write-offs]"]

# - Plot
(gOverlay_hist <- ggplot(plotData, aes(x=LossRate)) + 
    theme_bw() +
    geom_histogram(aes(y=after_stat(density), fill=Type, colour=Type), alpha=0.35,
                   bins=50, position="identity") +
    geom_vline(xintercept=MeanLoss_exp_W, linewidth=0.6, colour=vCol[2], linetype="dashed") + 
    annotate(geom="text", x=MeanLoss_exp_W*3,  y=9, family=chosenFont,
             label = paste0(sprintf("%.1f", MeanLoss_exp_W*100), "%"), size=3, colour=vCol[2], angle=90) + 
    theme(legend.position="none",text = element_text(size = 12, family = chosenFont),
          axis.text.y=element_text(size=9, margin=unit(c(0,0,0,0),"mm")),
          axis.text.x=element_text(size=9, margin=unit(c(0,0,0,0),"mm")),
          axis.title.x=element_blank(), axis.title.y=element_blank(),
          axis.ticks=element_blank(), panel.grid.major=element_blank(),
          panel.grid.minor=element_blank(),
          panel.background=element_rect(color="black", fill="white"),
          plot.background=element_rect(color="white"),
          plot.margin=unit(c(0,0,0,0),"mm"),
          strip.background=element_rect(fill="snow2", colour="snow2"),
          strip.text=element_text(size=8, colour="gray50"),
          strip.text.y.right=element_text(angle=90)) +
    annotate("label", x=0.6, y=20 , label=stats_text,
             hjust=0, vjust=1, family=chosenFont,
             size=4, fill="white", colour="black", label.size=0.5) +
    labs(x="", y="", title=paste0("Write-offs only")) +
    scale_x_continuous(breaks=pretty_breaks(), labels=scales::percent) +
    scale_colour_manual(values=c(vCol[1], vCol[2])) +
    scale_fill_manual(values   = c(vCol[1], vCol[2])))


# --- 5.3 Combine and save graphs
# - Combine graphs
ymin <- diff(ggplot_build(gOverlay)$layout$panel_params[[1]]$y.range) * 0.2
ymax <- max(ggplot_build(gOverlay)$layout$panel_params[[1]]$y.range) * 0.95
(plot.full <- gOverlay + annotation_custom(grob = ggplotGrob(gOverlay_hist), xmin=0.2, xmax=1, ymin=ymin, ymax=ymax))

# - Save plot
dpi <- 240
ggsave(plot.full, file=paste0(genFigPath,"/ActvsExp_twostage_LR_A.png"),width=1200/dpi, height=1000/dpi,dpi=dpi, bg="white")




# ------ 6. LGDs from survival tree and the severity model

# --- 6.1 Compare overall expected LGDs with actuals
# - Filter for non-sensical loss rates
datCredit[, LossRate_est_survtree:=ifelse(LossRate_est_survtree<=1 & LossRate_est_survtree>=0, LossRate_est_survtree, NA)]

# - Mean expected loss
(MeanLoss_exp <- mean(datCredit$LossRate_est_survtree, na.rm=T))

# - Estimate statistics on distributional differences
metrics<-evalModel_twostage(data_train=datCredit, actField="LossRate_Real", estField="LossRate_est_survtree")

# - Create plotting data
stats_text <- paste("KS: ", sprintf("%.1f%%", metrics$KS * 100), "\n",
                    "KL: ", sprintf("%.4f", metrics$KL), "\n",
                    "JS: ", sprintf("%.4f", metrics$JS),
                    sep = "")

# - Create plotting dataset
plotData <- melt(datCredit, measure.vars=c("LossRate_Real", "LossRate_est_survtree"),
                 variable.name="Type", value.name="LossRate")
plotData[, Type:=factor(Type,levels=c("LossRate_Real", "LossRate_est_survtree"),
                        labels=c("Empirical", "ST"))]
plotData[, FacetLabel:="Resolved defaults [cures/write-offs]"]

# - Graphing parameters
chosenFont <-"Cambria"
vCol <- brewer.pal(10, "Paired")[c(8,6)]

# - Plot
(gOverlay <- ggplot(plotData, aes(x=LossRate)) + 
    theme_bw() +
    geom_histogram(aes(y=after_stat(density), fill=Type, colour=Type),
                   alpha=0.35,bins=50, position="identity") +
    geom_vline(xintercept=MeanLoss_exp, linewidth=0.6, colour=vCol[2], linetype="dashed") + 
    annotate(geom="text", x=MeanLoss_exp*4,  y=25, family=chosenFont,
             label = paste0(sprintf("%.1f", MeanLoss_exp*100), "%"), size=3, colour=vCol[2], angle=90) +  
    labs(x=bquote({Realised~loss~rate~italic(L)}), y="Histogram of loss rates" ) +
    theme(text=element_text(family=chosenFont),legend.position="bottom",
          strip.background=element_rect(fill="snow2", colour="snow2"),
          strip.text=element_text(size=8, colour="gray50"),
          strip.placement="outside",        
          strip.text.y.right=element_text(angle = 90)) +
    scale_x_continuous(breaks=pretty_breaks(), labels = scales::percent) +
    scale_colour_manual(values=c(vCol[1], vCol[2])) +
    scale_fill_manual(values=c(vCol[1], vCol[2])) +
    facet_grid(FacetLabel ~., scales="free")+
    guides(fill=guide_legend(title = NULL), colour=guide_legend(title = NULL)))


# --- 6.2 Compare expected write-off LGDs with actuals
# - Filter for non-sensical loss rates
datCredit_WOFFs[, LossRate_est_survtree:=ifelse(LossRate_est_survtree<=1 & LossRate_est_survtree>=0,
                                                LossRate_est_survtree, NA)]

# Mean expected loss
(MeanLoss_exp_W <- mean(datCredit_WOFFs$LossRate_est_survtree, na.rm=T))

# - Create plotting data
plotData <- melt(datCredit_WOFFs, measure.vars = c("LossRate_Real", "LossRate_est_survtree"),
                 variable.name = "Type",value.name = "LossRate")
plotData[, Type:=factor(Type,levels=c("LossRate_Real", "LossRate_est_survtree"),
                        labels=c("Actual loss rate", "ST"))]
plotData[, FacetLabel:="Resolved defaults [cures/write-offs]"]

# - Plot
(gOverlay_hist <- ggplot(plotData, aes(x=LossRate)) + 
    theme_bw() +
    geom_histogram(aes(y=after_stat(density), fill=Type, colour=Type), alpha=0.35,
                   bins=50, position="identity") +
    geom_vline(xintercept=MeanLoss_exp_W, linewidth=0.6, colour=vCol[2], linetype="dashed") + 
    annotate(geom="text", x=MeanLoss_exp_W*3,  y=9, family=chosenFont,
             label = paste0(sprintf("%.1f", MeanLoss_exp_W*100), "%"), size=3, colour=vCol[2], angle=90) + 
    theme(legend.position="none",text = element_text(size = 12, family = chosenFont),
          axis.text.y=element_text(size=9, margin=unit(c(0,0,0,0),"mm")),
          axis.text.x=element_text(size=9, margin=unit(c(0,0,0,0),"mm")),
          axis.title.x=element_blank(), axis.title.y=element_blank(),
          axis.ticks=element_blank(), panel.grid.major=element_blank(),
          panel.grid.minor=element_blank(),
          panel.background=element_rect(color="black", fill="white"),
          plot.background=element_rect(color="white"),
          plot.margin=unit(c(0,0,0,0),"mm"),
          strip.background=element_rect(fill="snow2", colour="snow2"),
          strip.text=element_text(size=8, colour="gray50"),
          strip.text.y.right=element_text(angle=90)) +
    annotate("label", x=0.6, y=20 , label=stats_text,
             hjust=0, vjust=1, family=chosenFont,
             size=4, fill="white", colour="black", label.size=0.5) +
    labs(x="", y="", title=paste0("Write-offs only")) +
    scale_x_continuous(breaks=pretty_breaks(), labels=scales::percent) +
    scale_colour_manual(values=c(vCol[1], vCol[2])) +
    scale_fill_manual(values   = c(vCol[1], vCol[2])))


# --- 6.3 Combine and save graphs
# - Combine graphs
ymin <- diff(ggplot_build(gOverlay)$layout$panel_params[[1]]$y.range) * 0.2
ymax <- max(ggplot_build(gOverlay)$layout$panel_params[[1]]$y.range) * 0.95
(plot.full <- gOverlay + annotation_custom(grob = ggplotGrob(gOverlay_hist), xmin=0.2, xmax=1, ymin=ymin, ymax=ymax))

# - Save plot
dpi <- 240
ggsave(plot.full, file=paste0(genFigPath,"/ActvsExp_twostage_ST.png"),width=1200/dpi, height=1000/dpi,dpi=dpi, bg="white")


# --- 6.4 Cleanup
suppressWarnings(rm(datCredit, datCredit_train, datCredit_valid, datAdd, datCredit_acc, datCredit_WOFFs_acc,
   datCredit_WOFFs, modLR_Adv, modLR_Bas, modLR_Classic, modGLM_Severity_CPG,
   g1, g2, gOverlay, gOverlay_hist, gOverlay_WOFFs, plot.full, plotData))
gc()
