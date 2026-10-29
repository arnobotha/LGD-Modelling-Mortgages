# ================= LGD DISTRIBUTION ANALYSIS - Two-STAGE MODELS ===============
# Comparing actual LGDs with expected LGDs from the fitted probability and 
# severity models
# ------------------------------------------------------------------------------
# PROJECT TITLE: Loss Modelling (LGD) for Residential Mortgages
# SCRIPT AUTHOR(S): Mohammed Gabru (MG), Marcel Muller (MM), Dr Arno Botha (AB)
# ------------------------------------------------------------------------------
# -- Script dependencies:
#   - 0.Setup.R
#   - 4b(i).InputSpace_DiscreteCox.R
#   - 4b(ii).InputSpace_DiscreteCox_Basic.R
#   - 4c.InputSpace_LogisticRegression.R
#   - 4d.InputSpace_TwoStage_LossSeverity.R
#   - 4e.SurvivalTrees.R
#
# -- Inputs:
#   - datCredit_train_CDH | Prepared from script 2g
#   - datCredit_valid_CDH | Prepared from script 2g
#   - modLR_Bas | Basic discrete time model as fitted in script 4b(ii)
#   - modLR_Adv | Advanced discrete time model as fitted in script 4b(i)
#   - modLR_Classic | Classical logistic regression model as fitted in script 4c
#   - modGLM_Severity_CPG | Single stage GLM model with a Tweedie link function
#                           as fitted in script 4d
#   - SurvTree_CTree | Survival tree as fitted in script 4e
#
# -- Outputs:
#   - <Analytics> | Graphs
# ==============================================================================

### MM: - Note that these LGD-distributions are constructed using each observation of an account (i.e., multiple LGDs from each account are used)
###     - Should we consider giving a view of the distribution only at default point?
###     - The question then is why do we evaluate it only at default, should we take a few other time in default points? Maybe even all of them and
###       get a aggregated view over time in default spell?

# ============ 1. Preliminaries ============

# --- 1.1 Load and prepare data
# - Confirm prepared datasets are loaded into memory
if (!exists('datCredit_train_CDH')) unpack.ffdf(paste0(genPath,"creditdata_train_CDH"), tempPath);gc()
if (!exists('datCredit_valid_CDH')) unpack.ffdf(paste0(genPath,"creditdata_valid_CDH"), tempPath);gc()

# - Ensure order of data is correct
setorder(datCredit_train_CDH, LoanID, Date)
setorder(datCredit_valid_CDH, LoanID, Date)

# - Use only default spells
datCredit_train <- datCredit_train_CDH[!is.na(DefSpell_Key),]
datCredit_valid <- datCredit_valid_CDH[!is.na(DefSpell_Key),]

# - Create a spell-level target variable for write-off instance
datCredit_train[, DefSpell_Event2:=DefSpell_Event]; datCredit_valid[, DefSpell_Event2:=DefSpell_Event]
datCredit_train[, DefSpell_Event:=DefSpellResol_Type_Hist=="WOFF"]; datCredit_valid[, DefSpell_Event:=DefSpellResol_Type_Hist=="WOFF"]

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
datCredit <- rbind(datCredit, datAdd); setorder(datCredit, DefSpell_Key, Date)

# - Remove objects
rm(datCredit_train_CDH, datCredit_valid_CDH); gc()


# --- 1.2 Load models
# - Basic discrete-time hazard model
modLR_Bas <- readRDS(paste0(genObjPath,"CoxDisc_Basic_Model.rds"))

# - Advanced discrete-time hazard model
modLR_Adv <- readRDS(paste0(genObjPath,"CoxDisc_Advanced_Model.rds"))

# - Classical logit model
modLR_Classic <- readRDS(paste0(genObjPath,"LR_Model.rds"))

# - Tree model
SurvTree_CTree <- readRDS(paste0(genObjPath,"SurvTree_CTree.rds"))

# - CPG severity model
modGLM_Severity_CPG <- readRDS(paste0(genObjPath,"Severity_CPH_Model.rds"))


# --- 1.3 Estimate event rates
# - Predict hazard h(t) = P(T=t | T>= t) in discrete-time
datCredit[, Hazard_adv:=predict(modLR_Adv, newdata=datCredit, type = "response")]
datCredit[, Hazard_bas:=predict(modLR_Bas, newdata=datCredit, type = "response")]
datCredit[, Hazard_classic:=predict(modLR_Classic, newdata=.SD[], type="response")]
### MM: The logistic regression model does not produce a hazard rate, it produces the full probability of writing-off??

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
datCredit_train_SurvTree <- SurvTree_CTree$datTrain

# - Get node predictions from tree
datCredit[,Node:=predict(SurvTree_CTree$survTree, datCredit, type="node")]
datCredit_train_SurvTree[,Node:=predict(SurvTree_CTree$survTree, datCredit_train_SurvTree, type="node")]

# - Fit a survival object for each node | Using the training data only to align with how the tree was fitted
datHaz <- predSurv(survTree=SurvTree_CTree$survTree, datGiven=datCredit_train_SurvTree,
                   fld_DefSpell_Age="DefSpell_Age", fld_DefSpell_Event="DefSpell_Event")

# - Join hazards back to main dataset
datCredit <- merge(datCredit, datHaz[, list(Node, Time, Survival_survtree=Surv, Hazard_survtree=Hazard,
                                            EventRate_survtree=EventRate)],
                   by.x=c("Node", "TimeInDefSpell"), by.y=c("Node", "Time"), all.x=T)


# --- 1.5 Estimate loss severities & interact them with write-off probabilities
# - Forecast severities
datCredit[, LossSeverity:=predict(modGLM_Severity_CPG, newdata=datCredit, type="response")]

# - Interact write-off probabilities with severities
datCredit[, `:=`(LossRate_est_bas=LossSeverity*EventRate_bas, # Basic discrete time hazard model
                 LossRate_est_adv=LossSeverity*EventRate_adv, # Advanced discrete time hazard model
                 LossRate_est_classic=LossSeverity*EventRate_classic, # Classical logistic regression model
                 LossRate_est_survtree=LossSeverity*EventRate_survtree)] # Survival tree

# - Get a lookup to check predictions
lookup <- datCredit[DefSpell_Key==unique(datCredit[DefSpellResol_Type_Hist=="WOFF" & DefSpell_Age>=30 & LossRate_Real>0, DefSpell_Key])[3000],
                    c("DefSpell_Key", "TimeInDefSpell", "Date",
                      "EventRate_adv", "LossSeverity", "LossRate_est_adv", "LossRate_Real",
                      "EventRate_bas", "LossSeverity", "LossRate_est_bas", "LossRate_Real",
                      "EventRate_survtree", "LossSeverity", "LossRate_est_survtree", "LossRate_Real")]


# --- 1.6 Correct for non-sensical loss rates
# Severity model
datCredit[, LossSeverity:=ifelse(LossSeverity<0 | LossSeverity>1, NA, LossSeverity)]
# Basic discrete time hazard model
datCredit[, LossRate_est_bas:=ifelse(LossRate_est_bas<0 | LossRate_est_bas>1, NA, LossRate_est_bas)]
# Advanced discrete time hazard model
datCredit[, LossRate_est_adv:=ifelse(LossRate_est_adv<0 | LossRate_est_adv>1, NA, LossRate_est_adv)]
# Classical logistic regression model
datCredit[, LossRate_est_classic:=ifelse(LossRate_est_classic<0 | LossRate_est_classic>1, NA, LossRate_est_classic)]
# Survival tree
datCredit[, LossRate_est_survtree:=ifelse(LossRate_est_survtree<0 | LossRate_est_survtree>1, NA, LossRate_est_survtree)]


# --- 1.7 Subset data
# - Filter to maximum spell counter
# datCredit <- subset(datCredit, DefSpell_Counter==1)
# datCredit <- datCredit[, .SD[which.max(DefSpell_Counter)], by=DefSpell_Key]

# - Identify where the loss rate is out of bounds and not feasible
datCredit[, OOB_Ind:=ifelse(LossRate_Real<0 | LossRate_Real>1,1,0)]

# - Subset to include only relevant data
datCredit <- subset(datCredit, OOB_Ind==0)

# - Subset for write-offs only to create inset plots
datCredit_WOFFs <- subset(datCredit, DefSpellResol_Type_Hist=="WOFF" & OOB_Ind==0)




# ============ 2. Distributions of LGDs from basic discrete time hazard model and the severity model ============

# --- 2.1 Compare overall expected LGDs with actuals
# - Estimate statistics on distributional differences
metrics<-evalModel_twostage(data_train=datCredit, actField="LossRate_Real", estField="LossRate_est_bas")

# - Create plotting data
stats_text <- paste("KS: ", sprintf("%.1f%%", metrics$KS * 100), "\n",
                    "KL: ", sprintf("%.4f", metrics$KL), "\n",
                    "JS: ", sprintf("%.4f", metrics$JS),
                    sep = "")

# - Mean expected loss
(MeanLoss_exp <- mean(datCredit$LossRate_est_bas, na.rm=T))
(MeanLoss_act <- mean(datCredit$LossRate_Real, na.rm=T))

# - Create plotting dataset
plotData <- melt(datCredit, measure.vars=c("LossRate_Real", "LossRate_est_bas"),
                 variable.name="Type",value.name="LossRate")
plotData[, Type:=factor(Type,levels=c("LossRate_Real", "LossRate_est_bas"),
                        labels=c("Empirical", "DtH-Basic"))]
plotData[, FacetLabel:="Resolved defaults [cures/write-offs]"]

# - Graphing parameters
chosenFont <-"Cambria"
vCol <- brewer.pal(10, "Paired")[c(8,6)]

# - Plot
(gOverlay <- ggplot(plotData, aes(x=LossRate)) + 
    theme_bw() +
    geom_histogram(aes(y=after_stat(density), fill=Type, colour=Type),
                   alpha=0.35,bins=50, position="identity") +
    labs(x=bquote({Realised~loss~rate~italic(L)}), y="Histogram of loss rates" ) +
    theme(text=element_text(family=chosenFont),legend.position="bottom",
          strip.background=element_rect(fill="snow2", colour="snow2"),
          strip.text=element_text(size=8, colour="gray50"),
          strip.placement="outside",        
          strip.text.y.right=element_text(angle = 90)) +
    # Annotations
    geom_vline(xintercept=MeanLoss_act, linewidth=0.6, colour=vCol[1], linetype="dashed") + 
    annotate(geom="text", x=MeanLoss_act*0.8, y=25, family=chosenFont,
             label = paste0(sprintf("%.1f", MeanLoss_act*100), "%"), size=3, colour=vCol[1], angle=90) +
    geom_vline(xintercept=MeanLoss_exp, linewidth=0.6, colour=vCol[2], linetype="dashed") + 
    annotate(geom="text", x=MeanLoss_exp*3, y=25, family=chosenFont,
             label = paste0(sprintf("%.1f", MeanLoss_exp*100), "%"), size=3, colour=vCol[2], angle=90) +  
    # Facets & scales
    facet_grid(FacetLabel ~., scales="free") +
    scale_x_continuous(breaks=pretty_breaks(), labels = scales::percent) +
    scale_colour_manual(values=c(vCol[1], vCol[2])) +
    scale_fill_manual(values=c(vCol[1], vCol[2])) +
    guides(fill=guide_legend(title=NULL), colour=guide_legend(title=NULL)))


# --- 2.2 Compare expected write-off LGDs with actuals
# - Filter for non-sensical loss rates
datCredit_WOFFs[, LossRate_est_bas:=ifelse(LossRate_est_bas<=1 & LossRate_est_bas>=0, LossRate_est_bas, NA)]

# - Mean expected loss
(MeanLoss_act_W <- mean(datCredit_WOFFs$LossRate_Real, na.rm=T))
(MeanLoss_exp_W <- mean(datCredit_WOFFs$LossRate_est_bas, na.rm=T))

# - Create plotting data
plotData <- melt(datCredit_WOFFs, measure.vars=c("LossRate_Real", "LossRate_est_bas"),
                 variable.name="Type", value.name="LossRate")
plotData[, Type:=factor(Type,levels=c("LossRate_Real", "LossRate_est_bas"),
                        labels=c("Actual loss rate", "DtH-Basic"))]
plotData[, FacetLabel:="Resolved defaults [cures/write-offs]"]

# - Plot
(gOverlay_WOFFs <- ggplot(plotData, aes(x=LossRate)) + 
    theme_bw() +
    geom_histogram(aes(y=after_stat(density), fill=Type, colour=Type), alpha=0.35,
                   bins=50, position="identity") +
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
    labs(x="", y="", title=paste0("Write-offs only")) +
    # Annotations
    geom_vline(xintercept=MeanLoss_exp_W, linewidth=0.6, colour=vCol[2], linetype="dashed") + 
    annotate(geom="text", x=MeanLoss_exp_W*5,  y=17.5, family=chosenFont,
             label = paste0(sprintf("%.1f", MeanLoss_exp_W*100), "%"), size=3, colour=vCol[2], angle=90) + 
    geom_vline(xintercept=MeanLoss_act_W, linewidth=0.6, colour=vCol[1], linetype="dashed") + 
    annotate(geom="text", x=MeanLoss_act_W*0.9, y=17.5, family=chosenFont,
             label = paste0(sprintf("%.1f", MeanLoss_act_W*100), "%"), size=3, colour=vCol[1], angle=90) + 
    annotate("label", x=0.6, y=20, label = stats_text,
             hjust=0, vjust =1, family = chosenFont,
             size=4, fill="white", colour="black", label.size=0.5) +
    # Facets & scales
    scale_x_continuous(breaks=pretty_breaks(), labels=scales::percent) +
    scale_colour_manual(values=c(vCol[1], vCol[2])) +
    scale_fill_manual(values=c(vCol[1], vCol[2])))


# --- 2.3 Combine and save graphs
# - Combine graphs
ymin <- diff(ggplot_build(gOverlay)$layout$panel_params[[1]]$y.range) * 0.2
ymax <- max(ggplot_build(gOverlay)$layout$panel_params[[1]]$y.range) * 0.95
(plot.full <- gOverlay + annotation_custom(grob=ggplotGrob(gOverlay_WOFFs), xmin=0.2, xmax=1, ymin=ymin, ymax=ymax))

# - Save plot
dpi <- 240
ggsave(plot.full, file=paste0(genFigPath,"ActvsExp_twostage_DtH_Bas_A.png"),width=1200/dpi, height=1000/dpi,dpi=dpi, bg="white")




# ============ 3. Distributions of LGDs from advanced discrete time hazard model and the severity model ============

# --- 3.1 Compare overall expected LGDs with actuals
# - Estimate statistics on distributional differences
metrics<-evalModel_twostage(data_train=datCredit, actField="LossRate_Real", estField="LossRate_est_adv")

# Mean expected loss
(MeanLoss_act <- mean(datCredit$LossRate_Real, na.rm=T))
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
                        labels=c("Empirical", "DtH-Advanced"))]
plotData[, FacetLabel:="Resolved defaults [cures/write-offs]"]

# - Graphing parameters
chosenFont <-"Cambria"
vCol <- brewer.pal(10, "Paired")[c(8,6)]

# - Plot
(gOverlay <- ggplot(plotData, aes(x=LossRate)) + 
    theme_bw() +
    geom_histogram(aes(y=after_stat(density), fill=Type, colour=Type),
                   alpha=0.35,bins=50, position="identity") +
    labs(x=bquote({Realised~loss~rate~italic(L)}), y="Histogram of loss rates" ) +
    theme(text=element_text(family=chosenFont),legend.position="bottom",
          strip.background=element_rect(fill="snow2", colour="snow2"),
          strip.text=element_text(size=8, colour="gray50"),
          strip.placement="outside",        
          strip.text.y.right=element_text(angle = 90)) +
    # Annotations
    geom_vline(xintercept=MeanLoss_act, linewidth=0.6, colour=vCol[1], linetype="dashed") + 
    annotate(geom="text", x=MeanLoss_act*0.8,  y=25, family=chosenFont,
             label=paste0(sprintf("%.1f", MeanLoss_act*100), "%"), size=3, colour=vCol[1], angle=90) +
    geom_vline(xintercept=MeanLoss_exp, linewidth=0.6, colour=vCol[2], linetype="dashed") + 
    annotate(geom="text", x=MeanLoss_exp*5,  y=25, family=chosenFont,
             label = paste0(sprintf("%.1f", MeanLoss_exp*100), "%"), size=3, colour=vCol[2], angle=90) +
    # Facets & scales
    scale_x_continuous(breaks=pretty_breaks(), labels = scales::percent) +
    scale_colour_manual(values=c(vCol[1], vCol[2])) +
    scale_fill_manual(values=c(vCol[1], vCol[2])) +
    facet_grid(FacetLabel ~., scales="free")+
    guides(fill=guide_legend(title = NULL), colour=guide_legend(title = NULL)))


# --- 3.2 Compare expected write-off LGDs with actuals
# - Filter for non-sensical loss rates
datCredit_WOFFs[, LossRate_est_adv:=ifelse(LossRate_est_adv<=1 & LossRate_est_adv>=0, LossRate_est_adv, NA)]

# - Mean expected loss
(MeanLoss_act_W <- mean(datCredit_WOFFs$LossRate_Real, na.rm=T))
(MeanLoss_exp_W <- mean(datCredit_WOFFs$LossRate_est_adv, na.rm=T))

# - Create plotting data
plotData <- melt(datCredit_WOFFs, measure.vars=c("LossRate_Real", "LossRate_est_adv"),
                 variable.name="Type",value.name="LossRate")
plotData[, Type:=factor(Type,levels=c("LossRate_Real", "LossRate_est_adv"),
                        labels=c("Actual loss rate", "DtH-Advanced"))]
plotData[, FacetLabel:="Resolved defaults [cures/write-offs]"]

# - Plot
(gOverlay_WOFFs <- ggplot(plotData, aes(x=LossRate)) + 
    theme_bw() +
    geom_histogram(aes(y=after_stat(density), fill=Type, colour=Type), alpha=0.35,
                   bins=50, position="identity") +
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
    labs(x="", y="", title=paste0("Write-offs only")) +
    # Annotations
    geom_vline(xintercept=MeanLoss_act_W, linewidth=0.6, colour=vCol[1], linetype="dashed") + 
    annotate(geom="text", x=MeanLoss_act_W*0.9, y=30, family=chosenFont,
             label=paste0(sprintf("%.1f", MeanLoss_act_W*100), "%"), size=3, colour=vCol[1], angle=90) +
    geom_vline(xintercept=MeanLoss_exp_W, linewidth=0.6, colour=vCol[2], linetype="dashed") + 
    annotate(geom="text", x=MeanLoss_exp_W*3, y=30, family=chosenFont,
             label=paste0(sprintf("%.1f", MeanLoss_exp_W*100), "%"), size=3, colour=vCol[2], angle=90) +
    annotate("label", x=0.6, y=40, label=stats_text,
             hjust=0, vjust=1, family=chosenFont,
             size=4, fill="white", colour="black", label.size=0.5) +
    # Facets & scales
    scale_x_continuous(breaks=pretty_breaks(), labels=scales::percent) +
    scale_colour_manual(values=c(vCol[1], vCol[2])) +
    scale_fill_manual(values=c(vCol[1], vCol[2])))


# --- 3.3 Combine and save graphs
# - Combine graphs
ymin <- diff(ggplot_build(gOverlay)$layout$panel_params[[1]]$y.range) * 0.2
ymax <- max(ggplot_build(gOverlay)$layout$panel_params[[1]]$y.range) * 0.95
(plot.full <- gOverlay + annotation_custom(grob = ggplotGrob(gOverlay_WOFFs), xmin=0.2, xmax=1, ymin=ymin, ymax=ymax))

# - Save plot
dpi <- 240
ggsave(plot.full, file=paste0(genFigPath,"ActvsExp_twostage_DtH_Adv_A.png"),width=1200/dpi, height=1000/dpi,dpi=dpi, bg="white")




# ============ 4. LGDs from classical logistic regression model and the severity model ============

# --- 4.1 Compare overall expected LGDs with actuals
# - Calculate alternative loss rate using full write-off prediction
datCredit[, LossRate_est_classic_alt:=Hazard_classic*LossSeverity]
datCredit_WOFFs[, LossRate_est_classic_alt:=Hazard_classic*LossSeverity]

# - Mean expected loss
(MeanLoss_act <- mean(datCredit$LossRate_Real, na.rm=T))
(MeanLoss_exp <- mean(datCredit$LossRate_est_classic_alt, na.rm=T))

# - Estimate statistics on distributional differences
metrics<-evalModel_twostage(data_train=datCredit, actField="LossRate_Real", estField="LossRate_est_classic_alt")

# - Create plotting data
stats_text <- paste("KS: ", sprintf("%.1f%%", metrics$KS * 100), "\n",
                    "KL: ", sprintf("%.4f", metrics$KL), "\n",
                    "JS: ", sprintf("%.4f", metrics$JS),
                    sep = "")

# - Create plotting dataset
plotData <- melt(datCredit, measure.vars=c("LossRate_Real", "LossRate_est_classic_alt"),
                 variable.name="Type", value.name="LossRate")
plotData[, Type:=factor(Type,levels=c("LossRate_Real", "LossRate_est_classic_alt"),
                        labels=c("Empirical", "Logistic Regression"))]
plotData[, FacetLabel:="Resolved defaults [cures/write-offs]"]

# - Graphing parameters
chosenFont <-"Cambria"
vCol <- brewer.pal(10, "Paired")[c(8,6)]

# - Plot
(gOverlay <- ggplot(plotData, aes(x=LossRate)) + 
    theme_bw() +
    geom_histogram(aes(y=after_stat(density), fill=Type, colour=Type),
                   alpha=0.35,bins=50, position="identity") +
    labs(x=bquote({Realised~loss~rate~italic(L)}), y="Histogram of loss rates" ) +
    theme(text=element_text(family=chosenFont),legend.position="bottom",
          strip.background=element_rect(fill="snow2", colour="snow2"),
          strip.text=element_text(size=8, colour="gray50"),
          strip.placement="outside",        
          strip.text.y.right=element_text(angle = 90)) +
    # Annotations
    geom_vline(xintercept=MeanLoss_act, linewidth=0.6, colour=vCol[1], linetype="dashed") + 
    annotate(geom="text", x=MeanLoss_act*0.8,  y=25, family=chosenFont,
             label = paste0(sprintf("%.1f", MeanLoss_act*100), "%"), size=3, colour=vCol[1], angle=90) + 
    geom_vline(xintercept=MeanLoss_exp, linewidth=0.6, colour=vCol[2], linetype="dashed") + 
    annotate(geom="text", x=MeanLoss_exp*1.2,  y=25, family=chosenFont,
             label = paste0(sprintf("%.1f", MeanLoss_exp*100), "%"), size=3, colour=vCol[2], angle=90) + 
    # Facets & scales
    facet_grid(FacetLabel~., scales="free") +
    scale_x_continuous(breaks=pretty_breaks(), labels = scales::percent) +
    scale_colour_manual(values=c(vCol[1], vCol[2])) +
    scale_fill_manual(values=c(vCol[1], vCol[2])) +
    guides(fill=guide_legend(title = NULL), colour=guide_legend(title = NULL)))


# --- 4.2 Compare expected write-off LGDs with actuals
# - Filter for non-sensical loss rates
datCredit_WOFFs[, LossRate_est_classic_alt:=ifelse(LossRate_est_classic_alt<=1 & LossRate_est_classic_alt>=0, LossRate_est_classic_alt, NA)]

# - Mean expected loss
(MeanLoss_act_W <- mean(datCredit_WOFFs$LossRate_Real, na.rm=T))
(MeanLoss_exp_W <- mean(datCredit_WOFFs$LossRate_est_classic_alt, na.rm=T))

# - Create plotting data
plotData <- melt(datCredit_WOFFs, measure.vars = c("LossRate_Real", "LossRate_est_classic_alt"), variable.name = "Type",value.name = "LossRate")
plotData[, Type:=factor(Type,levels=c("LossRate_Real", "LossRate_est_classic_alt"),
                        labels=c("Actual loss rate", "Logistic Regression"))]
plotData[, FacetLabel:="Resolved defaults [cures/write-offs]"]

# - Plot
(gOverlay_hist <- ggplot(plotData, aes(x=LossRate)) + 
    theme_bw() +
    geom_histogram(aes(y=after_stat(density), fill=Type, colour=Type), alpha=0.35,
                   bins=50, position="identity") +
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
    labs(x="", y="", title=paste0("Write-offs only")) +
    # Annotations
    geom_vline(xintercept=MeanLoss_act_W, linewidth=0.6, colour=vCol[1], linetype="dashed") + 
    annotate(geom="text", x=MeanLoss_act_W*0.925, y=3, family=chosenFont,
             label = paste0(sprintf("%.1f", MeanLoss_act_W*100), "%"), size=3, colour=vCol[1], angle=90) + 
    geom_vline(xintercept=MeanLoss_exp_W, linewidth=0.6, colour=vCol[2], linetype="dashed") + 
    annotate(geom="text", x=MeanLoss_exp_W*0.925, y=3, family=chosenFont,
             label = paste0(sprintf("%.1f", MeanLoss_exp_W*100), "%"), size=3, colour=vCol[2], angle=90) + 
    annotate("label", x=0.6, y=4, label=stats_text,
             hjust=0, vjust=1, family=chosenFont,
             size=4, fill="white", colour="black", label.size=0.5) +
    # Facets & scales
    scale_x_continuous(breaks=pretty_breaks(), labels=scales::percent) +
    scale_colour_manual(values=c(vCol[1], vCol[2])) +
    scale_fill_manual(values   = c(vCol[1], vCol[2])))


# --- 4.3 Combine and save graphs
# - Combine graphs
ymin <- diff(ggplot_build(gOverlay)$layout$panel_params[[1]]$y.range) * 0.2
ymax <- max(ggplot_build(gOverlay)$layout$panel_params[[1]]$y.range) * 0.95
(plot.full <- gOverlay + annotation_custom(grob = ggplotGrob(gOverlay_hist), xmin=0.2, xmax=1, ymin=ymin, ymax=ymax))

# - Save plot
dpi <- 240
ggsave(plot.full, file=paste0(genFigPath,"ActvsExp_twostage_LR_A.png"),width=1200/dpi, height=1000/dpi,dpi=dpi, bg="white")




# ============ 5. LGDs from survival tree and the severity model ============

# --- 5.1 Compare overall expected LGDs with actuals
# - Mean expected loss
(MeanLoss_act <- mean(datCredit$LossRate_Real, na.rm=T))
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
    labs(x=bquote({Realised~loss~rate~italic(L)}), y="Histogram of loss rates" ) +
    theme(text=element_text(family=chosenFont),legend.position="bottom",
          strip.background=element_rect(fill="snow2", colour="snow2"),
          strip.text=element_text(size=8, colour="gray50"),
          strip.placement="outside",        
          strip.text.y.right=element_text(angle = 90)) +
    # Annotations
    geom_vline(xintercept=MeanLoss_act, linewidth=0.6, colour=vCol[1], linetype="dashed") + 
    annotate(geom="text", x=MeanLoss_act*0.8, y=25, family=chosenFont,
             label=paste0(sprintf("%.1f", MeanLoss_act*100), "%"), size=3, colour=vCol[1], angle=90) +
    geom_vline(xintercept=MeanLoss_exp, linewidth=0.6, colour=vCol[2], linetype="dashed") + 
    annotate(geom="text", x=MeanLoss_exp*4, y=25, family=chosenFont,
             label=paste0(sprintf("%.1f", MeanLoss_exp*100), "%"), size=3, colour=vCol[2], angle=90) +
    # Facets & scales
    facet_grid(FacetLabel ~., scales="free") +
    scale_x_continuous(breaks=pretty_breaks(), labels = scales::percent) +
    scale_colour_manual(values=c(vCol[1], vCol[2])) +
    scale_fill_manual(values=c(vCol[1], vCol[2])) +
    guides(fill=guide_legend(title = NULL), colour=guide_legend(title = NULL)))


# --- 5.2 Compare expected write-off LGDs with actuals
# - Filter for non-sensical loss rates
datCredit_WOFFs[, LossRate_est_survtree:=ifelse(LossRate_est_survtree<=1 & LossRate_est_survtree>=0,
                                                LossRate_est_survtree, NA)]

# Mean expected loss
(MeanLoss_act_W <- mean(datCredit_WOFFs$LossRate_Real, na.rm=T))
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
    labs(x="", y="", title=paste0("Write-offs only")) +
    # Annotations
    geom_vline(xintercept=MeanLoss_act_W, linewidth=0.6, colour=vCol[1], linetype="dashed") + 
    annotate(geom="text", x=MeanLoss_act_W*0.95, y=15, family=chosenFont,
             label=paste0(sprintf("%.1f", MeanLoss_act_W*100), "%"), size=3, colour=vCol[1], angle=90) + 
    geom_vline(xintercept=MeanLoss_exp_W, linewidth=0.6, colour=vCol[2], linetype="dashed") + 
    annotate(geom="text", x=MeanLoss_exp_W*4, y=15, family=chosenFont,
             label=paste0(sprintf("%.1f", MeanLoss_exp_W*100), "%"), size=3, colour=vCol[2], angle=90) + 
    annotate("label", x=0.6, y=20, label=stats_text, hjust=0, vjust=1, family=chosenFont,
             size=4, fill="white", colour="black", label.size=0.5) +
    # Facets & scales
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
ggsave(plot.full, file=paste0(genFigPath,"ActvsExp_twostage_ST.png"), width=1200/dpi, height=1000/dpi,dpi=dpi, bg="white")


# --- 5.4 Cleanup
suppressWarnings(rm(datCredit, datCredit_train, datCredit_valid, datAdd, datCredit_acc, datCredit_WOFFs_acc,
                    datCredit_WOFFs, modLR_Adv, modLR_Bas, modLR_Classic, modGLM_Severity_CPG,
                    g1, g2, gOverlay, gOverlay_hist, gOverlay_WOFFs, plot.full, plotData))
gc()




# ============ 7. Housekeeping ============

cleanEnviron()


