# ================= LGD DISTRIBUTION ANALYSIS - Two-STAGE MODELS ===============
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
#   - 4e.SurvivalTrees.R
#   - 4f.InputSpace_TwoStage_MultiDtH
#   - 4g.InputSpace_TwoStage_MultiSurvTrees
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
#   - MultiDtH_WOff | DtH-Multinomial model fitted in script 4f
#   - MultiCI_Tree | Multinomial CI-tree fitted in script 4g
#
# -- Outputs:
#   - <Analytics> | Graphs
# ==============================================================================

### MM: - Note that these LGD-distributions are constructed using each observation of an account (i.e., multiple LGDs from each account are used)
###     - Should we consider giving a view of the distribution only at default point?
###     - The question then is why do we evaluate it only at default, should we take a few other time in default points? Maybe even all of them and
###       get a aggregated view over time in default spell?
### AB: For the CS-LR model, we could perhaps take a randomly selected observation within a default spell
#       and produce the write-off probability accordingly. This would put the CS-LR model on a fairer foot when
#       comparing


# ============ 1. Preliminaries ============

# --- 1.1 Load and prepare data
# - Confirm prepared datasets are loaded into memory
if (!exists('datCredit_train_CDH_sub')) unpack.ffdf(paste0(genPath,"creditdata_train_CDH_sub"), tempPath); gc()
if (!exists('datCredit_valid_CDH_sub')) unpack.ffdf(paste0(genPath,"creditdata_valid_CDH_sub"), tempPath); gc()

# - Ensure order of data is correct
setorder(datCredit_train_CDH_sub, LoanID, Date)
setorder(datCredit_valid_CDH_sub, LoanID, Date)

# - Use only default spells
datCredit_train <- datCredit_train_CDH_sub[!is.na(DefSpell_Key),]
datCredit_valid <- datCredit_valid_CDH_sub[!is.na(DefSpell_Key),]

# - Remove objects
rm(datCredit_train_CDH_sub, datCredit_valid_CDH_sub); gc()

# - Combine training and validation datasets to facilitate "better" (smooth) graphs
datCredit <- rbind(datCredit_train, datCredit_valid)

# - Create a spell-level target variable for write-off instance
datCredit[, DefSpell_Event2:=DefSpell_Event]
datCredit[, DefSpell_Event:=DefSpellResol_Type_Hist=="WOFF"]

# - Transform categorical variables to factor
# Payment method
datCredit[,pmnt_method_grp_fac:=as.factor(pmnt_method_grp)]
# Previous defaults
datCredit[,PrevDefaults_fac:=as.factor(PrevDefaults)]


# --- 1.2 Subset data to contain only one record per spell
# - Choose a randomly selected point during spell life and create dataset accordingly
set.seed(1, kind="Mersenne-Twister")
datLGD <- datCredit[, .SD[sample(.N, 1)], by=.(DefSpell_Key)]

# - Quick diagnostics on spell-level records
# Overall distribution
describe2(datLGD$LossRate_Real); hist(datLGD$LossRate_Real, freq=F)
# Write-offs only
describe2(datLGD[DefSpellResol_Type_Hist=="WOFF", LossRate_Real])
hist(datLGD[DefSpellResol_Type_Hist=="WOFF", LossRate_Real], freq=F)

# - Merge selected point back onto main datao object
### NOTE: This chosen point will be instrumental in calculating key quantities later
datCredit <- merge(datCredit, datLGD[,.(DefSpell_Key, SelPoint=DefSpell_Counter)],
                   by=c("DefSpell_Key"), all.x=T)

# [SANITY CHECK] Perform lookup on specific default spell
lookup <- subset(datCredit, DefSpell_Key == unique(datCredit[DefSpellResol_Type_Hist=="WOFF" & DefSpell_Age>=30 & LossRate_Real>0, DefSpell_Key])[3000],
                 select=c("DefSpell_Key", "Date", "TimeInDefSpell", "DefSpell_Counter", "SelPoint", "LossRate_Real"))


# --- 1.3 Handle left-truncated spells by adding a starting record 
### NOTE:  This is necessary for calculating certain survival quantities later
# Create an additional record for each default spell
datAdd <- subset(datCredit, Counter==1 & TimeInDefSpell>1)
datAdd[, TimeInDefSpell:=TimeInDefSpell-1]
datAdd[, Time_Binned := timeBinning(TimeInDefSpell)]
datAdd[, Counter:=0]
# Add record to main dataset
datCredit <- rbind(datCredit, datAdd); setorder(datCredit, DefSpell_Key, Date)


# --- 1.4 Load models
# - Basic discrete-time hazard model
modLR_Bas <- readRDS(paste0(genObjPath,"CoxDisc_Basic_Model.rds"))

# - Advanced discrete-time hazard model
modLR_Adv <- readRDS(paste0(genObjPath,"CoxDisc_Advanced_Model.rds"))

# - Cross-sectional logistic regression (CS-LR) model
modLR_Classic <- readRDS(paste0(genObjPath,"LR_Model.rds"))

# - Conditional Inference (CI) Tree model
SurvTree_CTree <- readRDS(paste0(genObjPath,"SurvTree_CTree.rds"))

# - Compound Poisson-Gamma (CPG) loss severity model
modGLM_Severity_CPG <- readRDS(paste0(genObjPath,"Severity_CPH_Model.rds"))

# - Multinomial discrete-time hazard model
if (!exists('modMLR')) unpack.ffdf(paste0(genPath,"MultiDtH_WOff"), tempPath)

# - Multinomial Conditional inference (CI) tree model
if (!exists('modMultiSurvTree')) unpack.ffdf(paste0(genPath,"MultiCI_Tree"), tempPath)




# ============ 2. Prediction tasks ============
# All tasks relating to prediction of key quantities

# --- 2.1 Estimate event rates amongst survival quantities from DtH-models
# - Predict hazard h(t) = P(T=t | T>= t) in discrete-time
datCredit[, Hazard_adv:=predict(modLR_Adv, newdata=datCredit, type = "response")]
datCredit[, Hazard_bas:=predict(modLR_Bas, newdata=datCredit, type = "response")]

# - Predict cause-specific hazard h_g(t) = P(T=t, G=g | T >= t) in discrete-time | DtH-Multinomial
matPredictions <- predict(modMLR, newdata=datCredit, type = "probs")
datCredit[, Hazard_WriteOff:=matPredictions[,"Write-off"]]
datCredit[, Hazard_Cure:=matPredictions[,"Cured"]]

# - Predict cause-specific hazard h_g(t) = P(T=t, G=g | T >= t) in discrete-time | Multinomial CI-tree
matPredictions_tree <- predict(modMultiSurvTree, newdata=datCredit, type = "prob")
datCredit[, Hazard_WriteOff_tree := matPredictions_tree[, "Write-off"]]
datCredit[, Hazard_Cured_tree := matPredictions_tree[, "Cured"]]

# - Derive survival probability S(t) = prod(1 - hazard)
datCredit[, Survival_adv:=cumprod(1-Hazard_adv), by=.(DefSpell_Key)]
datCredit[, Survival_bas:=cumprod(1-Hazard_bas), by=.(DefSpell_Key)]

# - Derive (event-free) survival probability S(t) = prod(1 - h_1(t) - h_2(t))
datCredit[, Survival_overall:=cumprod(1-Hazard_WriteOff-Hazard_Cure), by=.(DefSpell_Key)]
datCredit[, Survival_overall_tree:=cumprod(1-Hazard_WriteOff_tree-Hazard_Cured_tree), by=.(DefSpell_Key)]

# - Derive discrete density, or event probability f(t) = S(t-1) - S(t)
datCredit[, EventRate_adv:=shift(Survival_adv, type="lag", n=1, fill=1) - Survival_adv, by=.(DefSpell_Key)]
datCredit[, EventRate_bas:=shift(Survival_bas, type="lag", n=1, fill=1) - Survival_bas, by=.(DefSpell_Key)]

# - Derive event rates as f_1(t) = S(t-1) . h_1(t)
datCredit[, EventRate_WriteOff := shift(Survival_overall, n=1,fill=1) * Hazard_WriteOff, by=.(DefSpell_Key)]
datCredit[, EventRate_WriteOff_tree := shift(Survival_overall_tree, n=1,fill=1) * Hazard_WriteOff_tree, by=.(DefSpell_Key)]

# - Remove added rows
datCredit <- subset(datCredit, Counter>0)


# --- 2.2 Predict survival quantities using survival tree
# - Get training data from survival tree
datCredit_train_SurvTree <- SurvTree_CTree$datTrain

# - Get node predictions from tree
datCredit[,Node:=predict(SurvTree_CTree$survTree, datCredit, type="node")]
datCredit_train_SurvTree[,Node:=predict(SurvTree_CTree$survTree, datCredit_train_SurvTree, type="node")]

# - Fit a survival object for each node | Using the training data only to align with how the tree was fitted
### NOTE: Event rates are calculated for each point in a spell's life based on only
###       the node prediction at each point
datHaz <- predSurv(survTree=SurvTree_CTree$survTree, datGiven=datCredit_train_SurvTree,
                   fld_DefSpell_Age="DefSpell_Age", fld_DefSpell_Event="DefSpell_Event")

# - Join hazards back to main dataset
datCredit <- merge(datCredit, datHaz[, list(Node, Time, Survival_survtree=Surv, Hazard_survtree=Hazard,
                                            EventRate_survtree=EventRate)],
                   by.x=c("Node", "TimeInDefSpell"), by.y=c("Node", "Time"), all.x=T)

# - Ensure order of data is correct
setkey(datCredit, LoanID, Date)


# --- 2.3 Calculate overall and conditional future write-off probabilities

# - Calculate overall future write-off probability
### NOTE: The prediction from the CS-LR model is already the future write-off probability by definition
datCredit[, EventProb_CSLR:=predict(modLR_Classic, newdata=.SD[], type="response")]

# - Calculate Cumulative unconditional write-off probabilities per survival model F(t)
datCredit[, Cumul_EventRate_bas := cumsum(EventRate_bas), by = .(DefSpell_Key)]
datCredit[, Cumul_EventRate_adv := cumsum(EventRate_adv), by = .(DefSpell_Key)]
datCredit[, Cumul_EventRate_survtree := cumsum(EventRate_survtree), by = .(DefSpell_Key)]
datCredit[, Cumul_EventRate_WriteOff := cumsum(EventRate_WriteOff), by = .(DefSpell_Key)]
datCredit[, Cumul_EventRate_WriteOff_tree := cumsum(EventRate_WriteOff_tree), by = .(DefSpell_Key)]

# - Calculate conditional future write-off probability
### NOTE: This hazard-type quantity represents the future write-off probability given survival up 
# to the randomly chosen selected point (SelPoint), u. The spell age T_{ij} is taken as the maximum
# observed time window, whereafter we express the conditional future write-off probability as:
# ( F(T_{ij}) - F(u) ) / S(u)
datCredit[, q_SelPoint_bas := condFutureProb(SelPoint[1], Survival_bas, Cumul_EventRate_bas), 
          by = .(DefSpell_Key)]
datCredit[, q_SelPoint_adv := condFutureProb(SelPoint[1], Survival_adv, Cumul_EventRate_adv), 
          by = .(DefSpell_Key)]
datCredit[, q_SelPoint_tree := condFutureProb(SelPoint[1], Survival_survtree, Cumul_EventRate_survtree), 
          by = .(DefSpell_Key)]
datCredit[, q_SelPoint_WriteOff := condFutureProb(SelPoint[1], Survival_overall, Cumul_EventRate_WriteOff), 
          by = .(DefSpell_Key)]
datCredit[, q_SelPoint_WriteOff_tree := condFutureProb(SelPoint[1], Survival_overall_tree, Cumul_EventRate_WriteOff_tree), 
          by = .(DefSpell_Key)]


# --- 2.4 Diagnostics on predicted write-off probabilities

# [DIAGNOSTIC] Conditional future write-off probabilities | DtH-Basic
describe2(datCredit[DefSpell_Counter==SelPoint, q_SelPoint_bas])
hist(datCredit[DefSpell_Counter==SelPoint, q_SelPoint_bas], freq=F)
### RESULTS: Diagnostic passed, no missings/NA-values, nor any implausible values <0 or >1

# [DIAGNOSTIC] Conditional future write-off probabilities | DtH-Advanced
describe2(datCredit[DefSpell_Counter==SelPoint, q_SelPoint_adv])
hist(datCredit[DefSpell_Counter==SelPoint, q_SelPoint_adv], freq=F)
### RESULTS: Diagnostic passed, no missings/NA-values, nor any implausible values <0 or >1

# [DIAGNOSTIC] Conditional future write-off probabilities | CI-tree
describe2(datCredit[DefSpell_Counter==SelPoint, q_SelPoint_tree])
hist(datCredit[DefSpell_Counter==SelPoint, q_SelPoint_tree], freq=F)
### RESULTS: Missing values and implausible values found

# [DIAGNOSTIC] Conditional future write-off probabilities | DtH-Multinomial
describe2(datCredit[DefSpell_Counter==SelPoint, q_SelPoint_WriteOff])
hist(datCredit[DefSpell_Counter==SelPoint, q_SelPoint_WriteOff], freq=F)
### RESULTS: Diagnostic passed, no missings/NA-values, nor any implausible values <0 or >1

# [DIAGNOSTIC] Conditional future write-off probabilities | DtH-Multinomial
describe2(datCredit[DefSpell_Counter==SelPoint, q_SelPoint_WriteOff_tree])
hist(datCredit[DefSpell_Counter==SelPoint, q_SelPoint_WriteOff_tree], freq=F)
### RESULTS: Diagnostic passed, no missings/NA-values, nor any implausible values <0 or >1


# --- 2.4 Prediction fixes for CI-tree
# Some implausible write-off probabilities obtained for CI-tree, likely due to node-switches during 
# spell life. Fix expediently by using mean-imputing missing values, which numbered only 3 cases for
# CI-tree, and 112 cases for Multinomial CI-tree; thus low prevalence.
# Also, we cap extreme probability values

# - Mean imputation of missing probabilities
datCredit[, q_SelPoint_tree := ifelse(is.na(q_SelPoint_tree), mean(q_SelPoint_tree,na.rm=T), q_SelPoint_tree),
          by=list(DefSpell_Key)]
datCredit[, q_SelPoint_WriteOff_tree := ifelse(is.na(q_SelPoint_WriteOff_tree), 
                                               mean(q_SelPoint_WriteOff_tree,na.rm=T), q_SelPoint_WriteOff_tree)]

# - Cap extreme values in probabilities
datCredit[, q_SelPoint_tree := ifelse(q_SelPoint_tree>1, 1, q_SelPoint_tree)]



# --- 2.5 Calculate expected LGD
# - Predict loss severities
datCredit[, LossSeverity:=predict(modGLM_Severity_CPG, newdata=datCredit, type="response")]

# - Interact write-off probabilities with severities
datCredit[, `:=`(LossRate_est_bas=LossSeverity*q_SelPoint_bas,
                 LossRate_est_adv=LossSeverity*q_SelPoint_adv,
                 LossRate_est_tree=LossSeverity*q_SelPoint_tree,
                 LossRate_est_WriteOff=LossSeverity*q_SelPoint_WriteOff,
                 LossRate_est_WriteOff_tree=LossSeverity*q_SelPoint_WriteOff_tree,
                 LossRate_est_CSLR=LossSeverity*EventProb_CSLR)]


# --- 2.6 Subset data again to spell-level, containing LGD-predictions
datLGD <- subset(datCredit, DefSpell_Counter==SelPoint)

# - Identify where the loss rate is out of bounds and not feasible
datLGD[, OOB_Ind:=ifelse(LossRate_Real<0 | LossRate_Real>1,1,0)]

# - Subset to include only relevant data
datLGD <- subset(datLGD, OOB_Ind==0)

# - Subset for write-offs only to create inset plots
datLGD_WOFFs <- subset(datLGD, DefSpellResol_Type_Hist=="WOFF" & OOB_Ind==0)





# ======= 3. LGD-distributions - DtH-Basic model ======= 

# --- 1. Compare overall expected LGDs with actuals
# - Estimate statistics on distributional differences
metrics <- evalModel_twostage(data_train=datLGD, actField="LossRate_Real", estField="LossRate_est_bas")

# - Create plotting data
stats_text <- paste("KL: ", sprintf("%.4f", metrics$KL), "\n",
                    "JS: ", sprintf("%.4f", metrics$JS), sep = "")

# - Mean expected loss (Actual vs expected)
(MeanLoss_exp <- mean(datLGD$LossRate_est_bas, na.rm=T))
(MeanLoss_act <- mean(datLGD$LossRate_Real, na.rm=T))

# - Create plotting dataset
plotData <- melt(datLGD, measure.vars=c("LossRate_Real", "LossRate_est_bas"),
                 variable.name="Type",value.name="LossRate")
plotData[, Type:=factor(Type,levels=c("LossRate_Real", "LossRate_est_bas"),
                        labels=c("Empirical", "DtH-Basic"))]

# - Graphing parameters
chosenFont <-"Cambria"
vCol <- brewer.pal(10, "Paired")[c(8,6)]

# - Plot
(gOverlay <- ggplot(plotData, aes(x=LossRate)) + theme_minimal() +
    geom_histogram(aes(y=after_stat(density), fill=Type, colour=Type),
                   alpha=0.35,bins=50, position="identity") +
    labs(x=bquote({Loss~rate~italic(L)}), y="Density" ) +
    theme(text=element_text(family=chosenFont),legend.position="bottom",
          strip.background=element_rect(fill="snow2", colour="snow2"),
          strip.text=element_text(size=8, colour="gray50"),
          strip.placement="outside",        
          strip.text.y.right=element_text(angle = 90)) +
    # Annotations
    geom_vline(xintercept=MeanLoss_exp, linewidth=0.6, colour=vCol[2], linetype="dashed") + 
    annotate(geom="text", x=MeanLoss_exp*1.45, y=25, family=chosenFont,
             label = paste0("Mean: ", sprintf("%.1f", MeanLoss_exp*100), "%"), size=3, colour=vCol[2], angle=90) +  
    # Facets & scales
    scale_x_continuous(breaks=pretty_breaks(), labels = scales::percent) +
    scale_colour_manual(values=c(vCol[1], vCol[2])) +
    scale_fill_manual(values=c(vCol[1], vCol[2])) +
    guides(fill=guide_legend(title=NULL), colour=guide_legend(title=NULL)))


# --- 2. Compare expected write-off LGDs with actuals
# - Filter for non-sensical loss rates
datLGD_WOFFs[, LossRate_est_bas:=ifelse(LossRate_est_bas<=1 & LossRate_est_bas>=0, LossRate_est_bas, NA)]

# - Mean expected loss
(MeanLoss_act_W <- mean(datLGD_WOFFs$LossRate_Real, na.rm=T))
(MeanLoss_exp_W <- mean(datLGD_WOFFs$LossRate_est_bas, na.rm=T))

# - Create plotting data
plotData <- melt(datLGD_WOFFs, measure.vars=c("LossRate_Real", "LossRate_est_bas"),
                 variable.name="Type", value.name="LossRate")
plotData[, Type:=factor(Type,levels=c("LossRate_Real", "LossRate_est_bas"),
                        labels=c("Actual loss rate", "DtH-Basic"))]
plotData[, FacetLabel:="Resolved defaults [cures/write-offs]"]

# - Plot
(gOverlay_WOFFs <- ggplot(plotData, aes(x=LossRate)) + theme_minimal() +
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
    annotate(geom="text", x=MeanLoss_exp_W*1.4,  y=7, family=chosenFont,
             label = paste0("Mean: ", sprintf("%.1f", MeanLoss_exp_W*100), "%"), size=3, colour=vCol[2], angle=90) + 
    annotate("label", x=0.6, y=10, label = stats_text,
             hjust=0, vjust =1, family = chosenFont,
             size=4, fill="white", colour="black", label.size=0.5) +
    # Facets & scales
    scale_x_continuous(breaks=pretty_breaks(), labels=scales::percent) +
    scale_colour_manual(values=c(vCol[1], vCol[2])) +
    scale_fill_manual(values=c(vCol[1], vCol[2])))


# --- 3 Combine and save graphs
# - Combine graphs
ymin <- diff(ggplot_build(gOverlay)$layout$panel_params[[1]]$y.range) * 0.2
ymax <- max(ggplot_build(gOverlay)$layout$panel_params[[1]]$y.range) * 0.95
(plot.full <- gOverlay + annotation_custom(grob=ggplotGrob(gOverlay_WOFFs), xmin=0.2, xmax=1, ymin=ymin, ymax=ymax))

# - Save plot
dpi <- 240
ggsave(plot.full, file=paste0(genFigPath,"ActvsExp_twostage_DtH_Bas_A.png"),width=1200/dpi, height=1000/dpi,dpi=dpi, bg="white")




# ======= 4. LGD-distributions - DtH-Advanced model ======= 

# --- 1. Compare overall expected LGDs with actuals
# - Estimate statistics on distributional differences
metrics <- evalModel_twostage(data_train=datLGD, actField="LossRate_Real", estField="LossRate_est_adv")

# - Create plotting data
stats_text <- paste("KL: ", sprintf("%.4f", metrics$KL), "\n",
                    "JS: ", sprintf("%.4f", metrics$JS), sep = "")

# - Mean expected loss (Actual vs expected)
(MeanLoss_exp <- mean(datLGD$LossRate_est_adv, na.rm=T))
(MeanLoss_act <- mean(datLGD$LossRate_Real, na.rm=T))

# - Create plotting dataset
plotData <- melt(datLGD, measure.vars=c("LossRate_Real", "LossRate_est_adv"),
                 variable.name="Type",value.name="LossRate")
plotData[, Type:=factor(Type,levels=c("LossRate_Real", "LossRate_est_adv"),
                        labels=c("Empirical", "DtH-Advanced"))]

# - Graphing parameters
chosenFont <-"Cambria"
vCol <- brewer.pal(10, "Paired")[c(8,6)]

# - Plot
(gOverlay <- ggplot(plotData, aes(x=LossRate)) + theme_minimal() +
    geom_histogram(aes(y=after_stat(density), fill=Type, colour=Type),
                   alpha=0.35,bins=50, position="identity") +
    labs(x=bquote({Loss~rate~italic(L)}), y="Density" ) +
    theme(text=element_text(family=chosenFont),legend.position="bottom",
          strip.background=element_rect(fill="snow2", colour="snow2"),
          strip.text=element_text(size=8, colour="gray50"),
          strip.placement="outside",        
          strip.text.y.right=element_text(angle = 90)) +
    # Annotations
    geom_vline(xintercept=MeanLoss_exp, linewidth=0.6, colour=vCol[2], linetype="dashed") + 
    annotate(geom="text", x=MeanLoss_exp*1.45, y=25, family=chosenFont,
             label = paste0("Mean: ", sprintf("%.1f", MeanLoss_exp*100), "%"), size=3, colour=vCol[2], angle=90) +  
    # Facets & scales
    scale_x_continuous(breaks=pretty_breaks(), labels = scales::percent) +
    scale_colour_manual(values=c(vCol[1], vCol[2])) +
    scale_fill_manual(values=c(vCol[1], vCol[2])) +
    guides(fill=guide_legend(title=NULL), colour=guide_legend(title=NULL)))


# --- 2. Compare expected write-off LGDs with actuals
# - Filter for non-sensical loss rates
datLGD_WOFFs[, LossRate_est_adv:=ifelse(LossRate_est_adv<=1 & LossRate_est_adv>=0, LossRate_est_adv, NA)]

# - Mean expected loss
(MeanLoss_act_W <- mean(datLGD_WOFFs$LossRate_Real, na.rm=T))
(MeanLoss_exp_W <- mean(datLGD_WOFFs$LossRate_est_adv, na.rm=T))

# - Create plotting data
plotData <- melt(datLGD_WOFFs, measure.vars=c("LossRate_Real", "LossRate_est_adv"),
                 variable.name="Type", value.name="LossRate")
plotData[, Type:=factor(Type,levels=c("LossRate_Real", "LossRate_est_adv"),
                        labels=c("Actual loss rate", "DtH-Advanced"))]
plotData[, FacetLabel:="Resolved defaults [cures/write-offs]"]

# - Plot
(gOverlay_WOFFs <- ggplot(plotData, aes(x=LossRate)) + theme_minimal() +
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
    annotate(geom="text", x=MeanLoss_exp_W*1.2,  y=5, family=chosenFont,
             label = paste0("Mean: ", sprintf("%.1f", MeanLoss_exp_W*100), "%"), size=3, colour=vCol[2], angle=90) + 
    annotate("label", x=0.6, y=7.5, label = stats_text,
             hjust=0, vjust =1, family = chosenFont,
             size=4, fill="white", colour="black", label.size=0.5) +
    # Facets & scales
    scale_x_continuous(breaks=pretty_breaks(), labels=scales::percent) +
    scale_colour_manual(values=c(vCol[1], vCol[2])) +
    scale_fill_manual(values=c(vCol[1], vCol[2])))


# --- 3 Combine and save graphs
# - Combine graphs
ymin <- diff(ggplot_build(gOverlay)$layout$panel_params[[1]]$y.range) * 0.2
ymax <- max(ggplot_build(gOverlay)$layout$panel_params[[1]]$y.range) * 0.95
(plot.full <- gOverlay + annotation_custom(grob=ggplotGrob(gOverlay_WOFFs), xmin=0.2, xmax=1, ymin=ymin, ymax=ymax))

# - Save plot
dpi <- 240
ggsave(plot.full, file=paste0(genFigPath,"ActvsExp_twostage_DtH_Adv_A.png"),width=1200/dpi, height=1000/dpi,dpi=dpi, bg="white")




# ======= 5. LGD-distributions - CS-LR model ======= 

# --- 1. Compare overall expected LGDs with actuals
# - Estimate statistics on distributional differences
metrics <- evalModel_twostage(data_train=datLGD, actField="LossRate_Real", estField="LossRate_est_CSLR")

# - Create plotting data
stats_text <- paste("KL: ", sprintf("%.4f", metrics$KL), "\n",
                    "JS: ", sprintf("%.4f", metrics$JS), sep = "")


# - Mean expected loss (Actual vs expected)
(MeanLoss_exp <- mean(datLGD$LossRate_est_CSLR, na.rm=T))
(MeanLoss_act <- mean(datLGD$LossRate_Real, na.rm=T))

# - Create plotting dataset
plotData <- melt(datLGD, measure.vars=c("LossRate_Real", "LossRate_est_CSLR"),
                 variable.name="Type",value.name="LossRate")
plotData[, Type:=factor(Type,levels=c("LossRate_Real", "LossRate_est_CSLR"),
                        labels=c("Empirical", "CS-LR"))]

# - Graphing parameters
chosenFont <-"Cambria"
vCol <- brewer.pal(10, "Paired")[c(8,6)]

# - Plot
(gOverlay <- ggplot(plotData, aes(x=LossRate)) + theme_minimal() +
    geom_histogram(aes(y=after_stat(density), fill=Type, colour=Type),
                   alpha=0.35,bins=50, position="identity") +
    labs(x=bquote({Loss~rate~italic(L)}), y="Density" ) +
    theme(text=element_text(family=chosenFont),legend.position="bottom",
          strip.background=element_rect(fill="snow2", colour="snow2"),
          strip.text=element_text(size=8, colour="gray50"),
          strip.placement="outside",        
          strip.text.y.right=element_text(angle = 90)) +
    # Annotations
    geom_vline(xintercept=MeanLoss_exp, linewidth=0.6, colour=vCol[2], linetype="dashed") + 
    annotate(geom="text", x=MeanLoss_exp*1.45, y=25, family=chosenFont,
             label = paste0("Mean: ", sprintf("%.1f", MeanLoss_exp*100), "%"), size=3, colour=vCol[2], angle=90) +  
    # Facets & scales
    scale_x_continuous(breaks=pretty_breaks(), labels = scales::percent) +
    scale_colour_manual(values=c(vCol[1], vCol[2])) +
    scale_fill_manual(values=c(vCol[1], vCol[2])) +
    guides(fill=guide_legend(title=NULL), colour=guide_legend(title=NULL)))


# --- 2. Compare expected write-off LGDs with actuals
# - Filter for non-sensical loss rates
datLGD_WOFFs[, LossRate_est_CSLR:=ifelse(LossRate_est_CSLR<=1 & LossRate_est_CSLR>=0, LossRate_est_CSLR, NA)]

# - Mean expected loss
(MeanLoss_act_W <- mean(datLGD_WOFFs$LossRate_Real, na.rm=T))
(MeanLoss_exp_W <- mean(datLGD_WOFFs$LossRate_est_CSLR, na.rm=T))

# - Create plotting data
plotData <- melt(datLGD_WOFFs, measure.vars=c("LossRate_Real", "LossRate_est_CSLR"),
                 variable.name="Type", value.name="LossRate")
plotData[, Type:=factor(Type,levels=c("LossRate_Real", "LossRate_est_CSLR"),
                        labels=c("Actual loss rate", "CS-LR"))]
plotData[, FacetLabel:="Resolved defaults [cures/write-offs]"]

# - Plot
(gOverlay_WOFFs <- ggplot(plotData, aes(x=LossRate)) + theme_minimal() +
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
    annotate(geom="text", x=MeanLoss_exp_W*1.2,  y=3, family=chosenFont,
             label = paste0("Mean: ", sprintf("%.1f", MeanLoss_exp_W*100), "%"), size=3, colour=vCol[2], angle=90) + 
    annotate("label", x=0.6, y=3.5, label = stats_text,
             hjust=0, vjust =1, family = chosenFont,
             size=4, fill="white", colour="black", label.size=0.5) +
    # Facets & scales
    scale_x_continuous(breaks=pretty_breaks(), labels=scales::percent) +
    scale_colour_manual(values=c(vCol[1], vCol[2])) +
    scale_fill_manual(values=c(vCol[1], vCol[2])))


# --- 3 Combine and save graphs
# - Combine graphs
ymin <- diff(ggplot_build(gOverlay)$layout$panel_params[[1]]$y.range) * 0.2
ymax <- max(ggplot_build(gOverlay)$layout$panel_params[[1]]$y.range) * 0.95
(plot.full <- gOverlay + annotation_custom(grob=ggplotGrob(gOverlay_WOFFs), xmin=0.2, xmax=1, ymin=ymin, ymax=ymax))

# - Save plot
dpi <- 240
ggsave(plot.full, file=paste0(genFigPath,"ActvsExp_twostage_LR_A.png"),width=1200/dpi, height=1000/dpi,dpi=dpi, bg="white")




# ======= 6. LGD-distributions - CI-tree ======= 

# --- 1. Compare overall expected LGDs with actuals
# - Estimate statistics on distributional differences
metrics <- evalModel_twostage(data_train=datLGD, actField="LossRate_Real", estField="LossRate_est_tree")

# - Create plotting data
stats_text <- paste("KL: ", sprintf("%.4f", metrics$KL), "\n",
                    "JS: ", sprintf("%.4f", metrics$JS), sep = "")


# - Mean expected loss (Actual vs expected)
(MeanLoss_exp <- mean(datLGD$LossRate_est_tree, na.rm=T))
(MeanLoss_act <- mean(datLGD$LossRate_Real, na.rm=T))

# - Create plotting dataset
plotData <- melt(datLGD, measure.vars=c("LossRate_Real", "LossRate_est_tree"),
                 variable.name="Type",value.name="LossRate")
plotData[, Type:=factor(Type,levels=c("LossRate_Real", "LossRate_est_tree"),
                        labels=c("Empirical", "CI-tree"))]

# - Graphing parameters
chosenFont <-"Cambria"
vCol <- brewer.pal(10, "Paired")[c(8,6)]

# - Plot
(gOverlay <- ggplot(plotData, aes(x=LossRate)) + theme_minimal() +
    geom_histogram(aes(y=after_stat(density), fill=Type, colour=Type),
                   alpha=0.35,bins=50, position="identity") +
    labs(x=bquote({Loss~rate~italic(L)}), y="Density" ) +
    theme(text=element_text(family=chosenFont),legend.position="bottom",
          strip.background=element_rect(fill="snow2", colour="snow2"),
          strip.text=element_text(size=8, colour="gray50"),
          strip.placement="outside",        
          strip.text.y.right=element_text(angle = 90)) +
    # Annotations
    geom_vline(xintercept=MeanLoss_exp, linewidth=0.6, colour=vCol[2], linetype="dashed") + 
    annotate(geom="text", x=MeanLoss_exp*1.45, y=25, family=chosenFont,
             label = paste0("Mean: ", sprintf("%.1f", MeanLoss_exp*100), "%"), size=3, colour=vCol[2], angle=90) +  
    # Facets & scales
    scale_x_continuous(breaks=pretty_breaks(), labels = scales::percent) +
    scale_colour_manual(values=c(vCol[1], vCol[2])) +
    scale_fill_manual(values=c(vCol[1], vCol[2])) +
    guides(fill=guide_legend(title=NULL), colour=guide_legend(title=NULL)))


# --- 2. Compare expected write-off LGDs with actuals
# - Filter for non-sensical loss rates
datLGD_WOFFs[, LossRate_est_tree:=ifelse(LossRate_est_tree<=1 & LossRate_est_tree>=0, LossRate_est_tree, NA)]

# - Mean expected loss
(MeanLoss_act_W <- mean(datLGD_WOFFs$LossRate_Real, na.rm=T))
(MeanLoss_exp_W <- mean(datLGD_WOFFs$LossRate_est_tree, na.rm=T))

# - Create plotting data
plotData <- melt(datLGD_WOFFs, measure.vars=c("LossRate_Real", "LossRate_est_tree"),
                 variable.name="Type", value.name="LossRate")
plotData[, Type:=factor(Type,levels=c("LossRate_Real", "LossRate_est_tree"),
                        labels=c("Actual loss rate", "CI-tree"))]
plotData[, FacetLabel:="Resolved defaults [cures/write-offs]"]

# - Plot
(gOverlay_WOFFs <- ggplot(plotData, aes(x=LossRate)) + theme_minimal() +
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
    annotate(geom="text", x=MeanLoss_exp_W*1.3, y=6, family=chosenFont,
             label = paste0("Mean: ", sprintf("%.1f", MeanLoss_exp_W*100), "%"), size=3, colour=vCol[2], angle=90) + 
    annotate("label", x=0.6, y=6, label = stats_text,
             hjust=0, vjust =1, family = chosenFont,
             size=4, fill="white", colour="black", label.size=0.5) +
    # Facets & scales
    scale_x_continuous(breaks=pretty_breaks(), labels=scales::percent) +
    scale_colour_manual(values=c(vCol[1], vCol[2])) +
    scale_fill_manual(values=c(vCol[1], vCol[2])))


# --- 3 Combine and save graphs
# - Combine graphs
ymin <- diff(ggplot_build(gOverlay)$layout$panel_params[[1]]$y.range) * 0.2
ymax <- max(ggplot_build(gOverlay)$layout$panel_params[[1]]$y.range) * 0.95
(plot.full <- gOverlay + annotation_custom(grob=ggplotGrob(gOverlay_WOFFs), xmin=0.2, xmax=1, ymin=ymin, ymax=ymax))

# - Save plot
dpi <- 240
ggsave(plot.full, file=paste0(genFigPath,"ActvsExp_twostage_ST.png"), width=1200/dpi, height=1000/dpi,dpi=dpi, bg="white")




# ======= 7. LGD-distributions - DtH-Multinomial model ======= 

# --- 1. Compare overall expected LGDs with actuals
# - Estimate statistics on distributional differences
metrics <- evalModel_twostage(data_train=datLGD, actField="LossRate_Real", estField="LossRate_est_WriteOff")

# - Create plotting data
stats_text <- paste("KL: ", sprintf("%.4f", metrics$KL), "\n",
                    "JS: ", sprintf("%.4f", metrics$JS), sep = "")


# - Mean expected loss (Actual vs expected)
(MeanLoss_exp <- mean(datLGD$LossRate_est_WriteOff, na.rm=T))
(MeanLoss_act <- mean(datLGD$LossRate_Real, na.rm=T))

# - Create plotting dataset
plotData <- melt(datLGD, measure.vars=c("LossRate_Real", "LossRate_est_WriteOff"),
                 variable.name="Type",value.name="LossRate")
plotData[, Type:=factor(Type,levels=c("LossRate_Real", "LossRate_est_WriteOff"),
                        labels=c("Empirical", "DtH-Multinomial"))]

# - Graphing parameters
chosenFont <-"Cambria"
vCol <- brewer.pal(10, "Paired")[c(8,6)]

# - Plot
(gOverlay <- ggplot(plotData, aes(x=LossRate)) + theme_minimal() +
    geom_histogram(aes(y=after_stat(density), fill=Type, colour=Type),
                   alpha=0.35,bins=50, position="identity") +
    labs(x=bquote({Loss~rate~italic(L)}), y="Density" ) +
    theme(text=element_text(family=chosenFont),legend.position="bottom",
          strip.background=element_rect(fill="snow2", colour="snow2"),
          strip.text=element_text(size=8, colour="gray50"),
          strip.placement="outside",        
          strip.text.y.right=element_text(angle = 90)) +
    # Annotations
    geom_vline(xintercept=MeanLoss_exp, linewidth=0.6, colour=vCol[2], linetype="dashed") + 
    annotate(geom="text", x=MeanLoss_exp*1.45, y=25, family=chosenFont,
             label = paste0("Mean: ", sprintf("%.1f", MeanLoss_exp*100), "%"), size=3, colour=vCol[2], angle=90) +  
    # Facets & scales
    scale_x_continuous(breaks=pretty_breaks(), labels = scales::percent) +
    scale_colour_manual(values=c(vCol[1], vCol[2])) +
    scale_fill_manual(values=c(vCol[1], vCol[2])) +
    guides(fill=guide_legend(title=NULL), colour=guide_legend(title=NULL)))


# --- 2. Compare expected write-off LGDs with actuals
# - Filter for non-sensical loss rates
datLGD_WOFFs[, LossRate_est_WriteOff:=ifelse(LossRate_est_WriteOff<=1 & LossRate_est_WriteOff>=0, LossRate_est_WriteOff, NA)]

# - Mean expected loss
(MeanLoss_act_W <- mean(datLGD_WOFFs$LossRate_Real, na.rm=T))
(MeanLoss_exp_W <- mean(datLGD_WOFFs$LossRate_est_WriteOff, na.rm=T))

# - Create plotting data
plotData <- melt(datLGD_WOFFs, measure.vars=c("LossRate_Real", "LossRate_est_WriteOff"),
                 variable.name="Type", value.name="LossRate")
plotData[, Type:=factor(Type,levels=c("LossRate_Real", "LossRate_est_WriteOff"),
                        labels=c("Actual loss rate", "DtH-Multinomial"))]
plotData[, FacetLabel:="Resolved defaults [cures/write-offs]"]

# - Plot
(gOverlay_WOFFs <- ggplot(plotData, aes(x=LossRate)) + theme_minimal() +
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
    annotate(geom="text", x=MeanLoss_exp_W*1.2, y=4, family=chosenFont,
             label = paste0("Mean: ", sprintf("%.1f", MeanLoss_exp_W*100), "%"), size=3, colour=vCol[2], angle=90) + 
    annotate("label", x=0.6, y=4.5, label = stats_text,
             hjust=0, vjust =1, family = chosenFont,
             size=4, fill="white", colour="black", label.size=0.5) +
    # Facets & scales
    scale_x_continuous(breaks=pretty_breaks(), labels=scales::percent) +
    scale_colour_manual(values=c(vCol[1], vCol[2])) +
    scale_fill_manual(values=c(vCol[1], vCol[2])))


# --- 3 Combine and save graphs
# - Combine graphs
ymin <- diff(ggplot_build(gOverlay)$layout$panel_params[[1]]$y.range) * 0.2
ymax <- max(ggplot_build(gOverlay)$layout$panel_params[[1]]$y.range) * 0.95
(plot.full <- gOverlay + annotation_custom(grob=ggplotGrob(gOverlay_WOFFs), xmin=0.2, xmax=1, ymin=ymin, ymax=ymax))

# - Save plot
dpi <- 240
ggsave(plot.full, file=paste0(genFigPath,"ActvsExp_twostage_DtH_Multi.png"), width=1200/dpi, height=1000/dpi,dpi=dpi, bg="white")




# ======= 8. LGD-distributions - Multinomial CI-tree ======= 

# --- 1. Compare overall expected LGDs with actuals
# - Estimate statistics on distributional differences
metrics <- evalModel_twostage(data_train=datLGD, actField="LossRate_Real", estField="LossRate_est_WriteOff_tree")

# - Create plotting data
stats_text <- paste("KL: ", sprintf("%.4f", metrics$KL), "\n",
                    "JS: ", sprintf("%.4f", metrics$JS), sep = "")

# - Mean expected loss (Actual vs expected)
(MeanLoss_exp <- mean(datLGD$LossRate_est_WriteOff_tree, na.rm=T))
(MeanLoss_act <- mean(datLGD$LossRate_Real, na.rm=T))

# - Create plotting dataset
plotData <- melt(datLGD, measure.vars=c("LossRate_Real", "LossRate_est_WriteOff_tree"),
                 variable.name="Type",value.name="LossRate")
plotData[, Type:=factor(Type,levels=c("LossRate_Real", "LossRate_est_WriteOff_tree"),
                        labels=c("Empirical", "Multinomial CI-tree"))]

# - Graphing parameters
chosenFont <-"Cambria"
vCol <- brewer.pal(10, "Paired")[c(8,6)]

# - Plot
(gOverlay <- ggplot(plotData, aes(x=LossRate)) + theme_minimal() +
    geom_histogram(aes(y=after_stat(density), fill=Type, colour=Type),
                   alpha=0.35,bins=50, position="identity") +
    labs(x=bquote({Loss~rate~italic(L)}), y="Density" ) +
    theme(text=element_text(family=chosenFont),legend.position="bottom",
          strip.background=element_rect(fill="snow2", colour="snow2"),
          strip.text=element_text(size=8, colour="gray50"),
          strip.placement="outside",        
          strip.text.y.right=element_text(angle = 90)) +
    # Annotations
    geom_vline(xintercept=MeanLoss_exp, linewidth=0.6, colour=vCol[2], linetype="dashed") + 
    annotate(geom="text", x=MeanLoss_exp*1.45, y=25, family=chosenFont,
             label = paste0("Mean: ", sprintf("%.1f", MeanLoss_exp*100), "%"), size=3, colour=vCol[2], angle=90) +  
    # Facets & scales
    scale_x_continuous(breaks=pretty_breaks(), labels = scales::percent) +
    scale_colour_manual(values=c(vCol[1], vCol[2])) +
    scale_fill_manual(values=c(vCol[1], vCol[2])) +
    guides(fill=guide_legend(title=NULL), colour=guide_legend(title=NULL)))


# --- 2. Compare expected write-off LGDs with actuals
# - Filter for non-sensical loss rates
datLGD_WOFFs[, LossRate_est_WriteOff_tree:=ifelse(LossRate_est_WriteOff_tree<=1 & LossRate_est_WriteOff_tree>=0, 
                                                  LossRate_est_WriteOff_tree, NA)]

# - Mean expected loss
(MeanLoss_act_W <- mean(datLGD_WOFFs$LossRate_Real, na.rm=T))
(MeanLoss_exp_W <- mean(datLGD_WOFFs$LossRate_est_WriteOff_tree, na.rm=T))

# - Create plotting data
plotData <- melt(datLGD_WOFFs, measure.vars=c("LossRate_Real", "LossRate_est_WriteOff_tree"),
                 variable.name="Type", value.name="LossRate")
plotData[, Type:=factor(Type,levels=c("LossRate_Real", "LossRate_est_WriteOff_tree"),
                        labels=c("Actual loss rate", "Multinomial CI-tree"))]
plotData[, FacetLabel:="Resolved defaults [cures/write-offs]"]

# - Plot
(gOverlay_WOFFs <- ggplot(plotData, aes(x=LossRate)) + theme_minimal() +
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
    annotate(geom="text", x=MeanLoss_exp_W*1.2, y=4, family=chosenFont,
             label = paste0("Mean: ", sprintf("%.1f", MeanLoss_exp_W*100), "%"), size=3, colour=vCol[2], angle=90) + 
    annotate("label", x=0.6, y=4.5, label = stats_text,
             hjust=0, vjust =1, family = chosenFont,
             size=4, fill="white", colour="black", label.size=0.5) +
    # Facets & scales
    scale_x_continuous(breaks=pretty_breaks(), labels=scales::percent) +
    scale_colour_manual(values=c(vCol[1], vCol[2])) +
    scale_fill_manual(values=c(vCol[1], vCol[2])))


# --- 3 Combine and save graphs
# - Combine graphs
ymin <- diff(ggplot_build(gOverlay)$layout$panel_params[[1]]$y.range) * 0.2
ymax <- max(ggplot_build(gOverlay)$layout$panel_params[[1]]$y.range) * 0.95
(plot.full <- gOverlay + annotation_custom(grob=ggplotGrob(gOverlay_WOFFs), xmin=0.2, xmax=1, ymin=ymin, ymax=ymax))

# - Save plot
dpi <- 240
ggsave(plot.full, file=paste0(genFigPath,"ActvsExp_twostage_Multi_CI_tree.png"), width=1200/dpi, height=1000/dpi,dpi=dpi, bg="white")


# ============ 6. Housekeeping ============

cleanEnviron()
