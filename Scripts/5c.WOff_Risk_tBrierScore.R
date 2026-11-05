# =========================== TIME-DEPENDENT BRIER SCORES ======================
# Calculate and compare the time-dependent Brier scores across write-off models
# ------------------------------------------------------------------------------
# PROJECT TITLE: Loss Modelling (LGD) for Residential Mortgages
# SCRIPT AUTHOR(S): Mohammed Garbu (MG), Marcel Muller (MM) Dr Arno Botha (AB)
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
#   - 4e.SurvivalTrees.R
#
# -- Inputs:
#   - datCredit_train_CDH | Prepared from script 2g
#   - datCredit_valid_CDH | Prepared from script 2g
#   - modLR_Bas | Basic discrete time model as fitted in script 4b(ii)
#   - modLR_Adv | Advanced discrete time model as fitted in script 4b(i)
#   - modLR_Classic | Classical cross-sectional (CS) logistic regression (LR) model as fitted in script 4c
#   - SurvTree_CTree | Survival tree as fitted in script 4e
#
# -- Outputs:
#   - <Analytics> | Graphs
# ------------------------------------------------------------------------------


# ============ 1. Preliminaries ============

# --- 1.1 Load and prepare data
# - Confirm prepared datasets are loaded into memory
if (!exists('datCredit_train_CDH_sub')) unpack.ffdf(paste0(genPath, "creditdata_train_CDH_sub"), tempPath); gc()

# - Ensure order of data is correct
setorder(datCredit_train_CDH_sub, LoanID, Date)

# - Filter data for default spells and date selections
datCredit_train <- datCredit_train_CDH_sub[!is.na(DefSpell_Key)]

# - Remove redundant objects
rm(datCredit_train_CDH_sub, datCredit_valid_CDH_sub); gc()

# - Create central pointer to the selected dataset (can be combined with validation set)
datCredit <- datCredit_train

# - Create a spell-level target variable for write-off instance
datCredit[, DefSpell_Event2:=DefSpell_Event]
datCredit[, DefSpell_Event:=DefSpellResol_Type_Hist=="WOFF"]

# - Transform categorical variables to factor
# Payment method
datCredit[, pmnt_method_grp_fac:=as.factor(pmnt_method_grp)]
# Previous defaults
datCredit[, PrevDefaults_fac:=as.factor(PrevDefaults)]

# - Recalculate counter variable with respect to new sampling window
datCredit[, Counter_Artificial:= 1:.N, by=.(LoanID)]

# - Handle left-truncated spells by adding a starting record 
### NOTE:  This is necessary for calculating certain survival quantities later
# Create an additional record for each default spell
datAdd <- subset(datCredit, Counter_Artificial==1 & TimeInDefSpell>1)
datAdd[, TimeInDefSpell:=TimeInDefSpell-1]
datAdd[, Time_Binned := timeBinning(TimeInDefSpell)]
datAdd[, Counter_Artificial:=0]
# Add record to main dataset
datCredit <- rbind(datCredit, datAdd); setorder(datCredit, DefSpell_Key, Date)


# --- 1.2 Load models
# - Basic discrete-time hazard model
modLR_Bas <- readRDS(paste0(genObjPath,"CoxDisc_Basic_Model.rds"))

# - Advanced discrete-time hazard model
modLR_Adv <- readRDS(paste0(genObjPath,"CoxDisc_Advanced_Model.rds"))

# - CI-tree model
SurvTree_CTree <- readRDS(paste0(genObjPath,"SurvTree_CTree.rds"))

# - Cross-sectional logistic regression model
modLR <- readRDS(paste0(genObjPath,"LR_Model.rds"))

# - Multinomial discrete-time hazard model
if (!exists('modMLR')) unpack.ffdf(paste0(genPath,"MultiDtH_WOff"), tempPath)

# - Multinomial CI-tree
if (!exists('modMultiSurvTree')) unpack.ffdf(paste0(genPath,"MultiCI_Tree"), tempPath)


# --- 1.3 Estimate event rates
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
datCredit[, Survival_adv:=cumprod(1-Hazard_adv), by=list(DefSpell_Key)]
datCredit[, Survival_bas:=cumprod(1-Hazard_bas), by=list(DefSpell_Key)]

# - Derive (event-free) survival probability S(t) = prod(1 - h_1(t) - h_2(t))
datCredit[, Survival_overall:=cumprod(1-Hazard_WriteOff-Hazard_Cure), by=.(DefSpell_Key)]
datCredit[, Survival_overall_tree:=cumprod(1-Hazard_WriteOff_tree-Hazard_Cured_tree), by=.(DefSpell_Key)]

# - Derive discrete density, or event probability f(t) = S(t-1) - S(t)
datCredit[, EventRate_adv:=shift(Survival_adv, type="lag", n=1, fill=1) - Survival_adv, by=list(DefSpell_Key)]
datCredit[, EventRate_bas:=shift(Survival_bas, type="lag", n=1, fill=1) - Survival_bas, by=list(DefSpell_Key)]

# - Derive event rates as f_1(t) = S(t-1) . h_1(t)
datCredit[, EventRate_WriteOff := shift(Survival_overall, n=1,fill=1) * Hazard_WriteOff, by=.(DefSpell_Key)]
datCredit[, EventRate_WriteOff_tree := shift(Survival_overall_tree, n=1,fill=1) * Hazard_WriteOff_tree, by=.(DefSpell_Key)]

# - Derive overall event probabilities from CS-LR model
datCredit[, EventRate_WriteOff_CSLR := predict(modLR, newdata=.SD[], type="response")]

# - Remove added rows
datCredit <- subset(datCredit, Counter_Artificial>0) %>% setorder(LoanID, Date)


# --- 1.4 Predict survival quantities using CI-tree
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





# ============ 2. Graph tBS-values across models ============

# --- 2.1 Calculate time-dependent Brier scores across write-off models
# - Basic discrete-time hazard model
(objCoxDisc_bas <- tBrierScore(datCredit, modGiven=modLR_Bas, spellPeriodMax=84, fldKey="DefSpell_Key", 
                               fldStart="Start", fldStop="TimeInDefSpell",fldCensored="DefSpell_Censored", 
                               fldSpellAge="DefSpell_Age", fldSpellOutcome="DefSpellResol_Type_Hist", 
                               fldEvent="DefSpell_Event", brierType="Survival", empiricalType="KM"))
### RESULTS: Integrated Brier Score = 2.119179

# - Advanced discrete-time hazard model
(objCoxDisc_adv <- tBrierScore(datCredit, modGiven=modLR_Adv, spellPeriodMax=84, fldKey="DefSpell_Key", 
                               fldStart="Start", fldStop="TimeInDefSpell",fldCensored="DefSpell_Censored", 
                               fldSpellAge="DefSpell_Age", fldSpellOutcome="DefSpellResol_Type_Hist", 
                               fldEvent="DefSpell_Event",  brierType="Survival", empiricalType="KM"))
### RESULTS: Integrated Brier Score = 0.6952368

# - CI-tree
(objCoxDisc_survtree <- tBrierScore(datCredit, fldKey="DefSpell_Key", spellPeriodMax=84,
                                    fldStart="Start", fldStop="TimeInDefSpell",fldCensored="DefSpell_Censored", 
                                    fldSpellAge="DefSpell_Age", fldSpellOutcome="DefSpellResol_Type_Hist", 
                                    fldEvent="DefSpell_Event", brierType="Given-Survival", empiricalType="KM",
                                    fldSurvival="Survival_survtree", fldHazard="Hazard_survtree"))
### RESULTS: Integrated Brier Score = 1.456852

# - Multinomial discrete-time hazard model
(objCoxDisc_Multi_DtH <- tBrierScore(datCredit, fldKey="DefSpell_Key", spellPeriodMax=84, 
                               fldStart="Start", fldStop="TimeInDefSpell",fldCensored="DefSpell_Censored", 
                               fldSpellAge="DefSpell_Age", fldSpellOutcome="DefSpellResol_Type_Hist", 
                               fldEvent="DefSpell_EventType", brierType="Given-Survival", empiricalType="AJ",
                               fldSurvival="Survival_overall", fldHazard="Hazard_WriteOff"))
### RESULTS: Integrated Brier Score = 3.340883

# - Multinomial CI-tree
(objCoxDisc_Multi_CI_Tree <- tBrierScore(datCredit, fldKey="DefSpell_Key", spellPeriodMax=84,
                                     fldStart="Start", fldStop="TimeInDefSpell",fldCensored="DefSpell_Censored", 
                                     fldSpellAge="DefSpell_Age", fldSpellOutcome="DefSpellResol_Type_Hist", 
                                     fldEvent="DefSpell_EventType", brierType="Given-Survival", empiricalType="AJ",
                                     fldSurvival="Survival_overall_tree", fldHazard="Hazard_WriteOff_tree"))
### RESULTS: Integrated Brier Score = 3.366442




# --- 2.2 Graph tBS-values 
# - Data fusion across models
datGraph <- rbind(data.table(objCoxDisc_bas$tBS, Type="a_Basic"),
                  data.table(objCoxDisc_adv$tBS, Type="b_Advanced"),
                  data.table(objCoxDisc_survtree$tBS, Type="c_CI_Tree"),
                  data.table(objCoxDisc_Multi_DtH$tBS, Type="d_Multinomial_DtH"),
                  data.table(objCoxDisc_Multi_CI_Tree$tBS, Type="e_Multinomial_CI_Tree"))

# - Aesthetic engineering
datGraph[, FacetLabel:="Survival models"]
specified_SpellAge <- 48

# - Recalculate Integrated tBS over zoomed spell age
(ibs_bas <- mean(objCoxDisc_bas$tBS[TimeInDefSpell<=specified_SpellAge, Brier]))
(ibs_adv <- mean(objCoxDisc_adv$tBS[TimeInDefSpell<=specified_SpellAge, Brier]))
(ibs_survtree <- mean(objCoxDisc_survtree$tBS[TimeInDefSpell<=specified_SpellAge, Brier]))
(ibs_multi_DtH <- mean(objCoxDisc_Multi_DtH$tBS[TimeInDefSpell<=specified_SpellAge, Brier]))
(ibs_multi_CI_Tree <- mean(objCoxDisc_Multi_CI_Tree$tBS[TimeInDefSpell<=specified_SpellAge, Brier]))

# - Graphing Parameters
chosenFont <- "Cambria"
vCol <- brewer.pal(8, "Dark2")[c(2,1,3,4,8)]
vLabel <- c("a_Basic"="DtH-Basic", "b_Advanced"="DtH-Advanced",
            "c_CI_Tree"="CI-tree", "d_Multinomial_DtH"="DtH-Multinomial",
            "e_Multinomial_CI_Tree"="Multinomial CI-tree")

# - Main graph of tBS
(gOuter <- ggplot(datGraph, aes(x=TimeInDefSpell, y=Brier, group=Type)) + theme_minimal() + 
    labs(y=bquote("Time-dependent Brier Score "*italic(B)[s](italic(t))), x=bquote("Spell time (months)"*~italic(t))) + 
    theme(text=element_text(family=chosenFont),legend.position="bottom", 
          strip.background=element_rect(fill="snow2", colour="snow2"),
          strip.text = element_text(size=8, colour="gray50"), strip.text.y.right = element_text(angle=90)) + 
    # Main graph
    geom_line(aes(colour=Type, linetype=Type), linewidth=0.5) + 
    # Annotations
    annotate(geom="text", x=18, y=4, label=paste0("IBS (DtH-Basic): ", round(objCoxDisc_bas$IBS,3)), 
             family=chosenFont, size=3.5, colour=vCol[1]) + 
    annotate(geom="text", x=18, y=3.25, label=paste0("IBS (DtH-Advanced): ", round(objCoxDisc_adv$IBS,3)), 
             family=chosenFont, size=3.5, colour=vCol[2]) + 
    annotate(geom="text", x=18, y=2.5, label=paste0("IBS (ST): ", round(objCoxDisc_survtree$IBS,3)), 
             family=chosenFont, size=3.5, colour=vCol[3]) + 
    annotate(geom="text", x=18, y=1.75, label=paste0("IBS (DtH-Multinomial): ", round(objCoxDisc_Multi_DtH$IBS,3)), 
             family=chosenFont, size=3.5, colour=vCol[4]) + 
    annotate(geom="text", x=18, y=1, label=paste0("IBS (Multinomial CI-Tree): ", round(objCoxDisc_Multi_CI_Tree$IBS,3)), 
             family=chosenFont, size=3.5, colour=vCol[5]) +     
    # Facets & scales
    facet_grid(FacetLabel ~ .) +  
    scale_colour_manual(name="Model", values=vCol, labels=vLabel) + 
    scale_linetype_discrete(name="Model", labels=vLabel) + 
    scale_y_continuous(breaks=breaks_pretty(), label=comma) + 
    scale_x_continuous(breaks=breaks_pretty(), label=comma) + 
    guides(colour=guide_legend(nrow=2))
)

# - Zoomed inset graph of tBS on a smaller time scale
(gInner <- ggplot(datGraph[TimeInDefSpell<=specified_SpellAge,], aes(x=TimeInDefSpell, y=Brier, group=Type)) + 
    theme_bw() + labs(y="", x="") + 
    theme(legend.position.inside=c(0.75,0.40), text=element_text(size=12, family="Cambria"),
          #specific for plot-in-plot
          axis.text.y=element_text(margin=unit(c(0,0,0,0), "mm"), size=9),
          axis.text.x=element_text(margin=unit(c(0,0,0,0), "mm"), size=9),
          axis.ticks=element_blank(), axis.title.x=element_blank(), #axis.title.y=element_blank(),
          panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
          panel.background = element_rect(color="black", fill="white"),
          plot.background = element_rect(color="white"), plot.margin = unit(c(0,0,0,0),"mm"),
          plot.title = element_text(hjust=0.55, vjust=-10)) + 
    # Main graph
    geom_line(aes(colour=Type, linetype=Type), linewidth=0.5, show.legend = F) + 
    # Annotations
    annotate(geom="text", x=17.5, y=2.8, label=paste0("IBS (DtH-Basic): ", round(ibs_bas,3)), 
             family=chosenFont, size=3.5, colour=vCol[1]) + 
    annotate(geom="text", x=17.5, y=2.45, label=paste0("IBS (DtH-Advanced): ", round(ibs_adv,3)), 
             family=chosenFont, size=3.5, colour=vCol[2]) +   
    annotate(geom="text", x=17.5, y=2.1, label=paste0("IBS (CI-tree): ", round(ibs_survtree,3)), 
             family=chosenFont, size=3.5, colour=vCol[3]) + 
    annotate(geom="text", x=17.5, y=1.75, label=paste0("IBS (DtH-Multinomial): ", round(ibs_multi_DtH,3)), 
             family=chosenFont, size=3.5, colour=vCol[4]) + 
    annotate(geom="text", x=17.5, y=1.4, label=paste0("IBS (Multinomial CI-tree): ", round(ibs_multi_CI_Tree,3)), 
             family=chosenFont, size=3.5, colour=vCol[5]) + 
    # Facets & scales
    scale_colour_manual(name="", values=vCol, labels=vLabel) + 
    scale_linetype_discrete(name="", labels=vLabel) +
    scale_y_continuous(breaks=breaks_pretty(), label=comma) + 
    scale_x_continuous(breaks=breaks_pretty(), label=comma)
)

# - Combining the two above plots onto a single graph
(plot.full <- gOuter + annotation_custom(grob = ggplotGrob(gInner), xmin=2, xmax=60, ymin=4.5, ymax=10.5))

# - Save plot
dpi <- 280
ggsave(plot.full, file=paste0(genFigPath,"tBrierScores_SurvModels.png"), width=1600/dpi, height=1200/dpi,dpi=dpi, bg="white")




# ============ 3. Compare Brier scores at a specific time period across models ============

# --- 3.1 Create a object for comparing Brier scores
# - Extract time-dependent Brier scores for each model
tBS_Bas <- objCoxDisc_bas$tBS
tBS_Adv <- objCoxDisc_adv$tBS
tBS_CI_Tree <- objCoxDisc_survtree$tBS
tBS_Multi_DtH <- objCoxDisc_Multi_DtH$tBS
tBS_Multi_CI_Tree <- objCoxDisc_Multi_CI_Tree$tBS

# - Extract time-dependent Brier scores at time 44 for each model
(tBrier_Bas <- as.numeric(tBS_Bas[TimeInDefSpell==41,Brier]))
(tBrier_Adv <- as.numeric(tBS_Adv[TimeInDefSpell==41,Brier]))
(Brier_CSLR <- mean((datCredit[TimeInDefSpell==41, as.integer(DefSpellResol_Type_Hist=="WOFF")] - 
    datCredit[TimeInDefSpell==41, EventRate_WriteOff_CSLR])^2, na.rm=T))
(tBrier_CI_Tree <- as.numeric(tBS_CI_Tree[TimeInDefSpell==41,Brier]))
(tBrier_Multi_DtH <- as.numeric(tBS_Multi_DtH[TimeInDefSpell==41,Brier]))
(tBrier_Multi_CI_Tree <- as.numeric(tBS_Multi_CI_Tree[TimeInDefSpell==41,Brier]))

# - Create a single table containing the three R^2 measures for each of the models
(datBrier <- data.table(Model=c("DtH-Basic","DtH-Advanced", "CS-LR", "CI-tree",
                                "DtH-Multinomial", "Multinomial CI-tree"),
                        Brier=c(tBrier_Bas, tBrier_Adv, Brier_CSLR, tBrier_CI_Tree,
                                tBrier_Multi_DtH, tBrier_Multi_CI_Tree)))

# - Save table to specified path
saveRDS(datBrier, paste0(genObjPath,"Brier_Table.rds"))


# --- 3.2 Visualise the time-dependent Brier scores at the chosen time point (44)
# - Create plotting dataset
datPlot <- data.table(Statistic="Models ",
                      Model=c("a_Basic","b_Advanced", "c_CSLR", "d_CI_Tree",
                              "e_Multi_DtH", "f_Multi_CI_Tree"),
                      Value=as.numeric(sub("%","",c(datBrier$Brier)))/100)

# - Aesthetic engineering
datPlot[, Label:=paste0(sprintf("%.2f", Value*100))]

# - Graphing parameters
chosenFont <- "Cambria"
vCol1 <- brewer.pal(8, "Dark2")[1:6]
vCol2 <- vCol1 
vCol3 <- rep("white", 6)  
vLabel <- list("a_Basic"="DtH-Basic", "b_Advanced"="DtH-Advanced",
               "c_CSLR"="CS-LR", "d_CI_Tree"="CI-tree",
               "e_Multi_DtH"="DtH-Multinomial", "f_Multi_CI_Tree"="Multinomial CI-tree")

# - Plot
(gPlot <- ggplot(datPlot, aes(x=Statistic, y=Value, group=Model)) + 
    theme_minimal() +
    theme(legend.position="bottom",
          text=element_text(family=chosenFont)) +
    labs(y=bquote("Time-dependent Brier Score "*italic(B)[s](italic(t))), x="") +
    geom_col(aes(colour=Model, fill=Model), position=position_dodge(width=0.9)) +
    geom_label(aes(label=Label), fill=vCol2, colour=vCol3, position=position_dodge(width=0.9),
               size=3, label.padding=unit(0.15, "lines")) +
  scale_colour_manual(name="Model", values=vCol1, labels=vLabel) +
  scale_fill_manual(name="Model", values=vCol1, labels=vLabel) +
  scale_y_continuous(labels=scales::percent_format(accuracy=1)) + 
  guides(col=guide_legend(nrows=2))
)

# - Save graph
dpi <- 180
ggsave(gPlot, file=paste0(genFigPath, "BrierPlot.png"), width=1200/dpi, height=1000/dpi, dpi=400, bg="white")




# ============ 4. Housekeeping ============

cleanEnviron()
