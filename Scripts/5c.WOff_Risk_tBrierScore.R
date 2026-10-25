# =========================== TIME-DEPENDENT BRIER SCORES ======================
# Calculate and compare the time-dependent Brier scores across write-off models
# ------------------------------------------------------------------------------
# PROJECT TITLE: Loss Modelling (LGD) for Residential Mortgages
# SCRIPT AUTHOR(S): Mohammed Garbu (MG), Marcel Muller (MM) Dr Arno Botha (AB)
# ------------------------------------------------------------------------------
# -- Script dependencies:
#   - 0.Setup.R
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
#   - modLR_Classic | Classical logistic regression model as fitted in script 4c
#   - SurvTree_CTree | Survival tree as fitted in script 4e
#
# -- Outputs:
#   - <Analytics> | Graphs
# ------------------------------------------------------------------------------


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

# - Classic logit model
modLR_Classic <- readRDS(paste0(genObjPath,"LR_Model.rds"))

# - Tree model
SurvTree_CTree <- readRDS(paste0(genObjPath,"SurvTree_CTree.rds"))


# --- 1.3 Estimate event rates
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
(objCoxDisc_bas <- tBrierScore(datCredit, modGiven=modLR_Bas, predType="response", spellPeriodMax=120, fldKey="DefSpell_Key", 
                               fldStart="Start", fldStop="TimeInDefSpell",fldCensored="DefSpell_Censored", 
                               fldSpellAge="DefSpell_Age2", fldSpellOutcome="DefSpellResol_Type_Hist", brierType="Survival"))
### RESULTS: Integrated Brier Score=7.722137

# - Advanced discrete-time hazard model
(objCoxDisc_adv <- tBrierScore(datCredit, modGiven=modLR_Adv, predType="response", spellPeriodMax=120, fldKey="DefSpell_Key", 
                               fldStart="Start", fldStop="TimeInDefSpell",fldCensored="DefSpell_Censored", 
                               fldSpellAge="DefSpell_Age2", fldSpellOutcome="DefSpellResol_Type_Hist", brierType="Survival"))
### RESULTS: Integrated Brier Score=2.012001

# - Classical logistic regression model
### MM: As with my commentary on the LGD-distributions, we are using the full write-off probability here to derive survival quantities,
###     which is not feasible
(objCoxDisc_classic <- tBrierScore(datCredit, modGiven=modLR_Classic, predType="response", spellPeriodMax=120, fldKey="DefSpell_Key", 
                                   fldStart="Start", fldStop="TimeInDefSpell",fldCensored="DefSpell_Censored", 
                                   fldSpellAge="DefSpell_Age2", fldSpellOutcome="DefSpellResol_Type_Hist", brierType="Survival"))
### RESULTS: Integrated Brier Score=14.25004

# - Survival tree
(objCoxDisc_survtree <- tBrierScore(datCredit, modGiven=NA, spellPeriodMax=120, fldKey="DefSpell_Key", 
                                    fldStart="Start", fldStop="TimeInDefSpell",fldCensored="DefSpell_Censored", 
                                    fldSpellAge="DefSpell_Age2", fldSpellOutcome="DefSpellResol_Type_Hist", brierType="Given-Survival",
                                    fldSurvival="Survival_survtree", fldHazard="Hazard_survtree"))
### RESULTS: Integrated Brier Score=7.973127


# --- 2.2 Graph tBS-values 
# - Data fusion across models
datGraph <- rbind(data.table(objCoxDisc_bas$tBS, Type="a_Basic"),
                  data.table(objCoxDisc_adv$tBS, Type="b_Advanced"),
                  data.table(objCoxDisc_survtree$tBS, Type="c_SurvTree"))

# - Aesthetic engineering
datGraph[, FacetLabel:="Survival models"]
specified_SpellAge <- 48

# - Recalculate Integrated tBS over zoomed spell age
(ibs_bas <- mean(objCoxDisc_bas$tBS[TimeInDefSpell<=specified_SpellAge, Brier]))
(ibs_adv <- mean(objCoxDisc_adv$tBS[TimeInDefSpell<=specified_SpellAge, Brier]))
(ibs_survtree <- mean(objCoxDisc_survtree$tBS[TimeInDefSpell<=specified_SpellAge, Brier]))

# - Graphing Parameters
chosenFont <- "Cambria"
vCol <- brewer.pal(8, "Set2")[c(2,1,3)]
vLabel <- c("a_Basic"="DtH-Basic", "b_Advanced"="DtH-Advanced",
            "c_SurvTree"="ST")

# - Main graph of tBS
(gOuter <- ggplot(datGraph, aes(x=TimeInDefSpell, y=Brier, group=Type)) + theme_minimal() + 
    labs(y=bquote("Time-dependent Brier Score "*italic(B)[s](italic(t))), x=bquote("Spell time (months)"*~italic(t))) + 
    theme(text=element_text(family=chosenFont),legend.position="bottom", 
          strip.background=element_rect(fill="snow2", colour="snow2"),
          strip.text = element_text(size=8, colour="gray50"), strip.text.y.right = element_text(angle=90)) + 
    # Main graph
    geom_line(aes(colour=Type, linetype=Type), linewidth=0.5) + 
    # Annotations
    annotate(geom="text", x=24, y=12, label=paste0("IBS (Basic A): ", round(objCoxDisc_bas$IBS,3)), 
             family=chosenFont, size=3.5, colour=vCol[1]) + 
    annotate(geom="text", x=22, y=10, label=paste0("IBS (Advanced A): ", round(objCoxDisc_adv$IBS,3)), 
             family=chosenFont, size=3.5, colour=vCol[2]) + 
    annotate(geom="text", x=22, y=8, label=paste0("IBS (ST): ", round(objCoxDisc_survtree$IBS,3)), 
             family=chosenFont, size=3.5, colour=vCol[3]) + 
    # Facets & scales
    facet_grid(FacetLabel ~ .) +  
    scale_colour_manual(name="Model", values=vCol, labels=vLabel) + 
    scale_linetype_discrete(name="Model", labels=vLabel) + 
    scale_y_continuous(breaks=breaks_pretty(), label=comma) + 
    scale_x_continuous(breaks=breaks_pretty(), label=comma)
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
    annotate(geom="text", x=20, y=1.75, label=paste0("IBS (Basic A): ", round(ibs_bas,3)), 
             family=chosenFont, size=3.5, colour=vCol[1]) + 
    annotate(geom="text", x=20, y=1.4, label=paste0("IBS (Advanced A): ", round(ibs_adv,3)), 
             family=chosenFont, size=3.5, colour=vCol[2]) +   
    annotate(geom="text", x=20, y=1.05, label=paste0("IBS (ST): ", round(ibs_survtree,3)), 
             family=chosenFont, size=3.5, colour=vCol[3]) + 
    # Facets & scales
    scale_colour_manual(name="", values=vCol, labels=vLabel) + 
    scale_linetype_discrete(name="", labels=vLabel) +
    scale_y_continuous(breaks=breaks_pretty(), label=comma) + 
    scale_x_continuous(breaks=breaks_pretty(), label=comma)
)

# - Combining the two above plots onto a single graph
(plot.full <- gOuter + annotation_custom(grob = ggplotGrob(gInner), xmin=2, xmax=85, ymin=15, ymax=30))

# - Save plot
dpi <- 280
ggsave(plot.full, file=paste0(genFigPath,"tBrierScores_CoxDisc.png"), width=1600/dpi, height=1200/dpi,dpi=dpi, bg="white")




# ============ 3. Compare Brier scores at a specific time period across models ============

# --- 3.1 Create a object for comparing Brier scores
# - Extract time-dependent Brier scores for each model
tBS_Bas <- objCoxDisc_bas$tBS; tBS_Adv <- objCoxDisc_adv$tBS
tBS_Classic <- objCoxDisc_classic$tBS; tBS_ST <- objCoxDisc_survtree$tBS

# - Extract time-dependent Brier scores at time 44 for each model
(tBrier_Bas <- as.numeric(tBS_Bas[TimeInDefSpell==44,Brier]))
(tBrier_Adv <- as.numeric(tBS_Adv[TimeInDefSpell==44,Brier]))
### MM: Again, I do not think this is valid as this value is calculated using full write-off probabilities from the classical model
(tBrier_Classic <- as.numeric(tBS_Classic[TimeInDefSpell==44,Brier]))
(tBrier_ST <- as.numeric(tBS_ST[TimeInDefSpell==44,Brier]))

# - Create a single table containing the three R^2 measures for each of the models
(datBrier <- data.table(Model=c("DtH-Basic A","DtH-Advanced", "Logistic Regression", "ST"),
                        Brier=c(tBrier_Bas, tBrier_Adv, tBrier_Classic, tBrier_ST)))

# - Save table to specified path
saveRDS(datBrier, paste0(genObjPath,"Brier_Table.rds"))


# --- 3.2 Visualise the time-dependent Brier scores at the chosen time point (44)
# - Create plotting dataset
datPlot <- data.table(Statistic=rep(c("Models "),each=4),
                      Model=rep(c("a_Basic","b_Advanced", "c_Logistic_Regression", "d_ST"), times=1),
                      Value=as.numeric(sub("%","",c(datBrier$Brier)))/100)

# - Aesthetic engineering
datPlot[, Label:=paste0(sprintf("%.2f", Value*100),"%")]

# - Graphing parameters
chosenFont <- "Cambria"
vCol1 <- c("a_Basic"=brewer.pal(9, "BuGn")[5],
           "b_Advanced"=brewer.pal(9, "BuGn")[7],
           "c_Logistic_Regression"=brewer.pal(9, "BuGn")[9],
           "d_ST"=brewer.pal(9, "BuGn")[4])
vCol2 <- vCol1 
vCol3 <- rep("white", 4)  
vLabel <- list("a_Basic"="DtH-Basic",
               "b_Advanced"="DtH-Advanced",
               "c_Logistic_Regression"="Logistic Regression",
               "d_ST"="ST")

# - Plot
(gPlot <- ggplot(datPlot, aes(x=Statistic, y=Value, group=Model)) + 
    theme_minimal() +
    theme(legend.position="bottom",
          text=element_text(family=chosenFont)) +
    labs(y=bquote("Time-dependent Brier Score "*italic(B)[s](italic(t))), x="") +
    geom_col(aes(colour=Model, fill=Model), position=position_dodge(width=0.9)) +
    geom_label(aes(label=Label), fill=vCol2, colour=vCol3, position=position_dodge(width=0.9),
               size=3, label.padding=unit(0.15, "lines")) +
  scale_colour_manual(name="Model:", values=vCol1, labels=vLabel) +
  scale_fill_manual(name="Model:", values=vCol1, labels=vLabel) +
  scale_y_continuous(labels=scales::percent_format(accuracy=1)))

# - Save graph
dpi <- 180
ggsave(gPlot, file=paste0(genFigPath, "BrierPlot.png"), width=1200/dpi, height=1000/dpi, dpi=400, bg="white")




# ============ 4. Housekeeping ============

cleanEnviron()



