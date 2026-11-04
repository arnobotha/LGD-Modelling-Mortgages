# ============================== CONSTRUCTING TERM-STUCTURES ===================
# Construct and compare empirical and expected term-structures of default risk
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
#   - 4e.SurvivalTrees.R
#
# -- Inputs:
#   - datCredit_train_CDH | Prepared from script 2g
#   - datCredit_valid_CDH | Prepared from script 2g
#   - modLR_Bas | Basic discrete time model as fitted in script 4b(ii)
#   - modLR_Adv | Advanced discrete time model as fitted in script 4b(i)
#   - SurvTree_CTree | Survival tree as fitted in script 4e
#
# -- Outputs:
#   - <Analytics> | Graphs
# ------------------------------------------------------------------------------


# ============ 1. Preliminaries ============

# --- 1.1 Load and prepare data
# - Confirm prepared datasets are loaded into memory
if (!exists('datCredit_train_CDH_sub')) unpack.ffdf(paste0(genPath,"creditdata_train_CDH_sub"), tempPath); gc()
if (!exists('datCredit_valid_CDH_sub')) unpack.ffdf(paste0(genPath,"creditdata_valid_CDH_sub"), tempPath); gc()

# - Filter data for default spells and date selections
datCredit_train <- subset(datCredit_train_CDH_sub, !is.na(DefSpell_Key))
datCredit_valid <- subset(datCredit_valid_CDH_sub, !is.na(DefSpell_Key))


# --- 1.2 Data preparation for prediction purposes
# - Combine training and validation datasets to facilitate "better" (smooth) graphs
datCredit <- rbind(datCredit_train, datCredit_valid)
setorder(datCredit, LoanID, Date)

# - Create a spell-level target variable for write-off instance
datCredit[, DefSpell_Event2:=DefSpell_Event]
datCredit[, DefSpell_Event:=DefSpellResol_Type_Hist=="WOFF"]

# - Transform categorical variables to factor
# Payment method
datCredit[,pmnt_method_grp_fac:=as.factor(pmnt_method_grp)]
# Previous defaults
datCredit[,PrevDefaults_fac:=as.factor(PrevDefaults)]

# - Recalculate counter variable with respect to new sampling window
datCredit[, Counter_Artificial := 1:.N, by=.(LoanID)]

# - Handle left-truncated spells by adding a starting record 
### NOTE:  This is necessary for calculating certain survival quantities later
# Create an additional record for each default spell
datAdd <- subset(datCredit, Counter_Artificial==1 & TimeInDefSpell>1)
datAdd[, TimeInDefSpell:=TimeInDefSpell-1]
datAdd[, Time_Binned := timeBinning(TimeInDefSpell)]
datAdd[, Counter_Artificial:=0]
# Add record to main dataset
datCredit <- rbind(datCredit, datAdd); setorder(datCredit, DefSpell_Key, Date)

# - Remove objects
rm(datCredit_train_CDH_sub, datCredit_valid_CDH_sub); gc()


# --- 1.3 Load models
# - Basic discrete-time hazard model
modLR_Bas <- readRDS(paste0(genObjPath,"CoxDisc_Basic_Model.rds"))

# - Advanced discrete-time hazard model
modLR_Adv <- readRDS(paste0(genObjPath,"CoxDisc_Advanced_Model.rds"))

# - Tree model
SurvTree_CTree <- readRDS(paste0(genObjPath,"SurvTree_CTree.rds"))

# Km-estimator of survival probability and related quantities
if (!exists('datSurv')) unpack.ffdf(paste0(genPath,"datSurv_KM_MultiSpell"), tempPath);gc()


# --- 1.4 Estimate event rates for DtH models
# - Predict hazard h(t) = P(T=t | T>= t) in discrete-time
datCredit[, Hazard_adv:=predict(modLR_Adv, newdata=datCredit, type = "response")]
datCredit[, Hazard_bas:=predict(modLR_Bas, newdata=datCredit, type = "response")]

# - Derive survival probability S(t) = prod(1 - hazard)
datCredit[, Survival_adv:=cumprod(1-Hazard_adv), by=list(DefSpell_Key)]
datCredit[, Survival_bas:=cumprod(1-Hazard_bas), by=list(DefSpell_Key)]

# - Derive discrete density, or event probability f(t) = S(t-1) - S(t)
datCredit[, EventRate_adv:=shift(Survival_adv, type="lag", n=1, fill=1) - Survival_adv, by=list(DefSpell_Key)]
datCredit[, EventRate_bas:=shift(Survival_bas, type="lag", n=1, fill=1) - Survival_bas, by=list(DefSpell_Key)]

# - Derive survival probability S(t) = prod(1 - hazard)
datCredit[, Survival_adv:=cumprod(1-Hazard_adv), by=.(DefSpell_Key)]
datCredit[, Survival_bas:=cumprod(1-Hazard_bas), by=.(DefSpell_Key)]

# - Derive discrete density, or event probability f(t) = S(t-1) - S(t)
datCredit[, EventRate_adv:=shift(Survival_adv, type="lag", n=1, fill=1) - Survival_adv, by=.(DefSpell_Key)]
datCredit[, EventRate_bas:=shift(Survival_bas, type="lag", n=1, fill=1) - Survival_bas, by=.(DefSpell_Key)]

# - Remove added rows
datCredit <- subset(datCredit, Counter_Artificial>0)

# [SANITY CHECK]
lookup <- subset(datCredit, LoanID == unique(datCredit[DefSpell_Num==1 & DefSpell_Age >= 12, LoanID])[1],
                 select=c("DefSpell_Num", "TimeInDefSpell", "Hazard_adv", "EventRate_adv", "Survival_adv"))


# --- 1.5 Get event rates from the survival tree
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




# ============ 2. Constructing the term-structures of write-off ============

# --- 2.1 Aggregate event rates to period-level
# - Aggregate data
datSurv_exp <- datCredit[,.(
  # Basic model
  EventRate_bas=mean(EventRate_bas, na.rm=T),
  # Advanced model
  EventRate_adv=mean(EventRate_adv, na.rm=T),
  # Survival tree
  EventRate_survtree=mean(EventRate_survtree, na.rm=T),
  # Empirical estimates
  Hazard_Emp=sum(DefSpell_Event2)/.N),
  by=list(TimeInDefSpell)]

# - Ensure order of fields is correct
setorder(datSurv_exp, TimeInDefSpell)


# --- 2.2 [SANITY CHECKS] Graphing survival quantities
# - Comparing the event rate and hazard rate of the actuals
plot(datSurv_exp[TimeInDefSpell<=120, Hazard_Emp], type="b", col="green") ### MM: Event rate as calculated by d(t)/n(t) -> Hazard_Emp=sum(DefSpell_Event2)/.N)
lines(datSurv[Time<=120, EventRate], type="b", col="red") ### MM: Event rate as calculated by KM-estimator (S(t-1)*h(t))
lines(datSurv[Time<=120, Hazard_Actual], type="b", col="blue") ### MM: Hazard rate as calculated by KM-estimator
all(datSurv_exp[TimeInDefSpell<=120, Hazard_Emp]==datSurv[Time<=120, Hazard_Actual]) # should be TRUE
### RESULTS: The hazard rate calculated by d(t)/n(t) is exactly the same as the hazard rate we get from the KM-estimator

# - Plotting event rates for actuals and all expected values
plot(datSurv[Time<=120, EventRate], type="b")
lines(datSurv_exp[TimeInDefSpell<=120, EventRate_adv], type="b", col="red")
lines(datSurv_exp[TimeInDefSpell<=120, EventRate_bas], type="b", col="green")
lines(datSurv_exp[TimeInDefSpell<=120, EventRate_survtree], type="b", col="blue")
### CONCLUSION: Note that the output from the CS-LR




# ============ 3. Graphing the event density / probability mass function f(t) | No splines ============ 

# --- 3.1 Preliminaries
sMaxSpellAge_graph <- 84 # Max for [DefSpell_Age] for graphing purposes


# --- 3.2 Create graphing data
# - Initialise graphing data
datGraph2 <- rbind(datSurv[,list(Time=Time, EventRate=EventRate, Type="a_Actual")],
                  datSurv_exp[,list(Time=TimeInDefSpell, EventRate=EventRate_bas, Type="b_Expected_bas")],
                  datSurv_exp[,list(Time=TimeInDefSpell, EventRate=EventRate_adv, Type="c_Expected_adv")],
                  datSurv_exp[,list(Time=TimeInDefSpell, EventRate=EventRate_survtree, Type="d_Expected_SurvTree")]
)

# - Set facet label
datGraph2[, FacetLabel:="Classical term-structure of write-off risk"]

# - Calculate MAE between event rates
datMAE <- datGraph2 %>%
  subset(Time<=sMaxSpellAge_graph, select=c("Time", "Type", "EventRate")) %>%
  pivot_wider(names_from=Type, values_from=EventRate) %>%
  as.data.table()
# Basic model
(MAE_eventProb_bas <- mean(abs(datMAE$a_Actual-datMAE$b_Expected_bas), na.rm=T))
# Advanced model
(MAE_eventProb_adv <- mean(abs(datMAE$a_Actual-datMAE$c_Expected_adv), na.rm=T))
# Survival tree
(MAE_eventProb_SurvTree <- mean(abs(datMAE$a_Actual-datMAE$d_Expected_SurvTree), na.rm=T))

# - Graphing parameters
vCol <- brewer.pal(8, "Dark2")
vLabel2 <- c("a_Actual"="Empirical (Kaplan-Meier)", "b_Expected_bas"="DtH-Basic", "c_Expected_adv"="DtH-Advanced",
             "d_Expected_SurvTree"="CI-tree")
chosenFont <- "Cambria"


# --- 3.3 Create & save graph
# - Create graph
(gsurv_ft2 <- ggplot(datGraph2[Time <= sMaxSpellAge_graph,], aes(x=Time, y=EventRate, group=Type)) + theme_minimal() +
    labs(y=bquote(plain("Write-off probability "*italic(w(t)))), 
         x=bquote("Spell time (months)"*~italic(t))) + 
    theme(text=element_text(family=chosenFont),legend.position = "bottom",
          strip.background=element_rect(fill="snow2", colour="snow2"),
          strip.text=element_text(size=8, colour="gray50"), strip.text.y.right=element_text(angle=90)) + 
    #coord_cartesian(ylim=c(0,0.018)) +
    # Main graph
    geom_point(aes(y=EventRate, colour=Type, shape=Type), size=0.6) + 
    geom_line(aes(y=EventRate, colour=Type, linetype=Type)) + 
    # Annotations
    annotate("text", y=0.017,x=75, label=paste0("MAE (DtH-Basic): ", percent(MAE_eventProb_bas, accuracy=0.0001)), family=chosenFont, size=3) + 
    annotate("text", y=0.0155,x=75, label=paste0("MAE (DtH-Advanced): ", percent(MAE_eventProb_adv, accuracy=0.0001)), family=chosenFont, size=3) + 
    annotate("text", y=0.0140,x=75, label=paste0("MAE (CI-tree): ", percent(MAE_eventProb_SurvTree, accuracy=0.0001)), family=chosenFont, size=3) +    
    # Scales and options
    facet_grid(FacetLabel~.) + 
    scale_colour_manual(name="", values=vCol, labels=vLabel2) + 
    scale_linetype_discrete(name="", labels=vLabel2) + 
    scale_shape_discrete(name="", labels=vLabel2) + 
    scale_y_continuous(breaks=breaks_pretty(), label=percent) + 
    scale_x_continuous(breaks=breaks_pretty(n=8), label=comma)
)

# - Save plot
dpi <- 335 # reset
ggsave(gsurv_ft2, file=paste0(genFigPath, "EventProb_Write-Off_ActVsExp_CoxDisc_NoSplines.png"),
       width=2400/dpi, height=1800/dpi,dpi=dpi, bg="white")




# ============ 4. Housekeeping ============

cleanEnviron()
