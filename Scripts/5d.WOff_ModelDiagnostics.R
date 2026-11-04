# ========================= WRITE-OFF RISK - MODEL DIAGNOSTICS =================
# Conducting additional model diagnostics
# ------------------------------------------------------------------------------
# PROJECT TITLE: Loss Modelling (LGD) for Residential Mortgages
# SCRIPT AUTHOR(S): Marcel Muller (MM), Mohammed Gabru (MG), Dr Arno Botha (AB)
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
#   - 4c.InputSpace_LogisticRegression
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
# ==============================================================================



# ============ 1. Preliminaries ============

# --- 1.1 Load and prepare data
# - Confirm prepared datasets are loaded into memory
if (!exists('datCredit_train_CDH_sub')) unpack.ffdf(paste0(genPath, "creditdata_train_CDH_sub"), tempPath); gc()
if (!exists('datCredit_valid_CDH_sub')) unpack.ffdf(paste0(genPath, "creditdata_valid_CDH_sub"), tempPath); gc()

# - Ensure order of data is correct
setorder(datCredit_train_CDH_sub, LoanID, Date)
setorder(datCredit_valid_CDH_sub, LoanID, Date)

# - Filter data for default spells and date selections
datCredit_train <- datCredit_train_CDH_sub[!is.na(DefSpell_Key)]
datCredit_valid <- datCredit_valid_CDH_sub[!is.na(DefSpell_Key)]

# - Remove redundant objects
rm(datCredit_train_CDH_sub, datCredit_valid_CDH_sub); gc()

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
datCredit <- rbind(data.table(datCredit_train, Sample="Train"), 
                   data.table(datCredit_valid, Sample="Validation"))

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

# - Classical cross sectional logistic regression model
modLR_Classic <- readRDS(paste0(genObjPath,"LR_Model.rds"))

# - Survival tree model
SurvTree_CTree <- readRDS(paste0(genObjPath,"SurvTree_CTree.rds"))

# - Multinomial logistic regression
unpack.ffdf(paste0(genPath,"MultiDtH_WOff"))

# - Multinomial survival model
unpack.ffdf(paste0(genPath,"MultiCI_Tree"))


# --- 1.3 Estimate event rates for DtH models
# - Predict hazard h(t) = P(T=t | T>= t) in discrete-time
datCredit[, Hazard_adv:=predict(modLR_Adv, newdata=datCredit, type="response")]
datCredit[, Hazard_bas:=predict(modLR_Bas, newdata=datCredit, type="response")]

# - Derive survival probability S(t) = prod(1 - hazard)
datCredit[, Survival_adv:=cumprod(1-Hazard_adv), by=list(DefSpell_Key)]
datCredit[, Survival_bas:=cumprod(1-Hazard_bas), by=list(DefSpell_Key)]

# - Derive discrete density, or event probability f(t) = S(t-1) - S(t)
datCredit[, EventRate_adv:=shift(Survival_adv, type="lag", n=1, fill=1) - Survival_adv, by=list(DefSpell_Key)]
datCredit[, EventRate_bas:=shift(Survival_bas, type="lag", n=1, fill=1) - Survival_bas, by=list(DefSpell_Key)]


# --- 1.4 Estimate event rates amidst competing risks
### Models: MLR-based DtH-Multinomial model, multinomial survival tree (conditional inference tree)

# - Predict cause-specific hazard h_g(t) = P(T=t, G=g | T >= t) in discrete-time | DtH-Multinomial
matPredictions <- predict(modMLR, newdata=datCredit, type = "probs")
datCredit[, Hazard_WriteOff:=matPredictions[,"Write-off"]]
datCredit[, Hazard_Cure:=matPredictions[,"Cured"]]

# - Predict cause-specific hazard h_g(t) = P(T=t, G=g | T >= t) in discrete-time | Multinomial CI-tree
matPredictions_tree <- predict(modMultiSurvTree, newdata=datCredit, type = "prob")
datCredit[, Hazard_WriteOff_tree := matPredictions_tree[, "Write-off"]]
datCredit[, Hazard_Cured_tree := matPredictions_tree[, "Cured"]]

# - Derive (event-free) survival probability S(t) = prod(1 - h_1(t) - h_2(t))
datCredit[, Survival_overall:=cumprod(1-Hazard_WriteOff-Hazard_Cure), by=.(DefSpell_Key)]
datCredit[, Survival_overall_tree:=cumprod(1-Hazard_WriteOff_tree-Hazard_Cured_tree), by=.(DefSpell_Key)]

# - Derive event rates as f_1(t) = S(t-1) . h_1(t)
datCredit[, EventRate_WriteOff := shift(Survival_overall, n=1,fill=1) * Hazard_WriteOff, by=.(DefSpell_Key)]
datCredit[, EventRate_WriteOff_tree := shift(Survival_overall_tree, n=1,fill=1) * Hazard_WriteOff_tree, by=.(DefSpell_Key)]


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

# - Remove added rows
datCredit <- subset(datCredit, Counter_Artificial>0) %>% setorder(LoanID, Date)


# --- 1.6 Estimate conditional (residual) CIFs
# - Survival models
### NOTE: - We sum the M residual event rates and condition them on an account having survived to t
###       - Here, M is selected as the mean restricted survival time for each t
datCredit[, `:=`(CIF_res_adv=calc_residual_CIF(EventRate_adv, Survival_adv, M=DefSpell_Age),
                 CIF_res_bas=calc_residual_CIF(EventRate_bas, Survival_bas, M=DefSpell_Age),
                 CIF_res_survtree=calc_residual_CIF(EventRate_survtree, Survival_survtree, M=DefSpell_Age),
                 CIF_res_mult=calc_residual_CIF(EventRate_WriteOff, Survival_overall, M=DefSpell_Age),
                 CIF_res_mult_survtree=calc_residual_CIF(EventRate_WriteOff_tree, Survival_overall_tree, M=DefSpell_Age)),
          by=list(DefSpell_Key)]
### MM: - For the survival tree we actually need to sum  over the hazards of the node that the account is at the current period
###     - Except for using future information (which we do by necessity in the DtH models), we can also get probabilities>1 due to the
####      the survival quantities being so different over nodes

# - Classical model
### NOTE: The prediction from the classical model is inherently the residual CIF since it
###       is calibrated to give the spell-level probability of write-off given account survival until t
datCredit[, CIF_res_classic:=predict(modLR_Classic, newdata=.SD[], type="response")]


# --- 1.6 Sanity checks
# - [SANITY CHECK] Lookup of survival quantities
lookup_bas <- datCredit[DefSpell_Key==datCredit[DefSpellResol_Type_Hist=="WOFF" & TimeInDefSpell==50, DefSpell_Key][1000],
                        c("DefSpell_Key", "Date", "Hazard_bas", "Survival_bas", "EventRate_bas", "CIF_res_bas", "WOff_Ind")]
lookup_adv <- datCredit[DefSpell_Key==datCredit[DefSpellResol_Type_Hist=="WOFF" & TimeInDefSpell==50, DefSpell_Key][1000],
                        c("DefSpell_Key", "Date", "Hazard_adv", "Survival_adv", "EventRate_adv", "CIF_res_adv", "WOff_Ind")]
lookup_classic <- datCredit[DefSpell_Key==datCredit[DefSpellResol_Type_Hist=="WOFF" & TimeInDefSpell==50, DefSpell_Key][1000],
                            c("DefSpell_Key", "Date", "CIF_res_classic", "WOff_Ind")]
lookup_survtree <- datCredit[DefSpell_Key==datCredit[DefSpellResol_Type_Hist=="WOFF" & TimeInDefSpell==50, DefSpell_Key][1000],
                             c("DefSpell_Key", "Date", "Node", "Hazard_survtree", "Survival_survtree", "EventRate_survtree", "CIF_res_survtree", "WOff_Ind")]
lookup_multi <- datCredit[DefSpell_Key==datCredit[DefSpellResol_Type_Hist=="WOFF" & TimeInDefSpell==50, DefSpell_Key][1000],
                             c("DefSpell_Key", "Date", "Node", "Hazard_WriteOff", "Survival_overall", "EventRate_WriteOff", "CIF_res_mult", "WOff_Ind")]
lookup_multi_tree <- datCredit[DefSpell_Key==datCredit[DefSpellResol_Type_Hist=="WOFF" & TimeInDefSpell==50, DefSpell_Key][1000],
                                c("DefSpell_Key", "Date", "Node", "Hazard_WriteOff_tree", "Survival_overall_tree", "EventRate_WriteOff_tree", "CIF_res_mult_survtree", "WOff_Ind")]

# - Distribution of predictions
hist(datCredit$CIF_res_bas); describe2(datCredit$CIF_res_bas)
hist(datCredit$CIF_res_adv); describe2(datCredit$CIF_res_adv)
hist(datCredit$CIF_res_classic); describe2(datCredit$CIF_res_classic)
hist(datCredit$CIF_res_survtree); describe2(datCredit$CIF_res_survtree)
hist(datCredit$CIF_res_mult); describe2(datCredit$CIF_res_mult)
hist(datCredit$CIF_res_mult_survtree); describe2(datCredit$CIF_res_mult_survtree)
sum(is.na(datCredit$CIF_res_survtree)); sum(is.nan(datCredit$CIF_res_survtree)); sum(is.infinite(datCredit$CIF_res_survtree))
lookup_check <-datCredit[DefSpell_Key==datCredit[is.na(CIF_res_survtree), DefSpell_Key][1],
                          c("DefSpell_Key", "Date", "Node", "Hazard_survtree", "Survival_survtree", "EventRate_survtree", "RMST", "CIF_res_survtree", "WOff_Ind")]
### RESULTS: - Basic DtH model, cross-sectinal logistic regression model, multinomial model, and mutinomial survival tree
###            produce probabilities bounded by [0,1]
###          - Advanced DtH model has one observation over 1 and all other observations are bounded by [0,1]
###          - Survival has a few missing and infinite values, this happens at the end of certain spells when both
###            the event rates and survival probabilities are zero
### CONCLUSION: Correct these rates by applying ceilings of 1

# - Correct unintuitive CIF-values by applying a ceiling of 1
datCredit[, `:=`(CIF_res_adv=ifelse(CIF_res_adv>1,1,CIF_res_adv),
                 CIF_res_survtree=ifelse(CIF_res_survtree>1 | is.na(CIF_res_survtree),1,CIF_res_survtree))]

# - [SANITY CHECK] Are probabilities corrected?
hist(datCredit$CIF_res_adv); describe2(datCredit$CIF_res_adv)
hist(datCredit$CIF_res_survtree); describe2(datCredit$CIF_res_survtree)
### RESULTS: Probabilities corrected successfully




# ============ 2. Coefficient of determination (Pseudo R^2-measures) ============

# --- 2.1 Determine R^2-measures
# - Basic model
(coefDeter_Basic <- coefDeter_glm(modLR_Bas))
### RESULTS: McFadden = 4.08%
###          Cox Snell = 0.56%
###          Nagelkerke = 4.36%

# - Advanced model
(coefDeter_Adv <- coefDeter_glm(modLR_Adv))
### RESULTS: McFadden = 66.20%
###          Cox Snell = 8.70%
###          Nagelkerke = 98.41%

# - Classical model
(coefDeter_LR <- coefDeter_glm(modLR_Classic))
### RESULTS: McFadden = 37.05%
###          Cox Snell = 32.77%
###          Nagelkerke = 49.06%


# --- 2.2 Combine and save R^2 measures
# - Create a single table containing the three R^2 measures for each of the models
(PseudoR2_Table<-data.table(Model=c("Survival Analysis (Basic)","Survival Analysis (Advanced)", "CS-LR"),
                            CoxSnell=c(coefDeter_Basic$CoxSnell, coefDeter_Adv$CoxSnell, coefDeter_LR$CoxSnell),
                            McFadden=c(coefDeter_Basic$McFadden, coefDeter_Adv$McFadden, coefDeter_LR$McFadden),
                            Nagelkerke=c(coefDeter_Basic$Nagelkerke,coefDeter_Adv$Nagelkerke, coefDeter_LR$Nagelkerke)))

# - Save table to specified path
saveRDS(PseudoR2_Table, paste0(genObjPath,"PseudoR2_Table.RDS"))

# - Cleanup
suppressWarnings(rm(coefDeter_LR, coefDeter_Adv, coefDeter_LR, PseudoR2_Table)); gc()




# ============ 3. AUC over time | Resolution time | All write-off models ============

# --- 3.1 Preliminaries
### NOTE: Using custom AUC_overTime() function for each write-off risk models

# - Create a variable for last date in each default spell
datCredit[, `:=`(DefSpell_Min_Date=min(Date),
                 DefSpell_Max_Date=max(Date)),by=list(DefSpell_Key)]

# - Create a numeric version of the target
datCredit[, DefSpell_Event2_Num:=as.numeric(DefSpell_Event2)]

# - Calculate AUCs by default cohort start time
BasAUC_start <- AUC_overTime(datCredit, "DefSpell_Min_Date", "DefSpell_Event2_Num", "CIF_res_bas")
AdvAUC_start <- AUC_overTime(datCredit, "DefSpell_Min_Date", "DefSpell_Event2_Num", "CIF_res_adv")
LRAUC_start <- AUC_overTime(datCredit, "DefSpell_Min_Date", "DefSpell_Event2_Num", "CIF_res_classic")
STAUC_start <- AUC_overTime(datCredit, "DefSpell_Min_Date", "DefSpell_Event2_Num", "CIF_res_survtree")
MAUC_start <- AUC_overTime(datCredit, "DefSpell_Min_Date", "DefSpell_Event2_Num", "CIF_res_mult")
MTAUC_start <- AUC_overTime(datCredit, "DefSpell_Min_Date", "DefSpell_Event2_Num", "CIF_res_mult_survtree")

# - Calculate AUCs by default cohort STOP time
BasAUC_stop <- AUC_overTime(datCredit, "DefSpell_Max_Date", "DefSpell_Event2_Num", "CIF_res_bas")
AdvAUC_stop <- AUC_overTime(datCredit, "DefSpell_Max_Date", "DefSpell_Event2_Num", "CIF_res_adv")
LRAUC_stop <- AUC_overTime(datCredit, "DefSpell_Max_Date", "DefSpell_Event2_Num", "CIF_res_classic")
STAUC_stop <- AUC_overTime(datCredit, "DefSpell_Max_Date", "DefSpell_Event2_Num", "CIF_res_survtree")
MAUC_stop <- AUC_overTime(datCredit, "DefSpell_Max_Date", "DefSpell_Event2_Num", "CIF_res_mult")
MTAUC_stop <- AUC_overTime(datCredit, "DefSpell_Max_Date", "DefSpell_Event2_Num", "CIF_res_mult_survtree")


# --- 3.2 Prepare datasets for graphing
# - Convert values to percentages
# Basic model
BasAUC_start[,`:=`(AUC_Val=AUC_Val/100, AUC_LowerCI=AUC_LowerCI/100, AUC_UpperCI=AUC_UpperCI/100)]
BasAUC_stop[,`:=`(AUC_Val=AUC_Val/100, AUC_LowerCI=AUC_LowerCI/100, AUC_UpperCI=AUC_UpperCI/100)]
# Advanced model
AdvAUC_start[,`:=`(AUC_Val=AUC_Val/100, AUC_LowerCI=AUC_LowerCI/100, AUC_UpperCI=AUC_UpperCI/100)]
AdvAUC_stop[,`:=`(AUC_Val=AUC_Val/100, AUC_LowerCI=AUC_LowerCI/100, AUC_UpperCI=AUC_UpperCI/100)]
# Classical model
LRAUC_start[,`:=`(AUC_Val=AUC_Val/100, AUC_LowerCI=AUC_LowerCI/100, AUC_UpperCI=AUC_UpperCI/100)]
LRAUC_stop[,`:=`(AUC_Val=AUC_Val/100, AUC_LowerCI=AUC_LowerCI/100, AUC_UpperCI=AUC_UpperCI/100)]
# Survival tree
STAUC_start[,`:=`(AUC_Val=AUC_Val/100, AUC_LowerCI=AUC_LowerCI/100, AUC_UpperCI=AUC_UpperCI/100)]
STAUC_stop[,`:=`(AUC_Val=AUC_Val/100, AUC_LowerCI=AUC_LowerCI/100, AUC_UpperCI=AUC_UpperCI/100)]
# Multinomial model
MAUC_start[,`:=`(AUC_Val=AUC_Val/100, AUC_LowerCI=AUC_LowerCI/100, AUC_UpperCI=AUC_UpperCI/100)]
MAUC_stop[,`:=`(AUC_Val=AUC_Val/100, AUC_LowerCI=AUC_LowerCI/100, AUC_UpperCI=AUC_UpperCI/100)]
# Multinomial survival model
MTAUC_start[,`:=`(AUC_Val=AUC_Val/100, AUC_LowerCI=AUC_LowerCI/100, AUC_UpperCI=AUC_UpperCI/100)]
MTAUC_stop[,`:=`(AUC_Val=AUC_Val/100, AUC_LowerCI=AUC_LowerCI/100, AUC_UpperCI=AUC_UpperCI/100)]

# - Add label to differentiate in plotting
BasAUC_start[,Dataset:="A"]; BasAUC_stop[,Dataset:="A"]
AdvAUC_start[,Dataset:="B"]; AdvAUC_stop[,Dataset:="B"]
LRAUC_start[,Dataset:="C"]; LRAUC_stop[,Dataset:="C"]
STAUC_start[,Dataset:="D"]; STAUC_stop[,Dataset:="D"]
MAUC_start[,Dataset:="E"]; MAUC_stop[,Dataset:="E"]
MTAUC_start[,Dataset:="F"]; MTAUC_stop[,Dataset:="F"]


# --- 3.3 Create AUC over-time graph | Default cohort start time
# - Create final dataset for ggplot
datPlot <- rbind(BasAUC_start, AdvAUC_start, LRAUC_start,
                 STAUC_start, MAUC_start, MTAUC_start)

# - Annotation parameters
# Location of annotations
start_y <- 0.65; space <- 0.025
y_vals <- c(start_y,start_y-space*1:5)
# Creating an annotation dataset for easier annotations
datAnnotate <- data.table(MeanAUC=NULL, Dataset=c("A", "B", "C", "D", "E", "F"),
                          x=rep(as.Date("2012-05-31"),6), # Text x coordinates
                          y=y_vals)

# - TTC-mean & confidence interval calculations
confLevel <- 0.95
vEventRates_Mean <- c(mean(BasAUC_start$AUC_Val, na.rm=T), mean(AdvAUC_start$AUC_Val, na.rm=T),
                      mean(LRAUC_start$AUC_Val, na.rm=T), mean(STAUC_start$AUC_Val, na.rm=T),
                      mean(MAUC_start$AUC_Val, na.rm=T), mean(MTAUC_start$AUC_Val, na.rm=T))
vEventRates_stErr <- c(sd(BasAUC_start$AUC_Val, na.rm=T) / sqrt(BasAUC_start[, .N]),
                       sd(AdvAUC_start$AUC_Val, na.rm=T) / sqrt(AdvAUC_start[, .N]),
                       sd(LRAUC_start$AUC_Val, na.rm=T) / sqrt(LRAUC_start[, .N]),
                       sd(STAUC_start$AUC_Val, na.rm=T) / sqrt(STAUC_start[, .N]),
                       sd(MAUC_start$AUC_Val, na.rm=T) / sqrt(MAUC_start[, .N]),
                       sd(MTAUC_start$AUC_Val, na.rm=T) / sqrt(MTAUC_start[, .N]))
vMargin <- qnorm(1-(1-confLevel)/2) * vEventRates_stErr
vLabel <- c(paste0("'TTC-mean over '*italic(t)*' for '*italic(A[t])*' : ", sprintf("%.2f",vEventRates_Mean[1]*100),
                   "% ± ", sprintf("%1.3f", vMargin[1]*100),"%'"),
            paste0("'TTC-mean over '*italic(t)*' for '*italic(B[t])*' : ", sprintf("%.2f",vEventRates_Mean[2]*100),
                   "% ± ", sprintf("%1.3f", vMargin[2]*100),"%'"),
            paste0("'TTC-mean over '*italic(t)*' for '*italic(C[t])*' : ", sprintf("%.2f",vEventRates_Mean[3]*100),
                   "% ± ", sprintf("%1.3f", vMargin[3]*100),"%'"),
            paste0("'TTC-mean over '*italic(t)*' for '*italic(D[t])*' : ", sprintf("%.2f",vEventRates_Mean[4]*100),
                   "% ± ", sprintf("%1.3f", vMargin[4]*100),"%'"),
            paste0("'TTC-mean over '*italic(t)*' for '*italic(E[t])*' : ", sprintf("%.2f",vEventRates_Mean[5]*100),
                   "% ± ", sprintf("%1.3f", vMargin[5]*100),"%'"),
            paste0("'TTC-mean over '*italic(t)*' for '*italic(F[t])*' : ", sprintf("%.2f",vEventRates_Mean[6]*100),
                   "% ± ", sprintf("%1.3f", vMargin[6]*100),"%'"))
datAnnotate[, Label:=vLabel]

# - Graphing parameters
chosenFont <- "Cambria"; dpi <- 180
vCol <- brewer.pal(8, "Dark2")[c(2,1,3,5,4,8)]
vLabel <- c("A"=bquote(italic(A[t])~": DtH-Basic"), "B"=bquote(italic(B[t])~": DtH-Advanced"),
            "C"=bquote(italic(C[t])~": CS-LR"), "D"=bquote(italic(D[t])~": CI-tree"),
            "E"=bquote(italic(E[t])~": MLR"), "F"=bquote(italic(F[t])~": MCI-tree"))
vShape <- c(17, 20, 4, 7, 14, 24) 

# - Create graph
(g1_AUC <- ggplot(datPlot, aes(x=Date, y=AUC_Val)) + theme_minimal() + 
    labs(y=bquote("Discriminatory Accuracy: AUC (%) "), x=bquote("Default spell cohorts (mmmccyy): start time "*italic(t[e]))) + 
    theme(text=element_text(family=chosenFont), legend.position="bottom",
          axis.text.x=element_text(angle=90), 
          strip.background=element_rect(fill="snow2", colour="snow2"),
          strip.text=element_text(size=11, colour="gray50"), strip.text.y.right=element_text(angle=90)) + 
    # Main graph
    # geom_ribbon(aes(fill=Dataset, ymin=AUC_LowerCI, ymax=AUC_UpperCI), alpha=0.2,show.legend = FALSE) + 
    geom_line(aes(colour=Dataset, linetype=Dataset), linewidth=0.3) +    
    geom_point(aes(colour=Dataset, shape=Dataset), size=1.8) + 
    geom_hline(yintercept=0.7, linewidth=0.75) +
    geom_text(data=datAnnotate, aes(x=x, y=y, label=Label), family=chosenFont, size=3.5, hjust=0, parse=T) +
    # Facets & scale options
    scale_colour_manual(name="Model", values=vCol, labels=vLabel) + 
    scale_fill_manual(name="Model", values=vCol, labels=vLabel) +
    scale_shape_manual(name=bquote("Model"), values=vShape, labels=vLabel) + 
    scale_linetype_discrete(name=bquote("Model"), labels=vLabel) + 
    scale_x_date(date_breaks=paste0(6, " month"), date_labels = "%b %Y") +
    scale_y_continuous(breaks=pretty_breaks(), label=percent)
)

# - Save graph
ggsave(g1_AUC, file=paste0(genFigPath, "AUC_DefaultCohort_StartTime.png"),
       width=1200/dpi, height=1000/dpi, dpi=dpi, bg="white")


# --- 3.4 Create AUC over-time graph | Default cohort stop time
# - Create final dataset for ggplot
datPlot <- rbind(BasAUC_stop, LRAUC_stop, AdvAUC_stop,
                 STAUC_stop, MAUC_stop, MTAUC_stop)

# - Annotation parameters
# Location of annotations
start_y <- 0.675; space <- 0.025
y_vals <- c(start_y,start_y-space*1:5)
# Creating an annotation dataset for easier annotations
datAnnotate <- data.table(MeanAUC=NULL, Dataset=c("A","B","C", "D", "E", "F"),
                          x=rep(as.Date("2012-05-31"),6), # Text x coordinates
                          y=y_vals)

# - TTC-mean & confidence interval calculations
confLevel <- 0.95
vEventRates_Mean <- c(mean(BasAUC_stop$AUC_Val, na.rm=T), mean(AdvAUC_stop$AUC_Val, na.rm=T),
                      mean(LRAUC_stop$AUC_Val, na.rm=T), mean(STAUC_stop$AUC_Val, na.rm=T),
                      mean(MAUC_stop$AUC_Val, na.rm=T), mean(MTAUC_stop$AUC_Val, na.rm=T))
vEventRates_stErr <- c(sd(BasAUC_stop$AUC_Val, na.rm=T) / sqrt(BasAUC_stop[, .N]),
                       sd(AdvAUC_stop$AUC_Val, na.rm=T) / sqrt(AdvAUC_stop[, .N]),
                       sd(LRAUC_stop$AUC_Val, na.rm=T) / sqrt(LRAUC_stop[, .N]),
                       sd(STAUC_stop$AUC_Val, na.rm=T) / sqrt(STAUC_stop[, .N]),
                       sd(MAUC_stop$AUC_Val, na.rm=T) / sqrt(MAUC_stop[, .N]),
                       sd(MTAUC_stop$AUC_Val, na.rm=T) / sqrt(MTAUC_stop[, .N]))
vMargin <- qnorm(1-(1-confLevel)/2) * vEventRates_stErr
vLabel <- c(paste0("'TTC-mean over '*italic(t)*' for '*italic(A[t])*' : ", sprintf("%.2f",vEventRates_Mean[1]*100),
                   "% ± ", sprintf("%1.3f", vMargin[1]*100),"%'"),
            paste0("'TTC-mean over '*italic(t)*' for '*italic(B[t])*' : ", sprintf("%.2f",vEventRates_Mean[2]*100),
                   "% ± ", sprintf("%1.3f", vMargin[3]*100),"%'"),
            paste0("'TTC-mean over '*italic(t)*' for '*italic(C[t])*' : ", sprintf("%.2f",vEventRates_Mean[3]*100),
                   "% ± ", sprintf("%1.3f", vMargin[2]*100),"%'"),
            paste0("'TTC-mean over '*italic(t)*' for '*italic(D[t])*' : ", sprintf("%.2f",vEventRates_Mean[4]*100),
                   "% ± ", sprintf("%1.3f", vMargin[2]*100),"%'"),
            paste0("'TTC-mean over '*italic(t)*' for '*italic(E[t])*' : ", sprintf("%.2f",vEventRates_Mean[5]*100),
                   "% ± ", sprintf("%1.3f", vMargin[2]*100),"%'"),
            paste0("'TTC-mean over '*italic(t)*' for '*italic(F[t])*' : ", sprintf("%.2f",vEventRates_Mean[6]*100),
                   "% ± ", sprintf("%1.3f", vMargin[2]*100),"%'"))
datAnnotate[, Label:=vLabel]

# - Graphing parameters
chosenFont <- "Cambria"; dpi <- 180
vCol <- brewer.pal(8, "Dark2")[c(2, 1, 3, 5, 4, 8)]
vLabel <- c("A"=bquote(italic(A[t])~": DtH-Basic"), "B"=bquote(italic(B[t])~": DtH-Advanced"),
            "C"=bquote(italic(C[t])~": CS-LR"), "D"=bquote(italic(D[t])~": ST"),
            "E"=bquote(italic(E[t])~": MLR"), "F"=bquote(italic(F[t])~": MCI-tree"))
vShape <- c(17, 20, 4, 7, 14, 24) 

# - Create graph
(g2_AUC <- ggplot(datPlot, aes(x=Date, y=AUC_Val)) + theme_minimal() + 
    labs(y=bquote("Discriminatory Accuracy: AUC (%) "), x=bquote("Default spell cohorts (mmmccyy): stop time "*italic(t[s]))) + 
    theme(text=element_text(family=chosenFont), legend.position="bottom",
          axis.text.x=element_text(angle=90), 
          strip.background=element_rect(fill="snow2", colour="snow2"),
          strip.text=element_text(size=11, colour="gray50"), strip.text.y.right=element_text(angle=90)) + 
    # Main graph
    # geom_ribbon(aes(fill=Dataset, ymin=AUC_LowerCI, ymax=AUC_UpperCI), alpha=0.2,show.legend = FALSE) + 
    geom_line(aes(colour=Dataset, linetype=Dataset), linewidth=0.3) +    
    geom_point(aes(colour=Dataset, shape=Dataset), size=1.8) + 
    geom_hline(yintercept=0.7, linewidth=0.75) +
    geom_text(data=datAnnotate, aes(x=x, y=y, label=Label), family=chosenFont, size=3.5, hjust=0, parse=TRUE) +
    # Facets & scale options
    scale_colour_manual(name="Model", values=vCol, labels=vLabel) + 
    scale_fill_manual(name="Model", values=vCol, labels=vLabel) +
    scale_shape_manual(name=bquote("Model"), values=vShape, labels=vLabel) + 
    scale_linetype_discrete(name=bquote("Model"), labels=vLabel) + 
    scale_x_date(date_breaks=paste0(6, " month"), date_labels = "%b %Y") +
    scale_y_continuous(breaks=pretty_breaks(), label=percent)
)

# - Save graph
ggsave(g2_AUC, file=paste0(genFigPath, "AUC_DefaultCohort_StopTime.png"), width=1200/dpi, height=1000/dpi, dpi=dpi, bg="white")




# ============ 4. Write-off resolution rates ============

# --- 4.1 Prepare data for plotting
# -- 4.1.1 Plotting data
# - Aggregate data at default start
datPlot <- datCredit[TimeInDefSpell==1,
                     .(Empirical=mean(DefSpell_Event==1, na.rm=T),
                       Basic_DtH=mean(CIF_res_bas, na.rm=T),
                       Advanced_DtH=mean(CIF_res_adv, na.rm=T),
                       Classic=mean(CIF_res_classic, na.rm=T),
                       SurvTree=mean(CIF_res_survtree, na.rm=T),
                       Mult=mean(CIF_res_mult, na.rm=T),
                       MultSurvTree=mean(CIF_res_mult_survtree, na.rm=T)),
                     by=list(Date)]

# - Pivot data to facilitate grapging
datPlot <- datPlot %>% pivot_longer(cols=c("Empirical", "Basic_DtH", "Advanced_DtH", "Classic", "SurvTree",
                                           "Mult", "MultSurvTree"),
                                    values_to="WOff_Rate", names_to="Model") %>% data.table() %>% setorder(Date)


# -- 4.1.2 Annotation data
# - Location of annotations
start_y <- 0.4; space <- 0.035
y_vals <- c(start_y,start_y-space*1:5)

# - Creating an annotation dataset for easier annotations
datAnno <- data.table(MeanAUC=NULL, Dataset=c("A-B","A-C", "A-D", "A-E", "A-F", "A-G"),
                      x=rep(as.Date("2016-06-30"),6), # Text x coordinates
                      y=y_vals)

# - Estimate MAEs
vEventRates_MAE <- c(mean(abs(datPlot[Model=="Empirical", WOff_Rate]-datPlot[Model=="Basic_DtH", WOff_Rate]), na.rm=T),
                     mean(abs(datPlot[Model=="Empirical", WOff_Rate]-datPlot[Model=="Advanced_DtH", WOff_Rate]), na.rm=T),
                     mean(abs(datPlot[Model=="Empirical", WOff_Rate]-datPlot[Model=="Classic", WOff_Rate]), na.rm=T),
                     mean(abs(datPlot[Model=="Empirical", WOff_Rate]-datPlot[Model=="SurvTree", WOff_Rate]), na.rm=T),
                     mean(abs(datPlot[Model=="Empirical", WOff_Rate]-datPlot[Model=="Mult", WOff_Rate]), na.rm=T),
                     mean(abs(datPlot[Model=="Empirical", WOff_Rate]-datPlot[Model=="MultSurvTree", WOff_Rate]), na.rm=T))

# - Create and assign labels
series_labels_mae <- c("B[t]", "C[t]", "D[t]", "E[t]", "F(t)", "G(t)")
vLabel_MAE <- sapply(seq_along(series_labels_mae), function(i) {
  paste0("'MAE between '*italic(", series_labels_mae[i], ")*' and '*italic(A[t])*' : ",
         sprintf('%.2f', vEventRates_MAE[i] * 100), "%'")
})
datAnno[, Label:=vLabel_MAE]


# --- 4.2 Create plot
# - Refactor levels
datPlot[,Model:=case_when(Model=="Empirical"~"a_Empirical", Model=="Basic_DtH"~"b_Basic_DtH",
                          Model=="Advanced_DtH"~"c_Advanced_DtH", Model=="Classic"~"d_Classic",
                          Model=="SurvTree"~"e_SurvTree", Model=="Mult"~"f_Mult",
                          Model=="MultSurvTree"~"g_MultSurvTree")]

# - Graphing parameters
chosenFont <- "Cambria"; dpi <- 180
vCol <- brewer.pal(8, "Dark2")[c(2, 1, 3, 5, 7, 4, 8)]
vLabel <- c("a_Empirical"=bquote(italic(A[t])~": Empirical"), "b_Basic_DtH"=bquote(italic(B[t])~": DtH-Basic"),
            "c_Advanced_DtH"=bquote(italic(C[t])~": DtH-Advanced"), "d_Classic"=bquote(italic(D[t])~": CS-LR"),
            "e_SurvTree"=bquote(italic(E[t])~": CI-tree"), "f_Mult"=bquote(italic(F[t])~": MLR"),
            "g_MultSurvTree"=bquote(italic(G[t])~": MCI-tree"))
vShape <- c(17, 20, 4, 7, 12, 14, 24) 

# - Plot
(g2_ResolRate <- ggplot(data=datPlot, aes(x=Date, y=WOff_Rate)) + theme_minimal() +
  labs(y=bquote("Write-off resolution rate (%) "), x=bquote("Reporting time")) + 
  theme(text=element_text(family=chosenFont), legend.position="bottom",
        axis.text.x=element_text(angle=90), 
        strip.background=element_rect(fill="snow2", colour="snow2"),
        strip.text=element_text(size=11, colour="gray50"), strip.text.y.right=element_text(angle=90)) + 
  # Main graph
  geom_line(aes(colour=Model, linetype=Model)) +
  geom_point(aes(colour=Model, shape=Model)) +
  # Annotations
  geom_text(data=datAnno, aes(x=x, y=y, label=Label), parse=T) +
  # Facets & scale options
  scale_colour_manual(name="Model", values=vCol, labels=vLabel, guide=guide_legend(nrow=2, byrow=T)) +
  scale_fill_manual(name="Model", values=vCol, labels=vLabel, guide=guide_legend(nrow=2, byrow=T)) +
  scale_shape_manual(name="Model", values=vShape, labels=vLabel, guide=guide_legend(nrow=2, byrow=T)) +
  scale_linetype_discrete(name="Model", labels=vLabel, guide=guide_legend(nrow=2, byrow=T)) +
  scale_x_date(date_breaks="12 month", date_labels="%b %Y") +
  scale_y_continuous(breaks=pretty_breaks(), labels=percent))
### MM: - This view aligns with the resolution rate that we have in the ROT-aritcle, i.e., we look at the
###       probability of write-off over the entire spell.
###     - Very weird result that the basic model is the best.

# - Save graph
dpi <-180
ggsave(g2_ResolRate, file=paste0(genFigPath, "WriteOff_ResolRate_RepotingDate.png"),
       width=1600/dpi, height=1000/dpi, dpi=dpi, bg="white")




# ============ 5. Forecasting ============




# ============ 6. Housekeeping ============

cleanEnviron()



