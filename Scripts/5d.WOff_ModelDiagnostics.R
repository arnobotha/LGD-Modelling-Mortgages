# ========================= WRITE-OFF RISK - MODEL DIAGNOSTICS =================
# Conducting additional model diagnostics
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
#   - 4c.InputSpace_LogisticRegression
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
# ==============================================================================


# ============ 1. Preliminaries ============

# --- 1.1 Load & prepare data for analysis
# - Confirm prepared datasets are loaded into memory
if (!exists('datCredit_train_CDH')) unpack.ffdf(paste0(genPath,"creditdata_train_CDH"), tempPath);gc()
if (!exists('datCredit_valid_CDH')) unpack.ffdf(paste0(genPath,"creditdata_valid_CDH"), tempPath);gc()

# - Ensure order of data is correct
setorder(datCredit_train_CDH, LoanID, Date)
setorder(datCredit_valid_CDH, LoanID, Date)

# - Use only default spells
datCredit_train <- datCredit_train_CDH[!is.na(DefSpell_Key),]
datCredit_valid <- datCredit_valid_CDH[!is.na(DefSpell_Key),]

# - Score data using classic model for each instance of [TimeInDefSpell] as [DefSpell_Age]
datCredit_train[, DefSpell_Age2:=DefSpell_Age]; datCredit_train[, DefSpell_Age:=TimeInDefSpell]
datCredit_valid[, DefSpell_Age2:=DefSpell_Age]; datCredit_valid[, DefSpell_Age:=TimeInDefSpell]

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
datCredit[, Hazard_adv:=predict(modLR_Adv, newdata=datCredit, type="response")]
datCredit[, Hazard_bas:=predict(modLR_Bas, newdata=datCredit, type="response")]
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
datCredit <- merge(datCredit, datHaz[, list(Node, Time, Hazard_survtree=Hazard,
                                            Survival_survtree=Surv, EventRate_survtree=EventRate)],
                   by.x=c("Node","TimeInDefSpell"), by.y=c("Node","Time"), all.x=T)




# ============ 2. Coefficient of determination (Pseudo R^2-measures) ============

# --- 2.1 Determine R^2-measures
# - Basic model
(coefDeter_Basic <- coefDeter_glm(modLR_Bas))
### RESULTS: McFadden = 4.36%
###          Cox Snell = 0.56%
###          Nagelkerke = 4.63%

# - Advanced model
(coefDeter_Adv <- coefDeter_glm(modLR_Adv))
### RESULTS: McFadden = 64.39%
###          Cox Snell = 7.97%
###          Nagelkerke = 65.86%

# - Classical model
(coefDeter_LR <- coefDeter_glm(modLR_Classic))
### RESULTS: McFadden = 32.18%
###          Cox Snell = 28.16%
###          Nagelkerke = 43.85%


# --- 2.2 Combine and save R^2 measures
# - Create a single table containing the three R^2 measures for each of the models
(PseudoR2_Table<-data.table(Model=c("Survival Analysis (Basic)","Survival Analysis (Advanced)", "Logistic Regression"),
                            CoxSnell=c(coefDeter_Basic$CoxSnell,coefDeter_LR$CoxSnell,coefDeter_Adv$CoxSnell),
                            McFadden=c(coefDeter_Basic$McFadden,coefDeter_LR$McFadden,coefDeter_Adv$McFadden),
                            Nagelkerke=c(coefDeter_Basic$Nagelkerke,coefDeter_LR$Nagelkerke,coefDeter_Adv$Nagelkerke)))

# - Save table to specified path
saveRDS(PseudoR2_Table, paste0(genObjPath,"PseudoR2_Table.RDS"))

# - Cleanup
suppressWarnings(rm(coefDeter_LR, coefDeter_Adv, coefDeter_LR, PseudoR2_Table)); gc()




# ============ 3. AUC over time | All write-off models ============

# --- 3.1 Estimate AUC over time
### NOTE: Using custom AUC_overTime() function for each write-off risk models

# - Create a variable for last date in each default spell
datCredit[,DefSpell_Max_Date:=max(Date),by=list(DefSpell_Key)]

# - Basic model
BasAUC <- AUC_overTime(datCredit, "DefSpell_Max_Date","DefSpell_Event","Hazard_bas")

# - Advanced model
AdvAUC <- AUC_overTime(datCredit, "DefSpell_Max_Date","DefSpell_Event","Hazard_adv")

# - Classical model
### MM: As with my commentary on the LGD distributions and t-Brier Scores, this is not a valid comparison given
###     that the hazard rate is not calculated correctly for the classical model
LRAUC <- AUC_overTime(datCredit, "DefSpell_Max_Date","DefSpell_Event","Hazard_classic")

# - Survival tree
STAUC <- AUC_overTime(datCredit, "DefSpell_Max_Date", "DefSpell_Event", "Hazard_survtree")


# --- 3.2 Prepare datasets for graphing
# - Filter AUCs to ensure a sensible y-axis scale
AdvAUC <- AdvAUC[!AUC_Val<70,]
BasAUC <- BasAUC[!AUC_LowerCI<30,]

# - Convert valeus to percentages
# Basic model
BasAUC[,AUC_Val:=AUC_Val/100]; BasAUC[,AUC_LowerCI:=AUC_LowerCI/100]; BasAUC[,AUC_UpperCI:=AUC_UpperCI/100]
# Advanced model
AdvAUC[,AUC_Val:=AUC_Val/100]; AdvAUC[,AUC_LowerCI:=AUC_LowerCI/100]; AdvAUC[,AUC_UpperCI:=AUC_UpperCI/100]
# Classical model
LRAUC[,AUC_Val:=AUC_Val/100]; LRAUC[,AUC_LowerCI:=AUC_LowerCI/100]; LRAUC[,AUC_UpperCI:=AUC_UpperCI/100]
# Survival tree
STAUC[,AUC_Val:=AUC_Val/100]; STAUC[,AUC_LowerCI:=AUC_LowerCI/100]; STAUC[,AUC_UpperCI:=AUC_UpperCI/100]

# - Add label to differentiate in plotting
BasAUC[,Dataset:="A"]; AdvAUC[,Dataset:="B"]
LRAUC[,Dataset:="C"]; STAUC[,Dataset:="D"]


# --- 3.3 Create AUC over-time graph
# - Create final dataset for ggplot
datPlot <- rbind(BasAUC, LRAUC, AdvAUC, STAUC)

# - Annotation parameters
# Location of annotations
start_y <- 0.425; space <- 0.025
y_vals <- c(start_y,start_y-space,start_y-space*2,start_y-space*3)
# Creating an annotation dataset for easier annotations
datAnnotate <- data.table(MeanAUC=NULL, Dataset=c("A-B","A-C","A-D", "A-E"),
                          x=rep(as.Date("2013-05-31"),4), # Text x coordinates
                          y=y_vals)

# - TTC-mean & confidence interval calculations
confLevel <- 0.95
vEventRates_Mean <- c(mean(BasAUC$AUC_Val, na.rm=T), mean(LRAUC$AUC_Val, na.rm=T),
                      mean(AdvAUC$AUC_Val, na.rm=T), mean(STAUC$AUC_Val, na.rm=T))
vEventRates_stErr <- c(sd(BasAUC$AUC_Val, na.rm=T) / sqrt(BasAUC[, .N]),
                       sd(LRAUC$AUC_Val, na.rm=T) / sqrt(LRAUC[, .N]),
                       sd(AdvAUC$AUC_Val, na.rm=T) / sqrt(AdvAUC[, .N]),
                       sd(STAUC$AUC_Val, na.rm=T) / sqrt(STAUC[, .N]))
vMargin <- qnorm(1-(1-confLevel)/2) * vEventRates_stErr
vLabel <- c(paste0("'TTC-mean over '*italic(t)*' for '*italic(A[t])*' : ", sprintf("%.2f",vEventRates_Mean[1]*100),
                   "% ± ", sprintf("%1.3f", vMargin[1]*100),"%'"),
            paste0("'TTC-mean over '*italic(t)*' for '*italic(B[t])*' : ", sprintf("%.2f",vEventRates_Mean[3]*100),
                   "% ± ", sprintf("%1.3f", vMargin[3]*100),"%'"),
            paste0("'TTC-mean over '*italic(t)*' for '*italic(C[t])*' : ", sprintf("%.2f",vEventRates_Mean[2]*100),
                   "% ± ", sprintf("%1.3f", vMargin[2]*100),"%'"),
            paste0("'TTC-mean over '*italic(t)*' for '*italic(D[t])*' : ", sprintf("%.2f",vEventRates_Mean[4]*100),
                   "% ± ", sprintf("%1.3f", vMargin[2]*100),"%'"))
datAnnotate[, Label:=vLabel]

# - Graphing parameters
chosenFont <- "Cambria"; dpi <- 180
vCol <- brewer.pal(8, "Dark2")[c(2,1,3,5)]
vLabel <- c("A"=bquote(italic(A[t])~": DtH-Basic"), "C"=bquote(italic(C[t])~": Logistic Regression"), 
            "B"=bquote(italic(B[t])~": DtH-Advanced"), "D"=bquote(italic(D[t])~": ST"))
vShape <- c(17,20,4,7) 

# - Create graph
(g3 <- ggplot(datPlot, aes(x=Date, y=AUC_Val)) + theme_minimal() + 
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
ggsave(g3, file=paste0(genFigPath, "AUC_DefaultCohort_StopTime.png"), width=1200/dpi, height=1000/dpi, dpi=dpi, bg="white")




# ============ 4. Housekeeping ============

cleanEnviron()



