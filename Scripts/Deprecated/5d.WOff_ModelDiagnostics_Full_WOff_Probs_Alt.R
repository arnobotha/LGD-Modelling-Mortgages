# ========================= WRITE-OFF RISK - MODEL DIAGNOSTICS =================
# Conducting additional model diagnostics
# ------------------------------------------------------------------------------
# PROJECT TITLE: Loss Modelling (LGD) for Residential Mortgages
# SCRIPT AUTHOR(S): Mohammed Gabru (MG), Marcel Muller (MM), Dr Arno Botha (AB)
# ------------------------------------------------------------------------------
# -- Script dependencies:
#   - 0.Setup.R
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
# -- 1.2.1 Load pre-fitted models
# - Basic discrete-time hazard model
modLR_Bas <- readRDS(paste0(genObjPath,"CoxDisc_Basic_Model.rds"))

# - Advanced discrete-time hazard model
modLR_Adv <- readRDS(paste0(genObjPath,"CoxDisc_Advanced_Model.rds"))

# - Classical logit model
modLR_Classic <- readRDS(paste0(genObjPath,"LR_Model.rds"))

# - Tree model
SurvTree_CTree <- readRDS(paste0(genObjPath,"SurvTree_CTree.rds"))


# -- 1.2.2 Fit a basic continuous-time hazard model
# - Get inputs from basic model
vars <- colnames(modLR_Bas$model[,-c(1:2, length(modLR_Bas$model))])

# - Fit model
modCoxPH_Bas <- coxph(as.formula(paste0("Surv(time=TimeInDefSpell-1, time2=TimeInDefSpell, event=DefSpell_Event) ~", paste0(vars, collapse="+"))),
                      data=datCredit, ties="efron", cluster=DefSpell_Key)
summary(modCoxPH_Bas)
### RESULTS: All variables are significant


# -- 1.2.3 Fit an advanced continuous-time hazard model
# - Get inputs from advanced model
vars <- colnames(modLR_Adv$model[,-c(1:3)])

# - Refine input space
vars <- vars[!(vars %in% c("InterestRate_Margin_Aggr_Med_2", "DefaultStatus1_Aggr_Prop_Lag_12", "M_RealIncome_Growth_9", "g0_Delinq_Ave"))]
vars <- c(vars, "InterestRate_Margin")

# - Fit model
modCoxPH_Adv <- coxph(as.formula(paste0("Surv(time=TimeInDefSpell-1, time2=TimeInDefSpell, event=DefSpell_Event) ~", paste0(vars, collapse="+"))),
                      data=datCredit, ties="efron", cluster=DefSpell_Key)
summary(modCoxPH_Adv)
### RESULTS: All variables are significant


# --- 1.3 Estimate event rates
# -- 1.3.1 Discrete time models
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


# -- 1.3.2 Continuous time models
# - Fit a KM-estimator to recover baseline survival & cumulative hazard rate
# Fit estimator
kmFit <- survfit(Surv(TimeInDefSpell, DefSpell_Event)~1, data=datCredit)
# Create table to contain survival quantities
datHaz_cox <- data.table(TimeInDefSpell=kmFit$time, Survival_base=kmFit$surv)
# Calculate cumulative hazard
datHaz_cox[, Cum_Hazard_base:=-log(Survival_base)]
# Lag cumulative hazard
datHaz_cox[, Cum_Hazard_base_Lag_1:=shift(Cum_Hazard_base, type="lag", n=1)]
datHaz_cox[TimeInDefSpell==1, Cum_Hazard_base_Lag_1:=0]
# plot(datHaz_cox$Cum_Hazard_base, type="l")

# - Add KM cumulative hazard to data
datCredit <- merge(datCredit, datHaz_cox, by="TimeInDefSpell", all.x=T)

# - Predict proportional hazards
datCredit[, `:=`(CoxPH_Bas=predict(modCoxPH_Bas, newdata=.SD, type="lp"),
                 CoxPH_Adv=predict(modCoxPH_Adv, newdata=.SD, type="lp"))]

# - Get survival rates
datCredit[, `:=`(Survival_CoxPH_Bas=exp(-Cum_Hazard_base*CoxPH_Bas),
                 Survival_CoxPH_Bas_Lag_1=exp(-Cum_Hazard_base_Lag_1*CoxPH_Bas),
                 Survival_CoxPH_Adv=exp(-Cum_Hazard_base*CoxPH_Adv),
                 Survival_CoxPH_Adv_Lag_1=exp(-Cum_Hazard_base_Lag_1*CoxPH_Adv))]

# - Get event rates
datCredit[order(DefSpell_Key, DefSpell_Counter),
          `:=`(EventRate_CoxPH_Bas=Survival_CoxPH_Bas_Lag_1-Survival_CoxPH_Bas,
               EventRate_CoxPH_Adv=Survival_CoxPH_Adv_Lag_1-Survival_CoxPH_Adv),
          by=list(DefSpell_Key)]

# - Remove added rows
datCredit <- subset(datCredit, Counter>0)


# -- 1.3.3 Create a spell-level target variable for write-off instance
datCredit[, DefSpell_Event2:=DefSpell_Event]; datCredit[, DefSpell_Event:=DefSpellResol_Type_Hist=="WOFF"]



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


# --- 1.5 Full write-off probabilities
# -- 1.5.1 Discrete-teim quantitites
# - Calculate rolling cumulative write-off predictions over the default spell
datCredit[order(DefSpell_Key, DefSpell_Counter),
          `:=`(WOff_Prob_adv=rev(cumsum(rev(replace(EventRate_adv, is.na(EventRate_adv), 0))))/
                 ifelse(DefSpell_Counter==1,1,shift(Survival_adv, type="lag", n=1)),
               WOff_Prob_bas=rev(cumsum(rev(replace(EventRate_bas, is.na(EventRate_bas), 0))))/
                 ifelse(DefSpell_Counter==1,1,shift(Survival_bas, type="lag", n=1)),
               WOff_Prob_classic=Hazard_classic,
               WOff_Prob_survtree=rev(cumsum(rev(replace(EventRate_survtree, is.na(EventRate_survtree), 0))))/
                 ifelse(DefSpell_Counter==1,1,shift(Survival_survtree, type="lag", n=1))),
          by=list(DefSpell_Key)]

# - Investigate full write-off probabilities
datCredit[WOff_Prob_bas>1, .N]; datCredit[WOff_Prob_adv>1, .N]
datCredit[WOff_Prob_classic>1, .N]; datCredit[WOff_Prob_survtree>1, .N]
### RESULTS: - The way in which we apply the survival tree results in write-off probabilities larger than one
###          - This is since an spell can change nodes during its lifetime since it is rescored into a node at each period
###          - Since we sum all previous event rates together, which may come from various nodes which contain larger event rates,
###            we may get write-off probabilitites larger than one
###          - It is therefore suggested that if we aggregate the event probabilities, we do so for the current node only that the
###            spell is scored as.
###          - This may lead to some write-off probabilities jumping to one since the spell may have a larger observed time than
###            the last observation of that node
###          - It is fine to assess the current event rate and hazard rate as predicted by the survival tree, but we can't
###            use any past rates since this may results in non-sensical probabilities


# -- 1.5.2 Continuous-time quantities
# - Calculate rolling cumulative write-off predictions over the default spell
datCredit[order(DefSpell_Key, DefSpell_Counter),
          `:=`(WOff_Prob_CoxPH_Bas=rev(cumsum(rev(replace(EventRate_CoxPH_Bas, is.na(EventRate_CoxPH_Bas), 0))))/
                 ifelse(DefSpell_Counter==1,1,Survival_CoxPH_Bas_Lag_1),
               WOff_Prob_CoxPH_Adv=rev(cumsum(rev(replace(EventRate_CoxPH_Adv, is.na(EventRate_CoxPH_Adv), 0))))/
                 ifelse(DefSpell_Counter==1,1,Survival_CoxPH_Adv_Lag_1)),
          by=list(DefSpell_Key)]

# - Investigate full write-off probabilities
datCredit[WOff_Prob_CoxPH_Bas>1, .N]; datCredit[WOff_Prob_CoxPH_Adv>1, .N]
### RESULTS: No unintuitive probabilities detected


# -- 1.5.3 Create a look-up as a sanity check
lookuup <- datCredit[LoanID==datCredit[DefSpell_Age>=60 & DefSpellResol_Type_Hist=="WOFF", LoanID][10000],
                     .(LoanID, Date, Counter, DefSpell_Key, TimeInDefSpell, DefSpellResol_Type_Hist, WOff_Prob_bas, WOff_Prob_adv,
                       WOff_Prob_classic, WOff_Prob_survtree, WOff_Prob_CoxPH_Bas, WOff_Prob_CoxPH_Adv)]




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
### RESULTS: McFadden = 62.37%
###          Cox Snell = 10.23%
###          Nagelkerke = 64.38%


# --- 2.2 Combine and save R^2 measures
# - Create a single table containing the three R^2 measures for each of the models
(PseudoR2_Table<-data.table(Model=c("Survival Analysis (Basic)","Survival Analysis (Advanced)", "Logistic Regression"),
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
datCredit[, DefSpell_Event_Num:=as.numeric(DefSpell_Event)]

# - Calculate AUCs by default cohort start time
### MM: As with my commentary on the LGD distributions and t-Brier Scores, this is not a valid comparison given
###     that the hazard rate is not calculated correctly for the classical model
BasAUC_start <- AUC_overTime(datCredit, "DefSpell_Min_Date","DefSpell_Event_Num", "WOff_Prob_CoxPH_Bas")
AdvAUC_start <- AUC_overTime(datCredit, "DefSpell_Min_Date", "DefSpell_Event_Num", "WOff_Prob_CoxPH_Adv")
LRAUC_start <- AUC_overTime(datCredit, "DefSpell_Min_Date", "DefSpell_Event_Num", "WOff_Prob_classic")
STAUC_start <- AUC_overTime(datCredit, "DefSpell_Min_Date", "DefSpell_Event_Num", "WOff_Prob_survtree")

# - Calculate AUCs by default cohort STOP time
### MM: Same commentary as above
BasAUC_stop <- AUC_overTime(datCredit, "DefSpell_Max_Date", "DefSpell_Event", "WOff_Prob_CoxPH_Bas")
AdvAUC_stop <- AUC_overTime(datCredit, "DefSpell_Max_Date", "DefSpell_Event", "WOff_Prob_CoxPH_Adv")
LRAUC_stop <- AUC_overTime(datCredit, "DefSpell_Max_Date", "DefSpell_Event", "WOff_Prob_classic")
STAUC_stop <- AUC_overTime(datCredit, "DefSpell_Max_Date", "DefSpell_Event", "WOff_Prob_survtree")


# --- 3.2 Prepare datasets for graphing
# - Filter AUCs to ensure a sensible y-axis scale
AdvAUC_start <- AdvAUC_start[!AUC_Val<70,]; AdvAUC_stop <- AdvAUC_stop[!AUC_Val<70,]
BasAUC_start <- BasAUC_start[!AUC_LowerCI<30,]; BasAUC_stop <- BasAUC_stop[!AUC_LowerCI<30,]

# - Convert valeus to percentages
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

# - Add label to differentiate in plotting
BasAUC_start[,Dataset:="A"]; BasAUC_stop[,Dataset:="A"]
AdvAUC_start[,Dataset:="B"]; AdvAUC_stop[,Dataset:="B"]
LRAUC_start[,Dataset:="C"]; LRAUC_stop[,Dataset:="C"]
STAUC_start[,Dataset:="D"]; STAUC_stop[,Dataset:="D"]


# --- 3.3 Create AUC over-time graph | Default cohort start time
# - Create final dataset for ggplot
datPlot <- rbind(BasAUC_start, LRAUC_start, AdvAUC_start, STAUC_start)

# - Annotation parameters
# Location of annotations
start_y <- 0.5; space <- 0.025
y_vals <- c(start_y,start_y-space,start_y-space*2,start_y-space*3)
# Creating an annotation dataset for easier annotations
datAnnotate <- data.table(MeanAUC=NULL, Dataset=c("A-B","A-C","A-D", "A-E"),
                          x=rep(as.Date("2013-05-31"),4), # Text x coordinates
                          y=y_vals)

# - TTC-mean & confidence interval calculations
confLevel <- 0.95
vEventRates_Mean <- c(mean(BasAUC_start$AUC_Val, na.rm=T), mean(LRAUC_start$AUC_Val, na.rm=T),
                      mean(AdvAUC_start$AUC_Val, na.rm=T), mean(STAUC_start$AUC_Val, na.rm=T))
vEventRates_stErr <- c(sd(BasAUC_start$AUC_Val, na.rm=T) / sqrt(BasAUC_start[, .N]),
                       sd(LRAUC_start$AUC_Val, na.rm=T) / sqrt(LRAUC_start[, .N]),
                       sd(AdvAUC_start$AUC_Val, na.rm=T) / sqrt(AdvAUC_start[, .N]),
                       sd(STAUC_start$AUC_Val, na.rm=T) / sqrt(STAUC_start[, .N]))
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
datPlot <- rbind(BasAUC_stop, LRAUC_stop, AdvAUC_stop, STAUC_stop)

# - Annotation parameters
# Location of annotations
start_y <- 0.5; space <- 0.025
y_vals <- c(start_y,start_y-space,start_y-space*2,start_y-space*3)
# Creating an annotation dataset for easier annotations
datAnnotate <- data.table(MeanAUC=NULL, Dataset=c("A-B","A-C","A-D", "A-E"),
                          x=rep(as.Date("2013-05-31"),4), # Text x coordinates
                          y=y_vals)

# - TTC-mean & confidence interval calculations
confLevel <- 0.95
vEventRates_Mean <- c(mean(BasAUC_stop$AUC_Val, na.rm=T), mean(LRAUC_stop$AUC_Val, na.rm=T),
                      mean(AdvAUC_stop$AUC_Val, na.rm=T), mean(STAUC_stop$AUC_Val, na.rm=T))
vEventRates_stErr <- c(sd(BasAUC_stop$AUC_Val, na.rm=T) / sqrt(BasAUC_stop[, .N]),
                       sd(LRAUC_stop$AUC_Val, na.rm=T) / sqrt(LRAUC_stop[, .N]),
                       sd(AdvAUC_stop$AUC_Val, na.rm=T) / sqrt(AdvAUC_stop[, .N]),
                       sd(STAUC_stop$AUC_Val, na.rm=T) / sqrt(STAUC_stop[, .N]))
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

# --- 4.1 Prepare data for plottiong
# -- 4.1.1 Plotting data
# - Aggregate data at default start
datPlot <- datCredit[TimeInDefSpell==1,
                     .(Empirical=mean(DefSpell_Event==1, na.rm=T),
                       Basic_DtH=mean(WOff_Prob_bas, na.rm=T),
                       Advanced_DtH=mean(WOff_Prob_adv, na.rm=T),
                       Classic=mean(WOff_Prob_classic, na.rm=T),
                       SurvTree=mean(WOff_Prob_survtree, na.rm=T)),
                     by=list(Date)]

# - Pivot data to facilitate grapging
datPlot <- datPlot %>% pivot_longer(cols=c("Empirical", "Basic_DtH", "Advanced_DtH", "Classic", "SurvTree"),
                                    values_to="WOff_Rate", names_to="Model") %>% data.table() %>% setorder(Date)


# -- 4.1.2 Annotation data
# - Location of annotations
start_y <- 0.4; space <- 0.035
y_vals <- c(start_y,start_y-space,start_y-space*2,start_y-space*3)

# - Creating an annotation dataset for easier annotations
datAnno <- data.table(MeanAUC=NULL, Model=c("A-B","A-C", "A-D", "A-E"),
                      x=rep(as.Date("2016-06-30"),4), # Text x coordinates
                      y=y_vals)

# - Estimate MAEs
vEventRates_MAE <- c(mean(abs(datPlot[Model=="Empirical", WOff_Rate]-datPlot[Model=="Basic_DtH", WOff_Rate]), na.rm=T),
                     mean(abs(datPlot[Model=="Empirical", WOff_Rate]-datPlot[Model=="Advanced_DtH", WOff_Rate]), na.rm=T),
                     mean(abs(datPlot[Model=="Empirical", WOff_Rate]-datPlot[Model=="Classic", WOff_Rate]), na.rm=T),
                     mean(abs(datPlot[Model=="Empirical", WOff_Rate]-datPlot[Model=="SurvTree", WOff_Rate]), na.rm=T))

# - Create and assign labels
series_labels_mae <- c("B[t]", "C[t]", "D[t]", "E[t]")
vLabel_MAE <- sapply(seq_along(series_labels_mae), function(i) {
  paste0("'MAE between '*italic(", series_labels_mae[i], ")*' and '*italic(A[t])*' : ",
         sprintf('%.2f', vEventRates_MAE[i] * 100), "%'")
})
datAnno[, Label:=vLabel_MAE]


# --- 4.2 Create plot
# - Refactor levels
datPlot[,Model:=case_when(Model=="Empirical"~"a_Empirical", Model=="Basic_DtH"~"b_Basic_DtH",
                          Model=="Advanced_DtH"~"c_Advanced_DtH", Model=="Classic"~"d_Classic",
                          Model=="SurvTree"~"e_SurvTree")]

# - Graphing parameters
chosenFont <- "Cambria"; dpi <- 180
vCol <- brewer.pal(8, "Dark2")[c(2,1,3,5,7)]
vLabel <- c("a_Empirical"=bquote(italic(A[t])~": Empirical"), "b_Basic_DtH"=bquote(italic(B[t])~": DtH-Basic"),
            "c_Advanced_DtH"=bquote(italic(C[t])~": DtH-Advanced"), "d_Classic"=bquote(italic(E[t])~": Logistic Regression"),
            "e_SurvTree"=bquote(italic(E[t])~": ST"))
vShape <- c(17,20,4,7,12) 

# - Plot
### MM: Horrendous results, am I using the correct probabilities?
### MM: Also need to create such a loss-severity by default cohort graph
(g2_ResolRate <- ggplot(data=datPlot[Model!="e_SurvTree"], aes(x=Date, y=WOff_Rate)) + theme_minimal() +
    labs(y=bquote("Write-off resolution rate (%) "), x=bquote("Default spell cohorts (mmmccyy): entry time "*italic(t[e]))) + 
    theme(text=element_text(family=chosenFont), legend.position="bottom",
          axis.text.x=element_text(angle=90), 
          strip.background=element_rect(fill="snow2", colour="snow2"),
          strip.text=element_text(size=11, colour="gray50"), strip.text.y.right=element_text(angle=90)) + 
    # Main graph
    geom_line(aes(colour=Model, linetype=Model)) +
    geom_point(aes(colour=Model, shape=Model)) +
    # Annotations
    geom_text(data=datAnno[Model!="A-E"], aes(x=x, y=y, label=Label), parse=T) +
    # Facets & scale options
    scale_colour_manual(name="Model", values=vCol, labels=vLabel, guide=guide_legend(nrow=2, byrow=T)) +
    scale_fill_manual(name="Model", values=vCol, labels=vLabel, guide=guide_legend(nrow=2, byrow=T)) +
    scale_shape_manual(name="Model", values=vShape, labels=vLabel, guide=guide_legend(nrow=2, byrow=T)) +
    scale_linetype_discrete(name="Model", labels=vLabel, guide=guide_legend(nrow=2, byrow=T)) +
    scale_x_date(date_breaks="12 month", date_labels="%b %Y") +
    scale_y_continuous(breaks=pretty_breaks(), labels=percent))
### MM: - This view aligns with the resolution rate that we have in the ROT-aritcle, i.e., we look at the
###       probability of write-off over the entire spell.
###     - We get a horrendous view here since all models, except the classical, produce a probability for a single month
###       in default only.

# - Save graph
dpi <-180
ggsave(g2_ResolRate, file=paste0(genFigPath, "Deprecated/WriteOff_ResolRate_DefaultCohort_StartTime_FullProbs.png"),
       width=1600/dpi, height=1000/dpi, dpi=dpi, bg="white")




# ============ 5. Housekeeping ============

cleanEnviron()



