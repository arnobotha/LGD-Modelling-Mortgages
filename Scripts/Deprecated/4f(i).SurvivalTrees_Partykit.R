# ================================= SURVIVAL TREES ======================================
# Fitting and testing surival trees in predicting the occurence of write-off in defaulted
# loans
# ---------------------------------------------------------------------------------------
# PROJECT TITLE: Loss Modelling (LGD) for Residential Mortgages
# SCRIPT AUTHOR(S): Marcel Muller (MM)
#
# DESCRIPTION:
# This script performs fits survival trees and has the following logic
#   1) 
# ---------------------------------------------------------------------------------------
# -- Script dependencies:
#   - 0.Setup.R
#   - 2f.Data_Fusion1.R

# -- Inputs:
#   - datCredit_train_CDH | Prepared from script 2g
#   - datCredit_valid_CDH | Prepared from script 2g
#
# -- Outputs:
#   - ... | ...
# ---------------------------------------------------------------------------------------
# =======================================================================================


# ------ 1. Preliminaries

# --- 1.1 Load data
# - Confirm prepared datasets are loaded into memory
# Training set
if (!exists('datCredit_train_CDH')) unpack.ffdf(paste0(genPath,"creditdata_train_CDH"), tempPath)
# Validation set
if (!exists('datCredit_valid_CDH')) unpack.ffdf(paste0(genPath,"creditdata_valid_CDH"), tempPath)

# - Create copies of the training and validation data
datCredit_train <- copy(datCredit_train_CDH); datCredit_valid <- copy(datCredit_valid_CDH)
rm(datCredit_train_CDH, datCredit_valid_CDH); gc()


# --- 1.2 Create a super-subsample to facilitate rapid model development
# - Set sub-sample size (% of full training set)
smp_frac <- 0.25

# - Get unique account numbers (only select accounts that had a default event)
datKeys_train <- data.table(unique(datCredit_train[!is.na(DefSpell_Key),LoanID]))
datKeys_valid <- data.table(unique(datCredit_valid[!is.na(DefSpell_Key),LoanID]))

# - Subsample unique keys
datKeys_train_smp <- datKeys_train %>% slice_sample(prop=smp_frac) %>% as.data.table()
datKeys_valid_smp <- datKeys_valid %>% slice_sample(prop=smp_frac) %>% as.data.table()

# - Subset training data
datCredit_train_smp <- subset(datCredit_train, LoanID %in% datKeys_train_smp$V1)
datCredit_valid_smp <- subset(datCredit_valid, LoanID %in% datKeys_valid_smp$V1)

# - Subset only default spells
datCredit_train_smp <- subset(datCredit_train_smp, !is.na(DefSpell_Key))
datCredit_valid_smp <- subset(datCredit_valid_smp, !is.na(DefSpell_Key))


# --- 1.3 Final data preparations
# - Add an event indicator
datCredit_train_smp[, DefSpell_Event:=ifelse(DefSpellResol_Type_Hist=="WOFF",1,0)]
datCredit_valid_smp[, DefSpell_Event:=ifelse(DefSpellResol_Type_Hist=="WOFF",1,0)]

# - Transform categorical variables to factor
# Payment method
datCredit_train_smp[,pmnt_method_grp_fac:=as.factor(pmnt_method_grp)]
datCredit_valid_smp[,pmnt_method_grp_fac:=as.factor(pmnt_method_grp)]
# Previous defaults
datCredit_train_smp[,PrevDefaults_fac:=as.factor(PrevDefaults)]
datCredit_valid_smp[,PrevDefaults_fac:=as.factor(PrevDefaults)]

# - Create a cross-sectional dataset
datCredit_train_smp_cross <- subset(datCredit_train_smp, DefSpell_Counter==1)
datCredit_valid_smp_cross <- subset(datCredit_valid_smp, DefSpell_Counter==1)




# ------ 2. Fitting trees using partykit

# - Load package
require(partykit)

# - Fit tree
start_time <- proc.time()
SurvTree_PartyKit <- ctree(Surv(DefSpell_Age,DefSpell_Event)~
                           PrevDefaults_fac+InterestRate_Nom+BalanceToPrincipal_1+
                           pmnt_method_grp_fac+M_Repo_Rate_6+M_DTI_Growth_6,
                           data=datCredit_train_smp_cross,
                           control=ctree_control(mincriterion=0.99, # 1 - p-value threshold (default ≈ 5% significance)
                                                 minsplit=100, # minimum number of observations to attempt a split
                                                 minbucket=50, # minimum number in terminal node (common default)
                                                 testtype = "Bonferroni", # most conservative → classical feeling
                                                 maxdepth=2))
proc.time() - start_time
### Runtime = 1 sec

# - Visualise tree
plot(SurvTree_PartyKit)




# ------ 3. Comparing hazards from the survival tree "manually" and using built in packages

# --- 3.1 Generate survival quantity predictions given the fitted tree | Manual method
# - Point to main credit data
datCredit <- rbind(data.table(datCredit_train_smp, Sample="Training"),
                   data.table(datCredit_valid_smp, Sample="Validation"))

# - Handle left-truncated spells by adding a starting record 
### NOTE:  This is necessary for calculating certain survival quantities later
# Create an additional record for each default spell
datAdd <- subset(datCredit, Counter==1 & TimeInDefSpell>1)
datAdd[, TimeInDefSpell:=TimeInDefSpell-1]
datAdd[, Counter:=0]
# Add record to main dataset
datCredit <- rbind(datCredit, datAdd); setorder(datCredit, DefSpell_Key, TimeInDefSpell)

# - Get node from tree
datCredit[,Node:=predict(SurvTree_PartyKit, datCredit, type="node")]
datCredit_train_smp_cross[,Node:=predict(SurvTree_PartyKit, datCredit_train_smp_cross, type="node")]

# - Fit a survival object for each node | Using the training data only to align with how the tree was fitted
km_partykit <- survfit(Surv(DefSpell_Age, DefSpell_Event) ~ strata(Node),
                       data=datCredit_train_smp_cross)

# - Create a combined longitudinal survival dataset (grain = survival probability per time per node)
datSurv <- data.table(Time=km_partykit$time,
                      Surv=km_partykit$surv,
                      Node=rep(names(km_partykit$strata), km_partykit$strata)
)
datSurv[, Node:=as.integer(sub(".*=", "", Node))]

# - Create observations for the gaps in the KM objects
max_time <- max(datSurv$Time)
grid <- CJ(Node=unique(datSurv$Node), Time=1:max_time)
datSurv_full <- merge(grid,datSurv, by=c("Node", "Time"), all.x=T)
datSurv_full[, Surv:=na.locf(Surv, na.rm=FALSE), by=Node]

# - Estimate hazards
datHaz <- datSurv_full[,Hazard:=(data.table::shift(Surv,fill=1)-Surv)/data.table::shift(Surv,fill=1), by=list(Node)]

# - Impose a floor & ceiling for non-logical values
datHaz[is.infinite(Hazard) | Hazard<0, Hazard:=0]

# - Join hazards back to main dataset
datCredit <- merge(datCredit,
                   datHaz,
                   by.x=c("Node","TimeInDefSpell"), by.y=c("Node","Time"),
                   all.x=T)

datCredit[is.na(Hazard), Hazard:=0]

# - [SANITY CHECK] Inspect KM-estimated survival probabilities
n_nodes <- length(unique(km_partykit$strata))
plot(km_partykit, col=1:n_nodes, lty=1:n_nodes, xlab="Time", ylab="Survival Probability")
legend("topright", legend=unique(datSurv$Node), col=1:n_nodes, lty=1:n_nodes)
### RESUTLS: Estimators look reasonable and risk differentiation is present (though the lines seem very close in some instances)

### CONCLUSION: Hazard separation may become distorted in more complex trees as the survival probabilities start to converge
###             Consider testing for proportional hazards to ensure that the tree is indeed valid

# - [SANITY CHECK] Check hazard rate for a specific node
# plot(datHaz[Node==3 & !is.nan(Hazard), Time], datHaz[Node==3 & !is.nan(Hazard), Hazard], type="b")
### RESULTS: Individual hazard rate seems reasonable

# - [SANITY CHECK] Inspect hazards
ggplot(datHaz[Time<=120], aes(x=Time, y=Hazard, group=Node)) + geom_line(aes(col=Node))
### RESULTS: Hazards are crossing in a few instances, but their seems to be good differentiation

# - [SANITY CHECK] Inspect reasonableness of hazards
sum(is.nan(datCredit$Hazard))/datCredit[,.N]*100
### RESULTS: 0%
sum(is.na(datCredit$Hazard))/datCredit[,.N]*100
### RESULTS: 0


# --- 3.2 Generate survival quantity predictions given the fitted tree | Using package-functions
# - Predict survival curves (list of survfit objects): equal to number of rows in sample
survFits <- predict(SurvTree_PartyKit , newdata=datCredit_train_smp_cross, type="prob")
survFits_node <- predict(SurvTree_PartyKit , newdata=datCredit_train_smp_cross, type="node")
survFits_map <- data.table(survFits=survFits, Node=survFits_node)

# - Function for extracting survival probabilities within a mapply-setup (later)
extractSurv <- function(survFit, t, extrapolate="last", floor=NULL) {
  # Unit test conditions | Internal
  # survFit<-survFits[[1]]; t<-178; extrapolate<-"last"
  
  # Edge case: No events in terminal node → everyone survives
  if (length(survFit$time)==0) return(1)
  
  # Retrieve survival probability at given t
  survFit_t <- summary(survFit, times=t)$surv
  
  # Most common & recommended logic gate; simply return retrieved result
  if (length(survFit_t)>0) {
    if (!is.na(survFit_t)) return(survFit_t)
  }
  
  # Remaining cases need treatment. Extrapolation is now necessary  since t > last observed time in survFit object
  last_surv <- tail(survFit$surv, 1) # obtain last survival probability
  
  if (extrapolate=="last") {
    result <- last_surv
  } else if (extrapolate=="zero") {
    result <- 0
  } else if (extrapolate == "floor" && !is.null(floor)) {
    result <- last_surv
  } else result <- last_surv  # fallback
  
  # Optional safety floor (common in credit scoring)
  if (!is.null(floor) && is.numeric(floor)) result <- max(result, floor)
  
  return(result)

}

# - Obtain survival probabilities by feeding the list of KM-objects (survFits) together with [TimeInDefSpell]-vector
# into the extractSurv()-function across all rows
start_time <- proc.time()
datCredit[, SurvProb:=mapply(function(p, times) {
  extractSurv(p, times)}, p=survFits, times=TimeInDefSpell)]
proc.time() - start_time # 6.62 seconds.

# - Estimate hazard rates
datCredit[, Hazard2:=(data.table::shift(SurvProb,fill=1)-SurvProb)/
            data.table::shift(SurvProb,fill=1), by=list(DefSpell_Key)]
datCredit[is.infinite(Hazard2) | Hazard2<0, Hazard2:=0] # fail-safe

# - [SANITY CHECK] Inspect KM-estimated survival probabilities
survFits_map_uni <- survFits_map[!duplicated(Node)]
dat_survFits_uni <- rbindlist(lapply(1:survFits_map_uni[,.N],
                                     function(i){surv_i <- survFits_map_uni[i,1][[1]]
                                                 data.table(Time=surv_i[[1]]$time,
                                                            Surv=surv_i[[1]]$surv,
                                                            Node=survFits_map_uni[i,2][[1]])})
                              )
n_nodes <- unique(dat_survFits_uni$Node)
ggplot(dat_survFits_uni, aes(x=Time, y=Surv, group=Node)) + geom_line(aes(colour=Node))
### RESUTLS: Estimates look reasonable and risk differentiation is present (though the lines seem very close in some instances)

  
# --- 3.3 Compare approaches
# - Survival probabilities | Portfolio-level
# All nodes
ggplot(dat_survFits_uni, aes(x=Time, y=Surv, group=Node)) + geom_line(aes(colour=Node))
ggplot(datHaz, aes(x=Time, y=Surv, group=Node)) + geom_line(aes(colour=Node))
### RESULTS: Graphs appear identical
# A singular node
Node_Select <- 7
plot(dat_survFits_uni[Node==Node_Select & Time<=120, Time], dat_survFits_uni[Node==Node_Select & Time<=120, Surv], type="l", col="red")
lines(datHaz[Node==Node_Select & Time<=120, Time], datHaz[Node==Node_Select & Time<=120, Surv], type="l", col="blue")
### RESULTS: Graphs appear identical

# - Hazards | Account-level
diff <- datCredit$Hazard-datCredit$Hazard2
mean(diff)
### RESULTS: -0.04524568
sum(diff>0.001); sum(diff>0.001)/length(diff)
### RESULTS:  243 412 observations (~40%) where difference between hazards are large than 0.1

### CONCLUSION: Hazards rate are not the same, this likely stems from the following:
###              - The survival rates per node are identical, so the aggregate KM objects are fine
###              - The hazard rate in approach 1 is calculated within the aggregated dataset before joining it to the credit data
###              - The hazard rate in approach 2 is calculate within the credit data after the KM data has been added to it
###              - The calculation of the hazard outside/inside the data may be the cause of the differences




# ------ 4. Evaluate tree accuracy

# --- 4.1 ROC analysis
# - Subset validation dataset for only the first observation (to align with how the model was trained)

# - Perform ROC analysis | Cross-sectional data
objROC_ST <- roc(response=datCredit[Sample=="Validation" & DefSpell_Counter==1,DefSpell_Event],
                 predictor=datCredit[Sample=="Validation" & DefSpell_Counter==1,Hazard2])
objROC_ST$auc; plot(objROC_ST)
### RESULTS: AUC=50% (maximum node depth=2 & 100% training sample) (Hazard from approach 1)
### RESULTS: AUC=52.57% (maximum node depth=2 & 100% training sample) (Hazard from approach 2)

# - Perform time-dependent ROC analysis
predictTime <- 44
ptm <- proc.time() #IGNORE: for computation time calculation
objROC44_ST <- tROC.multi(datGiven=datCredit, modGiven=NA, month_End=predictTime, sLambda=0.05, estMethod="NN-0/1", numDigits=4, 
                          fld_ID="DefSpell_Key", fld_Event="WOff_Ind", eventVal=1, fld_StartTime="Start", fld_EndTime="TimeInDefSpell",
                          caseStudyName=paste0("Cond_SurvTree_", predictTime), numThreads=12, logPath=genPath, 
                          predType="response", MarkerGiven="Hazard2", Graph=F)
proc.time() - ptm
objROC44_ST$AUC #; objROC44_CDH_CoxDisc_bas_B$ROC_graph
### RESULTS: AUC up to t: 79.83602% %, achieved in 176 secs (Hazard from approach 1)
### RESULTS: AUC up to t: 79.83602% %, achieved in 176 secs (Hazard from approach 2)

### CONCLUSION: 


# --- 4.2 Constructing the empirical term-structure of write-off using a Kaplan-Meier estimator
# - Compute Kaplan-Meier survival estimates (product-limit) for main-event | Spell-level with right-censoring & left-truncation
km_Default <- survfit(Surv(time=TimeInDefSpell-1, time2=TimeInDefSpell, event=WOff_Ind==1, type="counting") ~ 1, 
                      id=DefSpell_Key, data=datCredit)

# - Create survival table
(datSurv_act <- surv_summary(km_Default))

# - Enrich data
datSurv_act <- datSurv_act %>%
  # Rename some fields
  rename(Time=time, AtRisk_n=`n.risk`, Event_n=`n.event`, Censored_n=`n.censor`, SurvivalProb_KM=`surv`) %>%
  # Calculate hazard rate
  mutate(Hazard_Actual=Event_n/AtRisk_n) %>% 
  # Calculate cumulative hazard rate
  mutate(CHaz=cumsum(Hazard_Actual)) %>%
  # Probability mass function f(t)=h(t)*S(t-1)
  mutate(EventRate=Hazard_Actual*data.table::shift(SurvivalProb_KM, n=1, fill=1)) %>%
  # Filter for observations with events and censoring
  filter(Event_n > 0 | Censored_n >0) %>% as.data.table()

# - Order dataset
setorder(datSurv_act, Time)

# - Calculate the at-risk population proportion
datSurv_act[,AtRisk_perc:=AtRisk_n/max(AtRisk_n, na.rm=T)]


# --- 4.3 Estimate event rates | Expected
# - Estimate event rate
datCredit[, EventRate:=data.table::shift(Surv, type="lag", n=1, fill=1)-Surv, by=list(DefSpell_Key)]

# - Remove added rows
datCredit<- subset(datCredit, Counter > 0)

# - Aggregate account-specific event rates
datSurv_exp <- datCredit[,.(EventRate=mean(EventRate, na.rm=T)),by=list(TimeInDefSpell)]


# --- 4.4 Compare event rates
plot(x=datSurv_exp[TimeInDefSpell<=120, TimeInDefSpell], y=datSurv_exp[TimeInDefSpell<=120, EventRate], type="b", col="green")
lines(datSurv_act[Time<=120, EventRate], type="b", col="red")



