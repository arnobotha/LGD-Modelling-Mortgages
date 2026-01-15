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
smp_frac <- 0.25 ### MM: Increase size

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
### MM: Impose a limit of default spell age of 240


# --- 1.4 Additional parameters
# - Maximum tree-depth
max_depth <- 3




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
                                                 testtype="Bonferroni", # most conservative → classical feeling
                                                 maxdepth=max_depth))
### MM: Try and use the advanced model's input space
proc.time() - start_time
### Runtime = 1 sec

# - Visualise tree
plot(SurvTree_PartyKit)




# ------ 3. Comparing hazards from the survival tree "manually" and using built in packages

# --- 3.1 Generate survival quantity predictions given the fitted tree | Manual method
# - Point to main credit data
datCredit <- rbind(data.table(datCredit_train_smp, Sample="Training"),
                   data.table(datCredit_valid_smp, Sample="Validation"))

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

# - Data corrections
# Take last known survival probabilities for missing periods (where no events occured)
datSurv_full[, Surv:=na.locf(Surv, na.rm=FALSE), by=Node]
# Replace all NA values with 1 since these constitute the start times of nodes with no events
datSurv_full[is.na(Surv),Surv:=1]

# - Estimate hazards
datHaz <- datSurv_full[,Hazard:=(data.table::shift(Surv,fill=1)-Surv)/data.table::shift(Surv,fill=1), by=list(Node)]

# - Impose a floor & ceiling for non-logical values
datHaz[is.infinite(Hazard) | Hazard<0, Hazard:=0]

# - Estimate event rate
datHaz[,EventRate:=data.table::shift(Surv, n=1, type="lag")-Surv, by=list(Node)]
datHaz[is.na(EventRate),EventRate:=0]

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
# - Render predictions
# Predict survival curves (list of survfit objects): equal to number of rows in sample
survFits <- predict(SurvTree_PartyKit , newdata=datCredit_train_smp_cross, type="prob")
# Predict nodes
survFits_node <- predict(SurvTree_PartyKit , newdata=datCredit_train_smp_cross, type="node")
# Combine predictions in a convenient data.table
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
# Filter data.table to have one the unique survival curves
survFits_map_uni <- survFits_map[!duplicated(Node)]
dat_survFits_uni <- rbindlist(lapply(1:survFits_map_uni[,.N],
                                     function(i){surv_i <- survFits_map_uni[i,1][[1]]
                                     data.table(Time=surv_i[[1]]$time,
                                                Surv=surv_i[[1]]$surv,
                                                Node=survFits_map_uni[i,2][[1]])})
)
# Get total number of nodes
(n_nodes <- unique(dat_survFits_uni$Node))
# Plot survival estimates
ggplot(dat_survFits_uni, aes(x=Time, y=Surv, group=Node)) + geom_line(aes(colour=Node))
### RESUTLS: Estimates look reasonable and risk differentiation is present (though the lines seem very close in some instances)


# --- 3.3 Generate survival quantity predictions given the fitted tree | Using package-functions - Alternative
# - Enrich the survival curves to carry over the last known survival probability for each missing time
max_time <- max(dat_survFits_uni$Time)
grid <- CJ(Node=unique(dat_survFits_uni$Node), Time=1:max_time)
datSurv_full_alt <- merge(grid, dat_survFits_uni, by=c("Node", "Time"), all.x=T)
datSurv_full_alt[, Surv:=na.locf(Surv, na.rm=FALSE), by=Node]

# - Data corrections
# Take last known survival probabilities for missing periods (where no events occurred)
datSurv_full_alt[, Surv:=na.locf(Surv, na.rm=FALSE), by=Node]
# Replace all NA values with 1 since these constitute the start times of nodes with no events
datSurv_full_alt[is.na(Surv), Surv:=1]

# - Estimate hazards
datHaz_alt <- datSurv_full_alt[,Hazard3:=(data.table::shift(Surv,fill=1)-Surv)/data.table::shift(Surv,fill=1), by=list(Node)]
datHaz_alt[,`:=`(Surv3=Surv, Surv=NULL)]

# - Impose a floor & ceiling for non-logical values
datHaz_alt[is.infinite(Hazard3) | Hazard3<0, Hazard3:=0]

# - Join hazards back to main dataset
datCredit <- merge(datCredit,
                   datHaz_alt,
                   by.x=c("Node","TimeInDefSpell"), by.y=c("Node","Time"),
                   all.x=T)

datCredit[is.na(Hazard3), Hazard3:=0]
 
# - [SANITY CHECK] Inspect hazards
ggplot(datHaz_alt[Time<=120], aes(x=Time, y=Hazard3, group=Node)) + geom_line(aes(col=Node))
### RESULTS: Hazards are crossing in a few instances, but their seems to be good differentiation

# - [SANITY CHECK] Inspect reasonableness of hazards
sum(is.nan(datCredit$Hazard3))/datCredit[,.N]*100
### RESULTS: 0%
sum(is.na(datCredit$Hazard3))/datCredit[,.N]*100
### RESULTS: 0


# --- 3.4 Compare approaches
# - Survival probabilities | Portfolio-level
# All nodes
ggplot(dat_survFits_uni, aes(x=Time, y=Surv, group=Node)) + geom_line(aes(colour=Node))
ggplot(datHaz, aes(x=Time, y=Surv, group=Node)) + geom_line(aes(colour=Node))
ggplot(datHaz_alt, aes(x=Time, y=Surv3, group=Node)) + geom_line(aes(colour=Node))
### RESULTS: Graphs appear identical
# A singular node
(Node_Select <- unique(dat_survFits_uni$Node)[[1]])
plot(dat_survFits_uni[Node==Node_Select & Time<=120, Time], dat_survFits_uni[Node==Node_Select & Time<=120, Surv], type="l", col="red")
lines(datHaz[Node==Node_Select & Time<=120, Time], datHaz[Node==Node_Select & Time<=120, Surv], type="l", col="blue")
lines(datHaz_alt[Node==Node_Select & Time<=120, Time], datHaz_alt[Node==Node_Select & Time<=120, Surv3], type="l", col="green")
### RESULTS: Graphs appear identical

# - Hazards | Account-level
# Approach 1 vs 2
diff <- datCredit$Hazard-datCredit$Hazard2
mean(diff, na.rm=T)
### RESULTS: -0.04313153 (maximum tree-depth=1)
### RESULTS: -0.04415482 (maximum tree-depth=2)
### RESULTS: -0.04493432 (maximum tree-depth=3)
### RESULTS: -0.05041631 (maximum tree-depth=4)
### RESULTS: -0.05319919 (maximum tree-depth=5)
### RESULTS: -0.05232045 (maximum tree-depth=10)
sum(diff>0.001); sum(diff>0.001)/length(diff)
### RESULTS:  240 798 observations (~40%) where difference between hazards are large than 0.001
# Approach 1 vs 3
diff <- datCredit$Hazard-datCredit$Hazard3
mean(diff, na.rm=T)
### RESULTS: 0 (maximum tree-depth=2)
### RESULTS: 0 (maximum tree-depth=5)
sum(diff>0.001); sum(diff>0.001)/length(diff)
### RESULTS:  0 observations (0%) where difference between hazards are large than 0.001 (maximum tree-depth=2)
### RESULTS:  0 observations (0%) where difference between hazards are large than 0.001 (maximum tree-depth=5)
# Approach 2 vs 3
diff <- datCredit$Hazard2-datCredit$Hazard3
mean(diff, na.rm=T)
### RESULTS: 0.04313153 (maximum tree-depth=1)
### RESULTS: 0.04415482 (maximum tree-depth=2)
### RESULTS: 0.04493432 (maximum tree-depth=3)
### RESULTS: 0.05041631 (maximum tree-depth=4)
### RESULTS: 0.05319919 (maximum tree-depth=5)
### RESULTS: 0.05232045 (maximum tree-depth=10)
sum(diff>0.001); sum(diff>0.001)/length(diff)
### RESULTS:  234 745 observations (~40%) where difference between hazards are large than 0.001

### CONCLUSION: 1) Hazard rates are not the same between approaches 1 & 2 and 2 & 3, this likely stems from the following:
###               - The survival rates per node are identical, so the aggregate KM objects are fine
###               - The hazard rate in approach 1 is calculated within the aggregated dataset before joining it to the credit data
###               - The hazard rate in approach 2 is calculate within the credit data after the KM data has been added to it
###               - The calculation of the hazard outside/inside the data may be the cause of the differences
###             2) Hazard rates are the same between approaches 1 & 3




# ------ 4. Evaluate tree accuracy

# --- 4.1 ROC analysis
# - Perform time-dependent ROC analysis | Approach 1
predictTime <- 44
ptm <- proc.time() #IGNORE: for computation time calculation
objROC44_ST <- tROC.multi(datGiven=datCredit, modGiven=NA, month_End=predictTime, sLambda=0.05, estMethod="NN-0/1", numDigits=4, 
                          fld_ID="DefSpell_Key", fld_Event="WOff_Ind", eventVal=1, fld_StartTime="Start", fld_EndTime="TimeInDefSpell",
                          caseStudyName=paste0("Cond_SurvTree_", predictTime), numThreads=12, logPath=genPath, 
                          predType="response", MarkerGiven="Hazard", Graph=F)
proc.time() - ptm
objROC44_ST$AUC #; objROC44_ST$ROC_graph

# -- Sampling fraction=0.25; No filtering on DefaultSpell_Age; Curated input space used:
### RESULTS: AUC up to t: 85.25415%, achieved in 176 secs (maximum node depth=1 & Hazard from approach 1)
### RESULTS: AUC up to t: 81.77467%, achieved in 176 secs (maximum node depth=2 & Hazard from approach 1)
### RESULTS: AUC up to t: 73.70919%, achieved in 176 secs (maximum node depth=3 & Hazard from approach 1)
### RESULTS: AUC up to t: %, achieved in 176 secs (maximum node depth=4 & Hazard from approach 1)
### RESULTS: AUC up to t: %, achieved in 176 secs (maximum node depth=5 & Hazard from approach 1)

# - Perform time-dependent ROC analysis | Approach 2
predictTime <- 44
ptm <- proc.time() #IGNORE: for computation time calculation
objROC44_ST <- tROC.multi(datGiven=datCredit, modGiven=NA, month_End=predictTime, sLambda=0.05, estMethod="NN-0/1", numDigits=4, 
                          fld_ID="DefSpell_Key", fld_Event="WOff_Ind", eventVal=1, fld_StartTime="Start", fld_EndTime="TimeInDefSpell",
                          caseStudyName=paste0("Cond_SurvTree_", predictTime), numThreads=12, logPath=genPath, 
                          predType="response", MarkerGiven="Hazard2", Graph=F)
proc.time() - ptm
objROC44_ST$AUC #; objROC44_ST$ROC_graph
### RESULTS: AUC up to t: 85.25415%, achieved in 176 secs (maximum node depth=1 & Hazard from approach 1)
### RESULTS: AUC up to t: 81.77467%, achieved in 176 secs (maximum node depth=2 & Hazard from approach 2)
### RESULTS: AUC up to t: 73.70919%, achieved in 176 secs (maximum node depth=3 & Hazard from approach 2)
### RESULTS: AUC up to t: 73.73953%, achieved in 176 secs (maximum node depth=4 & Hazard from approach 2)
### RESULTS: AUC up to t: 70.98365%, achieved in 176 secs (maximum node depth=5 & Hazard from approach 2)

### CONCLUSION: 


# --- 4.2 Constructing the empirical term-structure of write-off using a Kaplan-Meier estimator
# - Compute Kaplan-Meier survival estimates (product-limit) for main-event | Spell-level with right-censoring & left-truncation
km_Default <- survfit(Surv(time=TimeInDefSpell-1, time2=TimeInDefSpell, event=WOff_Ind==1, type="counting") ~ 1, 
                      id=DefSpell_Key, data=datCredit)

# - Create survival table
datSurv_act <- surv_summary(km_Default)

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


# --- 4.3 Estimate event rates | Expected - Approach 1
# - Handle left-truncated spells by adding a starting record 
### NOTE:  This is necessary for calculating certain survival quantities later
# Create an additional record for each default spell
datAdd <- subset(datCredit, Counter==1 & TimeInDefSpell>1)
datAdd[, TimeInDefSpell:=TimeInDefSpell-1]
datAdd[, Counter:=0]
# Add record to main dataset
datCredit <- rbind(datCredit, datAdd); setorder(datCredit, DefSpell_Key, TimeInDefSpell)

# - Estimate event rate
datCredit[, EventRate:=data.table::shift(Surv, type="lag", n=1, fill=1)-Surv, by=list(DefSpell_Key)]

# - Remove added rows
datCredit<- subset(datCredit, Counter > 0)

# - Aggregate account-specific event rates
datSurv_exp <- datCredit[,.(EventRate=mean(EventRate, na.rm=T)),by=list(TimeInDefSpell)]
datSurv_exp <- datSurv_exp[order(TimeInDefSpell)]


# --- 4.4 Estimate event rates | Expected - Approach 
# - Handle left-truncated spells by adding a starting record 
### NOTE:  This is necessary for calculating certain survival quantities later
# Create an additional record for each default spell
datAdd <- subset(datCredit, Counter==1 & TimeInDefSpell>1)
datAdd[, TimeInDefSpell:=TimeInDefSpell-1]
datAdd[, Counter:=0]
# Add record to main dataset
datCredit <- rbind(datCredit, datAdd); setorder(datCredit, DefSpell_Key, TimeInDefSpell)

# - Estimate event rate
datCredit[, EventRate2:=data.table::shift(SurvProb, type="lag", n=1, fill=1)-SurvProb, by=list(DefSpell_Key)]

# - Remove added rows
datCredit<- subset(datCredit, Counter > 0)

# - Aggregate account-specific event rates
datSurv_exp2 <- datCredit[,.(EventRate2=mean(EventRate2, na.rm=T)),by=list(TimeInDefSpell)]
datSurv_exp2 <- datSurv_exp2[order(TimeInDefSpell)]


# --- 4.5 Compare event rates
# - Approach 1
plot(x=datSurv_exp[TimeInDefSpell<=120, TimeInDefSpell], y=datSurv_exp[TimeInDefSpell<=120, EventRate], type="l", col="green")
lines(datSurv_act[Time<=120, EventRate], type="l", col="red")
mean(abs(datSurv_exp[TimeInDefSpell<=120,EventRate] - datSurv_act[Time<=120,EventRate]), na.rm=T)
### RESULTS: MAE=0.002126404 (0.2126404%) (maximum tree-depth=1)
### RESULTS: MAE=0.002484871 (0.3893818%) (maximum tree-depth=2)
### RESULTS: MAE=0.004670193 (0.4670193%) (maximum tree-depth=3)
### RESULTS: MAE=0.004355022 (0.4355022%) (maximum tree-depth=4)
### RESULTS: MAE=0.004415156 (0.4415156%) (maximum tree-depth=5)

# - Approach 2
plot(x=datSurv_exp2[TimeInDefSpell<=120, TimeInDefSpell], y=datSurv_exp2[TimeInDefSpell<=120, EventRate2], type="l", col="green")
lines(datSurv_act[Time<=120, EventRate], type="l", col="red")
mean(abs(datSurv_exp2[TimeInDefSpell<=120,EventRate2] - datSurv_act[Time<=120,EventRate]), na.rm=T)
### RESULTS: MAE=0.01814868 (1.814868%) (maximum tree-depth=1)
### RESULTS: MAE=0.02202688 (2.202688%) (maximum tree-depth=2)
### RESULTS: MAE=0.01873454 (1.873454%) (maximum tree-depth=3)
### RESULTS: MAE=0.02283583 (2.28583%) (maximum tree-depth=4)
### RESULTS: MAE=0.01856957 (1.856957%) (maximum tree-depth=5)


# --- 4.6 Inspect account-specific forecasts
(accnt_i <- unique(datCredit[TimeInDefSpell>60 & Date>"2015-01-30", DefSpell_Key])[[800]])
### DefSpell_Key=3000001353076_1; 3000000550609_1; 3000012184718_1
dat_accnt_i <- datCredit[DefSpell_Key==accnt_i,c("DefSpell_Key","Date","TimeInDefSpell",
                                                     "Node","Surv","SurvProb","EventRate",
                                                     "EventRate2","Hazard","Hazard2")]


