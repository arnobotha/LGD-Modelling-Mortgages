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


# --- 1.3 Final data preperations
# - Add an event indicator
datCredit_train_smp[, DefSpell_Event:=ifelse(DefSpellResol_Type_Hist=="WOFF",1,0)]
datCredit_valid_smp[, DefSpell_Event:=ifelse(DefSpellResol_Type_Hist=="WOFF",1,0)]

# - Transform categorical variables to numeric
# Payment method
datCredit_train_smp[,pmnt_method_grp_num:=as.numeric(pmnt_method_grp)]
datCredit_valid_smp[,pmnt_method_grp_num:=as.numeric(pmnt_method_grp)]
# Previous defaults
datCredit_train_smp[,PrevDefaults_num:=as.numeric(PrevDefaults)]
datCredit_valid_smp[,PrevDefaults_num:=as.numeric(PrevDefaults)]

# - Transform categorical variables to factor
# Payment method
datCredit_train_smp[,pmnt_method_grp_fac:=as.numeric(pmnt_method_grp)]
datCredit_valid_smp[,pmnt_method_grp_fac:=as.numeric(pmnt_method_grp)]
# Previous defaults
datCredit_train_smp[,PrevDefaults_fac:=as.numeric(PrevDefaults)]
datCredit_valid_smp[,PrevDefaults_fac:=as.numeric(PrevDefaults)]

# - Create a cross-sectional dataset
datCredit_train_smp_cross <- subset(datCredit_train_smp, DefSpell_Counter==1)
datCredit_valid_smp_cross <- subset(datCredit_valid_smp, DefSpell_Counter==1)




# ------ 2. Fitting trees using various packages

# --- 2.1 rpart
require(rpart)

# - Fit tree
start_time <- proc.time()
SurvTree_Rpart <- ctree(Surv(DefSpell_Age,DefSpell_Event)~
                             PrevDefaults+InterestRate_Nom+BalanceToPrincipal_1+
                             pmnt_method_grp+M_Repo_Rate_6+M_DTI_Growth_6,
                           data=datCredit_train_smp_cross,
                           control=ctree_control(mincriterion=0.95,
                                                 minsplit=50, minbucket=20, maxdepth=6))
end_time <- proc.time()
(delta_time <- end_time - start_time)


# --- 2.2 Partykit
# - Load package
require(partykit)
require(LongCART)

# - Fit tree
start_time <- proc.time()
SurvTree_PartyKit <- ctree(Surv(DefSpell_Age,DefSpell_Event)~
                           PrevDefaults_fac+InterestRate_Nom+BalanceToPrincipal_1+
                           pmnt_method_grp_fac+M_Repo_Rate_6+M_DTI_Growth_6,
                           data=datCredit_train_smp,
                           control=ctree_control(mincriterion=0.99,
                                                 minsplit=100, minbucket=50, maxdepth=4))
end_time <- proc.time()
(delta_time <- end_time - start_time)
### Runtime = 1 sec

# - Visualise tree
plot(SurvTree_PartyKit)

# - Evaluate prediction accuracy
# Get node from tree
datCredit_valid_smp[,SurvTree_PartyKit_Node:=predict(SurvTree_PartyKit, datCredit_valid_smp, type="node")]
# Fit a survival object for each node
km_partykit <- survfit(Surv(TimeInDefSpell, DefSpell_Event)~SurvTree_PartyKit_Node, data=datCredit_valid_smp)
# Create a survival dataset
surv_df <- data.frame(
  time   = km_partykit$time,
  surv   = km_partykit$surv,
  strata = rep(names(km_partykit$strata), km_partykit$strata)
)
# Estimate hazards
haz_df <- surv_df %>% group_by(strata) %>% arrange(time) %>% mutate(hazard = (lag(surv) - surv) / lag(surv)) %>% data.table()
haz_df$strata_clean <- as.integer(sub("SurvTree_PartyKit_Node=", "", haz_df$strata))
# Join hazards back to main dataset
datCredit_valid_smp <- merge(datCredit_valid_smp, haz_df,
                                   by.x=c("SurvTree_PartyKit_Node","TimeInDefSpell"), by.y=c("strata_clean","time"),
                                   all.x=T)
# Run time
objROC_LR <- roc(response=datCredit_valid_smp_cross[, DefSpell_Event], predictor=datCredit_valid_smp_cross$hazard.y)
objROC_LR$auc; plot(objROC_LR)
### RESULTS: AUC=89.56% (maximum node depth = 3)
### RESULTS: AUC=90.01% (maximum node depth = 4)
### CONCLUSION: AUCs increase with tree complexity, but the total number of events in terminal nodes decrease drastically such that
###             the survival curves are NAs in most cases.


# --- 2.3 uni.survival.tree
# - Load package
require(uni.survival.tree)

# - Create matrix for predictors
X <- model.matrix(~PrevDefaults_fac+InterestRate_Nom+BalanceToPrincipal_1+
                  pmnt_method_grp_fac+M_Repo_Rate_6+M_DTI_Growth_6,
                  data=datCredit_train_smp_cross)[,-1]

# - Fit tree
start_time <- proc.time()
SurvTree_UniSurvivalTree <- uni.tree(t.vec=datCredit_train_smp_cross$DefSpell_Age,
                                     d.vec=datCredit_train_smp_cross$DefSpell_Event,
                                     X.mat=X,
                                     P.value=0.01,
                                     d=0.01)
end_time <- proc.time()
(delta_time <- end_time - start_time)
### Runtime = 38.50

# - Inspect chosen variables
uni.survival.tree::feature.selected(SurvTree_UniSurvivalTree)
### RESULTS: Variables:
###             [PrevDefaultsTrue]; [pmnt_method_grpMISSING_DATA]; [pmnt_method_grpSalary/Suspense];
###             [pmnt_method_grpStatement]

# - Assess prediction power
SurvTree_UniSurvivalTree_Preds <- predict(object=SurvTree_UniSurvivalTree, newdata=datCredit_valid_smp_cross, type="node")


# --- 2.4 LongCART
# - Load package
require(LongCART)

# - Specify continuous and categorical variables
gvars<-c("PrevDefaults_num","InterestRate_Nom","BalanceToPrincipal_1",
         "pmnt_method_grp_num","M_Repo_Rate_6","M_DTI_Growth_6")
tgvars <- c(0,1,0,1,1)

# - Fit tree
start_time <- proc.time()
SurvTree_LongCART <- SurvCART(data=datCredit_train_smp, patid="DefSpell_Key",
                              censorvar="WOff_Ind",
                              timevar="TimeInDefSpell",
                              gvars=gvars,
                              tgvars=tgvars,
                              minsplit=80,
                              minbucket=40,
                              alpha=0.05)
end_time <- proc.time()
(delta_time <- end_time - start_time)
### Runtime = 501 sec

# - Visualise tree
par(xpd=T); plot(SurvTree_LongCART, compress=T); text(SurvTree_LongCART, use.n=T)

# - Visualise KM estimates for each node
KMPlot(x=SurvTree_LongCART, type=1)

# - Assess prediction power
# Get nodes for each prediction
datCredit_valid_smp[, SurvTree_Long_Node:=predict(SurvTree_LongCART, datCredit_valid_smp, response="node")[,"node"]]
# Estimate KM-estimators for each node
SurvTree_LongCART_predict

