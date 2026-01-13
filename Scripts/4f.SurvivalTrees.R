# ================================= SURVIVAL TREES ======================================
# Fitting and testing survival trees in predicting the occurrence of write-off in defaulted
# loans at each time point during a default spell
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
smp_frac <- 1

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
datCredit_train_smp[,pmnt_method_grp_fac:=as.factor(pmnt_method_grp)]
datCredit_valid_smp[,pmnt_method_grp_fac:=as.factor(pmnt_method_grp)]
# Previous defaults
datCredit_train_smp[,PrevDefaults_fac:=as.factor(PrevDefaults)]
datCredit_valid_smp[,PrevDefaults_fac:=as.factor(PrevDefaults)]

# - Create a cross-sectional dataset
datCredit_train_smp_cross <- subset(datCredit_train_smp, DefSpell_Counter==1)
datCredit_valid_smp_cross <- subset(datCredit_valid_smp, DefSpell_Counter==1)




# ------ 2. Fitting trees using various packages

# --- 2.1 Fit a tree with a maximum depth of 2 nodes
# - Fit tree
start_time <- proc.time()
SurvTree_PartyKit <- ctree(Surv(DefSpell_Age,DefSpell_Event)~
                             PrevDefaults_fac + DefSpell_Num_binned +
                             ArrearsToBalance_1_Aggr_Prop + Balance_Real_1 +
                             pmnt_method_grp_fac + InterestRate_Margin_Aggr_Med_2 + InterestRate_Nom +
                             DefaultStatus1_Aggr_Prop_Lag_12 + g0_Delinq_Ave + M_DTI_Growth_6 +
                             Principal_Real + M_RealGDP_Growth_12 + g0_Delinq_Num +
                             M_Repo_Rate_2 + M_Inflation_Growth_3,
                           data=datCredit_train_smp_cross,
                           control=ctree_control(mincriterion=0.99, # 1 - p-value threshold (default ≈ 5% significance)
                                                 minsplit=1000, # minimum number of observations to attempt a split
                                                 minbucket=50, # minimum number in terminal node (common default)
                                                 maxdepth=2))
end_time <- proc.time()
(delta_time <- end_time - start_time)
### Runtime = 1 sec

# - Visualise tree
plot(SurvTree_PartyKit)


# --- 2.2 Render predictions for each node
# - Get node from trees
datCredit_train_smp_cross[,SurvTree_PartyKit_Node:=predict(SurvTree_PartyKit, datCredit_train_smp_cross, type="node")]
datCredit_valid_smp_cross[,SurvTree_PartyKit_Node:=predict(SurvTree_PartyKit, datCredit_valid_smp_cross, type="node")]

# - Fit a survival object for each node
### NOTE: We use the training dataset
km_partykit <- survfit(Surv(TimeInDefSpell, DefSpell_Event)~SurvTree_PartyKit_Node, data=datCredit_train_smp_cross)

# - Create a survival dataset from the training data
km_complete <- summary(km_partykit, times=1:max(datCredit_train_smp_cross$DefSpell_Age, na.rm=T))
surv_df <- data.table(time=km_complete$time,
                      surv=km_complete$surv,
                      strata=km_complete$strata)

# - Add an extra observation to facilitate first time in default hazard estimation
datAdd <- data.table(time=rep(0,length(unique(km_partykit$strata))),
                     surv=rep(1,length(unique(km_partykit$strata))),
                     strata=rep(unique(names(km_partykit$strata)))
)
surv_df <- rbind(surv_df, datAdd)

# - Estimate hazards
haz_df <- surv_df %>% group_by(strata) %>% arrange(time) %>% mutate(hazard = (lag(surv)-surv)/lag(surv)) %>% data.table()

# - Join hazards back to main dataset
haz_df[, strata_clean:=as.integer(sub("SurvTree_PartyKit_Node=", "", haz_df$strata))]
datCredit_valid_smp_cross[,`:=`(hazard=NULL)]
datCredit_valid_smp_cross <- merge(datCredit_valid_smp_cross, haz_df[,c("strata_clean","time","hazard")],
                                   by.x=c("SurvTree_PartyKit_Node","DefSpell_Age"), by.y=c("strata_clean","time"),
                                   all.x=T)

# - Assess missingness
sum(is.na(datCredit_valid_smp_cross$hazard))/nrow(datCredit_valid_smp_cross)
unique(datCredit_valid_smp_cross[is.na(hazard),c("DefSpell_Age","SurvTree_PartyKit_Node")])
### RESULTS: Maximum node depth:
###           -1: 11.37% missingness
###           -2: 72.19% missingness

# - Evaluate using cross sectional data to align with how model is fit
objROC_LR <- roc(response=datCredit_valid_smp_cross[, DefSpell_Event], predictor=datCredit_valid_smp_cross$hazard)
objROC_LR$auc; plot(objROC_LR)
### RESULTS: Maximum node depth:
###           - 1: AUC=75.95%
###           - 2: AUC=81.13%
###           - 3: AUC=81.4%
###           - 4: AUC=83.6%
###           - 5: AUC=87.06%
###           - 10: AUC=90.26%
###           - 20: AUC=90.26%

### CONCLUSION: - AUCs increase with tree complexity, but the total number of events in terminal nodes decrease drastically such that
###             the survival curves are NAs in most cases.
###             - Maximum depth of 5 chosen since it is a good trade-off between accuracy and tree complexity


# --- 2.3 Evaluate prediction accuracy | Longitudinal data
# - Get node from trees
datCredit_valid_smp_cross[,SurvTree_PartyKit_Node:=predict(SurvTree_PartyKit, datCredit_valid_smp_cross, type="node")]

# - Fit a survival object for each node
km_partykit <- survfit(Surv(TimeInDefSpell, DefSpell_Event)~SurvTree_PartyKit_Node, data=datCredit_valid_smp_cross)

# - Create a survival dataset
surv_df <- data.frame(time=km_partykit$time,
                      surv=km_partykit$surv,
                      strata=rep(names(km_partykit$strata), km_partykit$strata)
)

# - Estimate hazards
haz_df <- surv_df %>% group_by(strata) %>% arrange(time) %>% mutate(hazard = (lag(surv)-surv)/lag(surv)) %>% data.table()

# - Join hazards back to main dataset
haz_df[, strata_clean:=as.integer(sub("SurvTree_PartyKit_Node=", "", haz_df$strata))]
datCredit_valid_smp_cross[,`:=`(hazard=NULL)]
datCredit_valid_smp_cross <- merge(datCredit_valid_smp_cross, haz_df[,c("strata_clean","time","hazard")],
                                   by.x=c("SurvTree_PartyKit_Node","TimeInDefSpell"), by.y=c("strata_clean","time"),
                                   all.x=T)

# - Evaluate using cross sectional data to align with how model is fit
objROC_LR <- roc(response=datCredit_valid_smp_cross[, DefSpell_Event], predictor=datCredit_valid_smp_cross$hazard)
objROC_LR$auc; plot(objROC_LR)

