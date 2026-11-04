# ================================= SURVIVAL TREES ======================================
# Fitting a multinomial survival tree in predicting the occurrence of both write-off and 
# cure events for defaulted loans, fitted at the spell-level.
# ---------------------------------------------------------------------------------------
# PROJECT TITLE: Loss Modelling (LGD) for Residential Mortgages
# SCRIPT AUTHOR(S): Dr Arno Botha (AB)
# ---------------------------------------------------------------------------------------
# -- Script dependencies:
#   - 0.Setup.R
#   - 2f.Data_Fusion1.R

# -- Inputs:
#   - datCredit_train_CDH | Prepared from script 2g
#   - datCredit_valid_CDH | Prepared from script 2g
#
# -- Outputs:
#   - SurvTree_CTree | A list containing both the survival tree object and its
#                      associated training dataset
# =======================================================================================


# ------ 1. Preliminaries ------

# --- 1.1 Load data
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


# --- 1.2 Final data preparations
# - Transform categorical variables to factor
# Payment method
datCredit_train[,pmnt_method_grp_fac:=as.factor(pmnt_method_grp)]
datCredit_valid[,pmnt_method_grp_fac:=as.factor(pmnt_method_grp)]
# Previous defaults
datCredit_train[,PrevDefaults_fac:=as.factor(PrevDefaults)]
datCredit_valid[,PrevDefaults_fac:=as.factor(PrevDefaults)]
# Time binned
datCredit_train[,Time_Binned_fac:=as.factor(Time_Binned)]
datCredit_valid[,Time_Binned_fac:=as.factor(Time_Binned)]

# Event outcome
datCredit_train[, DefSpellResol_Fac:=factor(DefSpellResol_Type_Hist,
                                            c("Censored", "WOFF", "Cured"))]
datCredit_valid[, DefSpellResol_Fac:=factor(DefSpellResol_Type_Hist,
                                            c("Censored", "WOFF", "Cured"))]


# --- 1.3 Load survival objects for empirical-expected term-structure diagnostic
if (!exists('datSurv2')) unpack.ffdf(paste0(genPath,"datSurv_KM_MultiSpell_CompRisks"), tempPath);gc()




# ------ 2. Fitting a survival tree and rendering predictions ------

# --- 2.1 Fit tree
# - Input space of multinomial DtH-model
# Time_Binned, DefSpell_Num_binned, log(TimeInDefSpell):DefSpell_Num_binned,
# InterestRate_Nom, M_Repo_Rate_12, M_DTI_Growth_12, M_RealIncome_Growth_9,
# M_Inflation_Growth_12, NewLoans_Aggr_Prop
### NOTE: The following changes to the input space are made interactively using epxert judgment
# Added DefSpell_Num_binned

# - Fit tree
max_depth <- 4 # Maximum tree-depth
start_time <- proc.time()
modMultiSurvTree <- ctree(DefSpell_EventType ~ TimeInDefSpell + DefSpell_Num_binned + 
                             InterestRate_Nom + M_Repo_Rate_12 + M_DTI_Growth_12 + 
                               pmnt_method_grp_fac + M_RealIncome_Growth_9 + M_Inflation_Growth_12 + 
                               NewLoans_Aggr_Prop,
                             #Arrears + pmnt_method_grp_fac
                           data=datCredit_train,
                           control=ctree_control(mincriterion=0.99, # 1 - p-value threshold (default ≈ 5% significance)
                                                 minsplit=1000, # minimum number of observations to attempt a split
                                                 minbucket=50, # minimum number in terminal node (common default)
                                                 testtype="Bonferroni", # most conservative → classical feeling
                                                 maxdepth=max_depth))
proc.time() - start_time # Runtime = 1 sec

# - Visualise tree
plot(modMultiSurvTree)
### NOTE: Variables included in fitted model:
###        InterestRate_Nom, pmnt_method_grp_fac, M_DTI_Growth_12, M_RealIncome_Growth_9,
###        TimeInDefSpell, NewLoans_Aggr_Prop, 

# - Save model
pack.ffdf(paste0(genPath,"MultiCI_Tree"), modMultiSurvTree)


# --- 2.2 Obtain predictions from tree
# - Combine training and validation datasets to facilitate "better" (smooth) graphs
datCredit <- rbind(datCredit_train, datCredit_valid)

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

# - Point to main credit data
datCredit <- rbind(data.table(datCredit_train, Sample="Training"),
                   data.table(datCredit_valid, Sample="Validation"))

# - Score data using fitted tree
matPredictions <- predict(modMultiSurvTree, newdata=datCredit, type = "prob")
datCredit[, Hazard_WriteOff_Exp := matPredictions[, "Write-off"]]
datCredit[, Hazard_Cured_Exp := matPredictions[, "Cured"]]

# - Derive (event-free) survival probability S(t) = prod(1 - h_1(t) - h_2(t))
datCredit[, Survival_overall_exp := cumprod(1-Hazard_WriteOff_Exp-Hazard_Cured_Exp), by=.(DefSpell_Key)]

# - Derive event rates as f_1(t) = S(t-1) . h_1(t)
datCredit[, EventRate_WriteOff_exp := shift(Survival_overall_exp, n=1,fill=1) * Hazard_WriteOff_Exp, by=.(DefSpell_Key)]

# - Remove added rows
datCredit <- subset(datCredit, Counter_Artificial>0)


# --- 2.3 Aggregate event rates to spell period level and plot and calculate MAE
# - Aggregate data
datSurv_exp <- datCredit[,.(
  EventRate_WriteOff=mean(EventRate_WriteOff_exp, na.rm=T)),
  by=.(TimeInDefSpell)]

# - Ensure order of fields is correct
setorder(datSurv_exp, TimeInDefSpell)

# - Cut-offs for analysis and graphs 
sMaxSpellAge <- 84 # Max for [DefSpell_Age], as determined in earlier analyses (script 3c)

# - Plotting event rates for actuals and all expected values
plot(datSurv2[Time<=sMaxSpellAge, EventRate_WriteOff], type="b")
lines(datSurv_exp[TimeInDefSpell<=sMaxSpellAge, EventRate_WriteOff], type="b", col="red")
# MAE
mean(abs(datSurv2[Time<=sMaxSpellAge, EventRate_WriteOff] - 
      datSurv_exp[TimeInDefSpell<=sMaxSpellAge, EventRate_WriteOff]), na.rm=T)
# MAE: 0.00461


# --- 2.4 tROC-analysis
# - Perform time-dependent ROC analysis
predictTime <- 44
ptm <- proc.time() #IGNORE: for computation time calculation
objROC44_MultiST <- tROC.multi(datGiven=datCredit[Sample=="Validation"], 
                               Marker = datCredit[Sample=="Validation", Hazard_WriteOff_Exp],
                               month_End=predictTime, sLambda=0.05, numDigits=4, fld_ID="DefSpell_Key", 
                               fld_Event="WOff_Ind", eventVal=1, fld_StartTime="Start", fld_EndTime="TimeInDefSpell",
                               caseStudyName=paste0("Cond_MultiSurvTree_", predictTime), numThreads=4, logPath=genPath, 
                               Graph=T)
proc.time() - ptm
objROC44_MultiST$AUC ;objROC44_MultiST$ROC_graph
### RESULTS: AUC up to t: 94.36%, achieved in 27 secs (maximum node depth=4)




# ------ 3. Clean-up ------

cleanEnviron()



