# ================================= SURVIVAL TREES ======================================
# Fitting and testing surival trees in predicting the occurence of write-off in defaulted
# loans
# ---------------------------------------------------------------------------------------
# PROJECT TITLE: Loss Modelling (LGD) for FNB Mortgages
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
#   - creditdata_train_CDH | Training dataset in a panel format
#   - creditdata_valid_CDH | Validation dataset in a panel formate
#
# -- Outputs:
#   - ... | ...
# ---------------------------------------------------------------------------------------
# =======================================================================================

# ------ 1. Preliminaries

# - Confirm prepared datasets are loaded into memory
# Training set
if (!exists('datCredit_train_CDH')) unpack.ffdf(paste0(genPath,"creditdata_train_CDH"), tempPath)
# Validation set
if (!exists('datCredit_valid_CDH')) unpack.ffdf(paste0(genPath,"creditdata_valid_CDH"), tempPath)




# ------ 2. Growing trees using the MST-package
# --- 2.1 Prepare the training and validation data
### NOTE: The data structure is identical to a discrete time survival model,
###       i.e., a panel dataset containing all observations over a loan's lifetime

# - Training data
datTrain_MST <- copy(datCredit_train_CDH)
rm(datCredit_train_CDH); gc()

# - Validation data
datValid_MST <- copy(datCredit_valid_CDH)
rm(datCredit_valid_CDH); gc()


# --- 2.2 Fit a multivariate survival tree using a marginal model
# - Fit a MST that is only lower bounded by the number of observaitons and event per node
modMST <- MST(formula = Surv(TimeInDefSpell, WOff_Ind) ~ InterestRate_Margin | DefSpell_Key, data = datTrain_MST, test = datValid_MST,
              method = "marginal", minsplit = 1000, minevents = 20, selection.method = "test.sample")

# - Assess model
# Pruning information
modMST$pruning.info
# Summary
summary(modMST)

