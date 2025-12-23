# ======================================= INPUT SPACE: LOSS SEVERITY-TWO STAGE============================
# Divide data into thematic groups and perform data analysis on them to compile an input space for 
# the loss severity component.
# ------------------------------------------------------------------------------------------------------
# PROJECT TITLE: Loss Modelling (LGD) for FNB Mortgages
# SCRIPT AUTHOR(S): Marcel Muller (MM), Mohammed Gabru (MG)
# ------------------------------------------------------------------------------------------------------
# -- Script dependencies:
#   - 0.Setup.R
#   - 1.Data_Import.R
#   - 2a.Data_Prepare_Credit_Basic.R
#   - 2b.Data_Prepare_Credit_Advanced.R
#   - 2c.Data_Prepare_Credit_Advanced2.R
#   - 2d.Data_Enrich.R
#   - 2f.Data_Fusion1.R
#   - 2g.Data_Fusion2.R

# -- Inputs:
#   - datCredit_train_CDH | Prepared from script 2g
#   - datCredit_valid_CDH | Prepared from script 2g
#
# -- Outputs:
#   - Input_Space
# ------------------------------------------------------------------------------------------------------



# ------ 1. Preliminaries

# --- 1.1 Load and prepare data
# - Confirm prepared datasets are loaded into memory
if (!exists('datCredit_train_CDH')) unpack.ffdf(paste0(genPath,"creditdata_train_CDH"), tempPath);gc()

# - Use only default spells
datCredit_train <- subset(datCredit_train_CDH,!is.na(DefSpell_Key))

# - Remove previous objects from memory
rm(datCredit_train_CDH); gc()

# - Weigh write-off cases as one, determined interactively based on calibration success (script 6e)
datCredit_train[, Weight:=ifelse(DefSpell_Event==1,1,1)]

# - Subset data for training of the one-stage model
datCredit_train_classic <- datCredit_train[!is.na(DefSpell_Key) & DefSpell_Counter==1,]

# - Re-create 
datCredit_train_classic[, DefSpell_Event:=ifelse(DefSpellResol_Type_Hist!="WOFF",0,1)]


# --- 1.3 Load models
# - Basic discrete-time hazard model
modLR_bas <- readRDS(paste0(genObjPath,"CoxDisc_Basic_Model.rds"))

# - Advanced discrete-time hazard model
modLR_adv <- readRDS(paste0(genObjPath,"CoxDisc_Advanced_Model.rds"))

# - Classic logit model
modLR_classic <- readRDS(paste0(genObjPath,"LR_Model.rds"))




# ------ 2.  Estimate event rates

# --- 2.1 Estimate survival quantities
# - Predict hazard h(t) = P(T=t | T>= t) in discrete-time
# Basic model
datCredit_train[, Hazard_bas:=predict(modLR_bas, newdata=datCredit_train, type = "response")]
# Advanced model
datCredit_train[, Hazard_adv:=predict(modLR_adv, newdata=datCredit_train, type = "response")]
# Classical model
datCredit_train_classic[, Hazard_classic:=predict(modLR_classic, newdata=.SD[], type = "response")]

# - Derive survival probability S(t) = prod ( 1- hazard)
# Basic model
datCredit_train[, Survival_bas:=cumprod(1-Hazard_bas), by=list(DefSpell_Key)]
# Advanced model
datCredit_train[, Survival_adv:=cumprod(1-Hazard_adv), by=list(DefSpell_Key)]
# Classical model
datCredit_train_classic[, Survival_classic:=cumprod(1-Hazard_classic), by=list(DefSpell_Key)]

# - Derive discrete density, or event probability f(t) = S(t-1) * h(t)
# Basic model
datCredit_train[, EventRate_bas:=shift(Survival_bas, type="lag", n=1, fill=1) * Hazard_bas, by=list(DefSpell_Key)]
# Advanced model
datCredit_train[, EventRate_adv:=shift(Survival_adv, type="lag", n=1, fill=1) * Hazard_adv , by=list(DefSpell_Key)]
# Classical model
datCredit_train_classic[, EventRate_classic:=shift(Survival_classic, type="lag", n=1, fill=1)*Hazard_classic, by=list(DefSpell_Key)]


# --- 2.2 Filtering
# - Identify where the loss rate is out of bounds and not feasible
datCredit_train[, OOB_Ind:=ifelse(LossRate_Real < 0 | LossRate_Real > 1, 1,0)]
datCredit_train_classic[, OOB_Ind:=ifelse(LossRate_Real < 0 | LossRate_Real > 1, 1,0)]

# - Subset to include only relevant data
datCredit_train <- subset(datCredit_train, OOB_Ind==0)
datCredit_train_classic <- subset(datCredit_train_classic, OOB_Ind==0)




# ------ 3. Determining the thresholds for dichotomisation
# --- 3.1 Determine thresholds | Discrete time models
# - Calculate prevalence of write-offs
(q1 <- mean(datCredit_train$DefSpell_Event,na.rm=TRUE))
### RESULTS: Prevalence = 0.01111503

# - Calculate the cost multiple
(a <- (1-q1)/q1)
### RESULTS: Cost-multiple = 88.96827

# - Basic model
(thresh_dth_bas <- GenYoudenIndex(optimise_type="Pre-determined", Trained_Model=modLR_bas,
                                  Train_DataSet=datCredit_train, Target="DefSpell_Event",
                                  prob_vals_given="EventRate_bas", a=1, replicate=100))
### RESULTS: Threshold at a = 1 = 0.05467001
### RESULTS: Threshold at (a <- (1-q1)/q1  = 0.2642033

# - Advanced model
(thresh_dth_adv <- GenYoudenIndex(optimise_type="Pre-determined", Trained_Model=modLR_adv,
                                  Train_DataSet=datCredit_train, Target="DefSpell_Event",
                                  prob_vals_given="EventRate_adv", a=1, replicate=100))
### RESULTS: Threshold at a = 1 = 0.3894059
### RESULTS: Threshold at (a <- (1-q1)/q1  = 0.4333787


# --- 3.2 Determine thresholds | Classical models
# - Calculate prevalence of write-offs
(q1 <- mean(datCredit_train_classic$DefSpell_Event,na.rm=TRUE))
### RESULTS: Prevalence = 0.1824916

(a <- (1-q1)/q1)
### RESULTS: Cost-multiple = 4.479705

# - Classical model
(thresh_lr_classic <- GenYoudenIndex(optimise_type="Pre-determined", Trained_Model=modLR_classic,
                                     Train_DataSet=datCredit_train_classic, Target="DefSpell_Event",
                                     prob_vals_given="EventRate_classic", a=1))
### RESULTS: Threshold at a = 1 = 0.564484
### RESULTS: Threshold at (a <- (1-q1)/q1  = 0.2358849



# --- 3.2 Save thresholds to disk
# - Combine thresholds into a single list
thres_lst <- list("Basic"=thresh_dth_bas$cutoff,
                  "Advanced"=thresh_dth_adv$cutoff,
                  "Classical"=thresh_lr_classic$cutoff)

# - Save list to disk
saveRDS(thres_lst, file=paste0(genObjPath,"Classification_Thresholds.rds"))


# --- 3.3 Clean-up
suppressWarnings(rm(datCredit_train, datCredit_train_classic,
                    modLR_adv, modLR_bas, modLR_classic, thresh_dth_bas,
                    thresh_dth_adv, thresh_lr_classic, thres_lst))
gc()


