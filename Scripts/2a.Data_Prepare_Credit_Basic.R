# ================================== DATA PREPARATION ===================================
# Perform various data checks, conduct data cleaning, apply exclusions, treat missingness
# ---------------------------------------------------------------------------------------
# PROJECT TITLE: Loss Modelling (LGD) for FNB Mortgages
# SCRIPT AUTHOR(S): Dr Arno Botha, Marcel Muller
# ---------------------------------------------------------------------------------------
# DESCRIPTION:
# This script prepares raw imported credit data into a much more meaningful form to 
# facilitate modelling. This preparation includes the following high-level steps:
#   1)  Perform high-level structural checks on raw data
#   2)  Conduct high-level basic cleaning
#   3a) Rename and reorder certain credit fields to align with broader platform
#   3b) Create basic features in checking for certain phenomena, i.e., account-level 
#       write-offs. 
#   3c) Conduct basic data cleaning and perform some more checks on terminal events
#   4)  Apply exclusions based on insight gleaned from Data Experiments
#   5)  Execute basic missing value treatments, supported by Data Experiments
# ---------------------------------------------------------------------------------------
# -- Script dependencies:
#   - 0.Setup.R
#   - 1.Data_Import.R

# -- Inputs:
#   - dat.raw | raw monthly loan performance data imported in script 1
#   - various parameters set in the setup script 0
#
# -- Outputs:
#   - datCredit_real | Enhanced version of input
# ---------------------------------------------------------------------------------------
# NOTE: This script predominantly comes from another project (Kasmeer). As such, all 
# references to associated Data Experiments reside within that project and will be taken 
# as established 'facts' within this project.
# =======================================================================================




# ------- 1. High-level data checks
ptm1 <- proc.time() # for runtime calculations (ignore)

# --- 1. Confirm raw data is loaded into memory
if (!exists('dat.raw')) unpack.ffdf(paste0(genPath,"creditdata_final1"), tempPath)

# --- 2. Confirm data grain and non-missingness
dat.raw[, list(Freq = .N), by=list(ACCT_NO, DATEX)][Freq>1,]
dat.raw[is.na(ACCT_NO), .N] / dat.raw[,.N] * 100 == 0 # Should be true
dat.raw[is.na(DATEX), .N] / dat.raw[,.N] * 100 == 0 # Should be true
### RESULTS: Confirmed with no missingness

# --- 3. Frequency analysis on DATE (period) in checking consistency across platforms (SAS + R)
test <- dat.raw[order(DATE), list(Freq=.N),by=list(DATE)]
# [SANITY CHECK] Are there any missing periods in aggregate?
test[, Prev_DATE := shift(DATE, n=1, type="lag")]
test[!is.na(Prev_DATE), Diff_Months := interval(Prev_DATE, DATE) %/% months(1)]
check_cred0 <- test[Diff_Months > 1, .N] == 0 # If TRUE then no missing periods.
cat( check_cred0 %?% 'SAFE: No missing periods found in aggregate.\n' %:% 'WARNING: Missing periods found in aggregate.\n')
rm(test)

# --- 4. Some parameter observations
(maxDate_observed <- max(dat.raw$DATE, na.rm=T) )
(minDate_observed <- rollback(min(dat.raw$DATE, na.rm=T), roll_to_first = T) )





# ------- 2. Necessary Data Cleaning on Raw Data
# - remove redundant fields for greater memory efficiency
suppressWarnings(dat.raw[, `:=`(SUB_STAT_CDE = NULL,
                      # Fields that were investigated and deemed irrelevant | See Data Experiment 10
                      PRE_D7_ACC_STATUS = NULL, slc_status_final_pred7 = NULL, slc_status_final = NULL,
                      # Fields yet to be created 
                      LoanAge = NULL, Max_Counter = NULL)] )

# - engineer fundamental loan parameter fields
dat.raw[, LoanAge := interval(Date_Origination, DATE)]
dat.raw[, LoanAge := LoanAge %/% months(1)]
dat.raw[, Max_Counter := max(Counter, na.rm=T), by=list(ACCT_NO)]
gc() # Memory optimization





# ------- 3. Data mapping & Basic Data Preparation (Cleaning & Feature Engineering)

# --- 1. Data mapping
# Map field names (and reorder them) to align with rest of the platform, and drop unnecessary ones
datCredit_real <- dat.raw %>% 
  rename(LoanID = ACCT_NO, Date = DATE, Age = LoanAge, AccountStatus = POST_D7_ACC_STATUS, 
         Curing_Ind = slc_curing_ind) %>% 
         as.data.table(key=c("LoanID", "Counter")) %>%
  relocate(LoanID, Date, Age, Counter, Max_Counter, Date_Origination, Principal, 
           Term, InterestRate_Nom, Instalment, Arrears, Balance, AccountStatus, 
           Curing_Ind, WOff_Ind, WriteOff_Amt, EarlySettle_Ind, EarlySettle_Amt, 
           FurtherLoan_Ind, FurtherLoan_Amt, Redraw_Ind, Redrawn_Amt, WOFF_DATE, CLS_STAMP,
           .before = LoanID) %>% 
  dplyr::select(-c(PIP_DATE, WOT_IND))


# --- 2. Basic Feature Engineering

# - Create Default Indicator based on given delinquency status field 
# NOTE: We will recreate this and related fields later
datCredit_real <- datCredit_real %>% mutate(DefaultStatus1 = 
                                              ifelse(AccountStatus == "05. NPL", 1, 0)) %>%
  relocate(DefaultStatus1, .after=AccountStatus)
datCredit_real <- datCredit_real %>% mutate(g0_Delinq = as.numeric(substr(AccountStatus,1,2))) %>%
  mutate(g0_Delinq = case_when(g0_Delinq==1 ~ 0, g0_Delinq <= 3 ~ 1, 
                               g0_Delinq == 4 ~ 2, g0_Delinq == 5 ~ 3)) %>%
  relocate(g0_Delinq, .after = AccountStatus)

# - Creating account-level aggregates of events to facilitate the creation of other features
datCredit_real[, HasWOff := max(WOff_Ind, na.rm=T), by=list(LoanID)]
datCredit_real[, HasSettle := max(EarlySettle_Ind, na.rm=T), by=list(LoanID)]
datCredit_real[, HasClosure := ifelse(any(!is.na(CLS_STAMP)), 1, 0) ,by=list(LoanID)]
datCredit_real[, HasFurtherLoan := max(FurtherLoan_Ind, na.rm=T), by=list(LoanID)]
datCredit_real[, HasRedraw := max(Redraw_Ind, na.rm=T), by=list(LoanID)]

# - Create repaid flag for unaccounted account closures | See Data Experiment 2
datCredit_real[, Repaid_Ind :=
       ifelse(Counter==Max_Counter & HasClosure==1 & HasSettle==0 & HasWOff == 0 &
                ( (Balance <= 250 & is.na(RMNG_TERM)) | 
                    (!is.na(RMNG_TERM) & RMNG_TERM <= 1 & Date < maxDate_observed)), 1,0),
     by=list(LoanID)]
datCredit_real[, HasRepaid := max(Repaid_Ind, na.rm=T), by=list(LoanID)]


# --- 3. Basic Data Cleaning
datCredit_real[is.na(Arrears), Arrears := 0]
datCredit_real[WOff_Ind == 0, WriteOff_Amt := 0] # See Data Experiment 2
datCredit_real[EarlySettle_Ind == 0, EarlySettle_Amt := 0] # See Data Experiment 2


# --- 4. Basic checks: Terminal event amounts (Write-off, Early Settlement) | See Data Experiment 2

# [SANITY CHECK] How often is the write-off amount non-zero (when it should be zero)?
(diag.real2b <- datCredit_real[WOff_Ind == 0 & WriteOff_Amt != 0, .N] / datCredit_real[WOff_Ind == 0, .N] * 100)
check_cred1 <- diag.real2b == 0 # Should yield TRUE for test to pass
cat( check_cred1 %?% 'SAFE: Write-off amounts are zero wherever write-off has not occurred.\n' %:% 'WARNING: Write-off amounts are non-zero in cases when they should be zero.\n')

# [SANITY CHECK] How often is the settlement amount non-zero (when it should be zero)?
(diag.real2f <- datCredit_real[EarlySettle_Ind == 0 & EarlySettle_Amt != 0, .N] / datCredit_real[EarlySettle_Ind == 0, .N] * 100)
check_cred2 <- diag.real2f == 0 # Should be TRUE
cat( check_cred2 %?% 'SAFE: Early settlement amounts are zero wherever settlement has not occurred.\n' %:% 'WARNING: Early settlement amounts are non-zero in cases when they should be zero.\n')





# ------ 4. Exclusions

# --- 0. Preliminaries
# Create a field that will be 0 for unaffected observations but >= 1 for all cases to be excluded subsequently,
# such that the value of this field denotes the Exclusion ID, itself tracked in a common table that measures the 
# sample impact per exclusion.
suppressWarnings({ datCredit_real[, ExclusionID := NULL]; rm(datExclusions) })
datCredit_real[, ExclusionID := 0]


# --- 1. Old single-record cases | See Data Experiment 4

# - maxDate set to be the first of the last month in which loans were last disbursed
maxDate <- rollback(max(datCredit_real$Date_Origination, na.rm=T), roll_to_first = T)

# [DIAGNOSTIC] Account-level and dataset-wide impacts of exclusion
diag.real4b <-  datCredit_real[ExclusionID == 0 & Counter == 1 & Max_Counter == 1 & 
                                  (Date_Origination < maxDate | is.na(Date_Origination) ), .N] / 
    datCredit_real[ExclusionID == 0 & Counter == 1, .N] * 100 
diag.real4b_abs <- datCredit_real[ExclusionID == 0 & Max_Counter == 1 & 
                                    (Date_Origination < maxDate | is.na(Date_Origination)) , .N]
diag.real4b_rec <-  diag.real4b_abs / datCredit_real[ExclusionID == 0, .N] * 100 

# - Conditional exclusion
if (diag.real4b > 0) {
  
  cat("EXCLUSION: Old single-record cases. Prevalence: ", round(diag.real4b,digits=1), "% of accounts (",
      round(diag.real4b_rec,digits=1), "% of records).\n")
  
  # - Mark affected records for exclusion later with an ID-value
  LoanIDs <- unique(subset(datCredit_real, ExclusionID == 0 & Counter == 1 & Max_Counter == 1 & 
                             (Date_Origination < maxDate | is.na(Date_Origination)), select="LoanID"))
  datCredit_real[LoanID %in% LoanIDs$LoanID, ExclusionID := 1]
  
  # [SANITY CHECK] Treatment success?
  check_excl1 <- datCredit_real[ExclusionID == 1 & Counter == 1, .N] / 
    datCredit_real[ExclusionID %in% c(0,1) & Counter == 1, .N] * 100 == diag.real4b
  cat( check_excl1 %?% 'SAFE: Exclusion 1 successfully applied.\n' %:% 
         'WARNING: Applying Exclusion 1 failed.\n')
  
  # - Create and add exclusion impact to a common table
  datExcl <- data.table("Excl_ID"=1, "Reason"="Old single-record cases",
                              "Impact_Account" = diag.real4b, "Impact_Dataset" = diag.real4b_rec,
                              "Impact_records" = diag.real4b_abs)
  if (exists('datExclusions')) datExclusions <- rbind(datExclusions, datExcl) else datExclusions <- datExcl
}


# --- 2. Zero-balance credit histories | See Data Experiment 6

# - Preliminaries
datCredit_real[ExclusionID==0, HasZeroBalances_all := all(ifelse(Balance <= 0, TRUE, FALSE)), by=list(LoanID)]

# [DIAGNOSTIC] What proportion of account's credit histories have zero balances throughout?
diag.real6a <- datCredit_real[ExclusionID == 0 & Counter == 1 & HasZeroBalances_all == TRUE, .N] /
    datCredit_real[ExclusionID == 0 & Counter == 1, .N] * 100
diag.real6a_abs <- datCredit_real[ExclusionID == 0 & HasZeroBalances_all == TRUE, .N]
diag.real6a_rec <- diag.real6a_abs  / datCredit_real[ExclusionID == 0, .N] * 100

# - Conditional exclusion
if (diag.real6a > 0) {

  cat("EXCLUSION: Zero-balance credit histories. Prevalence: ", round(diag.real6a,digits=1), "% of accounts (",
      round(diag.real6a_rec,digits=1), "% of records).\n")
  
  # - Mark affected records for exclusion later with an ID-value
  LoanIDs <- unique(subset(datCredit_real, ExclusionID == 0 & 
                             Counter == 1 & HasZeroBalances_all == TRUE, select="LoanID"))
  datCredit_real[LoanID %in% LoanIDs$LoanID, ExclusionID := 2]
  
  # [SANITY CHECK] Treatment success?
  check_excl2 <- datCredit_real[ExclusionID == 2 & Counter == 1, .N] / 
    datCredit_real[ExclusionID %in% c(0,2) & Counter == 1, .N] * 100 == diag.real6a
  cat( check_excl2 %?% 'SAFE: Exclusion 2 successfully applied.\n' %:% 
         'WARNING: Applying Exclusion 2 failed.\n')
  
  # - Create and add exclusion impact to a common table
  datExcl <- data.table("Excl_ID"=2, "Reason"="Zero-balance credit histories",
                        "Impact_Account" = diag.real6a, "Impact_Dataset" = diag.real6a_rec,
                        "Impact_records" = diag.real6a_abs)
  if (exists('datExclusions')) datExclusions <- rbind(datExclusions, datExcl) else datExclusions <- datExcl
}


# --- 3. Zero-Principal credit histories | See Data Experiment 5

# - Preliminaries
datCredit_real[ExclusionID==0, HasZeroPrincipal_all := all(Principal==0), by=list(LoanID)]

# [DIAGNOSTIC] Prevalence of zero-valued Principals throughout the entire account history?
diag.real5_1d <- datCredit_real[ExclusionID==0 & Counter==1 & HasZeroPrincipal_all==T, .N] /
    datCredit_real[ExclusionID==0 & Counter==1, .N] * 100 
diag.real5_1d_abs <- datCredit_real[ExclusionID==0 & HasZeroPrincipal_all==T, .N]
diag.real5_1d_rec <- diag.real5_1d_abs / datCredit_real[ExclusionID==0, .N] * 100 

# - Conditional treatment
if (diag.real5_1d > 0) {
  
  cat("EXCLUSION: Zero-principal credit histories. Prevalence: ", round(diag.real5_1d,digits=3), "% of accounts (",
      round(diag.real5_1d_rec,digits=4), "% of records).\n")
  
  # - Mark affected records for exclusion later with an ID-value
  LoanIDs <- unique(subset(datCredit_real, ExclusionID==0 & Counter==1 & 
                             HasZeroPrincipal_all==T, select="LoanID"))
  datCredit_real[LoanID %in% LoanIDs$LoanID, ExclusionID := 3]
  
  # [SANITY CHECK] Treatment success?
  check_excl3 <- datCredit_real[ExclusionID == 3 & Counter == 1, .N] / 
    datCredit_real[ExclusionID %in% c(0,3) & Counter == 1, .N] * 100 == diag.real5_1d
  cat( check_excl3 %?% 'SAFE: Exclusion 3 successfully applied.\n' %:% 
         'WARNING: Applying Exclusion 3 failed.\n')
  
  # - Create and add exclusion impact to a common table
  datExcl <- data.table("Excl_ID"=3, "Reason"="Zero-principal credit histories",
                        "Impact_Account" = diag.real5_1d, "Impact_Dataset" = diag.real5_1d_rec,
                        "Impact_records" = diag.real5_1d_abs)
  if (exists('datExclusions')) datExclusions <- rbind(datExclusions, datExcl) else datExclusions <- datExcl
}


# --- 4. Zero-Instalment & Zero-Arrears credit histories | See Data Experiment 8

# - Preliminaries
datCredit_real[ExclusionID==0, HasZeroIns_All := ifelse(all(Instalment==0),T,F),by=list(LoanID)]
datCredit_real[ExclusionID==0, HasZeroArrears_All := ifelse(all(Arrears==0),T,F),by=list(LoanID)]

# [DIAGNOSTIC] Prevalence of account-level zero-valued instalments
diag.real8_1a <- datCredit_real[ExclusionID==0 & Counter==1 & HasZeroIns_All==T, .N] / 
  datCredit_real[ExclusionID==0 & Counter==1, .N] * 100
diag.real8_1a_abs <- datCredit_real[ExclusionID==0 & HasZeroIns_All==T, .N]
diag.real8_1a_rec <- diag.real8_1a_abs / datCredit_real[ExclusionID==0, .N] * 100

# [DIAGNOSTIC] Prevalence of zero-valued arrears amongst zero-valued instalments?
diag.real8_1c <- datCredit_real[ExclusionID==0 & Counter==1 & HasZeroIns_All==T & HasZeroArrears_All==T, .N] /
  datCredit_real[ExclusionID==0 & Counter==1 & HasZeroIns_All==T, .N] * 100

# - Conditional treatment
if (diag.real8_1a > 0 & diag.real8_1c >= 0.9) {
  
  cat("EXCLUSION: Zero-instalment credit histories. Prevalence: ", round(diag.real8_1a,digits=3), "% of accounts (",
      round(diag.real8_1a_rec,digits=4), "% of records), \n\tof which ", round(diag.real8_1c,digits=3),
      "% also have zero-valued arrears throughout.\n")
  
  # - Mark affected records for exclusion later with an ID-value
  LoanIDs <- unique(subset(datCredit_real, ExclusionID==0 & Counter==1 & 
                             HasZeroIns_All==T & HasZeroArrears_All==T, select="LoanID"))
  datCredit_real[LoanID %in% LoanIDs$LoanID, ExclusionID := 4]
  
  # [SANITY CHECK] Treatment success?
  check_excl4 <- datCredit_real[ExclusionID == 4 & Counter == 1, .N] / 
    datCredit_real[ExclusionID %in% c(0,4) & Counter == 1, .N] * 100 == diag.real8_1a
  cat( check_excl4 %?% 'SAFE: Exclusion 4 successfully applied.\n' %:% 
         'WARNING: Applying Exclusion 4 failed.\n')
  
  # - Create and add exclusion impact to a common table
  datExcl <- data.table("Excl_ID"=4, "Reason"="Zero-instalment & zero-arrears credit histories",
                        "Impact_Account" = diag.real8_1a, "Impact_Dataset" = diag.real8_1a_rec,
                        "Impact_records" = diag.real8_1a_abs)
  if (exists('datExclusions')) datExclusions <- rbind(datExclusions, datExcl) else datExclusions <- datExcl  
}


# --- Clean up | Intermediary treatment-related fields no longer necessary
suppressWarnings( datCredit_real[,`:=`(HasZeroPrincipal_all = NULL, HasZeroBalances_all = NULL,
                                       HasZeroIns_All = NULL, HasZeroArrears_All = NULL)] )




# ------ 5. Missing value treatments | Sensitive to Exclusions

# --- 1. Treating [Term] | # See Data Experiment 5

# - Preliminaries
datCredit_real[, HasMissingTerm := any(is.na(Term)), by=list(LoanID)]
datCredit_real[, HasMissingTerm_all := all(is.na(Term)), by=list(LoanID)]

# [DIAGNOSTIC] How prevalent is missingness in parts of TERM within the wider dataset
diag.real5_2b <- datCredit_real[Counter == 1 & HasMissingTerm==T & ExclusionID==0, .N] / 
    datCredit_real[Counter == 1 & ExclusionID==0, .N] * 100  # account-level
diag.real5_2b_rec <- datCredit_real[ is.na(Term) & ExclusionID==0, .N] /
    datCredit_real[ExclusionID==0, .N] * 100  # dataset-level

# - Conditional treatment of missingness using the mode
if (diag.real5_2b > 0 | diag.real5_2b_rec > 0) {
  
  cat("DETECTED: Missingness in [Term]. Prevalence:", round(diag.real5_2b,digits=1), "% of accounts (",
      round(diag.real5_2b_rec,digits=1), "% of records).\n\tTreating by assiging the account-level mode ...\n")
  
  # [TREATMENT] Assign mode of account-level Term-vectors to its missing parts, 
  #   using custom "getmode()" function defined in script 0
  datCredit_real[HasMissingTerm == T & HasMissingTerm_all == F & ExclusionID==0, 
                 Term := ifelse(is.na(Term), getmode(Term), Term), by=list(LoanID)]
  
  # [TREATMENT] For those cases with universal missing terms, use mode imputation
  term_mode <- getmode(datCredit_real[Counter == 1 & HasMissingTerm_all == F, Term])
  datCredit_real[HasMissingTerm_all == T, Term := term_mode]
  
  # - Recalculate affected aggregates
  datCredit_real[, HasMissingTerm := any(is.na(Term)), by=list(LoanID)]
  datCredit_real[, HasMissingTerm_all := all(is.na(Term)), by=list(LoanID)]
  
  # [SANITY CHECK] Confirm successful treatment, considering previous sanity check
  check_treat1 <- datCredit_real[Counter == 1 & HasMissingTerm==T & ExclusionID==0, .N] / 
      datCredit_real[Counter == 1 & ExclusionID==0, .N] * 100 == 0 # account-level
  cat( check_treat1 %?% 'SAFE: Missing value treatment on [Term] succeeded.\n' %:% 
         'WARNING: Missing value treatment on [Term] failed.\n')
}


# --- 2. Treating [Date_Origination] | # See Data Experiment 3

# - Preliminaries
datCredit_real[, HasMissingOpenDate_all := all(ifelse(is.na(Date_Origination), TRUE, FALSE)), by=list(LoanID)]

# [DIAGNOSTIC] Prevalence of missingness in [Date_Origination]
diag.real3_1e <- datCredit_real[ExclusionID==0 & Counter==1 & HasMissingOpenDate_all==T, .N] / 
    datCredit_real[ExclusionID==0 & Counter==1, .N] * 100

# - Conditional treatment
if (diag.real3_1e > 0) {
  
  cat("DETECTED: Missingness in [Date_Origination]. Prevalence:", round(diag.real3_1e,digits=3),
      "% of accounts.\n\tTreating by assigning the first-observed [Date]-value ...\n")
  
  # [TREATMENT] Set first observed date as the "open date"
  datCredit_real[HasMissingOpenDate_all==T, 
                 Date_Origination := rollback(Date[1], roll_to_first = T)-days(1), by=list(LoanID)]
  
  # - Recalculate derived fields that depend on this treated field
  datCredit_real[HasMissingOpenDate_all==T, Age := interval(Date_Origination, Date) %/% months(1)]
  
  # - Recalculate affected aggregates 
  datCredit_real[, HasMissingOpenDate_all := all(ifelse(is.na(Date_Origination), TRUE, FALSE)), by=list(LoanID)]
  
  # [SANITY CHECK] Confirm successful treatment
  check_treat2 <- datCredit_real[Counter==1 & ExclusionID==0 & HasMissingOpenDate_all==T, .N] / 
      datCredit_real[Counter==1 & ExclusionID==0, .N] * 100 == 0 # account-level
  cat( check_treat2 %?% 'SAFE: Missing value treatment on [Date_Origination] succeeded.\n' %:% 
         'WARNING: Missing value treatment on [Date_Origination] failed.\n')
}

# --- Clean up | Intermediary treatment-related fields no longer necessary
suppressWarnings(
  datCredit_real[, `:=`(HasMissingTerm = NULL, HasMissingTerm_all = NULL, HasMissingOpenDate_all = NULL,
                        HasMissingOpenDate_any = NULL)]  
)

# --- Halfway point
# - Save to disk (zip) for quick disk-based retrieval later
rm(dat.raw, LoanIDs); gc() # memory optimization; no need for raw data anymore at this point
pack.ffdf(paste0(genPath,"creditdata_final1b"), datCredit_real)

# - Logging
cat('Data prepared (Basic). Applying Advanced Data Treatments next .. \n')
proc.time() - ptm1 # IGNORE: elapsed runtime
