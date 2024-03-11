# ===================================== DATA FUSION =====================================
# Applying exclusions, data fusion between credit and macroeconomic datasets, followed by
# engineering some basic features that must precede any (non-clustered) subsampling
# ---------------------------------------------------------------------------------------
# PROJECT TITLE: Classifier Diagnostics
# SCRIPT AUTHOR(S): Dr Arno Botha, Marcel Muller

# DESCRIPTION:
# This script performs the following high-level tasks:
#   1) Apply preliminary exclusions and assess impact using event rate
#   2a) Removes a few variables that are unlikely to be useful within the context of 
#      analysing/modelling of default risk.
#   2b) Fuses macroeconomic data unto the main credit dataset
#   3a) Creates a preliminary target/outcome variable for modelling default risk,
#       itself used in tracking influence of exclusions
#   3b) Engineers a few basic features that require entire loan histories, given
#       the intended (non-clustered) subsampling scheme in script 3-series
# ---------------------------------------------------------------------------------------
# -- Script dependencies:
#   - 0.Setup.R
#   - 1.Data_Import.R
#   - 2a.Data_Prepare_Credit_Basic.R
#   - 2b.Data_Prepare_Credit_Advanced.R
#   - 2c.Data_Prepare_Credit_Advanced2.R
#   - 2d.Data_Enrich.R
#   - 2e.Data_Prepare_Macro.R
#
# -- Inputs:
#   - datCredit_real | Prepared credit data from script 2d
#   - datMV | Prepared macroeconomic dataset from script 2e
#
# -- Outputs:
#   - datCredit_real | enriched credit dataset, fused with base macroeconomic variables
# =======================================================================================




# ------- 1. Apply exclusions on the credit dataset to increase available memory

ptm <- proc.time() # for runtime calculations (ignore)

# - Confirm prepared datasets are loaded into memory
if (!exists('datCredit_real')) unpack.ffdf(paste0(genPath,"creditdata_final3"), tempPath)
if (!exists('datMV')) unpack.ffdf(paste0(genPath,"datMV"), tempPath)
if (!exists('datExclusions')) unpack.ffdf(paste0(genObjPath,"Exclusions-TruEnd"), tempPath)

# - Population-level prevalence rate and record tally before any Exclusions
# NOTE: choose default prior probability as measure for evaluating impact
classPrior_Pop <- datCredit_real[, sum(DefaultStatus1, na.rm=T)]/datCredit_real[!is.na(DefaultStatus1), .N]
recCount_start <- datCredit_real[,.N]

# - Add a starting line for Exclusions table
datExclusions <- rbind(data.table(Excl_ID=NA, Reason="Base dataset", Impact_Account=0, Impact_Dataset=0, Impact_records=0),
                       datExclusions)

# - Define data structure for vectors wherein impacts of Exclusions are to be captured
excl_count <- datExclusions[!is.na(Excl_ID), .N]; vecExcl <- datExclusions[!is.na(Excl_ID), Excl_ID]
recCount_impact <- rep(0, excl_count); classPrior_remainRecs <- copy(recCount_impact); 
recCount_remain <- copy(recCount_impact); excl_impactRelat <- copy(recCount_impact)

# - Iterate through each listed Exclusion and assess impact
for (i in 1:excl_count) {
  recCount_impact[i] <- datCredit_real[ExclusionID == vecExcl[i], .N]
  recCount_remain[i] <- datCredit_real[ExclusionID > vecExcl[i] | ExclusionID == 0, .N]
  # [SANITY CHECK] Does record tallies remain logical and correct as we progress through ExclusionIDs?
  if (recCount_remain[i] == recCount_start - sum(recCount_impact[1:i])) cat('SAFE\t') else cat('ERROR\t') # TRUE = safe
  # Impact on event prevalence (Default prior probability)
  classPrior_remainRecs[i] <- datCredit_real[ExclusionID > vecExcl[i] | ExclusionID == 0, sum(DefaultStatus1, na.rm=T)] /
    datCredit_real[ExclusionID > vecExcl[i] | ExclusionID == 0 & !is.na(DefaultStatus1), .N] 
  # Relative impact on dataset (row-wise) given proposed sequential application of exclusions
  excl_impactRelat[i] <- recCount_impact[i] / recCount_remain[i]
}

# - Enrich Exclusions Table with new fields
datExclusions[, Records_Remain := c(recCount_start, recCount_remain)]
datExclusions[, Impact_Dataset_Cumul := c(NA, percent(excl_impactRelat, accuracy = 0.001))]
datExclusions[, ClassPrior_Remain := percent(c(classPrior_Pop, classPrior_remainRecs), accuracy = 0.001)]
datExclusions[, classPrior_Remain_Diff := c(NA,percent(diff(c(classPrior_Pop,classPrior_remainRecs)), accuracy = 0.001))]

# - Store experimental objects | Memory optimisation
pack.ffdf(paste0(genObjPath,"Exclusions-TruEnd-Enriched"), datExclusions);

# - Check the impact of the exclusions from script 2d | RECORD-LEVEL
(exclusions_credit <- datCredit_real[ExclusionID != 0, .N] / datCredit_real[, .N] * 100)
# Exclusions' impact: 5.88%

# - Now apply the exclusions
datCredit_real <- subset(datCredit_real, ExclusionID == 0); gc()

# - Successful?
cat( (datCredit_real[ExclusionID > 0, .N] == 0) %?% "SAFE: Exclusions applied successfully.\n" %:%
       "WARNING: Some Exclusions failed to apply.\n")

# - Remove unnecessary variables
datCredit_real <- subset(datCredit_real, select = -c(ExclusionID))





# ------- 2. Fuse the macroeconomic data with the credit data

# - Find intersection between fields in the credit dataset and the macroeconomic dataset
(overlap_flds <- intersect(colnames(datCredit_real), colnames(datMV))) # no overlapping fields except Date

# - Remove fields that will not likely be used in the eventual analysis/modelling of default risk, purely to save memory
names(datCredit_real)
datCredit_real <- subset(datCredit_real, 
                         select = -c(Age, #PerfSpell_Key, New_Ind, Max_Counter, Date_Origination, Principal,
                                     AccountStatus, #Instalment, Arrears, 
                                     DelinqState_g0, #DefaultStatus1, DefSpell_Num, TimeInDefSpell,
                                     DefSpell_LeftTrunc, DefSpell_Event, DefSpell_Censored,
                                     DefSpellResol_TimeEnd, #DefSpell_Age, DefSpellResol_Type_Hist,
                                     DefSpell_LastStart, ReceiptPV, LossRate_Real, #HasLeftTruncPerfSpell, 
                                     PerfSpell_LeftTrunc, PerfSpell_Event, PerfSpell_Censored,
                                     PerfSpell_TimeEnd, #PerfSpellResol_Type_Hist,
                                     Account_Censored, #Event_Time, Event_Type, HasLeftTruncDefSpell,
                                     HasTrailingZeroBalances, ZeroBal_Start, NCA_CODE, STAT_CDE, LN_TPE,
                                     #DefSpell_Key, DefSpell_Counter, PerfSpell_Counter,
                                     HasWOff, WriteOff_Amt, HasSettle, EarlySettle_Amt, # HasFurtherLoan, HasRedraw,
                                     HasClosure, CLS_STAMP, Curing_Ind, BOND_IND, Undrawn_Amt, # TreatmentID,
                                     slc_past_due_amt, WOff_Ind, EarlySettle_Ind, #PerfSpell_Num, PerfSpell_Age,
                                     FurtherLoan_Amt, FurtherLoan_Ind, Redraw_Ind, Redrawn_Amt, Repaid_Ind, HasRepaid)); gc()

# - Merge on Date by performing a left-join
datCredit_real <- merge(datCredit_real, datMV, by="Date", all.x=T); gc()

# - Create Interest Rate margin using the repo rate + 3.5% (Prime Rate's definition in South Africa)
datCredit_real <- datCredit_real %>% mutate(InterestRate_Margin = round(InterestRate_Nom - (M_Repo_Rate+0.035), digits=4)) %>%
  relocate(InterestRate_Margin, .after=InterestRate_Nom)

# - Validate merging success by checking for missingness (should be zero)
list_merge_variables <- list(colnames(datMV))
results_missingness <- list()
for (i in 1:length(list_merge_variables)){
  output <- sum(is.na(datCredit_real$list_merge_variables[i]))
  results_missingness[[i]] <- output
}
cat( (length(which(results_missingness > 0)) == 0) %?% "SAFE: No missingness, fusion with macroeconomic data is successful.\n" %:%
       "WARNING: Missingness in certain macroecnomic fields detected, fusion compromised.\n")

# - Cleanup
rm(datMV); gc()




# ------- 3. Feature Engineering that needs entire loan- and spell histories

# --- Create preliminary target/outcome variables for stated modelling objective
# NOTE: This particular field is instrumental to designing the subsampling & resampling scheme,
# as well as in tracking the impact of exclusions

# - Creating 12-month default indicators using the worst-ever approach
# NOTE: This step deliberately spans both performing and default spells
# NOTE: Need to specify a (k+1)-window for the "frollapply()" function, e.g., a 12-month outcome implies 13 elements
# Uses the custom function "imputLastKnown" defined in script 0
datCredit_real[, DefaultStatus1_lead_12_max := imputeLastKnown(frollapply(x=DefaultStatus1, n=13, align="left", FUN=max)), by=list(LoanID)]
datCredit_real$DefaultStatus1_lead_12_max %>% table() %>% prop.table() 
### RESULTS: 91.93% of observations have not defaulted in the next 12 months from reporting date, whilst 8.07% of accounts have.

# - Relocate variable next to current default status variable
datCredit_real <- datCredit_real %>% relocate(DefaultStatus1_lead_12_max, .after=DefaultStatus1)


# --- Delinquency-themed variables on a loan-level
# - Embed previous defaults into a new Boolean-valued input variable
datCredit_real[, PrevDefaults := ifelse(all(is.na(PerfSpell_Num)), F, max(PerfSpell_Num,na.rm = T) > 1), by=list(LoanID)]
cat( (datCredit_real[is.na(PrevDefaults), .N] == 0) %?% "SAFE: No missingness, [PrevDefaults] created successfully.\n" %:%
       "WARNING: Missingness detected, [PrevDefaults] compromised.\n")
describe(datCredit_real$PrevDefaults)
describe(datCredit_real[Counter==1, PrevDefaults])
### RESULTS: 11.6% of records had previous defaults, which is 6.9% of accounts

# - Spell-level indicator for when a shift occurs in the state of g0_Delinq (target event)
# NOTE: This is an intermediary field used in the creation of subsequent fields
datCredit_real[, g0_Delinq_Shift := ifelse(lag(g0_Delinq, n=1)==g0_Delinq,0,1), by=list(LoanID)]
datCredit_real[is.na(g0_Delinq_Shift), g0_Delinq_Shift := 0] # All first observations have g0_Delinq_Shift = NA; set these values to zero.
cat( (datCredit_real[is.na(g0_Delinq_Shift), .N] == 0) %?% "SAFE: No missingness, [g0_Delinq_Shift] created successfully.\n" %:%
       "WARNING: Missingness detected, [g0_Delinq_Shift] compromised.\n")
datCredit_real$g0_Delinq_Shift %>% table() %>% prop.table()
### RESULT: 96.05% of the records had no change in their delinquency level from their associated previous record.

# - Delinquency state number, where each change in g_0 denotes such a "state" that may span several periods
datCredit_real[, g0_Delinq_Num := cumsum(g0_Delinq_Shift) + 1, by=list(LoanID)] # Assign state numbers over the entire loan history (add one to ensure that there are no delinquency spell numbers equal to zero)
cat( (datCredit_real[is.na(g0_Delinq_Num), .N] == 0) %?% "SAFE: No missingness, [g0_Delinq_Num] created successfully.\n" %:%
       "WARNING: Missingness detected, [g0_Delinq_Num] compromised.\n")
describe(datCredit_real$g0_Delinq_Num)
### RESULT: Mean state number of 3.3 across all rows; median: 1; max of 100. 
# This high max suggests outlier-accounts with rapid and frequent changes in g0

# - Account-level standard deviation of the delinquency state
datCredit_real[, g0_Delinq_SD := sd(g0_Delinq, na.rm=T), by=list(LoanID)]
datCredit_real[is.na(g0_Delinq_SD), g0_Delinq_SD := 0] # Some missing values exist at loan accounts originating at the end of the sampling period | Assign zero values to these
cat( (datCredit_real[is.na(g0_Delinq_SD), .N] == 0) %?% "SAFE: No missingness, [g0_Delinq_SD] created successfully.\n" %:%
       "WARNING: Missingness detected, [g0_Delinq_SD] compromised.\n")
describe(datCredit_real[, list(g0_Delinq_SD=mean(g0_Delinq_SD, na.rm=T)), by=list(LoanID)]$g0_Delinq_SD)
### RESULT: mean account-level SD in delinquency states of 0.21; median: 0, but 95%-percentile of 1.2
# This suggests that most accounts do not vary significantly in their delinquency states over loan life, which is sensible

# - 4-,5-,6-,9- and 12 month rolling state standard deviation
# NOTE: Usefulness of each time window length will yet be determined during prototyping/modelling
datCredit_real[, g0_Delinq_SD_12 := frollapply(g0_Delinq, n=12, FUN=sd, align="right"), by=list(LoanID)]
datCredit_real[, g0_Delinq_SD_9 := frollapply(g0_Delinq, n=9, FUN=sd, align="right"), by=list(LoanID)]
datCredit_real[, g0_Delinq_SD_6 := frollapply(g0_Delinq, n=6, FUN=sd, align="right"), by=list(LoanID)]
datCredit_real[, g0_Delinq_SD_5 := frollapply(g0_Delinq, n=5, FUN=sd, align="right"), by=list(LoanID)]
datCredit_real[, g0_Delinq_SD_4 := frollapply(g0_Delinq, n=4, FUN=sd, align="right"), by=list(LoanID)]
cat( ((datCredit_real[is.na(g0_Delinq_SD_4), .N] == datCredit_real[Counter<4,.N]) &
      (datCredit_real[is.na(g0_Delinq_SD_5), .N] == datCredit_real[Counter<5,.N]) &
      (datCredit_real[is.na(g0_Delinq_SD_6), .N] == datCredit_real[Counter<6,.N]) &
      (datCredit_real[is.na(g0_Delinq_SD_9), .N] == datCredit_real[Counter<9,.N]) &
      (datCredit_real[is.na(g0_Delinq_SD_12), .N] == datCredit_real[Counter<12,.N])) %?% "SAFE: No excessive missingness, [g0_Delinq_SD_4], [g0_Delinq_SD_5], [g0_Delinq_SD_6], [g0_Delinq_SD_9], and [g0_Delinq_SD_12] created successfully.\n" %:%
        "WARNING: Excessive missingness detected, [g0_Delinq_SD_4], [g0_Delinq_SD_5], [g0_Delinq_SD_6], [g0_Delinq_SD_9], and/or [g0_Delinq_SD_12] compromised.\n")

# - Time in delinquency state
# NOTE: This variable is conceptually different to [TimeInPerfSpell].
# A performance spell starts when a loan is not in default and ends when it is in default.
# A delinquency spell starts when a loan "shifts" to a new delinquency level and ends the immediate period preceding the next shift to a different delinquency level.
datCredit_real[, TimeInDelinqState := 1:.N, by=list(LoanID, g0_Delinq_Num)]
cat( (datCredit_real[is.na(TimeInDelinqState), .N] == 0) %?% "SAFE: No missingness detected, [TimeInDelinqState] created successfully.\n" %:%
       "WARNING: Missingness detected, [TimeInDelinqState] compromised.\n")


# --- Delinquency-themed variables on a performance spell-level
# - Delinquency state number, where each change in g_0 denotes such a "state" that may span several periods during a performance spell
datCredit_real[!is.na(PerfSpell_Key), PerfSpell_g0_Delinq_Num := cumsum(g0_Delinq_Shift) + 1, by=list(PerfSpell_Key)] # Assign state numbers over each performance spell
# [SANITY CHECK] Check new feature for illogical values
cat( ( datCredit_real[is.na(PerfSpell_g0_Delinq_Num),.N]==datCredit_real[is.na(PerfSpell_Key),.N]) %?% 
       'SAFE: New feature [PerfSpell_g0_Delinq_Num] has logical values.\n' %:% 
       'WARNING: New feature [PerfSpell_g0_Delinq_Num] has illogical values \n' )

# - State standard deviation on the performance spell level
datCredit_real[!is.na(PerfSpell_Key), PerfSpell_g0_Delinq_SD := sd(g0_Delinq), by=list(PerfSpell_Key)]
datCredit_real[!is.na(PerfSpell_Key) & is.na(PerfSpell_g0_Delinq_SD), PerfSpell_g0_Delinq_SD := 0] # Assigning an standard deviation of zero to those performance spells that have an single observation
# [SANITY CHECK] Check new feature for illogical values
cat( ( datCredit_real[is.na(PerfSpell_g0_Delinq_SD),.N]==datCredit_real[is.na(PerfSpell_Key),.N]) %?% 
       'SAFE: New feature [PerfSpell_g0_Delinq_SD] has logical values.\n' %:% 
       'WARNING: New feature [PerfSpell_g0_Delinq_SD] has illogical values \n' )




# ------ 4. General cleanup & checks

# - remove intermediary fields, as a memory enhancement
datCredit_real[, g0_Delinq_Shift := NULL]

# - Clean-up
rm(list_merge_variables, results_missingness)

# - Save to disk (zip) for quick disk-based retrieval later
pack.ffdf(paste0(genPath, "creditdata_final4a"), datCredit_real); gc()
proc.time() - ptm # IGNORE: elapsed runtime
