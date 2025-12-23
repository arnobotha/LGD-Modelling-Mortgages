# ================================== DATA PREPARATION ===================================
# Apply various advanced data treatments, in preparation for applyin the TruEnd-procedure
# ---------------------------------------------------------------------------------------
# PROJECT TITLE: Loss Modelling (LGD) for Residential Mortgages
# SCRIPT AUTHOR(S): Dr Arno Botha (AB), Marcel Muller (MM)
# ---------------------------------------------------------------------------------------
# DESCRIPTION:
# In continuing with script 2a, this script applies the following Advanced Data Treatments:
#   1)  Starting zero-balances; treated by finding first non-zero instance and moving the
#       loan's starting point accordingly, while marking the rest for exclusion.
#   2)  Unflagged account closures; treated by marking affected accounts as early settlements.
#   3)  Partially zero-valued [Principal] and missing [InterestRate_Nom]-cases; treated by
#       assigning/interleaving the first and last-known non-zero account-level.
#       [InterestRate_Nom]-value accordingly. Same for [Principal], if applicable (it's not)
#   4)  Identifying truly 'new' accounts; detected by new accounts disbursed but with 
#       starting Age > 1; treated by flagging detected accounts as 'new'.
# ---------------------------------------------------------------------------------------
# -- Script dependencies:
#   - 0.Setup.R
#   - 1.Data_Import.R
#   - 2a.Data_Prepare_Credit_Basic.R

# -- Inputs:
#   - datCredit_real | Cleaned version of input dataset (script 2a)
#   - various parameters set in the setup script 0
#
# -- Outputs:
#   - datCredit_real | Enhanced version of input dataset
# ---------------------------------------------------------------------------------------
# NOTE: This script predominantly comes from another project (Kasmeer). As such, all 
# references to associated Data Experiments reside within that project and will be taken 
# as established 'facts' within this project.
# =======================================================================================




# ------ 6. Advanced Data Treatments
ptm2 <- proc.time() # for runtime calculations (ignore)

# --- 0. Preliminaries

# - Confirm raw data is loaded into memory
if (!exists('datCredit_real')) unpack.ffdf(paste0(genPath,"creditdata_final1b"), tempPath)

# - Ensure any engineered features during treatments do not exist before starting advanced treatment
suppressWarnings( datCredit_real[, `:=`(TreatmentID = NULL, HasZeroBalance_Start = NULL, Counter_TruStart1 = NULL,
                                        Principal_LastNonzeroPos = NULL, Principal_FirstNonzeroPos = NULL,
                                        Principal_LastNonzero = NULL, Principal_FirstNonzero = NULL,
                                        ZeroBal_Start = NULL, HasTrailingZeroBalances = NULL,
                                        Var_Term = NULL, Term_Mode = NULL, HasZeroPrincipal = NULL)])

# - Similar to [ExclusionID], create an account-level field that will be flagged by the ID-value
# of the corresponding Advanced Data Treatment if affected.
datCredit_real[, TreatmentID := ""]; gc()


# --- 1. Advanced Data Treatment 1: Starting zero-balances | See Data Experiment 3

# - Preliminaries
datCredit_real[, HasZeroBalance_Start := max(ifelse(Counter == 1 & Balance == 0, 1, 0)), 
               by=list(LoanID)]

# [DIAGNOSTIC] Prevalence of accounts that start with a zero-valued balance?
diag.real3_1a <- datCredit_real[ExclusionID==0 & Counter==1 & HasZeroBalance_Start==1, .N] / 
  datCredit_real[ExclusionID==0 & Counter==1, .N] * 100 

# - Conditional treatment
if (diag.real3_1a > 0) {
  
  # Preparing treatment
  datCredit_real[HasZeroBalance_Start==1, Counter_TruStart1 := 
                   max(Counter[1], which(Balance > 0)[1]),
                 by=list(LoanID)]
  
  # [SANITY CHECK] Is the prepared treatment fit for purpose? Does it cover the affected data space?
  diag.real3_1d <- datCredit_real[ExclusionID==0 & Counter==1 & Counter_TruStart1 != Counter, .N] / 
    datCredit_real[ExclusionID==0 & Counter==1, .N] * 100 
  diag.real3_1d_abs <- datCredit_real[ExclusionID==0 & Counter<Counter_TruStart1, .N]
  diag.real3_1d_rec <- diag.real3_1d_abs / datCredit_real[ExclusionID==0, .N] * 100
  check_advTreat2a <- diag.real3_1d == diag.real3_1a # Should be TRUE
  cat( paste0("DETECTED: Zero-valued starting balances found in ", round(diag.real3_1a, digits=1),
              "% of accounts (", round(diag.real3_1d_rec,digits=1), "% of records). Preparing treatment ... \n",
              check_advTreat2a %?% 'SAFE: Treatment correctly prepared. Appplying treatment ...\n' %:% 
                'WARNING: Prepared treatment does not correspond 100% to the affected data space.\n') )
  
  # [TREATMENT] Mark the affected histories as an Exclusion
  datCredit_real[ExclusionID == 0 & HasZeroBalance_Start==1 & Counter<Counter_TruStart1, ExclusionID := 5]
  
  # [TREATMENT] Re-assign the affected counter fields accordingly using the true starting point
  datCredit_real[ExclusionID==0 & HasZeroBalance_Start==1, Max_Counter:= .N, by=list(LoanID)]
  datCredit_real[ExclusionID==0 & HasZeroBalance_Start==1, Counter:= 1:.N, by=list(LoanID)]
  
  # - Re-assign [Date_Origination] to be the newly-observed first record, purely
  # to facilitate the recalculation of loan [Age] accordingly. Although this
  # contaminates the previously observed field somewhat, information of this
  # event is recorded in the [TreatmentID] field.
  datCredit_real[ExclusionID==0 & HasZeroBalance_Start==1, 
                 Date_Origination := rollback(Date[1], roll_to_first = T)-days(1), by=list(LoanID)]
  datCredit_real[ExclusionID==0 & HasZeroBalance_Start==1, 
                 Age := interval(Date_Origination, Date) %/% months(1)]
  
  # - Mark the affected accounts with current treatment ID
  LoanIDs <- unique(datCredit_real[ExclusionID==0 & HasZeroBalance_Start==1, LoanID])
  datCredit_real[LoanID %in% LoanIDs, TreatmentID := paste0(TreatmentID, ";1")]
  
  # - Create and add exclusion impact to a common table
  datExcl <- data.table("Excl_ID"=5, "Reason"="Starting Zero-balances; partially excluded, the rest treated",
                        "Impact_Account" = diag.real3_1d, "Impact_Dataset" = diag.real3_1d_rec,
                        "Impact_records"=diag.real3_1d_abs) 
  if (exists('datExclusions')) datExclusions <- rbind(datExclusions, datExcl) else datExclusions <- datExcl
  
  # - Recalculate affected aggregates
  datCredit_real[ExclusionID==0, HasZeroBalance_Start := max(ifelse(Counter == 1 & Balance == 0, 1, 0)), 
                 by=list(LoanID)]
  
  # [SANITY CHECK] Account-level prevalence of accounts that start with a zero-valued balance?
  check_advTreat2b <- datCredit_real[ExclusionID==0 & Counter==1 & HasZeroBalance_Start == 1, .N] / 
    datCredit_real[ExclusionID==0 & Counter==1, .N] *100 == 0 # Should be true
  cat( check_advTreat2b %?% paste0('SAFE: Zero-valued starting balances succesfully treated by adjusting ',
                                   '\n\tthe true starting point, having excluded the zero-balance history at the start (', 
                                   round(diag.real3_1d_rec,digits=2), '% of records).\n') %:% 
         'WARNING: Failed to treat zero-valued starting balances.\n')
}

# - Cleanup
suppressWarnings(datCredit_real[, `:=`(HasZeroBalance_Start = NULL, Counter_TruStart1 = NULL)])



# --- 2. Advanced Data Treatment 2: Unflagged account closures | See Data Experiment 2

# [DIAGNOSTIC] Account-level prevalence of closures missed by [EarlySettle_Ind] or [WOff_Ind] or [Repaid_Ind] flags?
diag.real2_2h <- datCredit_real[ExclusionID==0 & Counter==1 & HasClosure==1 & HasSettle==0 & HasWOff==0 & HasRepaid==0, .N] / 
  datCredit_real[ExclusionID==0 & Counter==1, .N] *100 
diag.real2_2h_closures <- datCredit_real[ExclusionID==0 & Counter==1 & HasClosure==1 & HasSettle==0 & HasWOff==0 & HasRepaid==0, .N] / 
  datCredit_real[ExclusionID==0 & Counter==1 & (HasClosure==1 | HasSettle==1 | HasWOff==1 | HasRepaid==1), .N] *100 
diag.real2_2h_rec <- datCredit_real[ExclusionID==0 & HasClosure==1 & HasSettle==0 & HasWOff==0 & HasRepaid==0, .N] / 
  datCredit_real[ExclusionID==0, .N] *100 

# - Conditional treatment
if (diag.real2_2h > 0) {
  
  cat("DETECTED: Account closures unflagged by [EarlySettle_Ind], [WOff_Ind], or [Repaid_Ind].",
      "\n\tPrevalence: ", round(diag.real2_2h,digits=3), "% of accounts, ", round(diag.real2_2h_closures,digits=2), 
      "% of terminating accounts (those with a [CLS_STAMP]-value).\n\t", round(diag.real2_2h_rec,digits=2),
      "% of records affected.\n\tTreating affected accounts as early settlements ..\n")
  
  # [TREATMENT] Mark unaccounted closures as early settlements while redistributing any non-zero balances
  # NOTE: The associated [EarlySettle_Amt] and other fields will be curated later
  datCredit_real[ExclusionID==0 & Counter==Max_Counter & HasClosure==1 & HasSettle==0 & 
                   HasWOff==0 & HasRepaid==0, EarlySettle_Ind := 1]
  datCredit_real[ExclusionID==0 & Counter==Max_Counter & HasClosure==1 & HasSettle==0 & 
                   HasWOff==0 & HasRepaid==0, EarlySettle_Amt := EarlySettle_Amt + Balance]
  datCredit_real[ExclusionID==0 & Counter==Max_Counter & HasClosure==1 & HasSettle==0 & 
                   HasWOff==0 & HasRepaid==0, Balance := 0]
  
  # - Mark affected accounts with current treatment ID
  LoanIDs <- unique(datCredit_real[ExclusionID==0 & Counter==Max_Counter & HasClosure==1 & 
                                     HasSettle==0 & HasWOff == 0 & HasRepaid==0, LoanID])
  datCredit_real[LoanID %in% LoanIDs, TreatmentID := paste0(TreatmentID, ";2")]
  
  # - Recalculate affected aggregates
  datCredit_real[, HasSettle := max(EarlySettle_Ind, na.rm=T), by=list(LoanID)]
  
  # [SANITY CHECK] Account-level prevalence of closures unaccounted for by either [EarlySettle_Ind] or [WOff_Ind] flags?
  check_advTreat1 <- datCredit_real[ExclusionID==0 & Counter==1 & HasClosure==1 & HasSettle==0 & HasWOff==0 & HasRepaid==0, .N] / 
    datCredit_real[ExclusionID==0 & Counter==1, .N] *100 == 0 # Should be true
  cat( check_advTreat1 %?% 'SAFE: Unaccounted closures succesfully treated as early settlements.\n' %:% 
         'WARNING: Failed to treat unaccounted closures as early settlements.\n')
}



# --- 3. Advanced Data Treatment 3: Partially zero-valued [Principal] and missing [InterestRate_Nom]-cases
# See Data Experiment 5

# - Preliminaries
datCredit_real[ExclusionID==0, HasZeroPrincipal := any(Principal==0), by=list(LoanID)]
datCredit_real[ExclusionID==0, HasMissingInterest := any(is.na(InterestRate_Nom)), by=list(LoanID)]

# [DIAGNOSTIC] How prevalent are zero-valued Principals in parts of [Principal] within the wider dataset
diag.real5_1c <- datCredit_real[ExclusionID==0 & Counter==1 & HasZeroPrincipal==T, .N] / 
  datCredit_real[ExclusionID==0 & Counter==1 , .N] * 100  # account-level
diag.real5_1c_rec <- datCredit_real[HasZeroPrincipal==T, .N] / datCredit_real[, .N] * 100  # dataset-level

# - Conditional treatment | [Principal]
if (diag.real5_1c > 0) {
  
  cat(paste0("DETECTED: Partially zero-valued [Principal]-cases in ", round(diag.real5_1c,digits=2), 
             "% of accounts (", round(diag.real5_1c_rec,digits=2), " of records).",
             "\n\tTreating by assigning the last-known non-zero account-level [Principal]-value ...\n"))
  
  # - Find the account-level position of the last known non-zero Principal-value
  datCredit_real[ExclusionID==0 & HasZeroPrincipal==T, 
                 Principal_LastNonzeroPos := rev(which(Principal > 0))[1], 
                 by=list(LoanID)]
  
  # - Find the corresponding account-level last non-zero Principal-value itself
  datCredit_real[ExclusionID==0 & HasZeroPrincipal==T, 
                 Principal_LastNonzero := Principal[Principal_LastNonzeroPos], 
                 by=list(LoanID)]
  
  # [TREATMENT] Assign last known non-zero Principal-value to trailing zero-valued [Principal]-cases
  datCredit_real[ExclusionID==0 & HasZeroPrincipal==T & Counter>Principal_LastNonzeroPos,
                 Principal := Principal_LastNonzero]
  
  # - Find the account-level position of the first known non-zero Principal-value
  datCredit_real[ExclusionID==0 & HasZeroPrincipal==T, 
                 Principal_FirstNonzeroPos := which(Principal > 0)[1], 
                 by=list(LoanID)]
  
  # - Find the corresponding account-level first non-zero Principal-value itself
  datCredit_real[ExclusionID==0 & HasZeroPrincipal==T, 
                 Principal_FirstNonzero := Principal[Principal_FirstNonzeroPos], 
                 by=list(LoanID)]
  
  # [TREATMENT] Assign first known non-zero Principal-value to starting zero-valued [Principal]-cases
  datCredit_real[ExclusionID==0 & HasZeroPrincipal==T & Counter<Principal_FirstNonzeroPos,
                 Principal := Principal_FirstNonzero]
  
  # [TREATMENT] Assign the account-level mode of [Principal] to those remaining zero-valued cases
  datCredit_real[ExclusionID==0 & HasZeroPrincipal==T, 
                 Principal := ifelse(Principal > 0, Principal,
                                     getmode( .SD[Principal > 0, Principal]) # filter out zero-valued cases when getting mode
                 ), by=list(LoanID)]
  
  # - Mark the affected accounts with current treatment ID
  LoanIDs <- unique(datCredit_real[ExclusionID==0 & HasZeroPrincipal==T, LoanID])
  datCredit_real[LoanID %in% LoanIDs, TreatmentID := paste0(TreatmentID, ";3a")]
  
  # - Recalculate affected account-level aggregates
  datCredit_real[ExclusionID==0, HasZeroPrincipal := any(Principal==0), by=list(LoanID)]
  
  # [SANITY CHECK] Account-level prevalence of accounts that start with a zero-valued balance?
  check_advTreat3a <- datCredit_real[ExclusionID==0 & Counter==1 & HasZeroPrincipal==T, .N] / 
    datCredit_real[ExclusionID==0 & Counter==1, .N] *100 == 0 # Should be true
  cat( check_advTreat3a %?% 'SAFE: Zero-valued [Principal]-values succesfully treated by 
    interleaving the first and last known non-zero values accordingly, along with treating 
    the remaining cases with the account-level mode.\n' %:% 
         'WARNING: Failed to treat zero-valued [Principal]-values.\n')
}

# [DIAGNOSTIC] How prevalent is missingness in parts of [InterestRate_Nom] within the wider dataset
diag.real5_3b <- datCredit_real[Counter == 1 & HasMissingInterest==T, .N] / 
  datCredit_real[Counter == 1, .N] * 100  # account-level
diag.real5_3b_rec <- datCredit_real[ HasMissingInterest==T, .N] / datCredit_real[, .N] * 100  # dataset-level

# - Conditional treatment | [InterestRate_Nom]
if (diag.real5_3b > 0) {
  
  # - Conditional treatment
  cat(paste0("DETECTED: Partially missing [InterestRate_Nom]-cases in ", round(diag.real5_3b,digits=3), 
             "% of accounts (", round(diag.real5_3b_rec,digits=3), " of records).",
             "\n\tTreating by assigning the first/last-known non-zero account-level [InterestRate_Nom]-value ...\n"))  
  
  # - Mark the affected accounts with current treatment ID
  datCredit_real[ExclusionID==0 & is.na(InterestRate_Nom), TreatmentID := paste0(TreatmentID, ";3b")]
  
  # [TREATMENT] Back-fill starting missingness with the first non-missing element (if available),
  # then treat remaining partial missingness by imputing with the last-known non-missing element 
  datCredit_real[ExclusionID==0 & HasMissingInterest==T, InterestRate_Nom := 
                   imputeLastKnown(imputeFirstKnown(InterestRate_Nom)), by=list(LoanID)]
  
  # - Recalculate affected account-level aggregates
  datCredit_real[ExclusionID==0, HasMissingInterest := any(is.na(InterestRate_Nom)), by=list(LoanID)]
  
  # [SANITY CHECK] Account-level prevalence of accounts that start with a zero-valued balance?
  check_advTreat3b <- datCredit_real[ExclusionID==0 & Counter==1 & HasMissingInterest==T, .N] / 
    datCredit_real[ExclusionID==0 & Counter==1, .N] *100 == 0 # Should be true
  cat( check_advTreat3b %?% 'SAFE: Missing [InterestRate_Nom]-values succesfully treated by 
    interleaving the first and last known non-missing values accordingly.\n' %:% 
         'WARNING: Failed to treat missing [InterestRate_Nom]-values.\n')
}

# - Cleanup
suppressWarnings( datCredit_real[, `:=`(Principal_LastNonzeroPos = NULL, Principal_FirstNonzeroPos = NULL,
                                        Principal_LastNonzero = NULL, Principal_FirstNonzero = NULL,
                                        HasZeroPrincipal = NULL, HasMissingInterest = NULL)]  )



# --- 4. Advanced Data Treatment 4: Identifying truly 'new' accounts | See Data Experiment 7

# - Set parameters for New-indicator based on Balance-to-Principal and Age-to-Term
thresh_TermRatio <- 0.15
thresh_PrincipalRatio <- 0.9

# - Sample data for treatment purposes without contaminating the original dataset
treat_samp1 <- subset(datCredit_real, ExclusionID==0, 
                      select=c("LoanID", "Counter", "Age", "Date_Origination",
                               "Term", "Principal", "Balance", "ExclusionID", "TreatmentID"))

# - Basic feature engineering & account-level aggregates for analyses & treatments.
treat_samp1[Principal > 0, Principal_Ratio := Balance / Principal]
treat_samp1[, Term_Ratio := Age / Term]
treat_samp1[, First_Age := Age[1], by=list(LoanID)]

# [DIAGNOSTIC] Prevalence of older ages for seemingly new accounts that were
# disbursed during the sampling period?
diag.real7e <- treat_samp1[ExclusionID==0 & Counter==1 & Date_Origination >= minDate_observed & Age>0,.N] / 
  treat_samp1[ExclusionID==0 & Counter==1 & Date_Origination >= minDate_observed, .N] * 100 
diag.real7e_rec <- treat_samp1[ExclusionID==0 & Date_Origination >= minDate_observed & First_Age>0,.N] / 
  treat_samp1[ExclusionID==0 & Date_Origination >= minDate_observed, .N] * 100 

cat( paste0("Creating [New_Ind] to isolate truly 'new' accounts from data errors during the sampling period ..
            Thresholds selected: Age-to-Term <= ", round(thresh_TermRatio*100,digits=0), 
            "%; Balance-to-Principal >=", round(thresh_PrincipalRatio*100,digits=0), "%.\n"))

# [TREATMENT] Create an account-level new/old indicator based on derived decision rule
treat_samp2 <- treat_samp1 %>% filter(Counter==1) %>%
  mutate(New_Ind = ifelse(Date_Origination >= minDate_observed & 
                            (Age == 0 | (Term_Ratio <= thresh_TermRatio & Principal_Ratio >= thresh_PrincipalRatio)),
                          1, 0))

# [DIAGNOSTIC] Prevalence rate of new loans: a logic comparison of 'new'
diag.real7f_a <- treat_samp2[New_Ind == 1, .N] / treat_samp2[,.N] * 100 
diag.real7f_b <- treat_samp1[ExclusionID==0 & Counter==1 & Age==0,.N] / 
  treat_samp1[ExclusionID==0 & Counter==1, .N] * 100 

# [DIAGNOSTIC] Prevalence rate of new loans disbursed during sampling period?
diag.real7g <- treat_samp2[New_Ind == 1 & Date_Origination >= minDate_observed, .N] / 
  treat_samp2[Date_Origination >= minDate_observed,.N] * 100

# - Conditional Treatment
if (diag.real7e > 0 | diag.real7f_a != diag.real7f_b) {
  
  cat( paste0("DETECTED: Mis-aging of accounts disbursed during the sampling period with starting Age > 0.
              Prevalence: ", round(diag.real7e,digits=1), "% of new accounts.\n"))
  
  cat( paste0("DETECTED: Difference in prevalence rates of supposedly 'new' accounts in total sample.
              New accounts (old): ", round(diag.real7f_b,digits=1), "%. New accounts (using [New_Ind]): ",
              round(diag.real7f_a,digits=1), "%.\n") )
  
  # - Fuse account-level indicator [New_Ind] back into main longitudinal dataset
  datCredit_real <- merge(datCredit_real, treat_samp2[,list(LoanID, New_Ind)], by=c("LoanID"), all.x=T)
  
  # [SANITY CHECK] Treatment success?
  check_advTreat4a <- datCredit_real[ExclusionID==0 & Counter==1 & New_Ind==1, .N] / 
    datCredit_real[ExclusionID==0 & Counter==1, .N] * 100 == diag.real7f_a
  cat( check_advTreat4a %?% paste0('SAFE: Accounts that were disbursed during sampling period are successfully isolated as "new", \n',
                                   '\tdespite non-zero starting [Age]-values. This peculiarity is presumably ascribed to lagged disbursals, e.g., \n',
                                   '\tdue to lengthy bond registration processes.\n\tPrevalence of truly "new" accounts observed during sampling period: ', round(diag.real7g,digits=1), '%\n') %:% 
         'WARNING: Failed to isolate truly "new" accounts that were disbursed during sampling period.\n')
  
  # - Recalculate a new [Age] field, adjusted to start at 1 for survival modelling.
  datCredit_real[ExclusionID==0, Age_Adj := ifelse(New_Ind==0, Age, 1:.N),
                 by=list(LoanID)]
  
  # - Mark affected accounts with the current treatment ID
  LoanIDs <- subset(datCredit_real, ExclusionID==0 & Counter==1 & New_Ind==1 & 
                      Age != (Age_Adj), select="LoanID")
  datCredit_real[LoanID %in% LoanIDs$LoanID & ExclusionID==0, TreatmentID := paste0(TreatmentID, ";4")]
  
  # [SANITY CHECK] Are newly-disbursed accounts now correctly aged?
  diag.real7h <- datCredit_real[ExclusionID==0 & Counter==1 & Date_Origination >= minDate_observed & Age_Adj>1, .N] / 
    datCredit_real[ExclusionID==0 & Counter==1 & Date_Origination >= minDate_observed, .N] * 100
  check_advTreat4b <-  diag.real7h < diag.real7e/10 # Should be true
  cat( check_advTreat4b %?% 
         paste0('SAFE: Newly disbursed accounts (during sampling period) are now ', 
                'correctly aged for survival modelling.\n\tTreatment reduced the ',
                'original problem by at least 1000% (', round(diag.real7e / diag.real7h, digits=0),  
                ' times reduction in prevalence).\n') %:% 
         'WARNING: Failed to age newly disbursed accounts (during sampling period) for survival modelling.\n') 
  
  # - Reorder new fields
  datCredit_real <- datCredit_real %>% relocate(Age_Adj, New_Ind, .after=Age)
  
}

# - Cleanup
rm(treat_samp1, treat_samp2)

# - Save to disk (zip) for quick disk-based retrieval later
pack.ffdf(paste0(genPath,"creditdata_final1c"), datCredit_real)

# - Logging
cat('Data prepared (Advanced). Pending application (or not) of the TruEnd-procedure next ..\n')
proc.time() - ptm2 # IGNORE: elapsed runtime
