# ===================================== DATA ENRICH =====================================
# Use the macroeconomic dataset, performing various data preparation steps on it, and
# create features using the macroeconomic variables that can be used in model development
# ---------------------------------------------------------------------------------------
# PROJECT TITLE: Loss Modelling (LGD) for Residential Mortgages
# SCRIPT AUTHOR(S): Dr Arno Botha (AB), Esmerelda Oberholzer (EO)

# DESCRIPTION:
# This script prepares raw data into a more meaningful form to facilitate modelling.
# This preparation includes the following high-level steps:
#   1) creating some basic derived fields within macroeconomic datasets
#   2) removing redundant fields in macroeconomic datasets to optimise sizes
#   3) checking data grains and fusing datasets accordingly
#   4) Interleaving fused fields appropriately between either the left or right side 
#      during the previous merge
#   5) interpolating certain fields that represent important time series as a missing
#      value treatment that arose during merging quarterly with monthly data
#   6) checking and confirming that missingness has been successfully treated
#   7) subsetting data from pre-specified start point & scaling time series appropriately
# ---------------------------------------------------------------------------------------
# -- Script dependencies:
#   - 0.Setup.R
#   - 1.Data_Import.R
#   - 2a.Data_Prepare_Credit_Basic.R
#   - 2b.Data_Prepare_Credit_Advanced.R
#   - 2c.Data_Prepare_Credit_Advanced2.R
#   - 2d.Data_Enrich.R

# -- Inputs:
#   - various parameters set in the setup script 0
#   - macro_data_m | monthly macroeconomic data imported in script 1
#   - macro_data_q | quarterly macroeconomic data imported in script 1
#
# -- Outputs:
#   - datMV | enriched macroeconomic dataset, with various features
# ---------------------------------------------------------------------------------------
# NOTE: This script predominantly comes from another project (SICR-modelling), 
# but with a few changes to create more features, and enriched by the "C-FLI" project (Botha2020)
# =======================================================================================




# ------- 1. Macroeconomic data | Basic Data Cleaning & Checks
# Basic cleaning, light transforms, and confirming the supposed data grain

ptm <- proc.time() # for runtime calculations (ignore)


# -- ensure dates are correctly converted
macro_data_m[, Date_T := as.POSIXct(EffectiveDate, format="%Y-%m-%d")]
macro_data_q[, Date_T := as.POSIXct(EffectiveDate, format="%Y-%m-%d")]

# -- create YYYMM abstractions for easier reference
macro_data_m[, Period := year(Date_T)*100 + month(Date_T)]
macro_data_q[, Period := year(Date_T)*100 + month(Date_T)]

# -- remove redundant date fields 
# (these may be removed during initial data extraction within SAS, when engineering a proper data process)
macro_data_m[, `:=`(EffectiveDate = NULL, YEAR = NULL, Country = NULL, Source = NULL, Loaddatetime = NULL,
                    Process_Start_DateTime = NULL, Process_End_datetime = NULL, TABLE_freq = NULL)]
macro_data_q[, `:=`(EffectiveDate = NULL, Quarter = NULL, Country = NULL, Source = NULL, Loaddatetime = NULL,
                    Process_Start_DateTime = NULL, Process_End_DateTime = NULL, TABLE_freq = NULL)]

# the credit index has changed - remove forecasts
macro_data_m <- subset(macro_data_m, Scenario=='Historic')
macro_data_q <- subset(macro_data_q, Type=='Historic')

# [SANITY CHECK] Confirm all macroeconomic records to have no missingness in its supposed key (derived from EffectiveDate)
(check0 <- macro_data_m[is.na(Period), .N] == 0 & macro_data_q[is.na(Period), .N] == 0)
cat( check0 %?% 'SAFE: [Period]-key contains no missingness.\n' %:% 'WARNING: [Period]-key contains missingness.\n')

# -- Conditionally subset from all datasets to ensure we have non-zero values in all key fields
if (!check0) {
  macro_data_m <- subset(macro_data_m, !is.na(Period))
  macro_data_q <- subset(macro_data_q, !is.na(Period)) 
}

# - testing final data grains on proposed keys
cat( (macro_data_m[,list(Freqs = .N), by=list(Period, Scenario)][Freqs > 1, .N] == 0) %?% 'SAFE: Grain confirmed.\n' %:% 
       paste0('ERROR, grain broken for ', macro_data_m[,list(Freqs = .N), by=list(Period, Scenario)][Freqs > 1,.N], " cases.\n")
)

cat( (macro_data_q[,list(Freqs = .N), by=list(Period, Scenario)][Freqs > 1,.N] == 0) %?% cat('SAFE: Grain confirmed.\n') %:% 
       cat(paste0('ERROR, grain broken for ', macro_data_q[,list(Freqs = .N), by=list(Period, Scenario)][Freqs > 1,.N], " cases.\n"))
)

# - grains passed, create a single primary key
macro_data_m[, Key := paste0(Period,"-",Scenario)]
macro_data_q[, Key := paste0(Period,"-",Scenario)]





# --------- 2. Data fusion & Interleaving & Interpolation
# Merging monthly and quarterly data together and interleaving
# fields accordingly. Also a bit of interpolation on some time series

# - fuse data using a simple left join between monthly and quarterly
macro_data <- merge(macro_data_m, macro_data_q, by=c("Key"), all.x=T)

# - simple coalesces and interleaving transformations for one-sided missingness
# uses custom interpolation function "interleave()" defined in 0.Setup
macro_data[, Date_T := interleave(Date_T.x, Date_T.y, na.value = as.POSIXct(NA), pref='X') ]
macro_data[, Period := interleave(Period.x, Period.y, na.value = as.double(NA), pref='X') ]
macro_data[, Type := interleave(Type.x, Type.y, na.value = as.character(NA), pref='X') ]
macro_data[, Scenario := interleave(Scenario.x, Scenario.y, na.value = as.character(NA), pref='X') ]

# - remove fields made redundant due to fusion
suppressWarnings(macro_data[, `:=`(Date_T.x = NULL, Date_T.y = NULL, Type.x = NULL, Type.y = NULL, 
                                   Scenario.x = NULL, Scenario.y = NULL, Period.x = NULL, Period.y = NULL,
                                   Probability.x = NULL, Probability.y = NULL)])

# - create quarterly key, given that Date_T refers to last date of each month
# and that quarterly data points are retrospective
macro_data[, Period_Qtr := case_when(
  month(Date_T) >= 1 & month(Date_T) < 4 ~ paste0(year(Date_T), "Q1"),
  month(Date_T) >= 4 & month(Date_T) < 7 ~ paste0(year(Date_T), "Q2"),
  month(Date_T) >= 7 & month(Date_T) < 10 ~ paste0(year(Date_T), "Q3"),
  month(Date_T) >= 10 ~ paste0(year(Date_T), "Q4")
)]

# - Missing value treatment: Quarterly data have missing values for interleaved months. 
# We can linearly interpolate for the in-between months
# uses custom interpolation function "interPol()" defined in 0.Setup
macro_data[, Employment_Growth_YoY := interPol(Employment_Growth_YoY), by=list(Scenario)]
macro_data[, Household_debt_Level_income := interPol(Household_debt_Level_income), by=list(Scenario)]
macro_data[, Household_DSC_Level_income := interPol(Household_DSC_Level_income), by=list(Scenario)]
macro_data[, RealGDP_Growth_yoy := interPol(RealGDP_Growth_yoy), by=list(Scenario)]
macro_data[, Consumption_Growth_yoy := interPol(Consumption_Growth_yoy), by=list(Scenario)]
macro_data[, Durables_Growth_yoy := interPol(Durables_Growth_yoy), by=list(Scenario)]
macro_data[, Nominal_GDP_Growth_yoy := interPol(Nominal_GDP_Growth_yoy), by=list(Scenario)]
macro_data[, Nominal_income_Growth_yoy := interPol(Nominal_income_Growth_yoy), by=list(Scenario)]
macro_data[, Real_income_Growth_YoY := interPol(Real_income_Growth_YoY), by=list(Scenario)]

# [SANITY CHECK] Confirm successful treatment, considering previous sanity check
check2 <- all(is.na(macro_data$Employment_Growth_YoY) == F) & all(is.na(macro_data$Household_debt_Level_income) == F) & 
  (all(is.na(macro_data$Household_DSC_Level_income) == F)) & all(is.na(macro_data$RealGDP_Growth_yoy) == F) & 
  all(is.na(macro_data$Consumption_Growth_yoy) == F) & (all(is.na(macro_data$Durables_Growth_yoy) == F)) &
  (all(is.na(macro_data$Nominal_GDP_Growth_yoy) == F)) & (all(is.na(macro_data$Nominal_income_Growth_yoy) == F)) &
  (all(is.na(macro_data$Real_income_Growth_YoY) == F))
cat( check2 %?% 'SAFE: Interpolation successful with no residual missingness where relevant.\n' %:% 'WARNING: Residual missingness detected, treatment failed.\n')

# -- remove useless macroeconomic variables that are currently not forecast
macro_data[, `:=`(rbqn_rb1419w = NULL, HPI_Level_EOP = NULL, HPI_Level_SA = NULL, HPI_level_SA_MoM_Change = NULL,
                  sahp_fnbhpgp = NULL, sahp_fnbhpwc = NULL, sahp_fnbhpkzn = NULL, sahp_fnbhpec = NULL, sahp_fnbhpoth = NULL,
                  rbqn_rb5339m = NULL)]





# --------- 3. Missing Value Treatments & Scaling
# Subsetting only relevant periods (and fields), apply missing value treatments, and scale domains

# --- a. Subsetting

# - subet only relevant macroeconomic variables (chosen by discretion) from historic data (exclude forecasts) beyond a certain point
data.start <- "1980-01-01" # other (tested) options include 1980, 1999, 2005, 2010
macro_data_hist <- subset(macro_data, Type == "Historic" & Date_T >= data.start)[, list(Key, Period, Period_Qtr, Date_T, 
                                                                                        Inflation = Inflation_Growth_YoY,
                                                                                        Repo_Rate = Repo_rate_level_eop,
                                                                                        HousePriceIndex_Rate = HPI_Growth_Yoy_perc,
                                                                                        Employment_Rate = Employment_Growth_YoY,
                                                                                        DebtServiceCosts_Rate = Household_DSC_Level_income,
                                                                                        DebtToIncome_Rate = Household_debt_Level_income,
                                                                                        RealGDP_Rate = RealGDP_Growth_yoy,
                                                                                        NominalGDP_Rate = Nominal_GDP_Growth_yoy,
                                                                                        RealIncome_Rate = Real_income_Growth_YoY,
                                                                                        NominalIncome_Rate = Nominal_income_Growth_yoy,
                                                                                        Consumption_Rate = Consumption_Growth_yoy,
                                                                                        Durables_Rate = Durables_Growth_yoy)]

# --- b. Missing value treatments

# - quickly investigate any missings by conducting high-level distribution analysis 
describe(macro_data_hist[, list(Inflation, Repo_Rate, HousePriceIndex_Rate, Employment_Rate, DebtServiceCosts_Rate, DebtToIncome_Rate, RealGDP_Rate,
                                NominalGDP_Rate, RealIncome_Rate, NominalIncome_Rate, Consumption_Rate, Durables_Rate)])
# -- Results: Some series have missing values (not previously treated during quarterly-monthly fusion)

# - Interpolate all remaining macroeconomic variables as a failsafe against missing values in some months
# uses custom interpolation function "interPol()" defined in 0.Setup
macro_data_hist[, Inflation := interPol(Inflation)]
macro_data_hist[, Repo_Rate := interPol(Repo_Rate)]
macro_data_hist[, HousePriceIndex_Rate := interPol(HousePriceIndex_Rate)]

# - check success of treatment
check3 <- !any(is.na(macro_data_hist[, list(Inflation, Repo_Rate, HousePriceIndex_Rate, Employment_Rate, DebtServiceCosts_Rate, DebtToIncome_Rate, RealGDP_Rate,
                                            NominalGDP_Rate, RealIncome_Rate, NominalIncome_Rate, Consumption_Rate, Durables_Rate)]))
# Treatment worked as expected (FALSE). No more missing values.
cat( check3 %?% 'SAFE: Interpolation successful with no residual missingness where relevant.\n' %:% 'WARNING: Residual missingness detected, treatment failed.\n')





# --------- 4. Subsetting
# Subset macroeconomic fields and carry out light data preparation
# We only want the following macroeconomic variables (MVs), as found to be significant by Botha et al. (2020) during 
# the "C-FLI" project
# - Real income growth rate
# - Real GDP growth rate
# - Repo rate (not scaled as we want to use it for a new variable)
# - Employment index growth rate
# - Household DDI ratio
# - Inflation growth rate

# - Subsample monthly historical macroeconomic information with some light data preparation
datMV <- macro_data_hist[,list(Date=as.Date(Date_T, format="%Y-%m-%d"),
                                        M_Repo_Rate = Repo_Rate/100, 
                                        M_Inflation_Growth = round(Inflation/100,digits=4),
                                        M_DTI_Growth = round(DebtToIncome_Rate/100,digits=4),
                                        M_Emp_Growth = round(Employment_Rate/100,digits=4),
                                        M_RealGDP_Growth = round(RealGDP_Rate/100,digits=4),
                                        M_RealIncome_Growth = round(RealIncome_Rate/100,digits=4))]





# ------ 5. General cleanup & checks

# - Clean-up
rm(macro_data, macro_data_hist, macro_data_m, macro_data_q)

# - Save to disk (zip) for quick disk-based retrieval later
pack.ffdf(paste0(genPath, "datMV"), datMV); gc()
proc.time() - ptm # IGNORE: elapsed runtime
