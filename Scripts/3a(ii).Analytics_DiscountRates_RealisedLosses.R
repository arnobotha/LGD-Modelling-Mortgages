# ============================== Discount Rate Comparison ================================
# Analyse discount rates and subsequent realised losses across a few calculation methods
# ---------------------------------------------------------------------------------------
# PROJECT TITLE: TruEnd-procedure
# SCRIPT AUTHOR(S): Dr Arno Botha
# ---------------------------------------------------------------------------------------
# -- Script dependencies:
#   - 0.Setup.R
#   - 1.Data_Import.R
#   - 2a.Data_Prepare_Credit_Basic.R
#   - 2b.Date_Prepare_Credit_Advanced.R
#   - 2c(i).Data_Prepare_Credit_TruEnd.R
#   - 2c(ii).Data_Prepare_Credit_NoTruEnd.R
#   - 2d(i).Data_Enrich_TruEnd.R
#   - 2d(ii).Data_Enrich_NoTruEnd.R
#
# -- Inputs:
#   - datCredit_real | Enhanced version of input dataset (script 2b)
#
# -- Outputs:
#   - <analytics>
# =======================================================================================




# ------ 1. Preliminaries

### AB: Apply 3 candidates from script 3b(i) and conduct distributional analysis on overall loss rates

