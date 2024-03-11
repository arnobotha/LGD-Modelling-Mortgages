# ============================== Discount Rate Comparison ================================
# Analyse discount rates and subsequent realised losses across a few calculation methods
# ---------------------------------------------------------------------------------------
# PROJECT TITLE: Loss Modelling (LGD) for FNB Mortgages
# SCRIPT AUTHOR(S): Dr Arno Botha
# ---------------------------------------------------------------------------------------
# -- Script dependencies:
#   - 0.Setup.R
#   - 1.Data_Import.R
#   - 2a.Data_Prepare_Credit_Basic.R
#   - 2b.Date_Prepare_Credit_Advanced1.R
#   - 2c.Data_Prepare_Credit_Advanced2.R
#   - 2d.Data_Enrich.R
#   - 2e.Data_Prepare_Macro.R
#   - 2f.Data_Fusion1.R
#
# -- Inputs:
#   - datCredit_real | Enhanced version of input dataset (script 2f)
#
# -- Outputs:
#   - <analytics>
# =======================================================================================




# ------ 1. Preliminaries

### AB: Apply 3 candidates from script 3b(i) and conduct distributional analysis on overall loss rates

