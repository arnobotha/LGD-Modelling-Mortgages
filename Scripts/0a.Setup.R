# =================================== SETUP =============================================
# Setting up R environment, parameters, and function definitions
# ---------------------------------------------------------------------------------------
# PROJECT TITLE: Loss Modelling (LGD) for FNB Mortgages
# SCRIPT AUTHOR(S): Dr Arno Botha

# DESCRIPTION: 
# This script installs and loads various libraries and packages, compiles all
# custom functions, and set requisite parameters.
# ---------------------------------------------------------------------------------------
# -- Inputs:
#   - DelinqM.R | Delinquency measures and related functions
# =======================================================================================



# ================ 0. Library setup

# ------ Install and load packages
# - data access and big data management
require(haven) # for SAS imports
require(ETLUtils)
require(ffbase)
tempPath <- "C:/TempData"; options("fftempdir"=tempPath)

# for data wrangling
require(tidyr)
require(dplyr)
require(data.table)
require(lubridate)
require(readr)
require(bit64) # for very big numeric values
require(stringr) # common string operations, e.g, str_pad
require(purrr) # mapping functions from tidyverse in working with matrices, lists

# for advanced looping functionality in simulation tasks
require(doBy)
require(foreach)
require(doParallel)

# for analyses
require(Hmisc)
require(moments) # for using skewness() function
require(regclass) # for VIF

# for modelling
require(survival) # for survival modelling
require(zoo)
require(car)
require(survivalROC) # for time-dependent ROC-analysis from Heagerty et al.
#require(survAUC) # for time-dependent ROC-analysis (alternative from Potapov et al.)
#require(tdROC) # for time-dependent ROC-analysis ([outdated?] alternative from Li et al.)
#require(timeROC) # for time-dependent ROC-analysis from Blanche2013 (disavowed in script 0b(iii)). DO NOT USE IN CREDIT DOMAIN
#require(risksetROC) # for time-dependent ROC-analysis (I/D Cox regression method from Heagerty, P.J., Zheng Y. (2005))
require(pROC); require(ROCR) # both for cross-sectional ROC-analysis (main:pROC)
require(MASS)
require(sandwich) # for robust variance estimators when using weights in glm()
require(lmtest) # for coeftest() "summary" given robust variance estimators

#for plots
require(ggplot2)
require(ggpp) # Extensions to ggplot2, particularly geom_table
require(scales)
require(ggthemes)
require(RColorBrewer)
require(extrafont) #remotes::install_version("Rttf2pt1", version = "1.3.8"); Sys.setenv(R_GSCMD="C:/Program Files/gs/gs9.55.0/bin/gswin32c.exe"); font_import(); loadfonts(); loadfonts(device="win")
require(survminer)
require(gridExtra)
require(corrplot)
#require(Metrics)



# ================ 1. Parametrisation

# - general R options
options(scipen=999) # Suppress showing scientific notation

# - Parameters used in calculating delinquency measures
sc.Thres <- 0.9; # repayment ratio - g1
d <- 3 # default threshold for g0/g1-measures of delinquency (payments in arrears)
k <- 6 # Probation period


# -- Path variables | General

# - Common path for saving big data objects
genPath <- "C:/Data/LossModelling-Mortgages_Data/"

# - Common path for importing raw data
genRawPath <- "C:/Data/"


# -- Path variables | User-dependent

if (Sys.getenv("USERNAME") == "WRQ") {
  # - Custom path where R-scripts are saved
  path_cust <- "C:/Users/WRQ/OneDrive - FRG/Analytix/Research/LossModelling-Mortgages/Scripts/"
  
  # - Common path for storing important R-objects as back-up
  genObjPath <- "C:/Users/WRQ/OneDrive - FRG/Analytix/Research/LossModelling-Mortgages/Objects/"
  
  # - Common path for saving important analytics (e.g., sampling)
  genFigPath <- "C:/Users/WRQ/OneDrive - FRG/Analytix/Research/LossModelling-Mortgages/Figures/"
  
} else if (Sys.getenv("USERNAME") == "Arno Botha") {
  # - Custom path where R-scripts are saved
  
  path_cust <- "E:/WorkLife/Analytix/Research/LossModelling-Mortgages/Scripts/"
  
  # - Common path for storing important R-objects as back-up
  genObjPath <- "E:/WorkLife/Analytix/Research/LossModelling-Mortgages/Objects/"
  
  # - Common path for saving important analytics (e.g., sampling)
  genFigPath <- "E:/WorkLife/Analytix/Research/LossModelling-Mortgages/Figures/"
  
  # - Common path for saving big data objects
  genPath <- "E:/DataDump/FNB SLC/LossModelling-Mortgages_Data/"
  
  # - Common path for importing raw data
  genRawPath <- "E:/DataDump/FNB SLC/"
  
}  else if (Sys.getenv("USERNAME") == "R5667372") {
  # - Custom path where R-scripts are saved
  
  path_cust <- "C:/Repo/LGD-Modelling-Mortgages/Scripts/"
  
  # - Common path for storing important R-objects as back-up
  genObjPath <- "C:/Repo/LGD-Modelling-Mortgages/Objects/"
  
  # - Common path for saving important analytics (e.g., sampling)
  genFigPath <- "C:/Repo/LGD-Modelling-Mortgages/Figures/"
  
  # - Common path for saving big data objects
  genPath <- "C:/Datadump/Project_Data/"
  
  # - Common path for importing raw data
  genRawPath <- "C:/Datadump/"
  
}else if (Sys.getenv("USERNAME") == "S37596233e") {
  # - Custom path where R-scripts are saved
  
  path_cust <- "C:/Users/gudu/Documents/Repo/LGD-Modelling-Mortgages/Scripts/"
  
  # - Common path for storing important R-objects as back-up
  genObjPath <- "C:/Users/gudu/Documents/Repo/LGD-Modelling-Mortgages/Objects/"
  
  # - Common path for saving important analytics (e.g., sampling)
  genFigPath <- "C:/Users/gudu/Documents/Repo/LGD-Modelling-Mortgages/Figures/"
  
  # - Common path for saving big data objects
  genPath <- "C:/Users/gudu/Documents/Datadump"
  
  # - Common path for importing raw data
  genRawPath <- "H:/bwisas-em-win1/userdata/FNB-DATA-MORTGAGES/"
  
} else {
  stop("User-specific paths not set for current user: ", Sys.getenv("USERNAME"), ". Please fix in Setup script (0.Setup.R) before continuing")
}

# ================ 2. Custom functions

# ------ Custom function definitions
# - Load all custom functions defined in a separate R-script
source(paste0(path_cust,"0b.CustomFunctions_General.R"))
source(paste0(path_cust,"0c.Custom_Functions_VarSelect.R"))
# - Compile Delinquency Calculation Functions (CD, MD/DoD)
source(paste0(path_cust,'DelinqM.R'))

# - Compile the TruEnd-suite of evaluation (and auxiliary) functions
source(paste0(path_cust,'TruEnd.R'))
