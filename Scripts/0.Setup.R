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
require(ff)
tempPath <- "C:/TempData"; options("fftempdir"=tempPath)

# for data wrangling
require(tidyr)
require(dplyr)
require(data.table)
require(lubridate)
require(readr)
require(bit64) # for very big numeric values
require(foreach); require(doParallel) # for multi-threaded computing
require(stringr) # common string operations, e.g, str_pad
require(purrr) # mapping functions from tidyverse in working with matrices, lists
require(writexl) #for exporting to Excel
require(zoo)

# for analyses & modelling
require(Hmisc)
require(survival) # for survival modelling
require(pROC); require(ROCR) # both for conducting ROC-analyses

#for plots
require(ggplot2)
require(corrplot) # For correlation plots
require(scales)
require(ggthemes)
require(ggpp) # Extensions to ggplot2, particularly geom_table
require(RColorBrewer)
require(extrafont) #remotes::install_version("Rttf2pt1", version = "1.3.8"); Sys.setenv(R_GSCMD="C:/Program Files/gs/gs9.55.0/bin/gswin32c.exe"); font_import(); loadfonts(); loadfonts(device="win")
require(survminer)
require(gridExtra)



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
  
} else {
  stop("User-specific paths not set for current user: ", Sys.getenv("USERNAME"), ". Please fix in Setup script (0.Setup.R) before continuing")
}



# ================ 2. Custom functions

# ------ Custom function definitions
# - Load all custom functions defined in a separate R-script
source(paste0(path_cust,"0a.CustomFunctions.R"))

# - Compile Delinquency Calculation Functions (CD, MD/DoD)
source(paste0(path_cust,'DelinqM.R'))

# - Compile the TruEnd-suite of evaluation (and auxiliary) functions
source(paste0(path_cust,'TruEnd.R'))
