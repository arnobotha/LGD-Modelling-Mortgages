# ============================== CUSTOM FUNCTIONS ==============================
# Defining custom functions used across various projects
# ------------------------------------------------------------------------------
# PROJECT TITLE: Loss Modelling (LGD) for Residential Mortgages
# SCRIPT AUTHOR(S): Dr Arno Botha, Marcel Muller

# DESCRIPTION:
# This script defines various functions that are used elsewhere in this project
# or, indeed, used across other projects. Functions are grouped thematically.
# ==============================================================================




calc_AIC_LS <- function(formula, data_train, variables="", it=NA, logPath="", 
                        fldSpellID="DefSpell_Key", modelType="tweedie") {
  # - Testing conditions
  # j<-1; formula<-TimeDef_Form(TimeDef,variables[j], strataVar=strataVar); variables<-variables[j]
  # data_train<-data_train; it=j; logPath=genPath;  fldSpellID=fldSpellID; modelType<-modelType
  
  tryCatch({
    if (modelType=="tweedie") {
      # - Tweedie model
      # Fit model
      model <- cpglm(formula,data= data_train)
      # Generate model predictions
      data_train$score <- predict(model, newdata=data_train, type="response")
      # Calculate AIC of the model
      AIC <- AIC(model)
      # Get model deviance
      Dev <- deviance(model)
      # Calculate C-index
      c_ind <- rcorr.cens(data_train$score, data_train$LossRate_Real)[["C Index"]]
      a<-summary(model)
      a$r
      
    } else if (modelType=="gaussian") {
      # - Gaussian model
      model <- glm(formula, family=gaussian(link="identity"), data=data_train)
      # Generate model predictions
      data_train$score <- predict(model, newdata=data_train, type="response")
      # Calculate AIC of the model
      AIC <- AIC(model)
      # Get model deviance
      Dev <- deviance(model)
      # Calculate C-index
      c_ind <- rcorr.cens(data_train$score, data_train$LossRate_Real)[["C Index"]]
      
    } else stop("Unknown model type in calc_AIC().")
    
    # - Output the number of models built, where the log is stored in a text file afterwards.
    if (!is.na(it)) {
      cat(paste0("\n\t ", it,") Single-factor model built. "),
          file=paste0(logPath,"AIC_log_LS.txt"), append=T)
    }
    
    # - Return results as a data.table
    if (modelType %in% c("tweedie","gaussian")) {
      return(data.table(Variable=variables, AIC=AIC, Deviance=Dev, C=c_ind,pValue=summary(model)$coefficients[1,4]))
    }
    
  }, error=function(e) {
    AIC <- Inf
    Dev <- Inf
    c_ind <- Inf
    if (!is.na(it)) {
      cat(paste0("\n\t ", it,") Single-factor model failed. "),
          file=paste0(logPath,"AIC_log_LS.txt"), append=T)
    }
    return(data.table(Variable = variables, AIC = AIC,Deviance= Dev, pValue=NA,C=c_ind)) 
  })
  
}


# --- Function to extract the Akaike Information Criterion (AIC) from single-factor models
# Input:  [data_train]: Training data; [data_valid]: [variables]: List of variables used to build single-factor models;
#         [fldSpellID]: Field name of spell-level ID; [TimeDef]: Time definition incorporated.
#         [numThreads]: Number of threads used; [genPath]: Optional path for log file. 
# Output: [matResults]: Result matrix.
aicTable_LS <- function(data_train, variables, fldSpellID="DefSpell_Key",
                        TimeDef, numThreads=6, genPath, strataVar="", modelType="tweedie") {
  # - Testing conditions
  # data_train<-datCredit_train
  # variables<-c("g0_Delinq_Any_Aggr_Prop", "g0_Delinq_Any_Aggr_Prop_Lag_1", "g0_Delinq_Any_Aggr_Prop_Lag_2")
  # fldSpellID<-" DefSpell_Key"; TimeDef<-c("Cox_Discrete","LossRate_Real"); numThreads<-6; genPath<-genObjPath
  # strataVar<-"DefSpell_Num"; modelType<-"Tweedie"
  
  # - Iterate across loan space using a multi-threaded setup
  ptm <- proc.time() #IGNORE: for computation time calculation
  cl.port <- makeCluster(round(numThreads)); registerDoParallel(cl.port) # multi-threading setup
  cat("New Job: Estimating AIC for each variable as a single-factor model ..",
      file=paste0(genPath,"AIC_log_LS.txt"), append=F)
  
  results <- foreach(j=1:length(variables), .combine='rbind', .verbose=F, .inorder=T,
                     .packages=c('data.table', 'survival','cplm','Hmisc'), .export=c('calc_AIC_LS', 'TimeDef_Form')) %dopar%
    { # ----------------- Start of Inner Loop -----------------
      # - Testing conditions
      # j <- 1
      calc_AIC_LS(formula=TimeDef_Form(TimeDef,variables[j], strataVar=strataVar), variables=variables[j],
                  data_train=data_train, it=j, logPath=genPath,  fldSpellID=fldSpellID, modelType=modelType)
    } # ----------------- End of Inner Loop -----------------
  stopCluster(cl.port); proc.time() - ptm  
  
  # Sort by concordance in ascending order.
  setorder(results, AIC)
  
  # Return resulting table.
  return(results)
}


# --- Function to evaluate a cpglm model's performance (no weights)
# Optional: supply model_base or let the function fit an intercept-only baseline.
# Optional: pass newdata for OOS MAE/RMSE.
evalLS <- function(model_full, dat_train, targetFld, model_base = NULL) {
  
  # - Unit test conditions | Internal
  # model_full<-modGLM_full; dat_train<-datCredit_train;
  # targetFld<-"LossRate_Real"; model_base<-modGLM_base
  
  # Get actuals
  y_tr  <- dat_train[[targetFld]]
  # Get predictions
  mu_tr <- predict(model_full, newdata=dat_train, type="response")
  
  # Calculate RMSE
  rmse_tr <- sqrt(mean((mu_tr - y_tr)^2, na.rm=T))
  # Calculate MAE
  mae_tr  <- mean(abs(mu_tr - y_tr), na.rm=T)
  # Get AIC of full model
  aic_full <- AIC(model_full)
  # Get AIC of base model
  aic_base <- AIC(model_base)
  # Get residual deviance of full model
  dev_full <- deviance(model_full)
  # Get residual deviance of baseline model
  dev_base <- deviance(model_base)
  
  # Calculate Pseudo R^2 (deviance explained): 1 - D_full / D_base
  SS_res <- sum((y_tr - mu_tr)^2)
  SS_tot <- sum((y_tr - mean(y_tr))^2)
  R2 <- 1 - (SS_res / SS_tot)
  
  # Combine results and return data.table
  data.table(AIC_full = aic_full,AIC_base = aic_base, dAIC = aic_full - aic_base,
             Deviance_full = dev_full,
             Deviance_base = dev_base,
             R2 = R2,
             Train_RMSE = rmse_tr,
             Train_MAE  = mae_tr
  )
}



# --- A stepwise selection procedure to select variables from a GLM with a Tweedie link function based on AICs
# Input:  [base_model]: An empty (intercept-only) GLM model with a Tweedie link function
#         [full_model]: A non-empty GLM model with a Tweedie link function
#         [data]: The data used to train the GLM model
#         [threshold]: The threshold for adding/removing variables to the model
#                      Set to the number of observations in the training dataset by default
#         [trace]: Optional parameter for tracing errors
# Output: <GLM model with a Tweedie link function>
stepwise_cpglm_both <- function(base_model, full_model, data, threshold = log(nrow(data)), trace = TRUE) {
  # - Unit test conditions | Internal
  # base_model<-modGLM_base; full_model<-modGLM_full; data<-datCredit_train
  # threshold<-log(nrow(data)); trace<-T
  
  # - Initialize variables
  best_model <- base_model
  best_formula <- formula(base_model)
  best_aic <- AIC(best_model)
  full_terms <- attr(terms(full_model), "term.labels")
  improved <- TRUE
  
  if (trace) cat("Initial AIC:", round(best_aic, 3), "\n")
  
  # --- Conduct stepwise selection until the criteria is satisfied
  while (improved) {
    improved <- FALSE
    candidate_models <- list()
    candidate_aics <- c()
    
    current_terms <- attr(terms(best_model), "term.labels")
    
    # ---- FORWARD STEP ----
    addable <- setdiff(full_terms, current_terms)
    for (term in addable) {
      # term <- addable[1]
      f_try <- update(best_formula, paste(". ~ . +", term))
      m_try <- try(cpglm(f_try, data=data), silent = TRUE)
      if (!inherits(m_try, "try-error") && !any(is.na(coef(m_try)))) {
        candidate_models[[paste0("+", term)]] <- m_try
        candidate_aics[paste0("+", term)] <- AIC(m_try)
        if (trace) cat("Tried +", term, "AIC:", round(AIC(m_try), 2), "\n")
      } else if (trace) cat("Skipped +", term, "(failed)\n")
    }
    
    # ---- BACKWARD STEP ----
    removable <- current_terms
    for (term in removable) {
      # term <- removable[1]
      f_try <- update(best_formula, paste(". ~ . -", term))
      m_try <- try(cpglm(f_try, data=data), silent=TRUE)
      if (!inherits(m_try, "try-error") && !any(is.na(coef(m_try)))) {
        candidate_models[[paste0("-", term)]] <- m_try
        candidate_aics[paste0("-", term)] <- AIC(m_try)
        if (trace) cat("Tried -", term, "AIC:", round(AIC(m_try), 2), "\n")
      } else if (trace) cat("Skipped -", term, "(failed)\n")
    }
    
    if (length(candidate_aics)==0) break
    
    # --- Choose best candidate
    best_op <- names(which.min(candidate_aics))
    best_aic_new <- min(candidate_aics)
    
    # --- Accept only if AIC improves sufficiently
    if (best_aic_new + threshold < best_aic) {
      best_model <- candidate_models[[best_op]]
      best_formula <- formula(best_model)
      best_aic <- best_aic_new
      improved <- TRUE
      if (trace) cat("??? Step:", best_op, "| AIC:", round(best_aic, 3), "\n")
    }
  }
  
  # --- Print progress to log
  if (trace) {
    cat("\nFinal model formula:\n")
    print(best_formula)
    cat("Final AIC:", round(best_aic, 3), "\n")
  }
  
  return(best_model)
}



