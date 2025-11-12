


calc_AIC_LS <- function(formula, data_train, variables="", it=NA, logPath="", 
                        fldSpellID="DefSpell_Key", modelType="tweedie") {
  # - Testing conditions
  # j <- 1; formula=TimeDef_Form(TimeDef,variables[j], strataVar=strataVar); 
  
  tryCatch({
    if (modelType=="tweedie") {
      model <- cpglm(formula,data= data_train)# Fit tweedie model 
    } else if (modelType=="Cox_Discrete") {
      model <- glm(formula,data = data_train, family="binomial") # Fit discrete-time Cox model 
    } else stop("Unknown model type in calc_AIC().")
    
    if (!is.na(it)) {# Output the number of models built, where the log is stored in a text file afterwards.
      cat(paste0("\n\t ", it,") Single-factor model built. "),
          file=paste0(logPath,"AIC_log_LS.txt"), append=T)
    }
    data_train$score <- predict(model, newdata= data_train, type="response")
    AIC <- AIC(model) # Calculate AIC of the model.
    Dev <- deviance(model)
    c_ind <- rcorr.cens(data_train$score, data_train$LossRate_Real)[["C Index"]]
    # Return results as a data.table
    if (modelType=="tweedie") {
      return(data.table(Variable = variables, AIC = AIC, Deviance= Dev, C=c_ind,pValue=summary(model)$coefficients[1,4]))
    } else if (modelType=="Cox_Discrete") {
      return(data.table(Variable = variables, AIC = AIC,Deviance= Dev,,C=c_ind, pValue=summary(model)$coefficients[1,4]))
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
  # data_train <- datCredit; TimeDef=Cox_Discrete","PerfSpell_Event; numThreads=6
  # fldSpellID<-" DefSpell_Key"; variables<-"g0_Delinq_SD_4"; strataVar="DefSpell_Num"
  
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


# Evaluate Tweedie cpglm performance (no weights)
#), MAE, RMSE
# Optional: supply model_base or let the function fit an intercept-only baseline.
# Optional: pass newdata for OOS MAE/RMSE.
evalLS <- function(model_full,dat_train, targetFld,model_base = NULL) {
  
  
  y_tr  <- dat_train[[targetFld]]
  mu_tr <- predict(model_full, newdata = dat_train, type = "response")
  rmse_tr <- sqrt(mean((mu_tr - y_tr)^2))
  mae_tr  <- mean(abs(mu_tr - y_tr))
  
  aic_full <- AIC(model_full)
  aic_base <- AIC(model_base)
  dev_full <- deviance(model_full)        # residual deviance of full model
  dev_base <- deviance(model_base)        # residual deviance of baseline
  
  # Pseudo R^2 (deviance explained): 1 - D_full / D_base
  SS_res <- sum((y_tr - mu_tr)^2)
  SS_tot <- sum((y_tr - mean(y_tr))^2)
  
  R2 <- 1 - (SS_res / SS_tot)
  data.table(AIC_full = aic_full,AIC_base = aic_base, dAIC = aic_full - aic_base,
             Deviance_full = dev_full,
             Deviance_base = dev_base,
             R2 = R2,
             Train_RMSE = rmse_tr,
             Train_MAE  = mae_tr
  )
}




stepwise_cpglm_both <- function(base_model, full_model, data, threshold = log(nrow(data)), trace = TRUE) {
  # Initialize
  best_model <- base_model
  best_formula <- formula(base_model)
  best_aic <- AIC(best_model)
  full_terms <- attr(terms(full_model), "term.labels")
  
  if (trace) cat("Initial AIC:", round(best_aic, 3), "\n")
  
  improved <- TRUE
  while (improved) {
    improved <- FALSE
    candidate_models <- list()
    candidate_aics <- c()
    
    current_terms <- attr(terms(best_model), "term.labels")
    
    # ---- FORWARD STEP ----
    addable <- setdiff(full_terms, current_terms)
    for (term in addable) {
      f_try <- update(best_formula, paste(". ~ . +", term))
      m_try <- try(cpglm(f_try, data = data), silent = TRUE)
      if (!inherits(m_try, "try-error") && !any(is.na(coef(m_try)))) {
        candidate_models[[paste0("+", term)]] <- m_try
        candidate_aics[paste0("+", term)] <- AIC(m_try)
        if (trace) cat("Tried +", term, "AIC:", round(AIC(m_try), 2), "\n")
      } else if (trace) cat("Skipped +", term, "(failed)\n")
    }
    
    # ---- BACKWARD STEP ----
    removable <- current_terms
    for (term in removable) {
      f_try <- update(best_formula, paste(". ~ . -", term))
      m_try <- try(cpglm(f_try, data = data), silent = TRUE)
      if (!inherits(m_try, "try-error") && !any(is.na(coef(m_try)))) {
        candidate_models[[paste0("-", term)]] <- m_try
        candidate_aics[paste0("-", term)] <- AIC(m_try)
        if (trace) cat("Tried -", term, "AIC:", round(AIC(m_try), 2), "\n")
      } else if (trace) cat("Skipped -", term, "(failed)\n")
    }
    
    if (length(candidate_aics) == 0) break
    
    # Choose best candidate
    best_op <- names(which.min(candidate_aics))
    best_aic_new <- min(candidate_aics)
    
    # Accept only if AIC improves sufficiently
    if (best_aic_new + threshold < best_aic) {
      best_model <- candidate_models[[best_op]]
      best_formula <- formula(best_model)
      best_aic <- best_aic_new
      improved <- TRUE
      if (trace) cat("??? Step:", best_op, "| AIC:", round(best_aic, 3), "\n")
    }
  }
  
  if (trace) {
    cat("\nFinal model formula:\n")
    print(best_formula)
    cat("Final AIC:", round(best_aic, 3), "\n")
  }
  
  return(best_model)
}



