

# --- Function to return the appropriate formula object based on the time definition.
#         [TimeDef]: Time definition incorporated;
#         [variables]: List of variables used to build single-factor models;
TimeDef_Form <- function(TimeDef, variables, strataVar=""){
  # Create formula based on time definition of the dataset.
  if(TimeDef[1]=="TFD"){# Formula for time to first write-off definition (containing only the first default spell).
    formula <- as.formula(paste0("Surv(TimeInDefSpell-1,TimeInDefSpell,WOff_Ind) ~ ",
                                 paste(variables,collapse=" + ")))
    
  } else if(TimeDef[1]=="AG"){# Formula for Andersen-Gill (AG) time definition
    formula <- as.formula(paste0("Surv(TimeInDefSpell-1,TimeInDefSpell,WOff_Ind) ~ DefSpell_Num + ",
                                 paste(variables,collapse=" + ")))
    
  } else if(TimeDef[1]=="PWPST"){# Formula for Prentice-Williams-Peterson (PWP) Spell time definition
    formula <- as.formula(paste0("Surv(TimeInDefSpell-1,TimeInDefSpell,WOff_Ind) ~ strata(", strataVar, ") + ",
                                 paste(variables,collapse=" + ")))
  } else if(TimeDef[1]=="Cox_Discrete") { # Formula for a discrete-time Cox model (for use in glm())
      formula <- as.formula(paste0(TimeDef[2], " ~ ",
                                   paste(variables,collapse=" + ")))
  } else {stop("Unkown time definition")}
  
  return(formula)
}


# --- Function to extract the Akaike Information Criterion (AIC) from single-factor models
# Input:  [data_train]: Training data; [data_valid]: [variables]: List of variables used to build single-factor models;
#         [fldSpellID]: Field name of spell-level ID; [TimeDef]: Time definition incorporated.
#         [numThreads]: Number of threads used; [genPath]: Optional path for log file. 
# Output: [matResults]: Result matrix.
aicTable <- function(data_train, variables, fldSpellID="DefSpell_Key",
                     TimeDef, numThreads=6, genPath, strataVar="", modelType="Cox") {
  # - Testing conditions
  # data_train <- datCredit; TimeDef=Cox_Discrete","PerfSpell_Event; numThreads=6
  # fldSpellID<-" DefSpell_Key"; variables<-"g0_Delinq_SD_4"; strataVar="DefSpell_Num"
  
  # - Iterate across loan space using a multi-threaded setup
  ptm <- proc.time() #IGNORE: for computation time calculation
  cl.port <- makeCluster(round(numThreads)); registerDoParallel(cl.port) # multi-threading setup
  cat("New Job: Estimating AIC for each variable as a single-factor survival model ..",
      file=paste0(genPath,"AIC_log.txt"), append=F)
  
  results <- foreach(j=1:length(variables), .combine='rbind', .verbose=F, .inorder=T,
                     .packages=c('data.table', 'survival'), .export=c('calc_AIC', 'TimeDef_Form')) %dopar%
    { # ----------------- Start of Inner Loop -----------------
      # - Testing conditions
      # j <- 1
      calc_AIC(formula=TimeDef_Form(TimeDef,variables[j], strataVar=strataVar), variables=variables[j],
               data_train=data_train, it=j, logPath=genPath,  fldSpellID=fldSpellID, modelType=modelType)
    } # ----------------- End of Inner Loop -----------------
  stopCluster(cl.port); proc.time() - ptm  
  
  # Sort by concordance in ascending order.
  setorder(results, AIC)
  
  # Return resulting table.
  return(results)
}

# --- Function to fit a given formula within a Cox regression model towards extracting Harrell's C-statistic and related quantities
#         [formula]: Cox regression formula object; [data_train]: Training data;
#         [data_valid]: Validation data; [variables]: List of variables used to build single-factor models;
#         [it]: Number of variables being compared; [logPath], Optional path for log file for logging purposes;
#         [fldSpellID]: Field name of spell-level ID.
calc_conc <- function(formula, data_train, data_valid, variables="", it=NA, logPath="", 
                      fldSpellID="DefSpell_Key", modelType="Cox") {
  # formula <- TimeDef_Form(TimeDef,variables[j], strataVar=strataVar)
  
  tryCatch({
    if (modelType=="Cox") {
      model <- coxph(formula,id=get(fldSpellID), data = data_train) # Fit Cox model 
    } else if (modelType=="Cox_Discrete"){
      model <- glm(formula,data = data_train, family="binomial") # Fit discrete-time Cox model 
    } else stop("Unknown model type in calc_AIC().")
    
    if (!is.na(it)) {# Output the number of models built, where the log is stored in a text file afterwards.
      cat(paste0("\n\t ", it,") Single-factor survival model built. "),
          file=paste0(logPath,"HarrelsC_log.txt"), append=T)
    }
    
    c <- concordance(model, newdata=data_valid) # Calculate concordance of the model based on the validation set.
    conc <- as.numeric(c[1])# Extract concordance
    sd <- sqrt(c$var)# Extract concordance variability as a standard deviation
    if (modelType=="Cox") {
      lr_stat <- round(2 * (model$loglik[2] - model$loglik[1]),0)# Extract LRT from the model's log-likelihood 
    } else lr_stat <- NA
    
    # Return results as a data.table
    return(data.table(Variable = variables, Concordance = conc, SD = sd, LR_Statistic = lr_stat))
  }, error=function(e) {
    conc <- 0
    sd <- NA
    lr_stat <- NA
    if (!is.na(it)) {
      cat(paste0("\n\t ", it,") Single-factor survival model failed. "),
          file=paste0(logPath,"Concordance_log.txt"), append=T)
    }
    return(data.table(Variable = variables, Concordance = conc, SD = sd, LR_Statistic = lr_stat)) 
  })
}






# --- Function to extract the concordances (Harrell's C for Cox PH) from single-factor models
# Input:  [data_train]: Training data; [data_valid]: Validation data;
#         [variables]: List of variables used to build single-factor models;
#         [fldSpellID]: Field name of spell-level ID; [TimeDef]: Time definition incorporated.
# Output: [matResults]: Result matrix.
concTable <- function(data_train, data_valid, variables, fldSpellID="DefSpell_Key",
                      TimeDef, numThreads=6, genPath, strataVar="", modelType="Cox") {
  # - Testing conditions
  # data_valid <- datCredit_train_PWPST; TimeDef="PWPST"; numThreads=6
  # fldEventInd<-"Default_Ind"
  
  # - Iterate across loan space using a multi-threaded setup
  ptm <- proc.time() #IGNORE: for computation time calculation
  cl.port <- makeCluster(round(numThreads)); registerDoParallel(cl.port) # multi-threading setup
  cat("New Job: Estimating B-statistic (1-KS) for each variable as a single-factor survival model ..",
      file=paste0(genPath,"Concordance_log.txt"), append=F)
  
  results <- foreach(j=1:length(variables), .combine='rbind', .verbose=F, .inorder=T,
                     .packages=c('data.table', 'survival'), .export=c('calc_conc', 'TimeDef_Form')) %dopar%
    { # ----------------- Start of Inner Loop -----------------
      # - Testing conditions
      # j <- 1
      calc_conc(formula=TimeDef_Form(TimeDef,variables[j], strataVar=strataVar), variable=variables[j],
                data_train=data_train, data_valid=data_valid, it=j, logPath=genPath, 
                fldSpellID=fldSpellID, modelType=modelType)
    } # ----------------- End of Inner Loop -----------------
  stopCluster(cl.port); proc.time() - ptm  
  
  # Sort by concordance in descending order.
  setorder(results, -Concordance)
  
  # Return resulting table.
  return(results)
}


