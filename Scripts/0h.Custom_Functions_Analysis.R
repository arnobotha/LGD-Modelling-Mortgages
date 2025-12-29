# ============================== CUSTOM FUNCTIONS ==============================
# Defining custom functions used across various projects
# ------------------------------------------------------------------------------
# PROJECT TITLE: Loss Modelling (LGD) for Residential Mortgages
# SCRIPT AUTHOR(S): Dr Arno Botha, Marcel Muller

# DESCRIPTION:
# This script defines various functions that are used elsewhere in this project
# or, indeed, used across other projects. Functions are grouped thematically.
# ==============================================================================




# --------------------------------------------------------------
# YOUDEN FUNCTION 
# --------------------------------------------------------------
youden_threshold <- function(y_true, y_prob, model_name = "") {
  thresholds <- sort(unique(c(0, y_prob, 1)))
  
  metrics <- map_dfr(thresholds, ~{
    pred <- ifelse(y_prob >= .x, 1, 0)
    tp <- sum(pred == 1 & y_true == 1)
    fp <- sum(pred == 1 & y_true == 0)
    tn <- sum(pred == 0 & y_true == 0)
    fn <- sum(pred == 0 & y_true == 1)
    
    sens <- ifelse(tp + fn == 0, 0, tp / (tp + fn))
    spec <- ifelse(tn + fp == 0, 0, tn / (tn + fp))
    J    <- sens + spec - 1                   
    
    tibble(threshold = .x, J = J, sens = sens, spec = spec,
           tp = tp, fp = fp, tn = tn, fn = fn)
  })
  
  best <- metrics %>% arrange(desc(J)) %>% slice(1)
  
  cat("====", model_name, "====\n")
  cat("Optimal threshold :", round(best$threshold, 4), "\n")
  cat("Youden J          :", round(best$J, 4), "\n")
  cat("Sensitivity       :", round(best$sens*100, 1), "%\n")
  cat("Specificity       :", round(best$spec*100, 1), "%\n")
  cat("TP =", best$tp, " FP =", best$fp, " FN =", best$fn, " TN =", best$tn, "\n\n")
  
  return(best$threshold)
}



# ---------- KL divergence helpers ----------
KL_div <- function(p, q) sum(p * log(p / q))

JS_div <- function(p, q) {
  m <- 0.5 * (p + q)
  0.5 * sum(p * log(p / m)) + 0.5 * sum(q * log(q / m))
}

KL_one_stage <- function(model, df, n_bins = 200) {
  
  eps <- 1e-12
  
  # -------------------------------
  # Actual loss rate
  # -------------------------------
  y <- df$LossRate_Real
  
  # -------------------------------
  # Predicted severity (1-stage model)
  # -------------------------------
  mu_pred <- predict(model, newdata = df, type = "response")
  y_pred <- mu_pred   # one-stage predicts severity directly
  
  # -------------------------------
  # Convert actual & predicted values into probability distributions
  # -------------------------------
  rng <- range(c(y, y_pred))
  breaks <- seq(rng[1], rng[2], length.out = n_bins)
  
  p_emp <- hist(y, breaks = breaks, plot = FALSE)$density + eps
  q_emp <- hist(y_pred, breaks = breaks, plot = FALSE)$density + eps
  
  # normalize
  p <- p_emp / sum(p_emp)
  q <- q_emp / sum(q_emp)
  
  # -------------------------------
  # KL divergence
  # -------------------------------
  KL <- sum(p * log(p / q))
  
  # -------------------------------
  # JS divergence
  # -------------------------------
  M <- 0.5 * (p + q)
  JS <- 0.5 * sum(p * log(p / M)) + 0.5 * sum(q * log(q / M))
  
  data.frame(Model = "1-stage severity", KL = KL, JS = JS)
}


evalModel_onestage <- function(data_train, actField, estField,model_type = c("tweedie", "gaussian") ,model){
  y  <- data_train[[actField]]
  est <- data_train[[estField]]
  
  rmse <- sqrt(mean((est- y)^2))
  mae <- mean(abs(est - y))
  ks <- ks.test(y,est)$statistic
  kendalls <-cor.fk(y,est)
  spearman <-cor(y,est,method = "spearman",use    = "complete.obs")
  kl <- KL_one_stage(model, data_train)$KL
  js <- KL_one_stage(model, data_train)$JS
 

  
  data.table(RMSE = rmse, MAE=mae , KS = ks, KL =kl,JS=js,Kendalls_Tau=kendalls, Spearmans_rho=spearman)
}








evalModel_twostage <- function(data_train, actField, estField, writeoff_type = c("logistic", "survival_adv","survival_bas"),modWoff,modLS){
  y  <- data_train[[actField]]
  est <- data_train[[estField]]
  
  rmse <- sqrt(mean((est- y)^2))
  mae  <- mean(abs(est - y))
  ks <- ks.test(y,est)$statistic
  kendalls <-cor.fk(y,est)
  spearman <-cor(y,est,method = "spearman",use    = "complete.obs")
  if (writeoff_type == "logistic") {
    kl <- KL_two_stage(data_train,modWoff,modLS,"logistic","tweedie")$KL
    js <- KL_two_stage(data_train,modWoff,modLS,"logistic","tweedie")$JS
  } 
  else if (writeoff_type == "survival_adv") {
    kl <- KL_two_stage(data_train,modWoff,modLS,"survival_adv","tweedie")$KL
    js <- KL_two_stage(data_train,modWoff,modLS,"survival_adv","tweedie")$JS
  }
  else if (writeoff_type == "survival_bas") {
    kl <- KL_two_stage(data_train,modWoff,modLS,"survival_bas","tweedie")$KL
    js <- KL_two_stage(data_train,modWoff,modLS,"survival_adv","tweedie")$JS
  }
  
  data.table(RMSE = rmse, MAE=mae , KS = ks, KL =kl,JS=js,Kendalls_Tau=kendalls, Spearmans_rho=spearman)
}






KL_two_stage <- function(df, 
                                model_writeoff, 
                                model_severity,
                                writeoff_type = c("logistic", "survival_adv", "survival_bas"),
                                youden_cutoff = NULL,
                                n_bins = 200) {
  
  writeoff_type <- match.arg(writeoff_type)
  eps <- 1e-12
  
  # -------------------------------
  # Actual loss rate
  # -------------------------------
  y <- df$LossRate_Real
  
  # -------------------------------
  # Stage 1: Write-off probability
  # -------------------------------
  if (writeoff_type == "logistic") {
    p_loss_raw <- df$EventRate_PD
  } else if (writeoff_type == "survival_adv") {
    p_loss_raw <- df$EventRate_adv
  } else if (writeoff_type == "survival_bas") {
    p_loss_raw <- df$EventRate_bas
  }
  
  # Apply Youden cutoff -> hard default indicator
  if (!is.null(youden_cutoff)) {
    p_loss <- ifelse(p_loss_raw > youden_cutoff, 1, 0)
  } else {
    p_loss <- p_loss_raw   # soft PD
  }
  
  # -------------------------------
  # Stage 2: Predicted severity
  # -------------------------------
  mu_pred <- predict(model_severity, newdata = df, type = "response")
  
  # Predicted expected loss
  y_pred <- p_loss * mu_pred
  
  # -------------------------------
  # Convert actual & predicted values into probability distributions
  # -------------------------------
  rng <- range(c(y, y_pred))
  breaks <- seq(rng[1], rng[2], length.out = n_bins)
  
  p_emp <- hist(y, breaks = breaks, plot = FALSE)$density + eps
  q_emp <- hist(y_pred, breaks = breaks, plot = FALSE)$density + eps
  
  # Normalize
  p_emp <- p_emp / sum(p_emp)
  q_emp <- q_emp / sum(q_emp)
  
  # -------------------------------
  # KL & JS divergence
  # -------------------------------
  KL <- sum(p_emp * log(p_emp / q_emp))
  M  <- 0.5 * (p_emp + q_emp)
  JS <- 0.5 * sum(p_emp * log(p_emp / M)) +
    0.5 * sum(q_emp * log(q_emp / M))
  
  model_name <- ifelse(is.null(youden_cutoff),
                       paste(writeoff_type, "+ severity", "2-stage"),
                       paste(writeoff_type, "+ Youden", youden_cutoff, "+ severity"))
  
  data.frame(Model = model_name, KL = KL, JS = JS)
}


evalModel_twostage <- function(data_train, actField, estField, writeoff_type = c("logistic", "survival_adv","survival_bas"),modWoff,modLS, thresh){
  y  <- data_train[[actField]]
  est <- data_train[[estField]]
  
  rmse <- sqrt(mean((est- y)^2))
  mae  <- mean(abs(est - y))
  ks <- ks.test(y,est)$statistic
  kendalls <-cor.fk(y,est)
  spearman <-cor(y,est,method = "spearman",use    = "complete.obs")
  if (writeoff_type == "logistic") {
    kl <- KL_two_stage(data_train,modWoff, modLS,"logistic",thresh)$KL
    js <- KL_two_stage(data_train,modWoff, modLS,"logistic",thresh)$JS
  } 
  else if (writeoff_type == "survival_adv") {
    kl <- KL_two_stage(data_train,modWoff, modLS,"survival_adv",thresh)$KL
    js <- KL_two_stage(data_train,modWoff, modLS,"survival_adv",thresh)$JS
  }
  else if (writeoff_type == "survival_bas") {
    kl <- KL_two_stage(data_train,modWoff, modLS,"survival_bas",thresh)$KL
    js <- KL_two_stage(data_train,modWoff, modLS,"survival_bas",thresh)$JS
  }
  
  data.table(RMSE = rmse, MAE=mae , KS = ks, KL =kl,JS=js,Kendalls_Tau=kendalls, Spearmans_rho=spearman)
}












