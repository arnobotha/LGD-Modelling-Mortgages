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

KL_one_stage <- function(model, df, dist = c("tweedie", "gaussian")) {
  dist <- match.arg(dist)
  y <- df$LossRate_Real
  N <- length(y)
  p_emp <- rep(1/N, N)
  eps <- 1e-12
  
  mu_pred <- predict(model, newdata = df, type = "response")
  
  # ---------- Tweedie 1-stage ----------
  if (dist == "tweedie") {
    library(statmod)
    
    # Extract Tweedie parameters correctly from cpglm
    phi <- model@phi      # dispersion
    pwr <- model@p        # Tweedie power parameter
    
    # Density for each observed loss
    q_raw <- dtweedie(y, mu = mu_pred, phi = phi, power = pwr)
  }
  
  # ---------- Gaussian 1-stage ----------
  else if (dist == "gaussian") {
    sigma_est <- sqrt(sum(residuals(model, type="response")^2) / model$df.residual)
    q_raw <- dnorm(y, mean = mu_pred, sd = sigma_est)
  }
  
  # Avoid zeros and normalize to discrete pdf
  q_raw <- q_raw + eps
  q <- q_raw / sum(q_raw)
  
  KL <- KL_div(p_emp, q)
  JS <- JS_div(p_emp, q)
  
  data.frame(Model = paste(dist, "1-stage"), KL, JS)
}

evalModel_onestage <- function(data_train, actField, estField,model_type = c("tweedie", "gaussian") ,model){
  y  <- data_train[[actField]]
  est <- data_train[[estField]]
  
  rmse <- sqrt(mean((est- y)^2))
  mae <- mean(abs(est - y))
  ks <- ks.test(y,est)$statistic
  kendalls <-cor.fk(y,est)
  spearman <-cor(y,est,method = "spearman",use    = "complete.obs")
  if (model_type == "tweedie") {
    kl <- KL_one_stage(model,data_train,"tweedie")$KL
    js <- KL_one_stage(model,data_train,"tweedie")$JS
  } 
  else if (model_type == "gaussian") {
    kl <- KL_one_stage(model,data_train,"gaussian")$KL
    js <- KL_one_stage(model,data_train,"gaussian")$JS
  }
  
  data.table(RMSE = rmse, MAE=mae , KS = ks, KL =kl,JS=js,Kendalls_Tau=kendalls, Spearmans_rho=spearman)
}





# Two-stage Function


KL_two_stage <- function(df, model_writeoff, model_severity,
                         writeoff_type = c("logistic", "survival_adv","survival_bas"),
                         severity_type = c("tweedie", "gaussian")) {
  writeoff_type <- match.arg(writeoff_type)
  severity_type <- match.arg(severity_type)
  eps <- 1e-12
  N <- nrow(df)
  y <- df$LossRate_Real
  is_zero <- (y == 0)
  p_emp <- rep(1/N, N)
  
  # ---------- Stage 1: Write-off probability ----------
  if (writeoff_type == "logistic") {
    p_loss <- df$EventRate_PD
  } else if (writeoff_type == "survival_adv") {

    p_loss <- df$EventRate_adv
  }
  else if (writeoff_type == "survival_bas") {
    p_loss <- df$EventRate_bas
  }
  # ---------- Stage 2: Loss severity ----------
  mu_pred <- predict(model_severity, newdata = df, type = "response")
  
  if (severity_type == "tweedie") {
    require(statmod)
    phi <- model_severity@phi
    pwr <- model_severity@p
    f_y <- dtweedie(y, mu = mu_pred, phi = phi, power = pwr)
  } else if (severity_type == "gaussian") {
    sigma_est <-  sqrt(sum(residuals(model_severity, type="response")^2) / model$df.residual)
    f_y <- dnorm(y, mean = mu_pred, sd = sigma_est)
  }
  
  # ---------- Mixture density/mass ----------
  q_raw <- numeric(N)
  q_raw[is_zero] <- (1 - p_loss[is_zero]) + eps
  q_raw[!is_zero] <- p_loss[!is_zero] * f_y[!is_zero] + eps
  
  q <- q_raw / sum(q_raw)
  
  KL <- KL_div(p_emp, q)
  JS <- JS_div(p_emp, q)
  data.frame(Model = paste(writeoff_type, "+", severity_type, "2-stage"),
             KL,JS)
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






KL_two_stage_youden <- function(df, 
                         model_writeoff, 
                         model_severity,
                         writeoff_type = c("logistic", "survival_adv", "survival_bas"),
                         severity_type = c("tweedie", "gaussian"),
                         youden_cutoff = NULL) {   
  
  writeoff_type <- match.arg(writeoff_type)
  severity_type <- match.arg(severity_type)
  eps <- 1e-12
  N   <- nrow(df)
  y   <- df$LossRate_Real
  is_zero <- (y <= eps)
  p_emp <- rep(1/N, N)
  
  # ---------- Stage 1: raw probability ----------
  if (writeoff_type == "logistic") {
    p_loss_raw <- df$EventRate_PD
  } else if (writeoff_type == "survival_adv") {
    p_loss_raw <- df$EventRate_adv
  } else if (writeoff_type == "survival_bas") {
    p_loss_raw <- df$EventRate_bas
  }
  
  # Apply Youden cutoff if provided
  if (!is.null(youden_cutoff)) {
    p_loss <- ifelse(p_loss_raw > youden_cutoff, 1, 0)
  } else {
    p_loss <- p_loss_raw   # normal soft probabilities
  }
  
  # ---------- Stage 2: severity (only on the original non-zero observations) ----------
  mu_pred <- predict(model_severity, newdata = df, type = "response")
  
  if (severity_type == "tweedie") {
    require(statmod)
    phi <- model_severity@phi
    pwr <- model_severity@p
    f_y <- dtweedie(y, mu = mu_pred, phi = phi, power = pwr)
  } else if (severity_type == "gaussian") {
    sigma_est <- sqrt(sum(residuals(model_severity, type="response")^2) / 
                        model_severity$df.residual)
    f_y <- dnorm(y, mean = mu_pred, sd = sigma_est)
  }
  
  # ---------- Mixture density ----------
  q_raw <- numeric(N)
  

  pred_zero <- (p_loss <= 0.5) | (p_loss == 0)  
  
  q_raw[is_zero]  <- ifelse(pred_zero[is_zero],  1,  0) + eps
  q_raw[!is_zero] <- ifelse(pred_zero[!is_zero], 0,  f_y[!is_zero]) + eps
  
  q <- q_raw / sum(q_raw)
  
  KL <- KL_div(p_emp, q)
  JS <- JS_div(p_emp, q)
  
  model_name <- ifelse(is.null(youden_cutoff),
                       paste(writeoff_type, "+", severity_type, "2-stage"),
                       paste(writeoff_type, "+ Youden cutoff", youden_cutoff, "+", severity_type))
  
  data.frame(Model = model_name, KL = KL, JS = JS)
}


evalModel_twostage_youden <- function(data_train, actField, estField, writeoff_type = c("logistic", "survival_adv","survival_bas"),modWoff,modLS, thresh){
  y  <- data_train[[actField]]
  est <- data_train[[estField]]
  
  rmse <- sqrt(mean((est- y)^2))
  mae  <- mean(abs(est - y))
  ks <- ks.test(y,est)$statistic
  kendalls <-cor.fk(y,est)
  spearman <-cor(y,est,method = "spearman",use    = "complete.obs")
  if (writeoff_type == "logistic") {
    kl <- KL_two_stage_youden(data_train,modWoff, modLS,"logistic","tweedie",thresh)$KL
    js <- KL_two_stage_youden(data_train,modWoff, modLS,"logistic","tweedie",thresh)$JS
  } 
  else if (writeoff_type == "survival_adv") {
    kl <- KL_two_stage_youden(data_train,modWoff, modLS,"survival_adv","tweedie",thresh)$KL
    js <- KL_two_stage_youden(data_train,modWoff, modLS,"survival_adv","tweedie",thresh)$JS
  }
  else if (writeoff_type == "survival_bas") {
    kl <- KL_two_stage_youden(data_train,modWoff, modLS,"survival_bas","tweedie",thresh)$KL
    js <- KL_two_stage_youden(data_train,modWoff, modLS,"survival_bas","tweedie",thresh)$JS
  }
  
  data.table(RMSE = rmse, MAE=mae , KS = ks, KL =kl,JS=js,Kendalls_Tau=kendalls, Spearmans_rho=spearman)
}












