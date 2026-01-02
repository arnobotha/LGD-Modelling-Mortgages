# ============================== CUSTOM FUNCTIONS ==============================
# Defining custom functions used across various projects
# ------------------------------------------------------------------------------
# PROJECT TITLE: Loss Modelling (LGD) for Residential Mortgages
# SCRIPT AUTHOR(S): Mohammed Gabru (MG), Marcel Muller, Dr Arno Botha

# DESCRIPTION:
# This script defines various functions that are used elsewhere in this project
# or, indeed, used across other projects. Functions are grouped thematically.
# ==============================================================================



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


evalModel_onestage <- function(data_train, actField, estField, model){
  y  <- data_train[[actField]]
  est <- data_train[[estField]]
  df <- data.table(y,est) %>% drop_na()
  
  rmse <- sqrt(mean((df$est- df$y)^2))
  mae <- mean(abs(df$est - df$y))
  ks <- ks.test(df$y,df$est)$statistic
  kendalls <-cor.fk(df$y,df$est)
  spearman <-cor(df$y,df$est,method = "spearman",use    = "complete.obs")
  kl <- KL_one_stage(model, data_train)$KL
  js <- KL_one_stage(model, data_train)$JS
  
  data.table(RMSE=rmse, MAE=mae , KS=ks, KL=kl, JS=js, Kendalls_Tau=kendalls, Spearmans_rho=spearman)
}

KL_two_stage <- function(y, y_pred, n_bins=200) {
  
  # - Unit test | Internal
  # y<-y; y_pred<-est; n_bins<-200
  
  # - Set error tolerance
  eps <- 1e-12
  
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
  
  # - Return results
  data.frame(KL = KL, JS = JS)
}


evalModel_twostage <- function(data_train, actField, estField){
  
  # - Unit test | Internal
  # data_train<-datCredit_bas; actField<-"LossRate_Real";
  # estField<-"LossRate_est_bas"; modWoff<-modLR_Bas; modLS<-modGLM_Severity_CPG;
  # thresh<-NULL
  
  # - Get actual and expected fields
  y  <- data_train[[actField]]
  est <- data_train[[estField]]
  df <- data.table(y,est) %>% drop_na()
  
  # - Estimate error metrics
  rmse <- sqrt(mean((df$est- df$y)^2))
  mae  <- mean(abs(df$est - df$y))
  
  # - Estimate KS statistic
  ks <- ks.test(df$y,df$est)$statistic
  kendalls <- cor.fk(df$y,df$est)
  spearman <- cor(df$y, df$est, method = "spearman", use="complete.obs")
  
  # - Estimate KL and JS statistics
  kl_js <- KL_two_stage(y=df$y, y_pred=df$est)
  kl <- kl_js$KL
  js <- kl_js$JS

  # - Return results
  data.table(RMSE=rmse, MAE=mae , KS=ks, KL=kl, JS=js,
             Kendalls_Tau=kendalls, Spearmans_rho=spearman)
}












