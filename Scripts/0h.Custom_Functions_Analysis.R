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

# - Create binned version of [TimeInPerfSpell] for discrete-time hazard models
timeBinning <- function(x) {
  case_when(
    0 < x & x <= 3 ~ "01.[1,3]", 3 < x & x <= 6 ~ "02.(3,6]",
    6 < x & x <= 9 ~ "03.(6,9]", 9 < x & x <= 12 ~ "04.(9,12]",
    12 < x & x <= 15 ~ "05.(12,15]", 15 < x & x <= 18 ~ "06.(15,18]",
    18 < x & x <= 21 ~ "07.(18,21]", 21 < x & x <= 24 ~ "08.(21,24]",
    24 < x & x <= 27 ~ "09.(24,27]", 27 < x & x <= 30 ~ "10.(27,30]",
    30 < x & x <= 33 ~ "11.(30,33]", 33 < x & x <= 36 ~ "12.(33,36]",
    36 < x & x <= 39 ~ "13.(36,39]", 39 < x & x <= 42 ~ "14.(39,42]",
    42 < x & x <= 45 ~ "15.(42,45]", 45 < x & x <= 48 ~ "16.(45,48]",
    48 < x & x <= 51 ~ "17.(48,51]", 51 < x & x <= 54 ~ "18.(51,54]",
    54 < x & x <= 57 ~ "19.(54,57]", 57 < x & x <= 60 ~ "20.(57,60]",
    60 < x & x <= 63 ~ "21.(60,63]", 63 < x & x <= 66 ~ "22.(63,66]",
    66 < x & x <= 69 ~ "23.(66,69]", 69 < x & x <= 72 ~ "24.(69,72]",
    72 < x & x <= 80 ~ "25.(72,80]", 80 < x & x <= 86 ~ "26.(80,86]",
    86 < x & x <= 92 ~ "27.(86,92]", 92 < x & x <= 100 ~ "28.(92,100]",
    100 < x & x <= 108 ~ "29.(100,108]", 108 < x & x <= 120 ~ "30.(108,120]",
    TRUE ~ "31.(120+)"
  )
}

