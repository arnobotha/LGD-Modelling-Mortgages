# ========================= LOSS RATES OVER CALENDAR TIME ======================
# Comparing actual portfolio-level rates with expecteds from the fitted
# probability and severity models
# ------------------------------------------------------------------------------
# PROJECT TITLE: Loss Modelling (LGD) for Residential Mortgages
# SCRIPT AUTHOR(S): Mohammed Gabru (MG), Marcel Muller (MM), Dr Arno Botha (AB)
# ------------------------------------------------------------------------------
# -- Script dependencies:
#   - 0.Setup.R
#   - 1.Data_Import.R
#   - 2a.Data_Prepare_Credit_Basic.R
#   - 2b.Data_Prepare_Credit_Advanced.R
#   - 2c.Data_Prepare_Credit_Advanced2.R
#   - 2d.Data_Enrich.R
#   - 2f.Data_Fusion1.R
#   - 2g.Data_Fusion2.R
#   - 4b(i).InputSpace_DiscreteCox.R
#   - 4b(ii).InputSpace_DiscreteCox_Basic.R
#   - 4c.InputSpace_LogisticRegression.R
#   - 4d.InputSpace_TwoStage_LossSeverity.R
#   - 4e(ii).Dichotomisation
# -- Inputs:
#   - datCredit_train_CDH | Prepared from script 2g
#   - datCredit_valid_CDH | Prepared from script 2g
#   - modLR_Bas | Basic discrete time model as fitted in script 4b(ii)
#   - modLR_Adv | Advanced discrete time model as fitted in script 4b(i)
#   - modLR_Classic | Classical logistic regression model as fitted in script 4c
#   - modGLM_Severity_CPG | Single stage GLM model with a Tweedie link function
#                           as fitted in script 4d
#   - thres_lst | Thresholds for classifying predictions as determined in script 4e
# -- Outputs:
#   - <Analytics> | Graphs
# ==============================================================================



# ------ 1. Model fitting

# --- 1.1 Load and prepare datasets
# - Confirm prepared datasets are loaded into memory
if (!exists('datCredit_train_CDH')) unpack.ffdf(paste0(genPath,"creditdata_train_CDH"), tempPath);gc()
if (!exists('datCredit_valid_CDH')) unpack.ffdf(paste0(genPath,"creditdata_valid_CDH"), tempPath);gc()

# - Filter for default spells
datCredit_train <- subset(datCredit_train_CDH, !is.na(DefSpell_Key))
datCredit_valid <- subset(datCredit_valid_CDH, !is.na(DefSpell_Key))

# - Score data using classic model for each instance of [TimeInDefSpell] as [DefSpell_Age]
datCredit_train[, DefSpell_Age2:=DefSpell_Age]; datCredit_train[, DefSpell_Age:=TimeInDefSpell]
datCredit_valid[, DefSpell_Age2:=DefSpell_Age]; datCredit_valid[, DefSpell_Age:=TimeInDefSpell]

# - Combine training and validation datasets to facilitate "better" (smooth) graphs
datCredit <- rbind(datCredit_train, datCredit_valid)

# - Handle left-truncated spells by adding a starting record 
### NOTE:  This is necessary for calculating certain survival quantities later
# Create an additional record for each default spell
datAdd <- subset(datCredit, Counter==1 & TimeInDefSpell>1)
datAdd[, TimeInDefSpell:=TimeInDefSpell-1]
datAdd[, Counter:=0]
# Add record to main dataset
datCredit <- rbind(datCredit, datAdd); setorder(datCredit, DefSpell_Key, TimeInDefSpell)

# - Remove objects
rm(datCredit_train_CDH, datCredit_valid_CDH); gc()


# --- 1.2 Load models
# - GLM with Gaussian link function
modGLM_OneStage_Gaus <- readRDS(paste0(genObjPath,"OneStage_Gaus_Model.rds"))

# - GLM with Tweedie (Compound Poisson Gaussian) link function
modGLM_OneStage_CPG <- readRDS(paste0(genObjPath,"OneStage_CPH_Model.rds"))

# - Basic discrete-time hazard model
modLR_Bas <- readRDS(paste0(genObjPath,"CoxDisc_Basic_Model.rds"))

# - Advanced discrete-time hazard model
modLR_Adv <- readRDS(paste0(genObjPath,"CoxDisc_Advanced_Model.rds"))

# - Classical logit model
modLR_Classic <- readRDS(paste0(genObjPath,"LR_Model.rds"))

# - CPG severity model
modGLM_Severity_CPG <- readRDS(paste0(genObjPath,"Severity_CPH_Model.rds"))


# --- 1.3 Estimate event rates to facilitate the application of dichotomisation
# - Predict hazard h(t) = P(T=t | T>= t) in discrete-time
datCredit[, Hazard_adv:=predict(modLR_Adv, newdata=datCredit, type="response")]
datCredit[, Hazard_bas:=predict(modLR_Bas, newdata=datCredit, type="response")]
datCredit[, Hazard_classic:=predict(modLR_Classic, newdata=.SD[], type="response")]

# - Derive survival probability S(t) = prod(1 - hazard)
datCredit[, Survival_adv:=cumprod(1-Hazard_adv), by=list(DefSpell_Key)]
datCredit[, Survival_bas:=cumprod(1-Hazard_bas), by=list(DefSpell_Key)]
datCredit[, Survival_classic:=cumprod(1-Hazard_classic), by=list(DefSpell_Key)]

# - Derive discrete density, or event probability f(t) = S(t-1) - S(t)
datCredit[, EventRate_adv:=shift(Survival_adv, type="lag", n=1, fill=1) - Survival_adv, by=list(DefSpell_Key)]
datCredit[, EventRate_bas:=shift(Survival_bas, type="lag", n=1, fill=1) - Survival_bas, by=list(DefSpell_Key)]
datCredit[, EventRate_classic:=shift(Survival_classic, type="lag", n=1, fill=1) - Survival_classic, by=list(DefSpell_Key)]

# - Remove added rows
datCredit <- subset(datCredit, Counter>0)


# --- 1.4 Dichotomise predictions
# - Youden Index cut-offs
# Load thresholds
thresh_lst <- readRDS(file=paste0(genObjPath,"Classification_Thresholds.rds"))
# Basic discrete-time model
(thresh_dth_bas <- thresh_lst[["Basic"]]) # 0.01515866
# Advanced discrete-time model
(thresh_dth_adv <- thresh_lst[["Advanced"]]) # 0.2950287
# Classical logit model
(thresh_classic <- thresh_lst[["Classical"]]) # 0.0650852

# - Apply dichotomisation
datCredit[, DefSpell_Event_Adv_Youden:=ifelse(EventRate_adv>thresh_dth_adv,1,0)]
datCredit[, DefSpell_Event_Bas_Youden:=ifelse(EventRate_bas>thresh_dth_bas,1,0)]
datCredit[, DefSpell_Event_Classic_Youden:=ifelse(EventRate_classic>thresh_classic,1,0)]


# --- 1.5 Estimate severities and interact them with the probabilities
# - Forecast severities
# One-stage Gaussian
datCredit[, LossRate_Gaussian:=predict(modGLM_OneStage_Gaus, newdata=datCredit, type="response")]
# One-stage Tweedie
datCredit[, LossRate_Tweedie:=predict(modGLM_OneStage_CPG, newdata=datCredit, type="response")]
# Two-stage Tweedie
datCredit[, LossSeverity:=predict(modGLM_Severity_CPG, newdata=datCredit, type="response")]

# - Interact severities and probabilities
# Basic discrete time hazard model
datCredit[, LossRate_est_bas:=LossSeverity*EventRate_bas]
datCredit[, LossRate_est_bas_B:=LossSeverity*DefSpell_Event_Bas_Youden]
# Advanced discrete time hazard model
datCredit[, LossRate_est_adv:=LossSeverity*EventRate_adv]
datCredit[, LossRate_est_adv_B:=LossSeverity*DefSpell_Event_Adv_Youden]
# Classical logistic regression model
datCredit[, LossRate_est_classic:=LossSeverity*EventRate_classic]
datCredit[, LossRate_est_classic_B:=LossSeverity*DefSpell_Event_Classic_Youden]


# --- 1.6 Correct for non-sensical loss rates
# One-stage models
datCredit[, LossRate_Gaussian:=ifelse(LossRate_Gaussian<0 | LossRate_Gaussian>1, NA, LossRate_Gaussian)]
datCredit[, LossRate_Tweedie:=ifelse(LossRate_Tweedie<0 | LossRate_Tweedie>1, NA, LossRate_Tweedie)]
# Severity model
datCredit[, LossSeverity:=ifelse(LossSeverity<0 | LossSeverity>1, NA, LossSeverity)]
# Basic discrete time hazard model
datCredit[, LossRate_est_bas:=ifelse(LossRate_est_bas<0 | LossRate_est_bas>1, NA, LossRate_est_bas)]
datCredit[, LossRate_est_bas_B:=ifelse(LossRate_est_bas_B<0 | LossRate_est_bas_B>1, NA, LossRate_est_bas_B)]
# Advanced discrete time hazard model
datCredit[, LossRate_est_adv:=ifelse(LossRate_est_adv<0 | LossRate_est_adv>1, NA, LossRate_est_adv)]
datCredit[, LossRate_est_adv_B:=ifelse(LossRate_est_adv_B<0 | LossRate_est_adv_B>1, NA, LossRate_est_adv_B)]
# Classical logistic regression model
datCredit[, LossRate_est_classic:=ifelse(LossRate_est_classic<0 | LossRate_est_classic>1, NA, LossRate_est_classic)]
datCredit[, LossRate_est_classic_B:=ifelse(LossRate_est_classic_B<0 | LossRate_est_classic_B>1, NA, LossRate_est_classic_B)]


# --- 1.6 Subset data
# - Filter to maximum spell counter
# datCredit <- subset(datCredit, DefSpell_Counter==1)
datCredit <- datCredit[, .SD[which.max(DefSpell_Counter)], by=DefSpell_Key]

# - Identify where the loss rate is out of bounds and not feasible
datCredit[, OOB_Ind:=ifelse(LossRate_Real<0 | LossRate_Real>1,1,0)]

# - Subset to include only relevant data
datCredit <- subset(datCredit, OOB_Ind==0)




# ------ 2. Comparing loss rates quantitatively (account-level)

# --- 2.1 Compare loss rates
# - Create labeling object
model_names <- c(
  "1-stage Gaussian",
  "1-stage Tweedie",
  "2-stage LR A",
  "2-stage DtH-Basic A",
  "2-stage DtH-Advanced A",
  "2-stage LR B",
  "2-stage DtH-Basic B",
  "2-stage DtH-Advanced B"
)

# - Initiate table for comparisons
datComp <- data.table(
  Model=model_names,    
  RMSE=NA_real_,
  MAE=NA_real_,
  KS=NA_real_,
  KL=NA_real_,
  JS=NA_real_,
  Kendalls_Tau=NA_real_,
  Spearmans_rho=NA_real_
)

# - Compare actual and expected loss rates
# One stage
datComp[Model=="1-stage Gaussian",names(datComp)[-1]:=as.list(
  evalModel_onestage(datCredit,"LossRate_Real","LossRate_Gaussian",modGLM_OneStage_Gaus))]
datComp[Model=="1-stage Tweedie",names(datComp)[-1]:=as.list(
  evalModel_onestage(datCredit,"LossRate_Real","LossRate_Tweedie",modGLM_OneStage_CPG))]
# Two stage | A series (un-dichotomised)
datComp[Model=="2-stage DtH-Basic A",names(datComp)[-1]:=as.list(
  evalModel_twostage(datCredit,"LossRate_Real","LossRate_est_bas"))]
datComp[Model=="2-stage DtH-Advanced A",names(datComp)[-1]:=as.list(
  evalModel_twostage(datCredit,"LossRate_Real","LossRate_est_adv"))]
datComp[Model=="2-stage LR A",names(datComp)[-1]:=as.list(
  evalModel_twostage(datCredit,"LossRate_Real","LossRate_est_classic"))]
# Two stage | B series (dichotomised)
datComp[Model=="2-stage DtH-Basic B",names(datComp)[-1]:=as.list(
  evalModel_twostage(datCredit,"LossRate_Real","LossRate_est_bas_B"))]
datComp[Model=="2-stage DtH-Advanced B",names(datComp)[-1]:=as.list(
  evalModel_twostage(datCredit,"LossRate_Real","LossRate_est_adv_B"))]
datComp[Model=="2-stage LR B",names(datComp)[-1]:=as.list(
  evalModel_twostage(datCredit,"LossRate_Real","LossRate_est_classic_B"))]


# --- 2.2 Graph comparison metrics
# - Order data
df <- datComp %>%
  mutate(Model=factor(Model, levels=rev(model_names)))

# - Define a panel function to facilitate graphing
make_panel <- function(var, title, lower_better = TRUE, digits = 4) {
  best <- if(lower_better) min(df[[var]], na.rm = TRUE) else max(df[[var]], na.rm = TRUE)
  
  ggplot(df, aes(x = Model, y = .data[[var]])) +
    geom_col(aes(fill = .data[[var]] == best), width = 0.82) +
    
    geom_text(aes(label = round(.data[[var]], digits)),
              hjust = -0.15, colour = "black", size = 5.2, fontface = "bold") +
    coord_flip() +
    scale_fill_manual(values = c("FALSE" = "#757575", "TRUE" = "#1565c0"), guide = "none") +
    labs(title = title) +
    theme_minimal(base_size = 14) +
    theme(
      plot.title = element_text(size = 18, face = "bold", colour = "#1565c0", hjust = 0.5),
      axis.title = element_blank(),
      axis.text.y = element_text(size = 13.5),
      panel.grid = element_blank(),
      plot.margin = margin(20, 50, 20, 20)  
    ) +
    scale_y_continuous(expand = expansion(mult = c(0, 0.35))) 
}

# - Create a graph for each metric
p1 <- make_panel("MAE", "MAE",lower_better=TRUE,  digits=4)
p2 <- make_panel("KS", "KS", lower_better=TRUE, digits=4)
p3 <- make_panel("KL", "KL", lower_better=TRUE,  digits=4)
p4 <- make_panel("Spearmans_rho", bquote("Spearman's "~symbol(rho)), lower_better=FALSE, digits=4)

# - Combine comparison graphs
(LGD_metric <- (p1 | p2) / (p3 | p4) )

# - Save plot
dpi <- 300
ggsave(LGD_metric, file=paste0(genFigPath,"/LGD_metrics.png"),width=30, height=18,dpi=dpi, bg="white")




# ------ 3. Graphing LGDs over time

# --- 3.1 Aggregate loss rates
# - Aggregate loss rates
# Actuals
ActLoss_Rate <- datCredit[, mean(LossRate_Real, na.rm=T), by=list(Date)]
# One-stage
Gaussian_Loss_Rate <- datCredit[,mean(LossRate_Gaussian, na.rm=T), by=list(Date)]
Tweedie_Loss_Rate <- datCredit[, mean(LossRate_Tweedie, na.rm=T), by=list(Date)]
# Two stage | A-series (un-dichotomised)
Two_stage_bas_A_Loss_Rate <- datCredit[, mean(LossRate_est_bas, na.rm=T), by=list(Date)]
Two_stage_adv_A_Loss_Rate <- datCredit[, mean(LossRate_est_adv, na.rm=T), by=list(Date)]
Two_stage_classic_A_Loss_Rate <- datCredit[, mean(LossRate_est_classic, na.rm=T), by=list(Date)]
# Two stage | B-series (dichotomised)
Two_stage_bas_B_Loss_Rate <- datCredit[, mean(LossRate_est_bas_B, na.rm=T), by=list(Date)]
Two_stage_adv_B_Loss_Rate <- datCredit[, mean(LossRate_est_adv_B, na.rm=T), by=list(Date)]
Two_stage_classic_B_Loss_Rate <- datCredit[, mean(LossRate_est_classic_B, na.rm=T), by=list(Date)]


# --- 3.2 Preparations for plotting
# - Create dataset-specific labels
ActLoss_Rate[,Dataset:="A"]
Gaussian_Loss_Rate[,Dataset:="B"]
Tweedie_Loss_Rate[,Dataset:="C"]
Two_stage_bas_A_Loss_Rate[,Dataset:="D"]
Two_stage_adv_A_Loss_Rate[,Dataset:="E"]
Two_stage_classic_A_Loss_Rate[,Dataset:="F"]
Two_stage_bas_B_Loss_Rate[,Dataset:="G"]
Two_stage_adv_B_Loss_Rate[,Dataset:="H"]
Two_stage_classic_B_Loss_Rate[,Dataset:="I"]

# - Create plotting object
datPlot <- rbind(ActLoss_Rate, Tweedie_Loss_Rate, Gaussian_Loss_Rate,
                 Two_stage_bas_A_Loss_Rate, Two_stage_adv_A_Loss_Rate, Two_stage_classic_A_Loss_Rate,
                 Two_stage_bas_B_Loss_Rate, Two_stage_adv_B_Loss_Rate, Two_stage_classic_B_Loss_Rate)
colnames(datPlot) <- c("Date", "LossRate", "Dataset")
setorder(datPlot, Dataset, Date)

# - [SANITY CHECK] Inspect loss rates
# One-stage models
plot(x=datPlot[Dataset=="A",Date],y=datPlot[Dataset=="A",LossRate], type="l", col="black")
lines(x=datPlot[Dataset=="B",Date],y=datPlot[Dataset=="B",LossRate], col="blue")
lines(x=datPlot[Dataset=="C",Date],y=datPlot[Dataset=="C",LossRate], col="red")
# Two-stage models | A-series (un-dichotomised)
plot(x=datPlot[Dataset=="A",Date],y=datPlot[Dataset=="A",LossRate], type="l", col="black")
lines(x=datPlot[Dataset=="D",Date],y=datPlot[Dataset=="D",LossRate], col="blue")
lines(x=datPlot[Dataset=="E",Date],y=datPlot[Dataset=="E",LossRate], col="red")
lines(x=datPlot[Dataset=="F",Date],y=datPlot[Dataset=="F",LossRate], col="green")
# Two-stage models | B-series (dichotomised)
plot(x=datPlot[Dataset=="A",Date],y=datPlot[Dataset=="A",LossRate], type="l", col="black")
lines(x=datPlot[Dataset=="G",Date],y=datPlot[Dataset=="G",LossRate], col="blue")
lines(x=datPlot[Dataset=="H",Date],y=datPlot[Dataset=="H",LossRate], col="red")
lines(x=datPlot[Dataset=="I",Date],y=datPlot[Dataset=="I",LossRate], col="green")


# --- 3.3 Loss rates for one-stage model
# -- Preliminaries
# - Create dataset for ggplot
datPlot_OneStage <- subset(datPlot, Dataset %in% c("A", "B", "C"))

# - Location of annotations
start_y <- 0.275
space <- 0.02
y_vals <- c(start_y,start_y-space,start_y-space*2)

# -- Creating an annotation dataset for TTCs
# - Initialise dataset
datAnnotate <- data.table(MeanLossRate=NULL, Dataset=c("A","B","C"),
                          x=rep(as.Date("2012-10-31"),3), # Text x coordinates
                          y=y_vals)

# - Estimate TTC-mean & confidence interval calculations
confLevel <- 0.95
vEventRates_Mean <- c(mean(datPlot_OneStage[Dataset=="A",LossRate],na.rm=T),
                      mean(datPlot_OneStage[Dataset=="B",LossRate],na.rm=T),
                      mean(datPlot_OneStage[Dataset=="C",LossRate],na.rm=T))

# - Estimate standard errors
vEventRates_stErr <- c(
  sd(datPlot_OneStage[Dataset=="A",LossRate], na.rm = TRUE) / sqrt(sum(datPlot_OneStage[Dataset=="A",LossRate], na.rm=T)),
  sd(datPlot_OneStage[Dataset=="B",LossRate], na.rm = TRUE) / sqrt(sum(datPlot_OneStage[Dataset=="B",LossRate], na.rm=T)),
  sd(datPlot_OneStage[Dataset=="C",LossRate], na.rm = TRUE) / sqrt(sum(datPlot_OneStage[Dataset=="C",LossRate], na.rm=T))

)
vMargin <- qnorm(1-(1-confLevel)/2) * vEventRates_stErr

# - Create and assign labels
series_labels <- c("A[t]", "B[t]", "C[t]")
vLabel <- sapply(seq_along(series_labels), function(i) {
  paste0("'TTC-mean over '*italic(t)*' for '*italic(", series_labels[i], ")*' : ",
         sprintf("%.2f", vEventRates_Mean[i] * 100),
         "% ± ",
         sprintf("%1.3f", vEventRates_stErr[i] * 100),
         "%'")
})
datAnnotate[, Label:=vLabel]

# -- Creating an annotation dataset for MAEs
# - Set y-positions
start_y_mae <- 0.2
space_mae <- 0.02
y_vals_mae <- c(start_y_mae,
                start_y_mae - space_mae)

# - Creating an annotation dataset for easier annotations
datAnnotate_mae <- data.table(MeanLossRate = NULL, Dataset = c("A-B","A-C"),
                              x = rep(as.Date("2014-05-31"),2), # Text x coordinates
                              y = y_vals_mae)

# - Estimate MAEs
vEventRates_MAE <- c(mean(abs(datPlot_OneStage[Dataset=="A",LossRate]-datPlot_OneStage[Dataset=="B",LossRate]), na.rm=T),
                     mean(abs(datPlot_OneStage[Dataset=="A",LossRate]-datPlot_OneStage[Dataset=="C",LossRate]), na.rm=T))

# - Create and assign labels
series_labels_mae <- c("B[t]", "C[t]")
vLabel_MAE <- sapply(seq_along(series_labels_mae), function(i) {
  paste0(
    "'MAE ('*italic(", series_labels_mae[i], ")*') : ",
    sprintf('%.2f', vEventRates_MAE[i] * 100),  # convert to percent
    "%'"
  )
})
datAnnotate_mae[, Label:=vLabel_MAE]

# -- Create graph
# - Graphing parameters
chosenFont <- "Cambria"; dpi <- 180
vCol <- brewer.pal(9, "Dark2")[c(2,1,3)]
vLabel <- c("A"=bquote(italic(A[t])~": Empirical"),
            "B"=bquote(italic(B[t])~": One-stage: Gaussian"), 
            "C"=bquote(italic(C[t])~": One-stage: Tweedie"))
vShape <- c(17,20,4) 

# - Create graph
(g3 <- ggplot(datPlot_OneStage[Date>="2008-05-31" & Date<="2022-06-30"], aes(x=Date, y=LossRate)) + theme_minimal() + 
    labs(y=bquote("Loss Rate L "), x=bquote("Default spell cohorts (mmmccyy): stop time "*italic(t[s]))) + 
    theme(text=element_text(family=chosenFont),legend.position = "bottom",legend.margin=margin(-10, 0, 0, 0),
          axis.text.x=element_text(angle=90), 
          strip.background=element_rect(fill="snow2", colour="snow2"),
          strip.text=element_text(size=11, colour="gray50"), strip.text.y.right=element_text(angle=90)) + 
    # Main graph
    geom_line(aes(colour=Dataset, linetype=Dataset), linewidth=0.3) +    
    geom_point(aes(colour=Dataset, shape=Dataset), size=1.8) + 
    geom_text(data=datAnnotate, aes(x=x, y=y, label = Label), family=chosenFont, size=3.5, hjust=0, parse=TRUE) +
    geom_text(data=datAnnotate_mae, aes(x=x, y=y, label = Label),
              family=chosenFont, size=3.5, hjust=0, parse=TRUE) +
    # Facets & scale options
    scale_colour_manual(name = "Model", values = vCol, labels = vLabel, 
                        guide = guide_legend(nrow = 1, byrow = TRUE)) +
    scale_fill_manual(name = "Model", values = vCol, labels = vLabel, 
                      guide = guide_legend(nrow = 1, byrow = TRUE)) +
    scale_shape_manual(name = "Model", values = vShape, labels = vLabel, 
                       guide = guide_legend(nrow = 1, byrow = TRUE)) +
    scale_linetype_discrete(name = "Model", labels = vLabel, 
                            guide = guide_legend(nrow = 1, byrow = TRUE)) +
    scale_x_date(date_breaks = "6 month", date_labels = "%b %Y") +
    scale_y_continuous(breaks = pretty_breaks(), labels = percent)
)

# - Save graph
dpi <-180
ggsave(g3, file=paste0(genFigPath, "LossRate-time_onestage.png"), width=1600/dpi, height=1000/dpi, dpi=dpi, bg="white")


# --- 3.4 Loss rates for two-stage models | Series A (un-dichotomised)
# -- Preliminaries
# - Create dataset for ggplot
datPlot_TwoStage <- subset(datPlot, Dataset %in% c("A", "D", "E", "F"))

# - Location of annotations
start_y <- 0.3
space <- 0.02
y_vals <- c(start_y,start_y-space,start_y-space*2,start_y-space*3)

# -- Creating an annotation dataset for TTCs
# - Initialise dataset
datAnnotate <- data.table(MeanLossRate=NULL, Dataset=c("A","D","E","F"),
                          x=rep(as.Date("2012-10-31"),4), # Text x coordinates
                          y=y_vals)

# - Estimate TTC-mean & confidence interval calculations
confLevel <- 0.95
vEventRates_Mean <- c(mean(datPlot_TwoStage[Dataset=="A",LossRate],na.rm=T),
                      mean(datPlot_TwoStage[Dataset=="D",LossRate],na.rm=T),
                      mean(datPlot_TwoStage[Dataset=="E",LossRate],na.rm=T),
                      mean(datPlot_TwoStage[Dataset=="F",LossRate],na.rm=T))

# - Estimate standard errors
vEventRates_stErr <- c(
  sd(datPlot_TwoStage[Dataset=="A",LossRate], na.rm = TRUE) / sqrt(sum(datPlot_TwoStage[Dataset=="A",LossRate], na.rm=T)),
  sd(datPlot_TwoStage[Dataset=="D",LossRate], na.rm = TRUE) / sqrt(sum(datPlot_TwoStage[Dataset=="D",LossRate], na.rm=T)),
  sd(datPlot_TwoStage[Dataset=="E",LossRate], na.rm = TRUE) / sqrt(sum(datPlot_TwoStage[Dataset=="E",LossRate], na.rm=T)),
  sd(datPlot_TwoStage[Dataset=="F",LossRate], na.rm = TRUE) / sqrt(sum(datPlot_TwoStage[Dataset=="F",LossRate], na.rm=T))
)
vMargin <- qnorm(1-(1-confLevel)/2) * vEventRates_stErr

# - Create and assign labels
series_labels <- c("A[t]", "B[t]", "C[t]", "D[t]")
vLabel <- sapply(seq_along(series_labels), function(i) {
  paste0("'TTC-mean over '*italic(t)*' for '*italic(", series_labels[i], ")*' : ",
         sprintf("%.2f", vEventRates_Mean[i] * 100),
         "% ± ",
         sprintf("%1.3f", vEventRates_stErr[i] * 100),
         "%'")
})
datAnnotate[, Label:=vLabel]

# -- Creating an annotation dataset for MAEs
# - Set y-positions
start_y_mae <- 0.2
space_mae <- 0.02
y_vals_mae <- c(start_y_mae,
                start_y_mae - space_mae,
                start_y_mae - space_mae*2)

# - Creating an annotation dataset for easier annotations
datAnnotate_mae <- data.table(MeanLossRate = NULL, Dataset = c("D", "E", "F"),
                              x = rep(as.Date("2014-05-31"),3), # Text x coordinates
                              y = y_vals_mae)

# - Estimate MAEs
vEventRates_MAE <- c(mean(abs(datPlot_TwoStage[Dataset=="A",LossRate]-datPlot_TwoStage[Dataset=="D",LossRate]), na.rm=T),
                     mean(abs(datPlot_TwoStage[Dataset=="A",LossRate]-datPlot_TwoStage[Dataset=="E",LossRate]), na.rm=T),
                     mean(abs(datPlot_TwoStage[Dataset=="A",LossRate]-datPlot_TwoStage[Dataset=="F",LossRate]), na.rm=T))

# - Create and assign labels
series_labels_mae <- c("B[t]", "C[t]", "D[t]")
vLabel_MAE <- sapply(seq_along(series_labels_mae), function(i) {
  paste0(
    "'MAE ('*italic(", series_labels_mae[i], ")*') : ",
    sprintf('%.2f', vEventRates_MAE[i] * 100),  # convert to percent
    "%'"
  )
})
datAnnotate_mae[, Label:=vLabel_MAE]

# -- Create graph
# - Graphing parameters
chosenFont <- "Cambria"; dpi <- 180
vCol <- brewer.pal(8, "Dark2")[c(2,1,3,5)]
vLabel <- c("A"=bquote(italic(A[t])~": Empirical"),
            "D"=bquote(italic(B[t])~": Two-stage: DtH-Basic A"), 
            "E"=bquote(italic(C[t])~": Two-stage: DtH-Advanced A"),
            "F"=bquote(italic(D[t])~": Two-stage: LR A"))
vShape <- c(17,20,4,15) 

# - Create graph
(g4 <- ggplot(datPlot_TwoStage[Date>="2008-01-31" & Date<="2022-05-31"], aes(x=Date, y=LossRate)) + theme_minimal() + 
    labs(y=bquote("Loss Rate L "), x=bquote("Default spell cohorts (mmmccyy): stop time "*italic(t[s]))) + 
    theme(text=element_text(family=chosenFont),legend.position = "bottom",legend.margin=margin(-10, 0, 0, 0),
          axis.text.x=element_text(angle=90), 
          strip.background=element_rect(fill="snow2", colour="snow2"),
          strip.text=element_text(size=11, colour="gray50"), strip.text.y.right=element_text(angle=90)) + 
    # Main graph
    geom_line(aes(colour=Dataset, linetype=Dataset), linewidth=0.3) +    
    geom_point(aes(colour=Dataset, shape=Dataset), size=1.8) + 
    geom_text(data=datAnnotate, aes(x=x, y=y, label=Label), family=chosenFont, size=3.5, hjust=0, parse=TRUE) +
    geom_text(data=datAnnotate_mae, aes(x=x, y=y, label=Label),
              family=chosenFont, size=3.5, hjust=0, parse=TRUE) +
    # Facets & scale options
    scale_colour_manual(name = "Model", values = vCol, labels = vLabel, 
                        guide = guide_legend(nrow = 1, byrow = TRUE)) +
    scale_fill_manual(name = "Model", values = vCol, labels = vLabel, 
                      guide = guide_legend(nrow = 1, byrow = TRUE)) +
    scale_shape_manual(name = "Model", values = vShape, labels = vLabel, 
                       guide = guide_legend(nrow = 1, byrow = TRUE)) +
    scale_linetype_discrete(name = "Model", labels = vLabel, 
                            guide = guide_legend(nrow = 1, byrow = TRUE)) +
    scale_x_date(date_breaks = "6 month", date_labels = "%b %Y") +
    scale_y_continuous(breaks = pretty_breaks(), labels = percent)
)

# - Save graph
dpi <-180
ggsave(g4, file=paste0(genFigPath, "LossRate-time_twostage_A.png"), width=1600/dpi, height=1000/dpi, dpi=dpi, bg="white")


# --- 3.5 Loss rates for two-stage models | Series B (dichotomised)
# -- Preliminaries
# - Create dataset for ggplot
datPlot_TwoStage_B <- subset(datPlot, Dataset %in% c("A", "G", "H", "I"))

# - Location of annotations
start_y <- 0.3
space <- 0.02
y_vals <- c(start_y,start_y-space,start_y-space*2,start_y-space*3)

# -- Creating an annotation dataset for TTCs
# - Initialise dataset
datAnnotate <- data.table(MeanLossRate=NULL, Dataset=c("A","D","E", "F"),
                          x=rep(as.Date("2012-10-31"),4), # Text x coordinates
                          y=y_vals)

# - Estimate TTC-mean & confidence interval calculations
confLevel <- 0.95
vEventRates_Mean <- c(mean(datPlot_TwoStage_B[Dataset=="A",LossRate],na.rm=T),
                      mean(datPlot_TwoStage_B[Dataset=="G",LossRate],na.rm=T),
                      mean(datPlot_TwoStage_B[Dataset=="H",LossRate],na.rm=T),
                      mean(datPlot_TwoStage_B[Dataset=="I",LossRate],na.rm=T))

# - Estimate standard errors
vEventRates_stErr <- c(
  sd(datPlot_TwoStage_B[Dataset=="A",LossRate], na.rm = TRUE) / sqrt(sum(datPlot_TwoStage_B[Dataset=="A",LossRate], na.rm=T)),
  sd(datPlot_TwoStage_B[Dataset=="G",LossRate], na.rm = TRUE) / sqrt(sum(datPlot_TwoStage_B[Dataset=="G",LossRate], na.rm=T)),
  sd(datPlot_TwoStage_B[Dataset=="H",LossRate], na.rm = TRUE) / sqrt(sum(datPlot_TwoStage_B[Dataset=="H",LossRate], na.rm=T)),
  sd(datPlot_TwoStage_B[Dataset=="I",LossRate], na.rm = TRUE) / sqrt(sum(datPlot_TwoStage_B[Dataset=="I",LossRate], na.rm=T))
)
vMargin <- qnorm(1-(1-confLevel)/2) * vEventRates_stErr

# - Create and assign labels
series_labels <- c("A[t]", "B[t]", "C[t]", "D[t]")
vLabel <- sapply(seq_along(series_labels), function(i) {
  paste0("'TTC-mean over '*italic(t)*' for '*italic(", series_labels[i], ")*' : ",
         sprintf("%.2f", vEventRates_Mean[i] * 100),
         "% ± ",
         sprintf("%1.3f", vEventRates_stErr[i] * 100),
         "%'")
})
datAnnotate[, Label:=vLabel]

# -- Creating an annotation dataset for MAEs
# - Set y-positions
start_y_mae <- 0.2
space_mae <- 0.02
y_vals_mae <- c(start_y_mae,
                start_y_mae - space_mae,
                start_y_mae - space_mae*2)

# - Creating an annotation dataset for easier annotations
datAnnotate_mae <- data.table(MeanLossRate = NULL, Dataset = c("G", "H", "I"),
                              x = rep(as.Date("2014-05-31"),3), # Text x coordinates
                              y = y_vals_mae)

# - Estimate MAEs
vEventRates_MAE <- c(mean(abs(datPlot_TwoStage_B[Dataset=="A",LossRate]-datPlot_TwoStage_B[Dataset=="G",LossRate]), na.rm=T),
                     mean(abs(datPlot_TwoStage_B[Dataset=="A",LossRate]-datPlot_TwoStage_B[Dataset=="H",LossRate]), na.rm=T),
                     mean(abs(datPlot_TwoStage_B[Dataset=="A",LossRate]-datPlot_TwoStage_B[Dataset=="I",LossRate]), na.rm=T))

# - Create and assign labels
series_labels_mae <- c("B[t]", "C[t]", "D[t]")
vLabel_MAE <- sapply(seq_along(series_labels_mae), function(i) {
  paste0(
    "'MAE ('*italic(", series_labels_mae[i], ")*') : ",
    sprintf('%.2f', vEventRates_MAE[i] * 100),  # convert to percent
    "%'"
  )
})
datAnnotate_mae[, Label:=vLabel_MAE]

# -- Create graph
# - Graphing parameters
chosenFont <- "Cambria"; dpi <- 180
vCol <- brewer.pal(9, "Dark2")[c(2,1,3,5)]
vLabel <- c("A"=bquote(italic(A[t])~": Empirical"),
            "G"=bquote(italic(B[t])~": Two-stage: DtH-Basic B"), 
            "H"=bquote(italic(C[t])~": Two-stage: DtH-Advanced B"),
            "I"=bquote(italic(D[t])~": Two-stage: LR B"))
vShape <- c(17,20,4,15) 

# - Create graph
(g5 <- ggplot(datPlot_TwoStage_B[Date>="2008-01-31" & Date<="2022-05-31"], aes(x=Date, y=LossRate)) + theme_minimal() + 
    labs(y=bquote("Loss Rate L "), x=bquote("Default spell cohorts (mmmccyy): stop time "*italic(t[s]))) + 
    theme(text=element_text(family=chosenFont),legend.position = "bottom",legend.margin=margin(-10, 0, 0, 0),
          axis.text.x=element_text(angle=90), 
          strip.background=element_rect(fill="snow2", colour="snow2"),
          strip.text=element_text(size=11, colour="gray50"), strip.text.y.right=element_text(angle=90)) + 
    # Main graph
    geom_line(aes(colour=Dataset, linetype=Dataset), linewidth=0.3) +    
    geom_point(aes(colour=Dataset, shape=Dataset), size=1.8) + 
    geom_text(data=datAnnotate, aes(x=x, y=y, label = Label), family=chosenFont, size=3.5, hjust=0, parse=TRUE) +
    geom_text(data=datAnnotate_mae, aes(x=x, y=y, label = Label),
              family=chosenFont, size=3.5, hjust=0, parse=TRUE) +
    # Facets & scale options
    scale_colour_manual(name = "Model", values = vCol, labels = vLabel, 
                        guide = guide_legend(nrow = 1, byrow = TRUE)) +
    scale_fill_manual(name = "Model", values = vCol, labels = vLabel, 
                      guide = guide_legend(nrow = 1, byrow = TRUE)) +
    scale_shape_manual(name = "Model", values = vShape, labels = vLabel, 
                       guide = guide_legend(nrow = 1, byrow = TRUE)) +
    scale_linetype_discrete(name = "Model", labels = vLabel, 
                            guide = guide_legend(nrow = 1, byrow = TRUE)) +
    scale_x_date(date_breaks = "6 month", date_labels = "%b %Y") +
    scale_y_continuous(breaks = pretty_breaks(), labels = percent)
)

# - Save graph
dpi <-180
ggsave(g5, file=paste0(genFigPath, "LossRate-time_twostage_B.png"), width=1600/dpi, height=1000/dpi, dpi=dpi, bg="white")


# --- 3.6 Clean-up
suppressWarnings(rm(datAdd, datAnnotate, datAnnotate_mae,
                    datCredit_train, datCredit_valid, datCredit,
                    datComp, datPlot, datPlot_OneStage, datPlot_TwoStage,
                    datPlot_TwoStage_B, df, g3, g4 ,g5,
                    Gaussian_Loss_Rate, LGD_metric, modGLM_OneStage_CPG,
                    modGLM_OneStage_Gaus, modGLM_Severity_CPG, modLR_Bas,
                    modLR_Adv, modLR_Classic, p1, p2, p3, p4, thresh_lst, ActLoss_Rate,
                    Tweedie_Loss_Rate, Two_stage_adv_A_Loss_Rate, Two_stage_adv_B_Loss_Rate,
                    Two_stage_bas_A_Loss_Rate, Two_stage_bas_B_Loss_Rate, Two_stage_classic_A_Loss_Rate,
                    Two_stage_classic_B_Loss_Rate, vLabel))
gc()
