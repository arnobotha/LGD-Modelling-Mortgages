# ============================== DEFAULT RISK - MODEL DIAGNOSTICS =====================================
# Showcasing the use of ROC curves in evaluating the predictive power of logit models.
# -----------------------------------------------------------------------------------------------------------
# PROJECT TITLE: Loss Modelling (LGD) for FNB Mortgages
# SCRIPT AUTHOR(S): Marcel Muller (MM), Roland Breedt (RB), Dr Arno Botha (AB), Mohammed Gabru (MG)

# DESCRIPTION:
# This script uses the previously selected variables in fitting different logit models according to their
# level of complexity. ROC analysis are conducted on the models and the results are overlaid as to produce
# a single graph..
# -----------------------------------------------------------------------------------------------------------
# -- Script dependencies:
#   - 0.Setup.R
#   - 1.Data_Import.R
#   - 2a.Data_Prepare_Credit_Basic.R
#   - 2b.Data_Prepare_Credit_Advanced.R
#   - 2c.Data_Prepare_Credit_Advanced2.R
#   - 2d.Data_Enrich.R
#   - 2f.Data_Fusion1.R
#   - 2g.Data_Fusion2.R
#   - 4a(i).InputSpace_DiscreteCox.R
#   - 4a(ii).InputSpace_DiscreteCox_Basic.R
#
# -- Inputs:
#   - datCredit_train | Prepared credit data from script 2g
#   - datCredit_valid | Prepared credit data from script 2g
#   - Basic_surv_Formula | Model formula for basic write-off model
#   - LR_Formula | Model formula for write-off model
#   - Adv_Surv_Formula | Model formula for write-off model
#
# -- Outputs:
#   - <analytics> | Graphs showing various model-level diagnostics
# ===========================================================================================================




# ------ 1. Preliminaries



# - Confirm prepared datasets are loaded into memory
if (!exists('datCredit_train_CDH')) unpack.ffdf(paste0(genPath,"creditdata_train_CDH"), tempPath);gc()
if (!exists('datCredit_valid_CDH')) unpack.ffdf(paste0(genPath,"creditdata_valid_CDH"), tempPath);gc()



datCredit_train_CDH[!is.na(DefSpell_Key), c("DefSpell_Min_Date","DefSpell_Max_Date") := as.list(range(Date, na.rm=TRUE)), by=list(DefSpell_Key)]
datCredit_valid_CDH[!is.na(DefSpell_Key), c("DefSpell_Min_Date","DefSpell_Max_Date") := as.list(range(Date, na.rm=TRUE)), by=list(DefSpell_Key)]

# - Use only default spells
datCredit_train <- datCredit_train_CDH[!is.na(DefSpell_Key),]
datCredit_valid <- datCredit_valid_CDH[!is.na(DefSpell_Key),]


datCredit_train[, Weight := ifelse(DefSpell_Event==1,1,1)]
datCredit_valid[, Weight := ifelse(DefSpell_Event==1,1,1)]
datCredit <- rbind(datCredit_train, datCredit_valid)



# - Logistic Regression model
# - Use only default spells
datCredit_train_LR <- datCredit_train_CDH[!is.na(DefSpell_Key)&DefSpell_Counter==1,]
datCredit_valid_LR <- datCredit_valid_CDH[!is.na(DefSpell_Key)&DefSpell_Counter==1,]

datCredit_train_LR[, DefSpell_Event := ifelse(DefSpellResol_Type_Hist=="WOFF", 1, 0)]
datCredit_valid_LR[, DefSpell_Event := ifelse(DefSpellResol_Type_Hist=="WOFF", 1, 0)]
datCredit_LR <- rbind(datCredit_train_LR,datCredit_valid_LR)



# ---  Basic discrete-time hazard model
# - Initialize variables
vars_basic <- c("log(TimeInDefSpell)","DefSpell_Num_binned", "g0_Delinq_Lag_1",
                "M_Inflation_Growth_9","g0_Delinq_Any_Aggr_Prop_Lag_1")

# - Fit discrete-time hazard model with selected variables
modLR_basic <- glm( as.formula(paste("DefSpell_Event ~", paste(vars_basic, collapse = " + "))),
                    data=datCredit_train, family="binomial", weights= Weight)


# ---  Advanced discrete-time hazard model
# - Initialize variables
vars <- c("Time_Binned","log(TimeInDefSpell)*DefSpell_Num_binned", 
          "DefaultStatus1_Aggr_Prop_Lag_12","g0_Delinq_Ave",
          "InterestRate_Margin_Aggr_Med_9","NewLoans_Aggr_Prop","InterestRate_Nom",
          "Balance_1","pmnt_method_grp","Principal","g0_Delinq_Lag_1",
          "M_RealIncome_Growth_9", "M_Inflation_Growth_12","M_DTI_Growth_12","M_Repo_Rate_12","g0_Delinq_Any_Aggr_Prop_Lag_1")
modLR <- glm( as.formula(paste("DefSpell_Event ~", paste(vars, collapse = " + "))),
              data=datCredit_train, family="binomial", weights= Weight)


# ---  Logistic Regression model
# - Initialize variables
vars_LR <- c("g0_Delinq_Any_Aggr_Prop_Lag_12","DefaultStatus1_Aggr_Prop",
          "CuringEvents_Aggr_Prop","PrevDefaults",
          "DefSpell_Age", "g0_Delinq_Num", "Arrears",
          "slc_past_due_amt_imputed_med", "DefSpell_Num_binned",
          "InterestRate_Margin_Aggr_Med", "AgeToTerm_Aggr_Mean", "AgeToTerm",
          "BalanceToPrincipal", "pmnt_method_grp",
          "M_RealIncome_Growth_12", "M_Inflation_Growth_3","M_DTI_Growth_6","M_Repo_Rate_2")
modLR_log <- glm( as.formula(paste("DefSpell_Event ~", paste(vars, collapse = " + "))),
              data=datCredit_train_LR, family="binomial")



# -- Probability scoring
# - Training Set
datCredit_train[, prob_basic := predict(modLR_basic, newdata = datCredit_train, type="response")]
datCredit_train_LR[, prob_LR := predict(modLR_log, newdata = datCredit_train_LR, type="response")]
datCredit_train[, prob_adv := predict(modLR, newdata = datCredit_train, type="response")]

# - Validation Set
datCredit_valid[, prob_basic := predict(modLR_basic, newdata = datCredit_valid, type="response")]
datCredit_valid_LR[, prob_LR := predict(modLR_log, newdata = datCredit_valid_LR, type="response")]
datCredit_valid[, prob_adv := predict(modLR, newdata = datCredit_valid, type="response")]

# - Full Set
datCredit[, prob_basic := predict(modLR_basic, newdata = datCredit, type="response")]
datCredit_LR[, prob_LR := predict(modLR_log, newdata = datCredit_LR, type="response")]
datCredit[, prob_adv := predict(modLR, newdata = datCredit, type="response")]

# [SANITY CHECKS] Check for no missingness in probability scores in validation set
cat((anyNA(datCredit_valid[,prob_basic])) %?% "WARNING: Missingness detected in predicted probabilities of the Validation Set.\n" %:%
      "SAFE: No missingness in predicted probabilities.\n")
cat((anyNA(datCredit_valid[,prob_LR])) %?% "WARNING: Missingness detected in predicted probabilities of the Validation Set.\n" %:%
      "SAFE: No missingness in predicted probabilities.\n")
cat((anyNA(datCredit_valid[,prob_adv])) %?% "WARNING: Missingness detected in predicted probabilities of the Validation Set.\n" %:%
      "SAFE: No missingness in predicted probabilities.\n")



# --- 2. ROC analyis on the 3 models created
alpha <- 0.05 # Set confidence interval level

# - Basic model
roc_obj_basic <- pROC::roc(response=datCredit_valid$DefSpell_Event, predictor=datCredit_valid$prob_basic, ci.method="delong", ci=T, conf.level = 1-alpha, percent=T)
cat(paste0("AUC: ", percent(as.numeric(roc_obj_basic$auc)/100, accuracy=0.01), " ± ", sprintf("%.2f",(roc_obj_basic$ci[3]-roc_obj_basic$ci[1])/2),"%"))
### RESULTS: 70.18 +- 0.45%

# - Intermediate model
roc_obj_LR <- roc(response=datCredit_valid_LR$DefSpell_Event, predictor=datCredit_valid_LR$prob_LR, ci.method="delong", ci=T, conf.level = 1-alpha, percent=T)
cat(paste0("AUC: ", percent(as.numeric(roc_obj_LR$auc)/100, accuracy=0.01), " ± ", sprintf("%.2f",(roc_obj_LR$ci[3]-roc_obj_LR$ci[1])/2),"%"))
### RESULTS: 70,55% +- 0.62%

# - Advanced model
roc_obj_adv <- roc(response=datCredit_valid$DefSpell_Event, predictor=datCredit_valid$prob_adv, ci.method="delong", ci=T, conf.level = 1-alpha, percent=T)
cat(paste0("AUC: ", percent(as.numeric(roc_obj_adv$auc)/100, accuracy=0.01), " ± ", sprintf("%.2f",(roc_obj_adv$ci[3]-roc_obj_adv$ci[1])/2),"%"))
### RESULTS: 98.39% +- 0.16%
### CONCLUSION: Use the advanced model as it has strongest predictive power


# ------ 3. Model-level diagnostics and associated analytics

# --- 3.1 Coefficient of determination | Pseudo R^2-measures

# - Computing the coefficient of determination
# Basic model
(coefDeter_Basic <- coefDeter_glm(modLR_basic))
### RESULTS: McFadden = 4.36%
###          Cox Snell = 0.56%
###          Nagelkerke = 4.63%
# Intermediate model
(coefDeter_LR <- coefDeter_glm(modLR_log))
### RESULTS: McFadden = 9.10%
###          Cox Snell = 8.93%
###          Nagelkerke = 13.90%
# Advanced model
(coefDeter_Adv <- coefDeter_glm(modLR))
### RESULTS: McFadden = 64.52%
###          Cox Snell = 7.99%
###          Nagelkerke = 65.98%

# - Create a single table containing the three R^2 measures for each of the models
(PseudoR2_Table<-data.table(Model=c("Survival Analysis(Basic)","Logistic Regression","Survival Analysis(Advanced)"),CoxSnell=c(coefDeter_Basic$CoxSnell,coefDeter_LR$CoxSnell,coefDeter_Adv$CoxSnell),McFadden=c(coefDeter_Basic$McFadden,coefDeter_LR$McFadden,coefDeter_Adv$McFadden),
                            Nagelkerke=c(coefDeter_Basic$Nagelkerke,coefDeter_LR$Nagelkerke,coefDeter_Adv$Nagelkerke)))

# - Save table to specified path
pack.ffdf(paste0(genObjPath,"PseudoR2_Table"), PseudoR2_Table)

# -- Graph these measures together
# - Create a dataset to feed into ggplot2 (also change character R^2 values to numeric)
datPlot<-data.table(Statistic=rep(c("McFadden", "Nagelkerke"),
                                  each=3),Model=rep(c("a_Basic","b_Logistic_Regression", "c_Advanced"),times=2),Value=
                      as.numeric(sub("%","",c(PseudoR2_Table$McFadden,PseudoR2_Table$Nagelkerke)))/100)

# - Aesthetic engineering
datPlot[, Label:=paste0(sprintf("%.2f", Value*100),"%")]

# - Plotting parameters
chosenFont <- "Cambria"; dpi<-180
vCol1 <- c("a_Basic"=brewer.pal(9, "BuGn")[5], "b_Logistic_Regression"=brewer.pal(9, "BuGn")[7], "c_Advanced"=brewer.pal(9, "BuGn")[9])
vCol2 <- rep(c(vCol1[1],vCol1[2],vCol1[3]),2)
vCol3 <- rep("white", 3*2)
vLabel <- list("a_Basic"="Survival Analysis(Basic)", "b_Logistic_Regression"="Logistic Regression", "c_Advanced"="Survival Analysis(Advanced)")

# - Create the plot
(gPlot<-ggplot(datPlot, aes(group=Model, y=Value, x=Statistic)) + 
    theme_minimal() + theme(legend.position = "bottom", text=element_text(family=chosenFont), axis.title.x = element_text(margin = margin(t = 5))) + labs(x=bquote("Pseudo"~italic(R^{2})*"-measure"), y="Value", family=chosenFont) +
    geom_col(aes(colour=Model, fill=Model), position="dodge") +
    geom_label(aes(label=Label), fill = vCol2, colour = vCol3, position=position_dodge(0.90), size=2.75,label.padding = unit(0.15, "lines")) +
    scale_colour_manual(name="Model:", values=vCol1, labels=vLabel) +
    scale_fill_manual(name="Model:", values=vCol1, labels=vLabel) +
    scale_x_discrete(labels=c("McFadden"="McFadden","Nagelkerke"="Nagelkerke")) +
    scale_y_continuous(limits = c(0, 0.7), breaks = seq(0, 1, by = 0.1),label=percent))

# Saving the graph to specified path
ggsave(gPlot, file=paste0(genFigPath, "R2Plot_V2.png"), width=1200/dpi, height=1000/dpi, dpi=400, bg="white")

# - Cleanup
rm(datPlot, gPlot, PseudoR2_Table)
### MM: Mark this plot for deletion


# ------ 4. Model-level diagnostics and associated analytics

# --- 4.1 Brier Score

brier_surv  <- mean((datCredit$prob_basic  - datCredit$DefSpell_Event)^2)
brier_surv_adv  <- mean((datCredit$prob_adv  - datCredit$DefSpell_Event)^2)
brier_logit <- mean((datCredit_LR$prob_LR - datCredit_LR$DefSpell_Event)^2)

# - Create a single table containing the three R^2 measures for each of the models
brier_Table <- data.table(
  Model = c("Survival Analysis(Basic)", "Logistic Regression", "Survival Analysis(Advanced)"),
  Brier = c(brier_surv, brier_logit, brier_surv_adv)
)
# - Save table to specified path
pack.ffdf(paste0(genObjPath,"brier_Table"), brier_Table)


datPlot[, Label := paste0(sprintf("%.2f", Value * 100), "%")]

chosenFont <- "Cambria"
vCol1 <- c(
  "a_Basic" = brewer.pal(9, "BuGn")[5],
  "b_Logistic_Regression" = brewer.pal(9, "BuGn")[7],
  "c_Advanced" = brewer.pal(9, "BuGn")[9]
)
vCol2 <- vCol1 
vCol3 <- rep("white", 3)  
vLabel <- list(
  "a_Basic" = "Survival Analysis(Basic)",
  "b_Logistic_Regression" = "Logistic Regression",
  "c_Advanced" = "Survival Analysis(Advanced)"
)
gPlot <- ggplot(datPlot, aes(x = Statistic, y = Value, group = Model)) + 
  theme_minimal() +
  theme(
    legend.position = "bottom",
    text = element_text(family = chosenFont),
    axis.title.x = element_text(margin = margin(t = 5))
  ) +
  labs(
    x = "Statistic",
    y = "Brier Score",
    family = chosenFont
  ) +
  geom_col(aes(colour = Model, fill = Model), position = position_dodge(width = 0.9)) +
  geom_label(
    aes(label = Label),
    fill = vCol2,
    colour = vCol3,
    position = position_dodge(width = 0.9),
    size = 3,
    label.padding = unit(0.15, "lines")
  ) +
  scale_colour_manual(name = "Model:", values = vCol1, labels = vLabel) +
  scale_fill_manual(name = "Model:", values = vCol1, labels = vLabel) +
  scale_y_continuous(limits = c(0, 0.002), breaks = seq(0, 0.002, by = 0.0001))

# Saving the graph to specified path
ggsave(gPlot, file=paste0(genFigPath, "BrierPlot_V2.png"), width=1200/dpi, height=1000/dpi, dpi=400, bg="white")

# - Cleanup
rm(datPlot, gPlot, PseudoR2_Table)




# --- 5 Plotting the ROC curves

# - Creating the plotting dataset
datPlot_ROC <- rbind(data.table(TPR=roc_obj_basic$sensitivities/100,
                                FPR=1-roc_obj_basic$specificities/100,
                                Model="a_Basic"),
                     data.table(TPR=roc_obj_LR$sensitivities/100,
                                FPR=1-roc_obj_LR$specificities/100,
                                Model="b_Logistic_Regression"),
                     data.table(TPR=roc_obj_adv$sensitivities/100,
                                FPR=1-roc_obj_adv$specificities/100,
                                Model="c_Advanced"))

# - Getting the AUCs of each model (so that the values can be used as labels)
dat_anno <- data.table(Model=c("a_Basic", "b_Logistic_Regression","c_Advanced"),
                       AUC=c(roc_obj_basic$auc, roc_obj_LR$auc, roc_obj_adv$auc),
                       x=c(0.5,0.5,0.5), y=c(0.68,0.80, 0.95))
dat_anno[Model=="a_Basic",Label:=paste0("AUC=",sprintf("%.2f",AUC),"% ± ", sprintf("%.2f", (roc_obj_basic$ci[3]-roc_obj_basic$ci[1])/2), "%")]
dat_anno[Model=="b_Logistic_Regression",Label:=paste0("AUC=",sprintf("%.2f",AUC),"% ± ", sprintf("%.2f", (roc_obj_LR$ci[3]-roc_obj_LR$ci[1])/2), "%")]
dat_anno[Model=="c_Advanced",Label:=paste0("AUC=",sprintf("%.2f",AUC),"% ± ", sprintf("%.2f", (roc_obj_adv$ci[3]-roc_obj_adv$ci[1])/2), "%")]

# - Plotting parameters
chosenFont <- "Cambria"; dpi<-200
vCol1 <- brewer.pal(10, "Paired")[c(8,6,4)]
vFill <- brewer.pal(10, "Paired")[c(7,5,3)]
vLineType <- c(3,4)
vLabel <- list("a_Basic"="Survival Analysis(Basic)",
               "b_Logistic_Regression"="Intermediate",
               "c_Advanced"="Survival Analysis(Advanced)")

# - Create main ROC graph by overlaying competing ROC-curves
(g_ROC_compar <- ggplot(datPlot_ROC) + theme_minimal() +
    labs(x=bquote("False positive rate "*italic(F^{"+"})~" = "*italic(1-S^{"-"})), y=bquote("True positive rate "*italic(T^{"+"})~" = "*italic(S^{"+"}))) +
    theme(text=element_text(family=chosenFont), legend.position="bottom",
          axis.text.x=element_text(angle=90), legend.text=element_text(family=chosenFont), 
          strip.background=element_rect(fill="snow2", colour="snow2"),
          strip.text=element_text(size=8, colour="gray50"), strip.text.y.right=element_text(angle=90)) +
    geom_line(aes(x=FPR, y=TPR, colour=Model)) +
    # geom_area(aes(x=FPR, y=TPR, colour=Model, alpha=0.4)) +
    geom_abline(intercept=0, slope=1, linetype=1, linewidth=0.3) +
    geom_label(data=dat_anno, aes(x=x, label=Label, y=y, colour=Model), show.legend=F, size=2) +
    scale_colour_manual(name="Model", values=vCol1, label=vLabel) +
    scale_linetype_manual(name="Model", values=vLineType, label=vLabel) +
    scale_x_continuous(breaks=pretty_breaks(), label=percent) +
    scale_y_continuous(breaks=pretty_breaks(), label=percent))

# - Saving the graph
ggsave(g_ROC_compar, file=paste0(genFigPath, "ROC_Curves_Comparison.png"), width=1200/dpi, height=1000/dpi, dpi=dpi, bg="white")

# - Cleanup
rm(datPlot_ROC, dat_anno, g_ROC_compar)
### MM: Consider plot for deletion




# --- 6. Plotting model diagnostics: AUC vs pseudo R^2-measures

# - Creating the plotting dataset
datPlot_diag <- rbind(data.table(Statistic=c("Coef_Deter", "AUC"),
                                 Value = c(as.numeric(sub("%","",coefDeter_Basic[[1]])), roc_obj_basic$auc),
                                 Model=rep("a_Basic",2)),
                      data.table(Statistic=c("Coef_Deter","AUC"),
                                 Value = c(as.numeric(sub("%","",coefDeter_LR[[1]])), roc_obj_LR$auc),
                                 Model=rep("b_Logistic_Regression",2)),
                      data.table(Statistic=c("Coef_Deter","AUC"),
                                 Value = c(as.numeric(sub("%","",coefDeter_Adv[[1]])),roc_obj_adv$auc),
                                 Model=rep("c_Advanced",2)))
datPlot_diag[, Label:=paste0(as.character(sprintf("%.2f", Value)),"%")]
datPlot_diag[Statistic=="AUC" & Model=="a_Basic",Label:=paste0(sprintf("%.2f", Value),"% ± ", sprintf("%.2f", (roc_obj_basic$ci[3]-roc_obj_basic$ci[1])/2), "%")]
datPlot_diag[Statistic=="AUC" & Model=="b_Logistic_Regression",Label:=paste0(sprintf("%.2f", Value),"% ± ", sprintf("%.2f", (roc_obj_LR$ci[3]-roc_obj_LR$ci[1])/2), "%")]
datPlot_diag[Statistic=="AUC" & Model=="c_Advanced",Label:=paste0(sprintf("%.2f", Value),"% ± ", sprintf("%.2f", (roc_obj_adv$ci[3]-roc_obj_adv$ci[1])/2), "%")]

# - Plotting parameters
chosenFont <- "Cambria"; dpi<-180
vCol1 <- c("a_Basic"=brewer.pal(9, "Blues")[4], "b_Logistic_Regression"=brewer.pal(9, "Blues")[7], "c_Advanced"=brewer.pal(9, "Blues")[9])
vCol2 <- c(rep(vCol1[1],2), rep(vCol1[2],2), rep(vCol1[3],2))
vCol3 <- rep("white", 3*2)
vLineType <- c(3,4)
vLabel <- list("a_Basic"="Survival Analysis(Basic)",
               "b_Logistic_Regression"="Logistic Regression",
               "c_Advanced"="Survival Analysis(Advanced)")

# - Creating the clustered column chart
(g_model_diag_compar <- ggplot(datPlot_diag, aes(x=Statistic, y=Value, group=Model)) +
    theme_minimal() + theme(legend.position = "bottom", text=element_text(family=chosenFont)) + labs(x="Statistic", y="Value") +
    geom_col(aes(colour=Model, fill=Model), position="dodge") +
    geom_label(aes(label=Label), fill = vCol2, colour = vCol3, position=position_dodge(0.9)) +
    scale_colour_manual(name="Model:", values=vCol1, labels=vLabel) +
    scale_fill_manual(name="Model:", values=vCol1, labels=vLabel) +
    scale_x_discrete(labels=c("AUC"="AUC","Coef_Deter"="Coeffcient of Determination")) +
    scale_y_continuous(breaks=pretty_breaks(), label=percent))

# - Saving the graph
ggsave(g_model_diag_compar, file=paste0(genFigPath, "Diagnostics_Comparison.png"), width=1200/dpi, height=1000/dpi, dpi=dpi, bg="white")

# - Clean up
rm(datPlot_diag, g_model_diag_compar,coefDeter_Adv, coefDeter_Basic, coefDeter_LR); gc()
### MM: Consider deletion, but check if it is actually useful



# --- 7 AUC over time | All write-off models

# - Call custom AUC_overTime() function for each of the three write-off risk models

datCredit[, prob_basic := predict(modLR_basic, newdata = datCredit, type="response")]
datCredit_LR[, prob_LR := predict(modLR_log, newdata = datCredit_LR, type="response")]
datCredit[, prob_adv := predict(modLR, newdata = datCredit, type="response")]

BasAUC <- AUC_overTime(datCredit,"DefSpell_Max_Date","DefSpell_Event","prob_basic")
LRAUC <- AUC_overTime(datCredit_LR,"DefSpell_Max_Date","DefSpell_Event","prob_LR")
AdvAUC <- AUC_overTime(datCredit,"DefSpell_Max_Date","DefSpell_Event","prob_adv")
AdvAUC <- AdvAUC[!AUC_Val<70,]
BasAUC <- BasAUC[!AUC_LowerCI<30,]
BasAUC[,AUC_Val:=AUC_Val/100]
BasAUC[,AUC_LowerCI:=AUC_LowerCI/100]
BasAUC[,AUC_UpperCI:=AUC_UpperCI/100]
LRAUC[,AUC_Val:=AUC_Val/100]
LRAUC[,AUC_LowerCI:=AUC_LowerCI/100]
LRAUC[,AUC_UpperCI:=AUC_UpperCI/100]
AdvAUC[,AUC_Val:=AUC_Val/100]
AdvAUC[,AUC_LowerCI:=AUC_LowerCI/100]
AdvAUC[,AUC_UpperCI:=AUC_UpperCI/100]
# - Differentiation for plotting
BasAUC[,Dataset := "A"]
LRAUC[,Dataset := "C"]
AdvAUC[,Dataset := "B"]

# - Create final dataset for ggplot
datPlot <- rbind(BasAUC,LRAUC,AdvAUC)

# - Aesthetic engineering: annotations
# Location of annotations
start_y <- 0.425
space <- 0.025
y_vals <- c(start_y,start_y-space,start_y-space*2)

# - Creating an annotation dataset for easier annotations
datAnnotate <- data.table(MeanAUC = NULL, Dataset = c("A-B","A-C","A-D"),
                          x = rep(as.Date("2013-05-31"),3), # Text x coordinates
                          y = y_vals )

# - TTC-mean & confidence interval calculations
confLevel <- 0.95
vEventRates_Mean <- c(mean(BasAUC$AUC_Val, na.rm = T), mean(LRAUC$AUC_Val, na.rm = T), mean(AdvAUC$AUC_Val, na.rm = T))
vEventRates_stErr <- c(sd(BasAUC$AUC_Val, na.rm=T) / sqrt(BasAUC[, .N]),
                       sd(LRAUC$AUC_Val, na.rm=T) / sqrt(LRAUC[, .N]),
                       sd(AdvAUC$AUC_Val, na.rm=T) / sqrt(AdvAUC[, .N]) )
vMargin <- qnorm(1-(1-confLevel)/2) * vEventRates_stErr
vLabel <- c(paste0("'TTC-mean over '*italic(t)*' for '*italic(A[t])*' : ", sprintf("%.2f",vEventRates_Mean[1]*100),
                   "% ? ", sprintf("%1.3f", vMargin[1]*100),"%'"),
            paste0("'TTC-mean over '*italic(t)*' for '*italic(B[t])*' : ", sprintf("%.2f",vEventRates_Mean[3]*100),
                   "% ? ", sprintf("%1.3f", vMargin[3]*100),"%'"),
            paste0("'TTC-mean over '*italic(t)*' for '*italic(C[t])*' : ", sprintf("%.2f",vEventRates_Mean[2]*100),
                   "% ? ", sprintf("%1.3f", vMargin[2]*100),"%'") )
datAnnotate[, Label := vLabel]

# - Graphing parameters
chosenFont <- "Cambria"; dpi <- 180
vCol <- brewer.pal(8, "Dark2")[c(2,1,3)]
vLabel <- c("A"=bquote(italic(A[t])~": DtH-Basic A"), "C"=bquote(italic(C[t])~": Logistic Regression A"), 
            "B"=bquote(italic(B[t])~": DtH-Advanced A"))
vShape <- c(17,20,4) 

# - Create graph
(g3 <- ggplot(datPlot, aes(x=Date, y=AUC_Val)) + theme_minimal() + 
    labs(y=bquote("Prediction Accuracy: AUC (%) "), x=bquote("Default spell cohorts (mmmccyy): stop time "*italic(t[s]))) + 
    theme(text=element_text(family=chosenFont),legend.position = "bottom",legend.margin=margin(-10, 0, 0, 0),
          axis.text.x=element_text(angle=90), 
          strip.background=element_rect(fill="snow2", colour="snow2"),
          strip.text=element_text(size=11, colour="gray50"), strip.text.y.right=element_text(angle=90)) + 
    # Main graph
    geom_ribbon(aes(fill=Dataset, ymin=AUC_LowerCI, ymax=AUC_UpperCI), alpha=0.2,show.legend = FALSE) + 
    geom_line(aes(colour=Dataset, linetype=Dataset), linewidth=0.3) +    
    geom_point(aes(colour=Dataset, shape=Dataset), size=1.8) + 
    geom_hline(yintercept = 0.7, linewidth=0.75) +
    geom_text(data=datAnnotate, aes(x=x, y=y, label = Label), family=chosenFont, size=3.5, hjust=0, parse=TRUE) +
    # Facets & scale options
    scale_colour_manual(name="Model", values=vCol, labels=vLabel) + 
    scale_fill_manual(name="Model", values=vCol, labels=vLabel) +
    scale_shape_manual(name=bquote("Model"), values=vShape, labels=vLabel) + 
    scale_linetype_discrete(name=bquote("Model"), labels=vLabel) + 
    scale_x_date(date_breaks=paste0(6, " month"), date_labels = "%b %Y") +
    scale_y_continuous(breaks=pretty_breaks(), label=percent)
)

# - Save graph
ggsave(g3, file=paste0(genFigPath, "AUC-time.png"), width=1200/dpi, height=1000/dpi, dpi=dpi, bg="white")

# - Cleanup
rm(datAnnotate, g3, datPlot, BasAUC, LRAUC, AdvAUC)












