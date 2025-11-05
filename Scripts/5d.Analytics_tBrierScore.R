# ========================================= TIME-DEPENDENT BRIER SCORES ================================
# Calculate and compare the time-dependent Brier scores across survival models
# ------------------------------------------------------------------------------------------------------
# PROJECT TITLE: Default survival modelling
# SCRIPT AUTHOR(S): Dr Arno Botha (AB)
# ------------------------------------------------------------------------------------------------------
# -- Script dependencies:
#   - 0.Setup.R
#   - 1.Data_Import.R
#   - 2a.Data_Prepare_Credit_Basic.R
#   - 2b.Data_Prepare_Credit_Advanced.R
#   - 2c.Data_Prepare_Credit_Advanced2.R
#   - 2d.Data_Enrich.R
#   - 2f.Data_Fusion1.R
#   - 3b.Data_Fusion2_PWP_ST.R
#   - 5a(i).CoxPropHaz_PWP_Advanced.R
#   - 5a(i).CoxPropHaz_PWP_Basic.R
#   - 5b(i).CoxDiscreteTime_Advanced.R
#   - 5b(ii).CoxDiscreteTime_Basic.R

# -- Inputs:
#   - datCredit_train_PWPST | Prepared from script 3b
#   - datCredit_valid_PWPST | Prepared from script 3b
#
# -- Outputs:
#   - <Analytics> | Graphs
# ------------------------------------------------------------------------------------------------------






# ----------------- 1. Load & prepare data for analysis

# - Confirm prepared datasets are loaded into memory
if (!exists('datCredit_train_CDH')) unpack.ffdf(paste0(genPath,"creditdata_train_CDH"), tempPath);gc()
if (!exists('datCredit_valid_CDH')) unpack.ffdf(paste0(genPath,"creditdata_valid_CDH"), tempPath);gc()

# - Use only default spells
datCredit_train <- datCredit_train_CDH[!is.na(DefSpell_Key),]
datCredit_valid <- datCredit_valid_CDH[!is.na(DefSpell_Key),]

# Create start and stop columns
datCredit_train[, Start := TimeInDefSpell - 1]
datCredit_valid[, Start := TimeInDefSpell - 1]

# - Weigh default cases heavier. as determined interactively based on calibration success (script 6e)
datCredit_train[, Weight := ifelse(DefSpell_Event==1,1,1)]
datCredit_valid[, Weight := ifelse(DefSpell_Event==1,1,1)] # for merging purposes

# - Assign sample labels
datCredit_train[, Sample := "Training"]
datCredit_valid[, Sample := "Validation"]

# Validation set
datTrain_classic <- subset(datCredit_train, DefSpell_Counter==1)
datValid_classic <- subset(datCredit_valid, DefSpell_Counter==1)

# - Assign target/response field for the classical PD-model
datTrain_classic[, DefSpell_Event := ifelse(DefSpellResol_Type_Hist=="WOFF", 1, 0)]
datValid_classic[, DefSpell_Event := ifelse(DefSpellResol_Type_Hist=="WOFF", 1, 0)]
datTrain_classic[, Start := DefSpell_Age - 1]
datValid_classic[, Start := DefSpell_Age - 1]
# ----------------- 2b. Fit a discrete-time hazard model on the resampled prepared data


# ------ Basic discrete-time hazard model
# - Initialize variables
vars_basic <- c("log(TimeInDefSpell)","DefSpell_Num_binned", "g0_Delinq_Lag_1",
                "M_Inflation_Growth_9","g0_Delinq_Any_Aggr_Prop_Lag_1")

# - Fit discrete-time hazard model with selected variables
modLR_basic <- glm( as.formula(paste("DefSpell_Event ~", paste(vars_basic, collapse = " + "))),
                    data=datCredit_train, family="binomial", weights = Weight)



# ------ Advanced discrete-time hazard model
# - Initialize variables
vars <- c("Time_Binned","log(TimeInDefSpell)*DefSpell_Num_binned", 
          "DefaultStatus1_Aggr_Prop_Lag_12","g0_Delinq_Ave", "g0_Delinq_Lag_1",
          "InterestRate_Margin_Aggr_Med_9","NewLoans_Aggr_Prop","InterestRate_Nom",
          "Balance_1","pmnt_method_grp","Principal",
          "M_RealIncome_Growth_9", "M_Inflation_Growth_12","M_DTI_Growth_12","M_Repo_Rate_12","g0_Delinq_Any_Aggr_Prop_Lag_1")

# - Fit discrete-time hazard model with selected variables
modLR <- glm( as.formula(paste("DefSpell_Event ~", paste(vars, collapse = " + "))),
              data=datCredit_train, family="binomial", weights = Weight)

vars_classic <- c("g0_Delinq_Any_Aggr_Prop_Lag_12","DefaultStatus1_Aggr_Prop",
                  "CuringEvents_Aggr_Prop","PrevDefaults",
                  "DefSpell_Age", "g0_Delinq_Num", "Arrears",
                  "slc_past_due_amt_imputed_med", "DefSpell_Num_binned",
                  "InterestRate_Margin_Aggr_Med", "AgeToTerm_Aggr_Mean", "AgeToTerm",
                  "BalanceToPrincipal", "pmnt_method_grp",
                  "M_RealIncome_Growth_12", "M_Inflation_Growth_3","M_DTI_Growth_6","M_Repo_Rate_2")

# - Fit logistic regression model onto entire spell, similar to application credit scoring with an open-ended outcome window
modLR_classic <- glm( as.formula(paste("DefSpell_Event ~", paste(vars_classic, collapse = " + "))),
                      data=datTrain_classic, family="binomial")
summary(modLR_classic)

# ----------------- 3. Calculate time-dependent Brier scores across survival models

# - Create pointer to the appropriate data object 
datCredit <- rbind(datCredit_train, datCredit_valid)
datCredit_LR <- rbind(datTrain_classic, datValid_classic)


# --- Basic discrete-time hazard model

objCoxDisc_bas <- tBrierScore(datCredit, modGiven=modLR_basic, predType="response", spellPeriodMax=120, fldKey="DefSpell_Key", 
                              fldStart="Start", fldStop="TimeInDefSpell",fldCensored="DefSpell_Censored", 
                              fldSpellAge="DefSpell_Age", fldSpellOutcome="DefSpellResol_Type_Hist")

# --- Advanced discrete-time hazard model

objCoxDisc_adv <- tBrierScore(datCredit, modGiven=modLR, predType="response", spellPeriodMax=120, fldKey="DefSpell_Key", 
                              fldStart="Start", fldStop="TimeInDefSpell",fldCensored="DefSpell_Censored", 
                              fldSpellAge="DefSpell_Age", fldSpellOutcome="DefSpellResol_Type_Hist")



# ----------------- 4. Graph tBS-values across models


# --- Discrete-time hazard models

# - Data fusion across models
datGraph <- rbind(data.table(objCoxDisc_bas$tBS, Type="a_Basic"),
                  data.table(objCoxDisc_adv$tBS, Type="b_Advanced"))

# - Aesthetic engineering
datGraph[, FacetLabel := "Discrete-time hazard models"]
zoomedSpellAge <- 48

# - Recalculate Integrated tBS over zoomed spell age
ibs_bas <- mean(objCoxDisc_bas$tBS[TimeInDefSpell <= zoomedSpellAge, Brier])
ibs_adv <- mean(objCoxDisc_adv$tBS[TimeInDefSpell <= zoomedSpellAge, Brier])

# - Graphing Parameters
chosenFont <- "Cambria"
vCol <- brewer.pal(8, "Set2")[c(2,1)]
vLabel <- c("a_Basic"="Basic", "b_Advanced"="Advanced")

# - Main graph of tBS
(gOuter <- ggplot(datGraph, aes(x=TimeInDefSpell, y=Brier, group=Type)) + theme_minimal() + 
    labs(y=bquote("Time-dependent Brier Score "*italic(B)[s](italic(t))), x=bquote("Default spell age (months)"*~italic(t))) + 
    theme(text=element_text(family=chosenFont),legend.position="bottom", 
          strip.background=element_rect(fill="snow2", colour="snow2"),
          strip.text = element_text(size=8, colour="gray50"), strip.text.y.right = element_text(angle=90)) + 
    # Main graph
    geom_line(aes(colour=Type, linetype=Type), linewidth=0.5) + 
    # Annotations
    annotate(geom="text", x=95, y=20, label=paste0("IBS (Basic): ", round(objCoxDisc_bas$IBS,3)), 
             family=chosenFont, size=3.5, colour=vCol[1]) + 
    annotate(geom="text", x=91, y=18, label=paste0("IBS (Advanced): ", round(objCoxDisc_adv$IBS,3)), 
             family=chosenFont, size=3.5, colour=vCol[2]) +     
    # Facets & scales
    facet_grid(FacetLabel ~ .) +  
    scale_colour_manual(name="Model", values=vCol, labels=vLabel) + 
    scale_linetype_discrete(name="Model", labels=vLabel) + 
    scale_y_continuous(breaks=breaks_pretty(), label=comma) + 
    scale_x_continuous(breaks=breaks_pretty(), label=comma)
)

# - Zoomed inset graph of tBS on a smaller time scale
(gInner <- ggplot(datGraph[TimeInDefSpell<=zoomedSpellAge,], aes(x=TimeInDefSpell, y=Brier, group=Type)) + 
    theme_bw() + labs(y="", x="") + 
    theme(legend.position=c(0.75,0.40), text=element_text(size=12, family="Cambria"),
          #specific for plot-in-plot
          axis.text.y=element_text(margin=unit(c(0,0,0,0), "mm"), size=9),
          axis.text.x=element_text(margin=unit(c(0,0,0,0), "mm"), size=9),
          axis.ticks=element_blank(), axis.title.x=element_blank(), #axis.title.y=element_blank(),
          panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
          panel.background = element_rect(color="black", fill="white"),
          plot.background = element_rect(color="white"), plot.margin = unit(c(0,0,0,0),"mm"),
          plot.title = element_text(hjust=0.55,vjust=-10,margin=margin(t=-12))) + 
    # Main graph
    geom_line(aes(colour=Type, linetype=Type), linewidth=0.5, show.legend = F) + 
    # Annotations
    annotate(geom="text", x=20, y=0.6, label=paste0("IBS (Basic): ", round(ibs_bas,3)), 
             family=chosenFont, size=3.5, colour=vCol[1]) + 
    annotate(geom="text", x=17, y=0.45, label=paste0("IBS (Advanced): ", round(ibs_adv,3)), 
             family=chosenFont, size=3.5, colour=vCol[2]) +   
    # Facets & scales
    scale_colour_manual(name="", values=vCol, labels=vLabel) + 
    scale_linetype_discrete(name="", labels=vLabel) +
    scale_y_continuous(breaks=breaks_pretty(), label=comma) + 
    scale_x_continuous(breaks=breaks_pretty(), label=comma)
)

# - Combining the two above plots onto a single graph
(plot.full <- gOuter + annotation_custom(grob = ggplotGrob(gInner), xmin=10, xmax=90, ymin=5, ymax=15))

# - Save plot
dpi <- 280
ggsave(plot.full, file=paste0(genFigPath,"tBrierScores_CoxDisc.png"),width=1350/dpi, height=1000/dpi,dpi=dpi, bg="white")


# -- Cleanup
rm(gInner, datGraph, gOuter, objCoxDisc_adv,objCoxDisc_bas ,plot.full)

tBS_Adv <- objCoxDisc_adv$tBS
tBS_Bas <- objCoxDisc_bas$tBS
tBS_Adv <- tBS_Adv[TimeInDefSpell==44,]
tBS_Bas <- tBS_Bas[TimeInDefSpell==44,]
tBrier_Adv <- tBS_Adv$Brier
tBrier_Bas <- tBS_Bas$Brier



TimePeriod <- 44


datCredit_LR[, prob_LR := predict(modLR_classic, newdata= datCredit_LR ,type = "response")]
datCredit_LR[, S_hat   := 1 - prob_LR]                     # Pr(survive past t)
datCredit_LR[, y_t     := as.integer(DefSpell_Age > TimePeriod)]

datCredit_LR[, cens := as.integer(DefSpell_Event == 0)]


km_cens <- survfit(Surv(DefSpell_Age, cens) ~ 1, data = datCredit_LR)
eval_times <- sort(unique(c(datCredit_LR$DefSpell_Age, TimePeriod)))
cens_sum   <- summary(km_cens, times = eval_times)

G_TimePeriod <- approx(cens_sum$time, cens_sum$surv, xout = TimePeriod, rule = 2)$y
datCredit_LR[, G_Ti := approx(cens_sum$time, cens_sum$surv, xout = DefSpell_Age, rule = 2)$y]

datCredit_LR[, weight := fcase(
  DefSpell_Event == 1 & DefSpell_Age <= TimePeriod, 1 / G_Ti,
  DefSpell_Age > TimePeriod,                         1 / G_TimePeriod,
  default = 0
)]
datCredit_LR[, weight := pmin(weight, 10)]  # ??? REQUIRED for R²


datCredit_LR[, SE := (y_t - S_hat)^2]


brier <- weighted.mean(datCredit_LR$SE, datCredit_LR$weight, na.rm = TRUE)





##### TO-DO
# - Compare tBS for Cox PH model with that obtained from pec(). And debug!

library(riskRegression)

y <- Score(list("Cox.bas"=cox_PWPST_basic),
           formula=as.formula(paste0("Hist(TimeInPerfSpell,DefaultStatus1) ~ 1")),
           data=datCredit_train, conf.int=FALSE, summary=c("risks","IPA","ibs"),
           times=1:120,ncpus=4, parallel="multicore") 

plot(y$Brier$score$Brier)



library(pec)

test <- pec(
  object = list("Cox.bas" = cox_PWPST_basic),
  formula = Surv(TimeInPerfSpell, PerfSpell_Event) ~ 1,
  data = datCredit_train,
  times = 1:120,
  exact = T,
  cens.model = "marginal"
)
### REESULTS: This does not work. Need to apply mind.
