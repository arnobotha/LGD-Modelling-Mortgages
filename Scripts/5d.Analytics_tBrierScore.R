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

# - Create a copy of DefSpell_Age to serve as an input into the classical LGD-model
datTrain_classic[, DefSpell_Age2 := DefSpell_Age]
datValid_classic[, DefSpell_Age2 := DefSpell_Age]
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
                  "DefSpell_Age2", "g0_Delinq_Num", "Arrears",
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
datCredit[, DefSpell_Age2 := TimeInDefSpell]

# --- Basic discrete-time hazard model

objCoxDisc_bas <- tBrierScore(datCredit, modGiven=modLR_basic, predType="response", spellPeriodMax=120, fldKey="DefSpell_Key", 
                              fldStart="Start", fldStop="TimeInDefSpell",fldCensored="DefSpell_Censored", 
                              fldSpellAge="DefSpell_Age", fldSpellOutcome="DefSpellResol_Type_Hist")

# --- Advanced discrete-time hazard model

objCoxDisc_adv <- tBrierScore(datCredit, modGiven=modLR, predType="response", spellPeriodMax=120, fldKey="DefSpell_Key", 
                              fldStart="Start", fldStop="TimeInDefSpell",fldCensored="DefSpell_Censored", 
                              fldSpellAge="DefSpell_Age", fldSpellOutcome="DefSpellResol_Type_Hist")

# --- Advanced discrete-time hazard model

objLR <- tBrierScore_classic(datCredit, modGiven=modLR_classic, predType="response", spellPeriodMax=120, fldKey="DefSpell_Key", 
                              fldStart="Start", fldStop="DefSpell_Age2",fldCensored="DefSpell_Censored", 
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
vLabel <- c("a_Basic"="DtH-Basic A", "b_Advanced"="DtH-Advanced A")

# - Main graph of tBS
(gOuter <- ggplot(datGraph, aes(x=TimeInDefSpell, y=Brier, group=Type)) + theme_minimal() + 
    labs(y=bquote("Time-dependent Brier Score "*italic(B)[s](italic(t))), x=bquote("Spell time (months)"*~italic(t))) + 
    theme(text=element_text(family=chosenFont),legend.position="bottom", 
          strip.background=element_rect(fill="snow2", colour="snow2"),
          strip.text = element_text(size=8, colour="gray50"), strip.text.y.right = element_text(angle=90)) + 
    # Main graph
    geom_line(aes(colour=Type, linetype=Type), linewidth=0.5) + 
    # Annotations
    annotate(geom="text", x=26, y=8, label=paste0("IBS (Basic A): ", round(objCoxDisc_bas$IBS,3)), 
             family=chosenFont, size=3.5, colour=vCol[1]) + 
    annotate(geom="text", x=22, y=6, label=paste0("IBS (Advanced A): ", round(objCoxDisc_adv$IBS,3)), 
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
    annotate(geom="text", x=20, y=1.5, label=paste0("IBS (Basic A): ", round(ibs_bas,3)), 
             family=chosenFont, size=3.5, colour=vCol[1]) + 
    annotate(geom="text", x=17, y=1, label=paste0("IBS (Advanced A): ", round(ibs_adv,3)), 
             family=chosenFont, size=3.5, colour=vCol[2]) +   
    # Facets & scales
    scale_colour_manual(name="", values=vCol, labels=vLabel) + 
    scale_linetype_discrete(name="", labels=vLabel) +
    scale_y_continuous(breaks=breaks_pretty(), label=comma) + 
    scale_x_continuous(breaks=breaks_pretty(), label=comma)
)

# - Combining the two above plots onto a single graph
(plot.full <- gOuter + annotation_custom(grob = ggplotGrob(gInner), xmin=5, xmax=78, ymin=10, ymax=30))

# - Save plot
dpi <- 280
ggsave(plot.full, file=paste0(genFigPath,"tBrierScores_CoxDisc.png"),width=1600/dpi, height=1200/dpi,dpi=dpi, bg="white")




tBS_Adv <- objCoxDisc_adv$tBS
tBS_Bas <- objCoxDisc_bas$tBS
tBS_LR <- objLR$tBS
tBS_Adv <- tBS_Adv[TimeInDefSpell==44,]
tBS_Bas <- tBS_Bas[TimeInDefSpell==44,]
tBS_LR <- tBS_LR[DefSpell_Age2==44,]
tBrier_Adv <- tBS_Adv$Brier
tBrier_Bas <- tBS_Bas$Brier
tBrier_LR <- tBS_LR$Brier

# - Create a single table containing the three R^2 measures for each of the models
brier_Table <- data.table(
  Model = c("DtH-Advanced A","DtH-Basic A", "Logistic Regression" ),
  Brier = c(tBrier_Bas, tBrier_Adv, tBrier_LR)
)
# - Save table to specified path
pack.ffdf(paste0(genObjPath,"brier_Table"), brier_Table)

datPlot<-data.table(Statistic=rep(c("Models "),
                                  each=3),Model=rep(c("a_Basic","b_Advanced", "c_Logistic_Regression"),times=1),Value=
                      as.numeric(sub("%","",c(brier_Table$Brier)))/100)

# - Aesthetic engineering
datPlot[, Label:=paste0(sprintf("%.2f", Value*100),"%")]


chosenFont <- "Cambria"
vCol1 <- c(
  "a_Basic" = brewer.pal(9, "BuGn")[5],
  "b_Advanced" = brewer.pal(9, "BuGn")[7],
  "c_Logistic_Regression" = brewer.pal(9, "BuGn")[9]
)
vCol2 <- vCol1 
vCol3 <- rep("white", 3)  
vLabel <- list(
  "a_Basic" = "DtH-Basic A",
  "b_Advanced" = "DtH-Advanced A",
  "c_Logistic_Regression" = "Logistic Regression A"
)
gPlot <- ggplot(datPlot, aes(x = Statistic, y = Value, group = Model)) + 
  theme_minimal() +
  theme(
    legend.position = "bottom",
    text = element_text(family = chosenFont),
    axis.title.x = element_text(margin = margin(t = 5))
  ) +
  labs(y=bquote("Time-dependent Brier Score "*italic(B)[s](italic(t))), x="") +
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
  scale_y_continuous(labels = scales::percent_format(accuracy = 1))

# Saving the graph to specified path
dpi <- 180
ggsave(gPlot, file=paste0(genFigPath, "BrierPlot_V2.png"), width=1200/dpi, height=1000/dpi, dpi=400, bg="white")

# -- Cleanup
rm(gInner, datGraph, gOuter, objCoxDisc_adv,objCoxDisc_bas ,plot.full)





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
