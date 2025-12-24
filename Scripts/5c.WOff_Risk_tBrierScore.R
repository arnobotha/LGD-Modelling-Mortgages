# =========================== TIME-DEPENDENT BRIER SCORES ======================
# Calculate and compare the time-dependent Brier scores across write-off models
# ------------------------------------------------------------------------------
# PROJECT TITLE: Default survival modelling
# SCRIPT AUTHOR(S): Mohammed Garbu (MG), Marcel Muller (MM) Dr Arno Botha (AB)
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
#   - 4e.Dichotomisation 

# -- Inputs:
#   - datCredit_train_CDH | Prepared from script 2g
#   - datCredit_valid_CDH | Prepared from script 2g
#   - modLR_Bas | Basic discrete time model as fitted in script 4b(ii)
#   - modLR_Adv | Advanced discrete time model as fitted in script 4b(i)
#   - modLR_Classic | Classical logistic regression model as fitted in script 4c
#   - thres_lst | Thresholds for classifying predictions as determined in script 4e
# -- Outputs:
#   - <Analytics> | Graphs
# ------------------------------------------------------------------------------


# ------- 1. Load & prepare data for analysis

# --- 1.1 Load and prepare data
# - Confirm prepared datasets are loaded into memory
if (!exists('datCredit_train_CDH')) unpack.ffdf(paste0(genPath,"creditdata_train_CDH"), tempPath); gc()
if (!exists('datCredit_valid_CDH')) unpack.ffdf(paste0(genPath,"creditdata_valid_CDH"), tempPath); gc()

# - Use only default spells
datCredit_train <- datCredit_train_CDH[!is.na(DefSpell_Key),]
datCredit_valid <- datCredit_valid_CDH[!is.na(DefSpell_Key),]

# Create start and stop columns
datCredit_train[, Start:=TimeInDefSpell-1]
datCredit_valid[, Start:=TimeInDefSpell-1]

# - Weigh default cases heavier. as determined interactively based on calibration success (script 6e)
datCredit_train[, Weight:=ifelse(DefSpell_Event==1,1,1)]
datCredit_valid[, Weight:=ifelse(DefSpell_Event==1,1,1)]

# - Score data using classic model for each instance of [TimeInDefSpell] as [DefSpell_Age]
# Training dataset
datCredit_train[, DefSpell_Age2:=DefSpell_Age]; datCredit_train[, DefSpell_Age:=TimeInDefSpell]
# Validation dataset
datCredit_valid[, DefSpell_Age2:=DefSpell_Age]; datCredit_valid[, DefSpell_Age:=TimeInDefSpell]

# - Remove unfiltered data
rm(datCredit_train_CDH, datCredit_valid_CDH); gc()

# - Combine training and validation datasets to facilitate "better" (smooth) graphs
datCredit <- rbind(datCredit_train, datCredit_valid)


# --- 1.2 Load models
# - Basic discrete-time hazard model
modLR_Bas <- readRDS(paste0(genObjPath,"CoxDisc_Basic_Model.rds"))

# - Advanced discrete-time hazard model
modLR_Adv <- readRDS(paste0(genObjPath,"CoxDisc_Advanced_Model.rds"))

# - Classical logit model
modLR_Classic <- readRDS(paste0(genObjPath,"LR_Model.rds"))


# --- 1.3 Additional parameters
# - Youden Index cut-offs
# Load thresholds
thresh_lst <- readRDS(file=paste0(genObjPath,"Classification_Thresholds.rds"))
# Basic discrete-time model
(thresh_dth_bas <- thresh_lst[["Basic"]]) # 0.0525335
# Advanced discrete-time model
(thresh_dth_adv <- thresh_lst[["Advanced"]]) # 0.3894059
# Classical logit model
(thresh_classic <- thresh_lst[["Classical"]]) # 0.5652506


# --- 1.4 Dichotomise model predictions using estimated thresholds
# - Make predictions using the fitted models
datCredit[, prob_basic:=predict(modLR_Bas, newdata=datCredit, type="response")]
datCredit[, prob_adv:=predict(modLR_Adv, newdata=datCredit, type="response")]
datCredit[, prob_classic:=predict(modLR_Classic, newdata=datCredit, type="response")]

# - Dichotomise predictions
datCredit[, DefSpell_Event_Bas_Youden:=ifelse(prob_basic>thresh_dth_bas,1,0)]
datCredit[, DefSpell_Event_Adv_Youden:=ifelse(prob_adv>thresh_dth_adv,1,0)]
datCredit[, DefSpell_Event_Classic_Youden:=ifelse(prob_classic>thresh_classic,1,0)]




# ------ 2. Calculate time-dependent Brier scores across write-off models

# --- 2.1 Basic discrete-time hazard model | A-series (non-dichotomised)
(objCoxDisc_bas <- tBrierScore(datCredit, modGiven=modLR_Bas, predType="response", spellPeriodMax=120, fldKey="DefSpell_Key", 
                               fldStart="Start", fldStop="TimeInDefSpell",fldCensored="DefSpell_Censored", 
                               fldSpellAge="DefSpell_Age2", fldSpellOutcome="DefSpellResol_Type_Hist", brierType="EventRate"))
### RESULTS: Integrated Brier Score = 8.420888%


# --- 2.2 Advanced discrete-time hazard model | A-series (non-dichotomised)
(objCoxDisc_adv <- tBrierScore(datCredit, modGiven=modLR_Adv, predType="response", spellPeriodMax=120, fldKey="DefSpell_Key", 
                               fldStart="Start", fldStop="TimeInDefSpell",fldCensored="DefSpell_Censored", 
                               fldSpellAge="DefSpell_Age2", fldSpellOutcome="DefSpellResol_Type_Hist", brierType="EventRate"))
### RESULTS: Integrated Brier Score = 1.770236%


# --- 2.3 Classical logistic regression model | A-series (non-dichotomised)
(objCoxDisc_classic <- tBrierScore(datCredit, modGiven=modLR_Classic, predType="response", spellPeriodMax=120, fldKey="DefSpell_Key", 
                                   fldStart="Start", fldStop="TimeInDefSpell",fldCensored="DefSpell_Censored", 
                                   fldSpellAge="DefSpell_Age2", fldSpellOutcome="DefSpellResol_Type_Hist", brierType="EventRate"))
### RESULTS: Integrated Brier Score = 14.10252%


# --- 2.4 Basic discrete-time hazard model | B-series (dichotomised)
(objCoxDisc_bas <- tBrierScore(datCredit, modGiven=modLR_Bas, predType="response", spellPeriodMax=120, fldKey="DefSpell_Key", 
                               fldStart="Start", fldStop="TimeInDefSpell",fldCensored="DefSpell_Censored", 
                               fldSpellAge="DefSpell_Age2", fldSpellOutcome="DefSpellResol_Type_Hist",
                               threshold=thresh_dth_bas, brierType="Survival"))


# --- 2.5 Advanced discrete-time hazard model | B-series (dichotomised)
(objCoxDisc_adv <- tBrierScore(datCredit, modGiven=modLR_Adv, predType="response", spellPeriodMax=120, fldKey="DefSpell_Key", 
                               fldStart="Start", fldStop="TimeInDefSpell",fldCensored="DefSpell_Censored", 
                               fldSpellAge="DefSpell_Age2", fldSpellOutcome="DefSpellResol_Type_Hist",
                               threshold=thresh_dth_adv, brierType="EventRate"))


# --- 2.6 Advanced discrete-time hazard model | B-series (dichotomised)
(objCoxDisc_classic <- tBrierScore(datCredit, modGiven=modLR_Classic, predType="response", spellPeriodMax=120, fldKey="DefSpell_Key", 
                                   fldStart="Start", fldStop="TimeInDefSpell",fldCensored="DefSpell_Censored", 
                                   fldSpellAge="DefSpell_Age2", fldSpellOutcome="DefSpellResol_Type_Hist",
                                   threshold=thresh_classic, brierType="EventRate"))




# ------ 3. Graph tBS-values across models

# --- 3.1 Discrete-time hazard models | A series (non-dichotomised)
# - Data fusion across models
datGraph <- rbind(data.table(objCoxDisc_bas$tBS, Type="a_Basic"),
                  data.table(objCoxDisc_adv$tBS, Type="b_Advanced"))

# - Aesthetic engineering
datGraph[, FacetLabel:="Discrete-time hazard models"]
specified_SpellAge <- 48

# - Recalculate Integrated tBS over zoomed spell age
(ibs_bas <- mean(objCoxDisc_bas$tBS[TimeInDefSpell <= specified_SpellAge, Brier]))
(ibs_adv <- mean(objCoxDisc_adv$tBS[TimeInDefSpell <= specified_SpellAge, Brier]))

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
(gInner <- ggplot(datGraph[TimeInDefSpell<=specified_SpellAge,], aes(x=TimeInDefSpell, y=Brier, group=Type)) + 
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
ggsave(plot.full, file=paste0(genFigPath,"tBrierScores_CoxDisc.png"), width=1600/dpi, height=1200/dpi,dpi=dpi, bg="white")




# ------ 4. Compare Brier scores at a specific time period across models

# --- 4.1 Create a object for comparing Brier scores
# - Extract time-dependent Brier scores for each model
tBS_Adv <- objCoxDisc_adv$tBS
tBS_Bas <- objCoxDisc_bas$tBS
tBS_Classic <- objCoxDisc_classic$tBS

# - Extract time-dependent Brier scores at time 44 for each model
(tBrier_Bas <- as.numeric(tBS_Bas[TimeInDefSpell==44,Brier]))
(tBrier_Adv <- as.numeric(tBS_Adv[TimeInDefSpell==44,Brier]))
(tBrier_Classic <- as.numeric(tBS_Classic[TimeInDefSpell==44,Brier]))

# - Create a single table containing the three R^2 measures for each of the models
(datBrier <- data.table(Model=c("DtH-Basic A","DtH-Advanced A", "Logistic Regression"),
                        Brier=c(tBrier_Bas, tBrier_Adv, tBrier_Classic)))

# - Save table to specified path
saveRDS(datBrier, paste0(genObjPath,"Brier_Table.rds"))


# --- 4.2 Visualise the time-dependent Brier scores at the chosen time point (44)
# - Create plotting dataset
datPlot<-data.table(Statistic=rep(c("Models "),each=3),
                    Model=rep(c("a_Basic","b_Advanced", "c_Logistic_Regression"),times=1),
                    Value=as.numeric(sub("%","",c(brier_Table$Brier)))/100)

# - Aesthetic engineering
datPlot[, Label:=paste0(sprintf("%.2f", Value*100),"%")]

# - Graphing parameters
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

# - Plot
(gPlot <- ggplot(datPlot, aes(x = Statistic, y = Value, group = Model)) + 
  theme_minimal() +
  theme(legend.position = "bottom",
    text = element_text(family = chosenFont),
    axis.title.x = element_text(margin = margin(t = 5))) +
  labs(y=bquote("Time-dependent Brier Score "*italic(B)[s](italic(t))), x="") +
  geom_col(aes(colour = Model, fill = Model), position = position_dodge(width = 0.9)) +
  geom_label(aes(label = Label),
    fill = vCol2,
    colour = vCol3,
    position = position_dodge(width = 0.9),
    size = 3,
    label.padding = unit(0.15, "lines")) +
  scale_colour_manual(name = "Model:", values = vCol1, labels = vLabel) +
  scale_fill_manual(name = "Model:", values = vCol1, labels = vLabel) +
  scale_y_continuous(labels = scales::percent_format(accuracy = 1)))

# - Save graph
dpi <- 180
ggsave(gPlot, file=paste0(genFigPath, "BrierPlot_v2.png"), width=1200/dpi, height=1000/dpi, dpi=400, bg="white")


# --- 4.3 Cleanup
suppressWarnings(rm(gInner, datGraph, gOuter, objCoxDisc_adv,objCoxDisc_bas ,plot.full)); gc(0)


