# =================== DEFAULT RISK - MODEL DIAGNOSTICS =========================
# Conducting additional model diagnostics
# ------------------------------------------------------------------------------
# PROJECT TITLE: Loss Modelling (LGD) for FNB Mortgages
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
#   - 4a(i).InputSpace_DiscreteCox.R
#   - 4a(ii).InputSpace_DiscreteCox_Basic.R
#   - 4e.Dichotomisation
#
# -- Inputs:
#   - datCredit_train_CDH | Prepared from script 2g
#   - datCredit_valid_CDH | Prepared from script 2g
#   - modLR_Bas | Basic discrete time model as fitted in script 4b(ii)
#   - modLR_Adv | Advanced discrete time model as fitted in script 4b(i)
#   - modLR_Classic | Classical logistic regression model as fitted in script 4c
#   - thres_lst | Thresholds for classifying predictions as determined in script 4e
# -- Outputs:
#   - <Analytics> | Graphs
# ==============================================================================


# ------ 1. Preliminaries

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
datCredit_train[, DefSpell_Age2:=DefSpell_Age]; datCredit_train[, DefSpell_Age:=TimeInDefSpell]
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


# --- 1.4 Probability scoring
datCredit[, prob_basic:=predict(modLR_Bas, newdata = datCredit, type="response")]
datCredit[, prob_adv:=predict(modLR_Adv, newdata = datCredit, type="response")]
datCredit[, prob_LR:=predict(modLR_Classic, newdata = datCredit, type="response")]

# [SANITY CHECKS] Check for no missingness in probability scores in validation set
cat((anyNA(c(datCredit[,prob_basic]))) %?% "WARNING: Missingness detected in predicted probabilities of the Validation Set.\n" %:%
      "SAFE: No missingness in predicted probabilities from the basic discrete-time hazard model.\n")
cat((anyNA(c(datCredit[,prob_adv]))) %?% "WARNING: Missingness detected in predicted probabilities of the Validation Set.\n" %:%
      "SAFE: No missingness in predicted probabilities from the advanced discrete-time hazard model.\n")
cat((anyNA(c(datCredit[,prob_LR]))) %?% "WARNING: Missingness detected in predicted probabilities of the Validation Set.\n" %:%
      "SAFE: No missingness in predicted probabilities from the classical logistic regression model.\n")


# --- 1.5 Dichotomisation
datCredit[, DefSpell_Event_Bas_Youden:=ifelse(prob_basic>thresh_dth_bas,1,0)]
datCredit[, DefSpell_Event_Adv_Youden:=ifelse(prob_adv>thresh_dth_adv,1,0)]
datCredit[, DefSpell_Event_Classic_Youden:=ifelse(prob_LR>thresh_classic,1,0)]




# ------ 2. Coefficient of determination (Pseudo R^2-measures)

# --- 2.1 Determine R^2-measures
# - Basic model
(coefDeter_Basic <- coefDeter_glm(modLR_Bas))
### RESULTS: McFadden = 4.36%
###          Cox Snell = 0.56%
###          Nagelkerke = 4.63%

# - Advanced model
(coefDeter_Adv <- coefDeter_glm(modLR_Adv))
### RESULTS: McFadden = 64.39%
###          Cox Snell = 7.97%
###          Nagelkerke = 65.86%

# - Classical model
(coefDeter_LR <- coefDeter_glm(modLR_Classic))
### RESULTS: McFadden = 32.18%
###          Cox Snell = 28.16%
###          Nagelkerke = 43.85%


# --- 2.2 Combine and save R^2 measures
# - Create a single table containing the three R^2 measures for each of the models
(PseudoR2_Table<-data.table(Model=c("Survival Analysis (Basic)","Survival Analysis (Advanced)", "Logistic Regression"),
                            CoxSnell=c(coefDeter_Basic$CoxSnell,coefDeter_LR$CoxSnell,coefDeter_Adv$CoxSnell),
                            McFadden=c(coefDeter_Basic$McFadden,coefDeter_LR$McFadden,coefDeter_Adv$McFadden),
                            Nagelkerke=c(coefDeter_Basic$Nagelkerke,coefDeter_LR$Nagelkerke,coefDeter_Adv$Nagelkerke)))

# - Save table to specified path
saveRDS(PseudoR2_Table, paste0(genObjPath,"PseudoR2_Table.RDS"))

# - Cleanup
suppressWarnings(rm(coefDeter_LR, coefDeter_Adv, coefDeter_LR, PseudoR2_Table)); gc()




# ------ 3. AUC over time | All write-off models

# --- 3.1 Estimate AUC over time
### NOTE: Using custom AUC_overTime() function for each write-off risk models

# - Create a variable for last date in each default spell
datCredit[,DefSpell_Max_Date:=max(Date),by=list(DefSpell_Key)]

# - Basic model
BasAUC <- AUC_overTime(datCredit,"DefSpell_Max_Date","DefSpell_Event","prob_basic")
BasAUC_B <- AUC_overTime(datCredit,"DefSpell_Max_Date","DefSpell_Event","DefSpell_Event_Bas_Youden")

# - Advanced model
AdvAUC <- AUC_overTime(datCredit,"DefSpell_Max_Date","DefSpell_Event","prob_adv")
AdvAUC_B <- AUC_overTime(datCredit,"DefSpell_Max_Date","DefSpell_Event","DefSpell_Event_Adv_Youden")

# - Classical model
LRAUC <- AUC_overTime(datCredit,"DefSpell_Max_Date","DefSpell_Event","prob_LR")
LRAUC_B <- AUC_overTime(datCredit,"DefSpell_Max_Date","DefSpell_Event","DefSpell_Event_Classic_Youden")


# --- 3.2 Prepare datasets for graphing
# - Filter AUCs to ensure a sensible y-axis scale
AdvAUC <- AdvAUC[!AUC_Val<70,]
BasAUC <- BasAUC[!AUC_LowerCI<30,]

# - Convert valeus to percentages
# Basic model
BasAUC[,AUC_Val:=AUC_Val/100]; BasAUC[,AUC_LowerCI:=AUC_LowerCI/100]; BasAUC[,AUC_UpperCI:=AUC_UpperCI/100]
BasAUC_B[,AUC_Val:=AUC_Val/100]; BasAUC_B[,AUC_LowerCI:=AUC_LowerCI/100]; BasAUC_B[,AUC_UpperCI:=AUC_UpperCI/100]
# Advanced model
AdvAUC[,AUC_Val:=AUC_Val/100]; AdvAUC[,AUC_LowerCI:=AUC_LowerCI/100]; AdvAUC[,AUC_UpperCI:=AUC_UpperCI/100]
AdvAUC_B[,AUC_Val:=AUC_Val/100]; AdvAUC_B[,AUC_LowerCI:=AUC_LowerCI/100]; AdvAUC_B[,AUC_UpperCI:=AUC_UpperCI/100]
# Classical model
LRAUC[,AUC_Val:=AUC_Val/100]; LRAUC[,AUC_LowerCI:=AUC_LowerCI/100]; LRAUC[,AUC_UpperCI:=AUC_UpperCI/100]
LRAUC_B[,AUC_Val:=AUC_Val/100]; LRAUC_B[,AUC_LowerCI:=AUC_LowerCI/100]; LRAUC_B[,AUC_UpperCI:=AUC_UpperCI/100]

# - Differentiation for plotting
BasAUC[,Dataset := "A"]; BasAUC_B[,Dataset := "A"]
LRAUC[,Dataset := "C"]; LRAUC_B[,Dataset := "B"]
AdvAUC[,Dataset := "B"]; AdvAUC_B[,Dataset := "C"]


# --- 3.3 Create AUC over-time graph | A-series (non-dichotomised)
# - Create final dataset for ggplot
datPlot <- rbind(BasAUC, LRAUC, AdvAUC)

# - Annotation parametrs
# Location of annotations
start_y <- 0.425; space <- 0.025
y_vals <- c(start_y,start_y-space,start_y-space*2)
# Creating an annotation dataset for easier annotations
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
                   "% ± ", sprintf("%1.3f", vMargin[1]*100),"%'"),
            paste0("'TTC-mean over '*italic(t)*' for '*italic(B[t])*' : ", sprintf("%.2f",vEventRates_Mean[3]*100),
                   "% ± ", sprintf("%1.3f", vMargin[3]*100),"%'"),
            paste0("'TTC-mean over '*italic(t)*' for '*italic(C[t])*' : ", sprintf("%.2f",vEventRates_Mean[2]*100),
                   "% ± ", sprintf("%1.3f", vMargin[2]*100),"%'") )
datAnnotate[, Label:=vLabel]

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
suppressWarnings(rm(datAnnotate, g3, datPlot, BasAUC, LRAUC, AdvAUC)); gc()


# --- 3.4 Create AUC over-time graph | A-series (non-dichotomised)
# - Create final dataset for ggplot
datPlot <- rbind(BasAUC_B, LRAUC_B, AdvAUC_B)

# - Annotation parametrs
# Location of annotations
start_y <- 0.425; space <- 0.025
y_vals <- c(start_y,start_y-space,start_y-space*2)
# Creating an annotation dataset for easier annotations
datAnnotate <- data.table(MeanAUC = NULL, Dataset = c("A-B","A-C","A-B"),
                          x = rep(as.Date("2013-05-31"),3), # Text x coordinates
                          y = y_vals )

# - TTC-mean & confidence interval calculations
confLevel <- 0.95
vEventRates_Mean <- c(mean(BasAUC_B$AUC_Val, na.rm = T), mean(LRAUC_B$AUC_Val, na.rm = T), mean(AdvAUC_B$AUC_Val, na.rm = T))
vEventRates_stErr <- c(sd(BasAUC_B$AUC_Val, na.rm=T) / sqrt(BasAUC_B[, .N]),
                       sd(LRAUC_B$AUC_Val, na.rm=T) / sqrt(LRAUC_B[, .N]),
                       sd(AdvAUC_B$AUC_Val, na.rm=T) / sqrt(AdvAUC_B[, .N]) )
vMargin <- qnorm(1-(1-confLevel)/2) * vEventRates_stErr
vLabel <- c(paste0("'TTC-mean over '*italic(t)*' for '*italic(A[t])*' : ", sprintf("%.2f",vEventRates_Mean[1]*100),
                   "% ± ", sprintf("%1.3f", vMargin[1]*100),"%'"),
            paste0("'TTC-mean over '*italic(t)*' for '*italic(B[t])*' : ", sprintf("%.2f",vEventRates_Mean[3]*100),
                   "% ± ", sprintf("%1.3f", vMargin[3]*100),"%'"),
            paste0("'TTC-mean over '*italic(t)*' for '*italic(C[t])*' : ", sprintf("%.2f",vEventRates_Mean[2]*100),
                   "% ± ", sprintf("%1.3f", vMargin[2]*100),"%'") )
datAnnotate[, Label:=vLabel]

# - Graphing parameters
chosenFont <- "Cambria"; dpi <- 180
vCol <- brewer.pal(8, "Dark2")[c(2,1,3)]
vLabel <- c("A"=bquote(italic(A[t])~": DtH-Basic B"), "C"=bquote(italic(C[t])~": Logistic Regression B"), 
            "B"=bquote(italic(B[t])~": DtH-Advanced B"))
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
ggsave(g3, file=paste0(genFigPath, "AUC-time_B.png"), width=1200/dpi, height=1000/dpi, dpi=dpi, bg="white")

# - Cleanup
suppressWarnings(rm(datAnnotate, g3, datPlot, BasAUC_B, LRAUC_B, AdvAUC_B)); gc()






