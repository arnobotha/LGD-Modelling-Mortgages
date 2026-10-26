# ================= LGD DISTRIBUTION ANALYSIS - Two-STAGE MODELS ===============
# Comparing actual LGDs with expected LGDs from the fitted probability and 
# severity models
# ------------------------------------------------------------------------------
# PROJECT TITLE: Loss Modelling (LGD) for Residential Mortgages
# SCRIPT AUTHOR(S): Mohammed Gabru (MG), Marcel Muller (MM), Dr Arno Botha (AB)
# ------------------------------------------------------------------------------
# -- Script dependencies:
#   - 0.Setup.R
#   - 4b(i).InputSpace_DiscreteCox.R
#   - 4b(ii).InputSpace_DiscreteCox_Basic.R
#   - 4c.InputSpace_LogisticRegression.R
#   - 4d.InputSpace_TwoStage_LossSeverity.R
#   - 4e.SurvivalTrees.R
#
# -- Inputs:
#   - datCredit_train_CDH | Prepared from script 2g
#   - datCredit_valid_CDH | Prepared from script 2g
#   - modLR_Bas | Basic discrete time model as fitted in script 4b(ii)
#   - modLR_Adv | Advanced discrete time model as fitted in script 4b(i)
#   - modLR_Classic | Classical logistic regression model as fitted in script 4c
#   - modGLM_Severity_CPG | Single stage GLM model with a Tweedie link function
#                           as fitted in script 4d
#   - SurvTree_CTree | Survival tree as fitted in script 4e
#
# -- Outputs:
#   - <Analytics> | Graphs
# ==============================================================================

### MM: - Note that these LGD-distributions are constructed using each observation of an account (i.e., multiple LGDs from each account are used)
###     - Should we consider giving a view of the distribution only at default point?
###     - The question then is why do we evaluate it only at default, should we take a few other time in default points? Maybe even all of them and
###       get a aggregated view over time in default spell?

# ============ 1. Preliminaries ============

# --- 1.1 Load and prepare data
# - Confirm prepared datasets are loaded into memory
if (!exists('datCredit_train_CDH')) unpack.ffdf(paste0(genPath,"creditdata_train_CDH"), tempPath);gc()
if (!exists('datCredit_valid_CDH')) unpack.ffdf(paste0(genPath,"creditdata_valid_CDH"), tempPath);gc()

# - Ensure order of data is correct
setorder(datCredit_train_CDH, LoanID, Date)
setorder(datCredit_valid_CDH, LoanID, Date)

# - Use only default spells
datCredit_train <- datCredit_train_CDH[!is.na(DefSpell_Key),]
datCredit_valid <- datCredit_valid_CDH[!is.na(DefSpell_Key),]

# - Create a spell-level target variable for write-off instance
datCredit_train[, DefSpell_Event2:=DefSpell_Event]; datCredit_valid[, DefSpell_Event2:=DefSpell_Event]
datCredit_train[, DefSpell_Event:=DefSpellResol_Type_Hist=="WOFF"]; datCredit_valid[, DefSpell_Event:=DefSpellResol_Type_Hist=="WOFF"]

# - Transform categorical variables to factor
# Payment method
datCredit_train[,pmnt_method_grp_fac:=as.factor(pmnt_method_grp)]
datCredit_valid[,pmnt_method_grp_fac:=as.factor(pmnt_method_grp)]
# Previous defaults
datCredit_train[,PrevDefaults_fac:=as.factor(PrevDefaults)]
datCredit_valid[,PrevDefaults_fac:=as.factor(PrevDefaults)]

# - Combine training and validation datasets to facilitate "better" (smooth) graphs
datCredit <- rbind(datCredit_train, datCredit_valid)

# - Remove objects
rm(datCredit_train_CDH, datCredit_valid_CDH); gc()


# --- 1.2 Load & apply model
# - Load CPG severity model
modGLM_Severity_CPG <- readRDS(paste0(genObjPath,"Severity_CPH_Model.rds"))

# - Forecast loss severities
datCredit[, LossSeverity:=predict(modGLM_Severity_CPG, newdata=datCredit, type="response")]

# - Correct for nonsensical loss severities
datCredit[, LossSeverity:=ifelse(LossSeverity<0 | LossSeverity>1, NA, LossSeverity)]


# --- 1.3 Subset data
# - Identify where the loss rate is out of bounds and not feasible
datCredit[, OOB_Ind:=ifelse(LossRate_Real<0 | LossRate_Real>1,1,0)]

# - Subset to include only relevant data
datCredit <- subset(datCredit, OOB_Ind==0)

# - Subset for write-offs only to create inset plots
datCredit_WOFFs <- subset(datCredit, DefSpellResol_Type_Hist=="WOFF" & OOB_Ind==0)

# - Filter for the last default spell observations
datCredit_Last <- datCredit[, .SD[which.max(DefSpell_Counter)], by=DefSpell_Key]

# - Filter for written-off spells only
datCredit_Last_WOFFs <- datCredit_Last[DefSpellResol_Type_Hist=="WOFF"]

# - Filter for non-sensical loss rates
datCredit_Last_WOFFs[, LossSeverity:=ifelse(LossSeverity>=0 & LossSeverity<=1, LossSeverity, NA)]



# ============ 2. Distributions of actual and expected severities ============

# --- 2.1 Prepare data for plotting
# - Estimate statistics on distributional differences
metrics<-evalModel_twostage(data_train=datCredit_Last_WOFFs, actField="LossRate_Real", estField="LossSeverity")

# - Create plotting data
stats_text <- paste("KS: ", sprintf("%.1f%%", metrics$KS * 100), "\n",
                    "KL: ", sprintf("%.4f", metrics$KL), "\n",
                    "JS: ", sprintf("%.4f", metrics$JS),
                    sep = "")


# --- 2.2 Plot
# - Mean expected loss
(MeanLoss_exp_W <- mean(datCredit_Last_WOFFs$LossSeverity, na.rm=T))
(MeanLoss_act_W <- mean(datCredit_Last_WOFFs$LossRate_Real, na.rm=T))

# - Create plotting data
plotData <- melt(datCredit_Last_WOFFs, measure.vars=c("LossRate_Real", "LossSeverity"),
                 variable.name="Type", value.name="LossRate")
plotData[, Type:=factor(Type,levels=c("LossRate_Real", "LossSeverity"),
                        labels=c("Actual loss rate", "Compound Poisson GLM"))]
plotData[, FacetLabel:="Resolved defaults [cures/write-offs]"]

# - Graphing parameters
chosenFont <-"Cambria"
vCol <- brewer.pal(10, "Paired")[c(8,6)]

# - Plot
(g_WOFFs <- ggplot(plotData, aes(x=LossRate)) + 
    theme_bw() +
    theme(text=element_text(family=chosenFont),legend.position="bottom",
          strip.background=element_rect(fill="snow2", colour="snow2"),
          strip.text=element_text(size=8, colour="gray50"),
          strip.placement="outside",        
          strip.text.y.right=element_text(angle = 90)) +
    labs(x=bquote({Realised~loss~rate~italic(L)}), y="Histogram of loss rates" ) +
    geom_histogram(aes(y=after_stat(density), fill=Type, colour=Type), alpha=0.35,
                   bins=50, position="identity") +
    # Annotations
    geom_vline(xintercept=MeanLoss_act_W, linewidth=0.6, colour=vCol[1], linetype="dashed") + 
    annotate(geom="text", x=MeanLoss_act_W*1.05,  y=4, family=chosenFont,
             label = paste0(sprintf("%.1f", MeanLoss_act_W*100), "%"), size=3, colour=vCol[1], angle=90, vjust=-1.75) +       
    geom_vline(xintercept=MeanLoss_exp_W, linewidth=0.6, colour=vCol[2], linetype="dashed") + 
    annotate(geom="text", x=MeanLoss_exp_W*1.05,  y=4, family=chosenFont,
             label = paste0(sprintf("%.1f", MeanLoss_exp_W*100), "%"), size=3, colour=vCol[2], angle=90) +       
    annotate("label", x=0.7, y=3 , label = stats_text,
             hjust=0, vjust =1, family = chosenFont,
             size=4, fill="white", colour="black", label.size=0.5) +
    # Scales & facets
    scale_x_continuous(breaks=pretty_breaks(), labels=scales::percent) +
    scale_colour_manual(values=c(vCol[1], vCol[2])) +
    scale_fill_manual(values=c(vCol[1], vCol[2])))

# - Save plot
dpi <- 240
ggsave(g_WOFFs, file=paste0(genFigPath,"ActvsExp_twostage_LossSeverities.png"),
       width=1200/dpi, height=1000/dpi,dpi=dpi, bg="white")




# ============ 3. Loss-severity over default cohort (spell-exit) ============

# --- 3.1 Prepare data for plotting
# - Aggregate data at default start
datPlot <- datCredit_Last_WOFFs[, .(Empirical=mean(LossRate_Real, na.rm=T),
                                    Expected=mean(LossSeverity, na.rm=T)),
                                by=list(Date)]

# - Pivot data to facilitate grapging
datPlot <- datPlot %>% pivot_longer(cols=c("Empirical", "Expected"),
                                    values_to="LossSeverity", names_to="Model") %>% data.table() %>% setorder(Date)


# -- 3.1.2 Annotation data
# - Location of annotation
x_vals<-as.Date("2015-12-31"); y_vals <- 0.8

# - Estimate MAEs
vEventRates_MAE <- mean(abs(datPlot[Model=="Empirical", LossSeverity]-datPlot[Model=="Expected", LossSeverity]), na.rm=T)

# - Create labels
series_labels_mae <- c("B[t]")
vLabel_MAE <- sapply(seq_along(series_labels_mae), function(i) {
  paste0("'MAE between '*italic(", series_labels_mae[i], ")*' and '*italic(A[t])*' : ",
         sprintf('%.2f', vEventRates_MAE[i] * 100), "%'")
})


# --- 3.2 Create plot
# - Refactor levels
datPlot[,Model:=case_when(Model=="Empirical"~"a_Empirical", Model=="Expected"~"b_Expected")]

# - Graphing parameters
chosenFont <- "Cambria"; dpi <- 180
vCol <- brewer.pal(8, "Dark2")[c(2,1)]
vLabel <- c("a_Empirical"=bquote(italic(A[t])~": Empirical"), "b_Expected"=bquote(italic(B[t])~": Expected"))
vShape <- c(17,20) 

# - Plot
### MM: Horrendous results, am I using the correct probabilities?
### MM: Also need to create such a loss-severity by default cohort graph
(g_LossSeverities <- ggplot(data=datPlot, aes(x=Date, y=LossSeverity)) + theme_minimal() +
    labs(y=bquote("Loss severity (%) "), x=bquote("Default spell cohorts (mmmccyy): exit time "*italic(t[s]))) + 
    theme(text=element_text(family=chosenFont), legend.position="bottom",
          axis.text.x=element_text(angle=90), 
          strip.background=element_rect(fill="snow2", colour="snow2"),
          strip.text=element_text(size=11, colour="gray50"), strip.text.y.right=element_text(angle=90)) + 
    # Main graph
    geom_line(aes(colour=Model, linetype=Model)) +
    geom_point(aes(colour=Model, shape=Model)) +
    # Annotations
    annotate(geom="text", x=x_vals, y=y_vals, label=vLabel_MAE, parse=T) +
    # Facets & scale options
    scale_colour_manual(name="Model", values=vCol, labels=vLabel, guide=guide_legend(nrow=1, byrow=T)) +
    scale_fill_manual(name="Model", values=vCol, labels=vLabel, guide=guide_legend(nrow=1, byrow=T)) +
    scale_shape_manual(name="Model", values=vShape, labels=vLabel, guide=guide_legend(nrow=1, byrow=T)) +
    scale_linetype_discrete(name="Model", labels=vLabel, guide=guide_legend(nrow=1, byrow=T)) +
    scale_x_date(date_breaks="12 month", date_labels="%b %Y") +
    scale_y_continuous(breaks=pretty_breaks(), labels=percent))

# - Save graph
dpi <-180
ggsave(g_LossSeverities, file=paste0(genFigPath, "LossSeverity_DefaultCohort_ExitTime.png"),
       width=1600/dpi, height=1000/dpi, dpi=dpi, bg="white")




# ============ 4. Housekeeping ============

cleanEnviron()



