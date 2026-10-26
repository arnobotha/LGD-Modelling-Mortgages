# ============================== CONSTRUCTING TERM-STUCTURES ===================
# Construct and compare empirical and expected term-structures of default risk
# ------------------------------------------------------------------------------
# PROJECT TITLE: Loss Modelling (LGD) for Residential Mortgages
# SCRIPT AUTHOR(S): Dr Arno Botha (AB)
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
#   - 4e.SurvivalTrees.R
#
# -- Inputs:
#   - datCredit_train_CDH | Prepared from script 2g
#   - datCredit_valid_CDH | Prepared from script 2g
#   - modMLR | Multinomial DtH-model as fitted in script 4f

# -- Outputs:
#   - <Analytics> | Graphs
# ------------------------------------------------------------------------------


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

# - Combine training and validation datasets to facilitate "better" (smooth) graphs
datCredit <- rbind(datCredit_train, datCredit_valid)

# - Remove objects
rm(datCredit_train_CDH, datCredit_valid_CDH); gc()

# - Handle left-truncated spells by adding a starting record 
### NOTE:  This is necessary for calculating certain survival quantities later
# Create an additional record for each default spell
datAdd <- subset(datCredit, Counter==1 & TimeInDefSpell>1)
datAdd[, TimeInDefSpell:=TimeInDefSpell-1]
datAdd[, Time_Binned := timeBinning(TimeInDefSpell)]
datAdd[, Counter:=0]
# Add record to main dataset
datCredit <- rbind(datCredit, datAdd); setorder(datCredit, DefSpell_Key, Date)


# --- 1.2 Load models
if (!exists('modMLR')) unpack.ffdf(paste0(genPath,"MultiDtH_WOff"), tempPath)
if (!exists('datSurv2')) unpack.ffdf(paste0(genPath,"datSurv_KM_MultiSpell_CompRisks"), tempPath);gc()




# ============ 2. Constructing the term-structures of write-off ============

# --- 2.1 Estimate event rates amidst competing risks
# - Predict cause-specific hazard h_g(t) = P(T=t, G=g | T>= t) in discrete-time
datCredit[, Hazard_WOff:=predict(modMLR, newdata=datCredit, type = "probs")[,"Write-off"]]
datCredit[, Hazard_Cure:=predict(modMLR, newdata=datCredit, type = "probs")[,"Cured"]]

# - Derive survival probability S(t) = prod(1 - h_1 - h_2)
datCredit[, Survival_overall:=cumprod(1-Hazard_WOff-Hazard_Cure), by=.(DefSpell_Key)]

# - Derive event rates as f_1(t) = S(t-1) . h_1(t)
datCredit[, EventRate_WriteOff := shift(Survival_overall, n=1,fill=1) * Hazard_WOff, by=.(DefSpell_Key)]

# - Remove added rows
datCredit <- subset(datCredit, Counter>0)


# --- 2.3 Aggregate event rates to period-level
# - Aggregate data
datSurv_exp <- datCredit[,.(
  # Multinomial DtH-model
  EventRate_WriteOff=mean(EventRate_WriteOff, na.rm=T)),
  by=.(TimeInDefSpell)]

# - Ensure order of fields is correct
setorder(datSurv_exp, TimeInDefSpell)


# --- 2.4 Create graph

# - Preliminaries
# - Cut-offs for analysis and graphs 
sMaxSpellAge <- 84 # Max for [DefSpell_Age], as determined in earlier analyses (script 3c)

# - Plotting event rates for actuals and all expected values
plot(datSurv2[Time<=sMaxSpellAge, EventRate_WriteOff], type="b")
lines(datSurv_exp[TimeInDefSpell<=sMaxSpellAge, EventRate_WriteOff], type="b", col="red")

# - Plotting at risk volumes over spell timed
plot(y=datSurv2[Time<=120, AtRisk_n], x = datSurv2[Time<=120, Time], type="b",
     xlab="Spell time", ylab="Volume")

# - Initialise graphing data
datGraph <- rbind(datSurv2[,.(Time=Time, EventRate=EventRate_WriteOff, Type="a_Actual")],
                  datSurv_exp[,.(Time=TimeInDefSpell, EventRate=EventRate_WriteOff, Type="b_Expected_MLR_DtH")]
)

# - Set facet label
datGraph[, FacetLabel:="Competing Risks Term-structure"]

# - Calculate MAE between event rates
datMAE <- datGraph %>%
  subset(Time<=sMaxSpellAge, select=c("Time", "Type", "EventRate")) %>%
  pivot_wider(names_from=Type, values_from=EventRate) %>% as.data.table()
# Basic model
(MAE_eventProb_MLR <- mean(abs(datMAE$a_Actual-datMAE$b_Expected_MLR_DtH), na.rm=T))


# - Graphing parameters
vCol <- brewer.pal(8, "Dark2")
length(vCol)
vLabel <- c("a_Actual"="Empirical", "b_Expected_MLR_DtH"="MLR-DtH")
chosenFont <- "Cambria"

(gsurv_ft <- ggplot(datGraph[Time <= sMaxSpellAge,], aes(x=Time, y=EventRate, group=Type)) + theme_minimal() +
    labs(y=bquote(plain("Cause-specific write-off probability  "*italic(f[1](t)))), 
         x=bquote("Spell time (months)"*~italic(t))) + 
    theme(text=element_text(family=chosenFont),legend.position = "bottom",
          strip.background=element_rect(fill="snow2", colour="snow2"),
          strip.text=element_text(size=8, colour="gray50"), strip.text.y.right=element_text(angle=90)) + 
    geom_line(aes(colour=Type, linetype=Type)) + 
    geom_point(aes(colour=Type, shape=Type)) + 
    # Annotations
    annotate("text", y=0.006,x=75, label=paste0("MAE (MLR-DtH): ", percent(MAE_eventProb_MLR, accuracy=0.0001)), 
             family=chosenFont, size=3) + 
    # Scales and options
    facet_grid(FacetLabel~.) + 
    scale_colour_manual(name="", values=vCol, label=vLabel) + 
    scale_linetype_discrete(name="", label=vLabel) + 
    scale_shape_discrete(name="", label=vLabel) + 
    scale_y_continuous(breaks=breaks_pretty(), label=percent) + 
    scale_x_continuous(breaks=breaks_pretty(n=8), label=comma)
)


# - Save plot
dpi <- 335 # reset
ggsave(gsurv_ft, file=paste0(genFigPath, "EventProb_ActVsExp_CompRisks.png"),
       width=2400/dpi, height=1800/dpi,dpi=dpi, bg="white")

# - Cleanup
rm(datCredit, datCredit_train, datCredit_valid, modMLR, datSurv2, datGraph, datAdd,
   gsurv_ft, gsurv_ft_1, datSurv_exp)
