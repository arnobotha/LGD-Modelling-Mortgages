# ------ 1. Model fitting

# - Confirm prepared datasets are loaded into memory
if (!exists('datCredit_train_CDH')) unpack.ffdf(paste0(genPath,"creditdata_train_CDH"), tempPath);gc()
if (!exists('datCredit_valid_CDH')) unpack.ffdf(paste0(genPath,"creditdata_valid_CDH"), tempPath);gc()

# - Use only default spells
datCredit_train <- datCredit_train_CDH[!is.na(DefSpell_Key),]
datCredit_valid <- datCredit_valid_CDH[!is.na(DefSpell_Key),]
# remove previous objects from memory
rm(datCredit_train_CDH, datCredit_valid_CDH); gc()

# - Weigh default cases heavier. as determined interactively based on calibration success (script 6e)
datCredit_train[, Weight := ifelse(DefSpell_Event==1,1,1)]
datCredit_valid[, Weight := ifelse(DefSpell_Event==1,1,1)] # for merging purposes


# - Create subset of performing spells towards modelling each spell as a single record | Classical PD-modelling
datTrain_classic <- subset(datCredit_train, DefSpell_Counter==1)
datValid_classic <- subset(datCredit_valid, DefSpell_Counter==1)

# - Assign target/response field for the classical PD-model
datTrain_classic[, DefSpell_Event := ifelse(DefSpellResol_Type_Hist=="WOFF", 1, 0)]
datValid_classic[, DefSpell_Event := ifelse(DefSpellResol_Type_Hist=="WOFF", 1, 0)]

# - Create a copy of DefSpell_Age to serve as an input into the classical LGD-model
datTrain_classic[, DefSpell_Age2 := DefSpell_Age]
datValid_classic[, DefSpell_Age2 := DefSpell_Age]


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

# ------ Classical Logistic Regression model

# - Initialize variables
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

# Predict hazard h(t) = P(T=t | T>= t) in discrete-time
datCredit_train[, Hazard_adv := predict(modLR, newdata=datCredit_train, type = "response")]
datCredit_train[, Hazard_bas := predict(modLR_basic, newdata=datCredit_train, type = "response")]
# Derive survival probability S(t) = prod ( 1- hazard)
datCredit_train[, Survival_adv := cumprod(1-Hazard_adv), by=list(DefSpell_Key)]
datCredit_train[, Survival_bas := cumprod(1-Hazard_bas), by=list(DefSpell_Key)]
# Derive discrete density, or event probability f(t) = S(t-1) . h(t)
datCredit_train[, EventRate_adv := shift(Survival_adv, type="lag", n=1, fill=1) * Hazard_adv , by=list(DefSpell_Key)]
datCredit_train[, EventRate_bas := shift(Survival_bas, type="lag", n=1, fill=1) * Hazard_bas, by=list(DefSpell_Key)]



# - Score using classical PD-model each instance of TimeInDefSpell as DefSpell_Age
datCredit_train[, DefSpell_Age2 := TimeInDefSpell]
datCredit_train[!is.na(DefSpell_Num), Hazard_PD := predict(modLR_classic, newdata=.SD[], type = "response")]
datCredit_train[!is.na(DefSpell_Num), Survival_PD := cumprod(1-Hazard_PD), by=list(DefSpell_Key)]
datCredit_train[!is.na(DefSpell_Num), EventRate_PD := shift(Survival_PD, type="lag", n=1, fill=1) *Hazard_PD, by=list(DefSpell_Key)]


# Predict hazard h(t) = P(T=t | T>= t) in discrete-time
datCredit_valid[, Hazard_adv := predict(modLR, newdata=datCredit_valid, type = "response")]
datCredit_valid[, Hazard_bas := predict(modLR_basic, newdata=datCredit_valid, type = "response")]
# Derive survival probability S(t) = prod ( 1- hazard)
datCredit_valid[, Survival_adv := cumprod(1-Hazard_adv), by=list(DefSpell_Key)]
datCredit_valid[, Survival_bas := cumprod(1-Hazard_bas), by=list(DefSpell_Key)]
# Derive discrete density, or event probability f(t) = S(t-1) . h(t)
datCredit_valid[, EventRate_adv := shift(Survival_adv, type="lag", n=1, fill=1) * Hazard_adv, by=list(DefSpell_Key)]
datCredit_valid[, EventRate_bas := shift(Survival_bas, type="lag", n=1, fill=1) *Hazard_bas, by=list(DefSpell_Key)]



# - Score using classical PD-model each instance of TimeInDefSpell as DefSpell_Age
datCredit_valid[, DefSpell_Age2 := TimeInDefSpell]
datCredit_valid[!is.na(DefSpell_Num), Hazard_PD := predict(modLR_classic, newdata=.SD[], type = "response")]
datCredit_valid[!is.na(DefSpell_Num), Survival_PD := cumprod(1-Hazard_PD), by=list(DefSpell_Key)]
datCredit_valid[!is.na(DefSpell_Num), EventRate_PD := shift(Survival_PD, type="lag", n=1, fill=1)* Hazard_PD, by=list(DefSpell_Key)]



# - filter to maximum spell counter
datCredit_train <- datCredit_train[, .SD[which.max(DefSpell_Counter)], by = LoanID]
datCredit_valid <- datCredit_valid[, .SD[which.max(DefSpell_Counter)], by = LoanID]

# Identify where the loss rate is out of bounds and not feasible
datCredit_train <- datCredit_train[, OOB_Ind := ifelse(LossRate_Real < 0 | LossRate_Real > 1, 1,0)]
datCredit_valid <- datCredit_valid[, OOB_Ind := ifelse(LossRate_Real < 0 | LossRate_Real > 1, 1,0)]

# Subset to include only relevant data
datCredit_train <- subset(datCredit_train, OOB_Ind == 0)
datCredit_valid <- subset(datCredit_valid, OOB_Ind == 0)


# - Initialize variables to be tested
vars <- c("PrevDefaults","g0_Delinq_Any_Aggr_Prop_Lag_3","DefaultStatus1_Aggr_Prop_Lag_6", 
          "slc_acct_arr_dir_3","DefSpell_Age*DefSpell_Num_binned",
          "NewLoans_Aggr_Prop","Balance_1","Principal","pmnt_method_grp",
          "M_RealIncome_Growth", "M_Inflation_Growth_12","M_DTI_Growth_3","M_Repo_Rate_9")


# - Full model | Stepwise forward selection procedure
modLR_tweedie <- cpglm( as.formula(paste("LossRate_Real ~", paste(vars, collapse = " + "))),
                        data=datCredit_train)


datCredit_train <- datCredit_train[, LossSeverity:= predict(modLR_tweedie, newdata=datCredit_train, type="response")]
datCredit_valid <- datCredit_valid[, LossSeverity:= predict(modLR_tweedie, newdata=datCredit_valid, type="response")]


datCredit_train <- datCredit_train[, LossRate_est_bas:= LossSeverity*EventRate_bas]
datCredit_valid <- datCredit_valid[, LossRate_est_bas:= LossSeverity*EventRate_bas]

datCredit_train <- datCredit_train[, LossRate_est_adv:= LossSeverity*EventRate_adv]
datCredit_valid <- datCredit_valid[, LossRate_est_adv:= LossSeverity*EventRate_adv]

datCredit_train <- datCredit_train[, LossRate_est_LR:=  LossSeverity*EventRate_PD]
datCredit_valid <- datCredit_valid[, LossRate_est_LR:=  LossSeverity*EventRate_PD]

datCredit <- rbind(datCredit_train,datCredit_valid)

datCredit_hist <- subset(datCredit,DefSpellResol_Type_Hist=="WOFF")


chosenFont <-"Cambria"

# - graphing parameters
vCol <- brewer.pal(10, "Paired")[c(8,6)]

# - Aesthetic engineering: Statistical Summaries
meanLoss_TruEnd <- mean(datCredit_train$LossRate_Real, na.rm=T)


# - main graphs a) Overall LGD distribution
(g1 <- ggplot(datCredit, aes(x=LossRate_Real)) + theme_bw() +
    geom_histogram(aes(y=after_stat(density)), alpha=0.4, bins=round(2*datCredit[,.N]^(1/3)),
                   position="identity", fill=vCol[1], colour=vCol[1]) + 
    geom_density(linewidth=1, colour=vCol[1], linetype="dotted") + 
    geom_vline(xintercept=meanLoss_TruEnd, linewidth=0.6, colour=vCol[1], linetype="dashed") + 
    annotate(geom="text", x=meanLoss_TruEnd*0.8, y=10, family=chosenFont,
             label = paste0("Mean Loss: ", sprintf("%.1f", meanLoss_TruEnd*100), "%"), size=3, colour=vCol[1], angle=90) +     
    # facets & scale options
    labs(x=bquote({Realised~loss~rate~italic(L)}), 
         y="Histogram and density of resolved defaults [write-offs]") + 
    theme(text=element_text(family=chosenFont),legend.position="bottom",
          strip.background=element_rect(fill="snow2", colour="snow2"),
          strip.text = element_text(size=8, colour="gray50"), 
          strip.text.y.right = element_text(angle=90)) + 
    scale_x_continuous(breaks=pretty_breaks(), label=percent)
)

datCredit[LossRate_est_adv>1, LossRate_est_adv:=1]
# - Aesthetic engineering: Statistical Summaries
meanLoss_TruEnd <- mean(datCredit$LossRate_est_adv, na.rm=T)

# - main graphs a) Overall LGD distribution
(g2 <- ggplot(datCredit, aes(x=LossRate_est_adv)) + theme_bw() +
    geom_histogram(aes(y=after_stat(density)), alpha=0.4, bins=round(2*datCredit[,.N]^(1/3)),
                   position="identity", fill=vCol[1], colour=vCol[1]) + 
    geom_density(linewidth=1, colour=vCol[1], linetype="dotted") + 
    geom_vline(xintercept=meanLoss_TruEnd, linewidth=0.6, colour=vCol[1], linetype="dashed") + 
    annotate(geom="text", x=meanLoss_TruEnd*0.8, y=10, family=chosenFont,
             label = paste0("Mean Loss: ", sprintf("%.1f", meanLoss_TruEnd*100), "%"), size=3, colour=vCol[1], angle=90) +     
    # facets & scale options
    labs(x=bquote({Realised~loss~rate~italic(L)}), 
         y="Histogram and density of resolved defaults [write-offs]") + 
    theme(text=element_text(family=chosenFont),legend.position="bottom",
          strip.background=element_rect(fill="snow2", colour="snow2"),
          strip.text = element_text(size=8, colour="gray50"), 
          strip.text.y.right = element_text(angle=90)) + 
    scale_x_continuous(breaks=pretty_breaks(), label=percent)
)

metrics<-evalModel_twostage(datCredit,"LossRate_Real","LossRate_est_adv",writeoff_type = "survival_adv",modLR,modLR_tweedie)
stats_text <- paste(
  "RMSE: ",          sprintf("%.1f%%", metrics$RMSE * 100), "\n",
  "MAE: ",           sprintf("%.1f%%", metrics$MAE * 100), "\n",
  "KS: ",            sprintf("%.1f%%", metrics$KS * 100), "\n",
  "KL: ",            sprintf("%.4f", metrics$KL), "\n",
  "JS: ",            sprintf("%.4f", metrics$JS), "\n",
  "Kendall's Tau: ",   sprintf("%.2f", metrics$Kendalls_Tau), "\n",
  "Spearman's Rho: ",  sprintf("%.2f", metrics$Spearmans_rho),
  sep = ""
)

plotData <- melt(datCredit,measure.vars = c("LossRate_Real", "LossRate_est_adv"),variable.name = "Type",value.name = "LossRate")

plotData[, Type := factor(Type,levels = c("LossRate_Real", "LossRate_est_adv"),
                          labels = c("Actual loss rate", "DtH-Advanced"))]

gOverlay <- ggplot(plotData, aes(x = LossRate)) + 
  theme_bw() +
  geom_histogram(
    aes(y = after_stat(density), fill = Type, colour = Type),alpha = 0.35,
    bins = round(2 * datCredit[, .N]^(1/3)),position = "identity") +
  geom_density(aes(colour = Type),linewidth = 1,linetype = "dotted") +
  labs(x = bquote({Realised~loss~rate~italic(L)}),y = "Histogram and density of resolved defaults [cures/write-offs]") +
  annotate("label",x = 0.6,y = 70 * 0.95,label = stats_text,hjust = 0,vjust = 1,family = chosenFont,
           size = 4,fill = "white",colour = "black", label.size = 0.5)+
  theme(text = element_text(family = chosenFont),legend.position = "bottom",
        strip.background = element_rect(fill="snow2", colour="snow2"),
        strip.text = element_text(size=8, colour="gray50"),
        strip.text.y.right = element_text(angle=90)) +
  scale_x_continuous(breaks = pretty_breaks(),labels = scales::percent) +
  scale_colour_manual(values = c(vCol[1], vCol[2])) +
  scale_fill_manual(values   = c(vCol[1], vCol[2]))

# - save plot
dpi <- 180
ggsave(gOverlay, file=paste0(genFigPath,"/ActvsExp_twostage_adv.png"),width=1200/dpi, height=1000/dpi,dpi=dpi, bg="white")

# Now focus on the write-offs
plotData <- melt(datCredit_hist,measure.vars = c("LossRate_Real", "LossRate_est_adv"),variable.name = "Type",value.name = "LossRate")

plotData[, Type := factor(Type,levels = c("LossRate_Real", "LossRate_est_adv"),
                          labels = c("Actual loss rate", "DtH-Advanced"))]
gOverlay_hist <- ggplot(plotData, aes(x = LossRate)) + 
  theme_bw() +
  geom_histogram(
    aes(y = after_stat(density), fill = Type, colour = Type),alpha = 0.35,
    bins = round(2 * datCredit_hist[, .N]^(1/3)),position = "identity") +
  geom_density(aes(colour = Type),linewidth = 1,linetype = "dotted") +
  labs(x = bquote({Realised~loss~rate~italic(L)}),y = "Histogram and density of resolved defaults [write-offs]") +
  theme(text = element_text(family = chosenFont),legend.position = "bottom",
        strip.background = element_rect(fill="snow2", colour="snow2"),
        strip.text = element_text(size=8, colour="gray50"),
        strip.text.y.right = element_text(angle=90)) +
  scale_x_continuous(breaks = pretty_breaks(),labels = scales::percent) +
  scale_colour_manual(values = c(vCol[1], vCol[2])) +
  scale_fill_manual(values   = c(vCol[1], vCol[2]))

# - save plot
dpi <- 180
ggsave(gOverlay_hist, file=paste0(genFigPath,"/ActvsExp_twostage_adv_WOff.png"),width=1200/dpi, height=1000/dpi,dpi=dpi, bg="white")
# basic model

# - Aesthetic engineering: Statistical Summaries
meanLoss_TruEnd <- mean(datCredit$LossRate_est_bas, na.rm=T)

# - main graphs a) Overall LGD distribution
(g2 <- ggplot(datCredit, aes(x=LossRate_est_bas)) + theme_bw() +
    geom_histogram(aes(y=after_stat(density)), alpha=0.4, bins=round(2*datCredit[,.N]^(1/3)),
                   position="identity", fill=vCol[1], colour=vCol[1]) + 
    geom_density(linewidth=1, colour=vCol[1], linetype="dotted") + 
    geom_vline(xintercept=meanLoss_TruEnd, linewidth=0.6, colour=vCol[1], linetype="dashed") + 
    annotate(geom="text", x=meanLoss_TruEnd*0.8, y=10, family=chosenFont,
             label = paste0("Mean Loss: ", sprintf("%.1f", meanLoss_TruEnd*100), "%"), size=3, colour=vCol[1], angle=90) +     
    # facets & scale options
    labs(x=bquote({Realised~loss~rate~italic(L)}), 
         y="Histogram and density of resolved defaults [write-offs]") + 
    theme(text=element_text(family=chosenFont),legend.position="bottom",
          strip.background=element_rect(fill="snow2", colour="snow2"),
          strip.text = element_text(size=8, colour="gray50"), 
          strip.text.y.right = element_text(angle=90)) + 
    scale_x_continuous(breaks=pretty_breaks(), label=percent)
)

metrics<-evalModel_twostage(datCredit,"LossRate_Real","LossRate_est_bas",writeoff_type = "survival_adv",modLR_basic,modLR_tweedie)
stats_text <- paste(
  "RMSE: ",          sprintf("%.1f%%", metrics$RMSE * 100), "\n",
  "MAE: ",           sprintf("%.1f%%", metrics$MAE * 100), "\n",
  "KS: ",            sprintf("%.1f%%", metrics$KS * 100), "\n",
  "KL: ",            sprintf("%.4f", metrics$KL), "\n",
  "JS: ",            sprintf("%.4f", metrics$JS), "\n",
  "Kendall's Tau: ",   sprintf("%.2f", metrics$Kendalls_Tau), "\n",
  "Spearman's Rho: ",  sprintf("%.2f", metrics$Spearmans_rho),
  sep = ""
)

plotData <- melt(datCredit,measure.vars = c("LossRate_Real", "LossRate_est_bas"),variable.name = "Type",value.name = "LossRate")

plotData[, Type := factor(Type,levels = c("LossRate_Real", "LossRate_est_bas"),
                          labels = c("Actual loss rate", "DtH-Basic"))]

gOverlay <- ggplot(plotData, aes(x = LossRate)) + 
  theme_bw() +
  geom_histogram(
    aes(y = after_stat(density), fill = Type, colour = Type),alpha = 0.35,
    bins = round(2 * datCredit[, .N]^(1/3)),position = "identity")  +
  labs(x = bquote({Realised~loss~rate~italic(L)}),y = "Histogram and density of resolved defaults [cures/write-offs]") +
  annotate("label",x = 0.6,y =  80* 0.95,label = stats_text,hjust = 0,vjust = 1,family = chosenFont,
           size = 4,fill = "white",colour = "black", label.size = 0.5)+
  theme(text = element_text(family = chosenFont),legend.position = "bottom",
        strip.background = element_rect(fill="snow2", colour="snow2"),
        strip.text = element_text(size=8, colour="gray50"),
        strip.text.y.right = element_text(angle=90)) +
  scale_x_continuous(breaks = pretty_breaks(),labels = scales::percent) +
  scale_colour_manual(values = c(vCol[1], vCol[2])) +
  scale_fill_manual(values   = c(vCol[1], vCol[2]))

# - save plot
dpi <- 180
ggsave(gOverlay, file=paste0(genFigPath,"/ActvsExp_twostage_bas.png"),width=1200/dpi, height=1000/dpi,dpi=dpi, bg="white")
# Now focus on the write-offs
plotData <- melt(datCredit_hist,measure.vars = c("LossRate_Real", "LossRate_est_bas"),variable.name = "Type",value.name = "LossRate")

plotData[, Type := factor(Type,levels = c("LossRate_Real", "LossRate_est_bas"),
                          labels = c("Actual loss rate", "DtH-Basic"))]
gOverlay_hist <- ggplot(plotData, aes(x = LossRate)) + 
  theme_bw() +
  geom_histogram(
    aes(y = after_stat(density), fill = Type, colour = Type),alpha = 0.35,
    bins = round(2 * datCredit_hist[, .N]^(1/3)),position = "identity") +
  labs(x = bquote({Realised~loss~rate~italic(L)}),y = "Histogram and density of resolved defaults [write-offs]") +
  theme(text = element_text(family = chosenFont),legend.position = "bottom",
        strip.background = element_rect(fill="snow2", colour="snow2"),
        strip.text = element_text(size=8, colour="gray50"),
        strip.text.y.right = element_text(angle=90)) +
  scale_x_continuous(breaks = pretty_breaks(),labels = scales::percent) +
  scale_colour_manual(values = c(vCol[1], vCol[2])) +
  scale_fill_manual(values   = c(vCol[1], vCol[2]))

# - save plot
dpi <- 180
ggsave(gOverlay_hist, file=paste0(genFigPath,"/ActvsExp_twostage_bas_WOff.png"),width=1200/dpi, height=1000/dpi,dpi=dpi, bg="white")
# Logistic Regression model

meanLoss_TruEnd <- mean(datCredit$LossRate_est_LR, na.rm=T)

# - main graphs a) Overall LGD distribution
(g2 <- ggplot(datCredit, aes(x=LossRate_est_LR)) + theme_bw() +
    geom_histogram(aes(y=after_stat(density)), alpha=0.4, bins=round(2*datCredit[,.N]^(1/3)),
                   position="identity", fill=vCol[1], colour=vCol[1]) + 
    geom_vline(xintercept=meanLoss_TruEnd, linewidth=0.6, colour=vCol[1], linetype="dashed") + 
    annotate(geom="text", x=meanLoss_TruEnd*0.8, y=10, family=chosenFont,
             label = paste0("Mean Loss: ", sprintf("%.1f", meanLoss_TruEnd*100), "%"), size=3, colour=vCol[1], angle=90) +     
    # facets & scale options
    labs(x=bquote({Realised~loss~rate~italic(L)}), 
         y="Histogram and density of resolved defaults [write-offs]") + 
    theme(text=element_text(family=chosenFont),legend.position="bottom",
          strip.background=element_rect(fill="snow2", colour="snow2"),
          strip.text = element_text(size=8, colour="gray50"), 
          strip.text.y.right = element_text(angle=90)) + 
    scale_x_continuous(breaks=pretty_breaks(), label=percent)
)

metrics<-evalModel_twostage(datCredit,"LossRate_Real","LossRate_est_LR",writeoff_type = "survival_adv",modLR_basic,modLR_tweedie)
stats_text <- paste(
  "RMSE: ",          sprintf("%.1f%%", metrics$RMSE * 100), "\n",
  "MAE: ",           sprintf("%.1f%%", metrics$MAE * 100), "\n",
  "KS: ",            sprintf("%.1f%%", metrics$KS * 100), "\n",
  "KL: ",            sprintf("%.4f", metrics$KL), "\n",
  "JS: ",            sprintf("%.4f", metrics$JS), "\n",
  "Kendall's Tau: ",   sprintf("%.2f", metrics$Kendalls_Tau), "\n",
  "Spearman's Rho: ",  sprintf("%.2f", metrics$Spearmans_rho),
  sep = ""
)

plotData <- melt(datCredit,measure.vars = c("LossRate_Real", "LossRate_est_LR"),variable.name = "Type",value.name = "LossRate")

plotData[, Type := factor(Type,levels = c("LossRate_Real", "LossRate_est_LR"),
                          labels = c("Actual loss rate", "Logistic Regression"))]

gOverlay <- ggplot(plotData, aes(x = LossRate)) + 
  theme_bw() +
  geom_histogram(
    aes(y = after_stat(density), fill = Type, colour = Type),alpha = 0.35,
    bins = round(2 * datCredit[, .N]^(1/3)),position = "identity")  +
  labs(x = bquote({Realised~loss~rate~italic(L)}),y = "Histogram and density of resolved defaults [cures/write-offs]") +
  annotate("label",x = 0.6,y =  80* 0.95,label = stats_text,hjust = 0,vjust = 1,family = chosenFont,
           size = 4,fill = "white",colour = "black", label.size = 0.5)+
  theme(text = element_text(family = chosenFont),legend.position = "bottom",
        strip.background = element_rect(fill="snow2", colour="snow2"),
        strip.text = element_text(size=8, colour="gray50"),
        strip.text.y.right = element_text(angle=90)) +
  scale_x_continuous(breaks = pretty_breaks(),labels = scales::percent) +
  scale_colour_manual(values = c(vCol[1], vCol[2])) +
  scale_fill_manual(values   = c(vCol[1], vCol[2]))

# - save plot
dpi <- 180
ggsave(gOverlay, file=paste0(genFigPath,"/ActvsExp_twostage_classic.png"),width=1200/dpi, height=1000/dpi,dpi=dpi, bg="white")
# Now focus on the write-offs
plotData <- melt(datCredit_hist,measure.vars = c("LossRate_Real", "LossRate_est_LR"),variable.name = "Type",value.name = "LossRate")

plotData[, Type := factor(Type,levels = c("LossRate_Real", "LossRate_est_LR"),
                          labels = c("Actual loss rate", "Logistic Regression"))]
gOverlay_hist <- ggplot(plotData, aes(x = LossRate)) + 
  theme_bw() +
  geom_histogram(
    aes(y = after_stat(density), fill = Type, colour = Type),alpha = 0.35,
    bins = round(2 * datCredit_hist[, .N]^(1/3)),position = "identity") +
  labs(x = bquote({Realised~loss~rate~italic(L)}),y = "Histogram and density of resolved defaults [write-offs]") +
  theme(text = element_text(family = chosenFont),legend.position = "bottom",
        strip.background = element_rect(fill="snow2", colour="snow2"),
        strip.text = element_text(size=8, colour="gray50"),
        strip.text.y.right = element_text(angle=90)) +
  scale_x_continuous(breaks = pretty_breaks(),labels = scales::percent) +
  scale_colour_manual(values = c(vCol[1], vCol[2])) +
  scale_fill_manual(values   = c(vCol[1], vCol[2]))


# - save plot
dpi <- 180
ggsave(gOverlay, file=paste0(genFigPath,"/ActvsExp_twostage_classic_WOff.png"),width=1200/dpi, height=1000/dpi,dpi=dpi, bg="white")














