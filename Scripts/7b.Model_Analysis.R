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


# - Gaussian One-stage model
vars <- c("PrevDefaults", "g0_Delinq_Any_Aggr_Prop_Lag_3","DefaultStatus1_Aggr_Prop_Lag_9",
          "g0_Delinq_Ave", "CuringEvents_Aggr_Prop",
          "slc_acct_arr_dir_3","DefSpell_Age","DefSpell_Num_binned","slc_past_due_amt_imputed_med",
          "InstalmentToBalance_Aggr_Prop", "DefSpell_Maturity_Aggr_Mean", "NewLoans_Aggr_Prop",
          "Principal", "InterestRate_Margin_imputed_mean", "pmnt_method_grp","InterestRate_Nom","Balance_1",
          "M_Repo_Rate_12","M_DTI_Growth_12","M_Inflation_Growth", "M_RealIncome_Growth_2")

modLR_gaussian<- glm( as.formula(paste("LossRate_Real ~", paste(vars, collapse = " + "))),
                      data=datCredit_train,family = gaussian(link = "identity"))

# - Tweedie One-stage model
vars <- c("InterestRate_Nom","pmnt_method_grp","Arrears","DefSpell_Age","DefSpell_Num_binned",
          "Balance_1","Principal","DefaultStatus1_Aggr_Prop_Lag_9",
          "M_Repo_Rate_12","M_RealIncome_Growth")

modLR_tweedie <- cpglm( as.formula(paste("LossRate_Real ~", paste(vars, collapse = " + "))),
                        data=datCredit_train)

# - Initialize variables to be tested
vars <- c("PrevDefaults","g0_Delinq_Any_Aggr_Prop_Lag_3","DefaultStatus1_Aggr_Prop_Lag_6", 
          "slc_acct_arr_dir_3","DefSpell_Age*DefSpell_Num_binned",
          "NewLoans_Aggr_Prop","Balance_1","Principal","pmnt_method_grp",
          "M_RealIncome_Growth", "M_Inflation_Growth_12","M_DTI_Growth_3","M_Repo_Rate_9")


# - Full model | Stepwise forward selection procedure
modLR_tweedie_two_stage <- cpglm( as.formula(paste("LossRate_Real ~", paste(vars, collapse = " + "))),
                        data=datCredit_train)




datCredit <- rbind(datCredit_train,datCredit_valid)
thresh_glm <- 0.2352999
thresh_dth <- 0.1037777
thresh_dth_bas <-  0.00795927


datCredit <- datCredit[, Youden_LR:= ifelse(EventRate_PD >= thresh_glm,1,0)]
datCredit <- datCredit[, Youden_bas:= ifelse(EventRate_bas >= thresh_dth_bas,1,0)]
datCredit <- datCredit[, Youden_adv:= ifelse(EventRate_adv >= thresh_dth,1,0)]


datCredit <- datCredit[, LossSeverity:= predict(modLR_tweedie_two_stage, newdata=datCredit, type="response")]


datCredit <- datCredit[, LossRate_est_bas_youden:= LossSeverity*Youden_bas]
datCredit <- datCredit[, LossRate_est_adv_youden:= LossSeverity*Youden_adv]
datCredit <- datCredit[, LossRate_est_LR_youden:=  LossSeverity*Youden_LR]
datCredit <- datCredit[, LossRate_est_bas:= LossSeverity*EventRate_bas]
datCredit <- datCredit[, LossRate_est_adv:= LossSeverity*EventRate_adv]
datCredit <- datCredit[, LossRate_est_LR:=  LossSeverity*EventRate_PD]
datCredit <- datCredit[, LossRate_tweedie:= predict(modLR_tweedie, newdata=datCredit,type="response")]
datCredit <- datCredit[, LossRate_gaussian:= predict(modLR_gaussian, newdata=datCredit,type="response")]


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


final_table <- tibble(
  Model         = model_names,    
  RMSE          = NA_real_,
  MAE           = NA_real_,
  KS            = NA_real_,
  KL            = NA_real_,
  JS            = NA_real_,
  Kendalls_Tau  = NA_real_,
  Spearmans_rho = NA_real_
)


final_table[1, -1] <- evalModel_onestage(datCredit,"LossRate_Real","LossRate_gaussian","gaussian",modLR_gaussian)
final_table[2, -1] <- evalModel_onestage(datCredit,"LossRate_Real","LossRate_tweedie","tweedie",modLR_tweedie)

final_table[3, -1] <- evalModel_twostage(datCredit,"LossRate_Real","LossRate_est_LR",writeoff_type = "logistic",modLR_classic,modLR_tweedie_two_stage,NULL)
final_table[4, -1] <- evalModel_twostage(datCredit,"LossRate_Real","LossRate_est_bas",writeoff_type = "survival_bas",modLR_basic,modLR_tweedie_two_stage,NULL)
final_table[5, -1] <- evalModel_twostage(datCredit,"LossRate_Real","LossRate_est_adv",writeoff_type = "survival_adv",modLR,modLR_tweedie_two_stage,NULL)

final_table[6, -1] <- evalModel_twostage(datCredit,"LossRate_Real","LossRate_est_LR_youden",writeoff_type = "logistic",modLR_classic,modLR_tweedie_two_stage,thresh_glm)
final_table[7, -1] <- evalModel_twostage(datCredit,"LossRate_Real","LossRate_est_bas_youden",writeoff_type = "survival_bas",modLR_basic,modLR_tweedie_two_stage,thresh_dth_bas)
final_table[8, -1] <- evalModel_twostage(datCredit,"LossRate_Real","LossRate_est_adv_youden",writeoff_type = "survival_adv",modLR,modLR_tweedie_two_stage,thresh_dth)



# Model order
df <- final_table %>%
  mutate(Model = factor(Model, levels = rev(model_names)))

# Panel function 
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

p1 <- make_panel("MAE",          "MAE",          lower_better = TRUE,  digits = 4)
p2 <- make_panel("KS",            "KS",            lower_better = TRUE, digits = 4)
p3 <- make_panel("KL",            "KL",            lower_better = TRUE,  digits = 4)
p4 <- make_panel("Spearmans_rho", "Spearman's ??",  lower_better = FALSE, digits = 4)


LGD_metric <- (p1 | p2) / (p3 | p4) 

dpi <- 300
ggsave(LGD_metric, file=paste0(genFigPath,"/LGD_metrics.png"),width=30, height=18,dpi=dpi, bg="white")

# --------------------------------------------------------------
thresh_glm <- youden_threshold(datCredit$DefSpell_Event, datCredit$EventRate_PD,
                               model_name = "Logistic regression")

thresh_dth <- youden_threshold(datCredit$DefSpell_Event, datCredit$EventRate_adv,
                               model_name = "DTH-advanced")
thresh_dth_bas <- youden_threshold(datCredit$DefSpell_Event, datCredit$EventRate_bas,
                                   model_name = "DTH-basic")



# Graphing the loss severities over time

ActLoss_Rate <- datCredit[,mean(LossRate_Real), by=list(Date)]
Tweedie_Loss_Rate <- datCredit[,mean(LossRate_tweedie), by=list(Date)]
Gaussian_Loss_Rate <- datCredit[,mean(LossRate_gaussian), by=list(Date)]
Two_stage_LR_A_Loss_Rate <- datCredit[,mean(LossRate_est_LR), by=list(Date)]
Two_stage_LR_B_Loss_Rate <- datCredit[,mean(LossRate_est_LR_youden), by=list(Date)]
Two_stage_bas_A_Loss_Rate <- datCredit[,mean(LossRate_est_bas), by=list(Date)]
Two_stage_bas_B_Loss_Rate <- datCredit[,mean(LossRate_est_bas_youden), by=list(Date)]
Two_stage_adv_A_Loss_Rate <- datCredit[,mean(LossRate_est_adv), by=list(Date)]
Two_stage_adv_B_Loss_Rate <- datCredit[,mean(LossRate_est_adv_youden), by=list(Date)]

# - Differentiation for plotting
ActLoss_Rate[,Dataset := "A"]
Tweedie_Loss_Rate[,Dataset := "B"]
Gaussian_Loss_Rate[,Dataset := "C"]
Two_stage_LR_A_Loss_Rate[,Dataset := "D"]
Two_stage_LR_B_Loss_Rate[,Dataset := "E"]
Two_stage_bas_A_Loss_Rate[,Dataset := "F"]
Two_stage_bas_B_Loss_Rate[,Dataset := "G"]
Two_stage_adv_A_Loss_Rate[,Dataset := "H"]
Two_stage_adv_B_Loss_Rate[,Dataset := "I"]


# - Create final dataset for ggplot
datPlot <- rbind(ActLoss_Rate,Tweedie_Loss_Rate,Gaussian_Loss_Rate,Two_stage_LR_A_Loss_Rate,Two_stage_LR_B_Loss_Rate,Two_stage_bas_A_Loss_Rate
                 ,Two_stage_bas_B_Loss_Rate,Two_stage_adv_A_Loss_Rate,Two_stage_adv_B_Loss_Rate)

# - Aesthetic engineering: annotations
# Location of annotations
start_y <- 0.425
space <- 0.025
y_vals <- c(start_y,start_y-space,start_y-space*2)

# - Creating an annotation dataset for easier annotations
datAnnotate <- data.table(MeanLossRate = NULL, Dataset = c("A-B","A-C","A-D","A-E","A-F","A-G","A-H","A-I","A-J"),
                          x = rep(as.Date("2013-05-31"),9), # Text x coordinates
                          y = y_vals )

# - TTC-mean & confidence interval calculations
confLevel <- 0.95
vEventRates_Mean <- c(mean(ActLoss_Rate$V1,na.rm=T),mean(Tweedie_Loss_Rate$V1,na.rm=T),mean(Gaussian_Loss_Rate$V1,na.rm=T),mean(Two_stage_LR_A_Loss_Rate$V1,na.rm=T)
                    ,mean(Two_stage_LR_A_Loss_Rate$V1,na.rm=T),mean(Two_stage_LR_B_Loss_Rate$V1,na.rm=T),mean(Two_stage_bas_A_Loss_Rate$V1,na.rm=T),
                    mean(Two_stage_bas_B_Loss_Rate$V1,na.rm=T),mean(Two_stage_adv_A_Loss_Rate$V1,na.rm=T),mean(Two_stage_adv_B_Loss_Rate$V1,na.rm=T))
                                                                                                                                                                                            
# Standard errors
vEventRates_stErr <- c(
  sd(ActLoss_Rate$V1, na.rm = TRUE) / sqrt(sum(!is.na(ActLoss_Rate$V1))),
  sd(Tweedie_Loss_Rate$V1, na.rm = TRUE) / sqrt(sum(!is.na(Tweedie_Loss_Rate$V1))),
  sd(Gaussian_Loss_Rate$V1, na.rm = TRUE) / sqrt(sum(!is.na(Gaussian_Loss_Rate$V1))),
  sd(Two_stage_LR_A_Loss_Rate$V1, na.rm = TRUE) / sqrt(sum(!is.na(Two_stage_LR_A_Loss_Rate$V1))),
  sd(Two_stage_LR_B_Loss_Rate$V1, na.rm = TRUE) / sqrt(sum(!is.na(Two_stage_LR_B_Loss_Rate$V1))),
  sd(Two_stage_bas_A_Loss_Rate$V1, na.rm = TRUE) / sqrt(sum(!is.na(Two_stage_bas_A_Loss_Rate$V1))),
  sd(Two_stage_bas_B_Loss_Rate$V1, na.rm = TRUE) / sqrt(sum(!is.na(Two_stage_bas_B_Loss_Rate$V1))),
  sd(Two_stage_adv_A_Loss_Rate$V1, na.rm = TRUE) / sqrt(sum(!is.na(Two_stage_adv_A_Loss_Rate$V1))),
  sd(Two_stage_adv_B_Loss_Rate$V1, na.rm = TRUE) / sqrt(sum(!is.na(Two_stage_adv_B_Loss_Rate$V1)))
)
vMargin <- qnorm(1-(1-confLevel)/2) * vEventRates_stErr
series_labels <- c("A[t]", "B[t]", "C[t]", "D[t]", "E[t]", "F[t]", "G[t]", "H[t]", "I[t]")
vLabel <- sapply(seq_along(series_labels), function(i) {
  paste0("'TTC-mean over '*italic(t)*' for '*italic(", series_labels[i], ")*' : ",
    sprintf("%.2f", vEventRates_Mean[i] * 100),
    "% ± ",
    sprintf("%1.3f", vEventRates_stErr[i] * 100),
    "%'")
})

datAnnotate[, Label := vLabel]

# - Graphing parameters
chosenFont <- "Cambria"; dpi <- 180
vCol <- brewer.pal(9, "Dark2")[c(2,1,3,4,5,6,7,8,9)]
vLabel <- c("A"=bquote(italic(A[t])~": Empirical"), "B"=bquote(italic(B[t])~": One-stage: Tweedie"), 
            "C"=bquote(italic(B[t])~": One-stage: Gaussian"), "D"=bquote(italic(D[t])~":Two-stage: LR A"),
            "E"=bquote(italic(E[t])~": Two-stage: LR B"), "F"=bquote(italic(F[t])~": Two-stage: DtH-Basic A"),
            "G"=bquote(italic(G[t])~": Two-stage: DtH-Basic B"), "H"=bquote(italic(H[t])~": Two-stage: DtH-Advanced A"),
            "I"=bquote(italic(I[t])~": Two-stage: DtH-Advanced B"))
vShape <- c(17,20,4,5,6,7,8,9,10,11) 

# - Create graph
(g3 <- ggplot(datPlot, aes(x=Date, y=V1)) + theme_minimal() + 
    labs(y=bquote("Loss Rate L "), x=bquote("Default spell cohorts (mmmccyy): stop time "*italic(t[s]))) + 
    theme(text=element_text(family=chosenFont),legend.position = "bottom",legend.margin=margin(-10, 0, 0, 0),
          axis.text.x=element_text(angle=90), 
          strip.background=element_rect(fill="snow2", colour="snow2"),
          strip.text=element_text(size=11, colour="gray50"), strip.text.y.right=element_text(angle=90)) + 
    # Main graph
    geom_line(aes(colour=Dataset, linetype=Dataset), linewidth=0.3) +    
    geom_point(aes(colour=Dataset, shape=Dataset), size=1.8) + 
    # Facets & scale options
    scale_colour_manual(name = "Model", values = vCol, labels = vLabel, 
                        guide = guide_legend(nrow = 4, byrow = TRUE)) +
    scale_fill_manual(name = "Model", values = vCol, labels = vLabel, 
                      guide = guide_legend(nrow = 4, byrow = TRUE)) +
    scale_shape_manual(name = "Model", values = vShape, labels = vLabel, 
                       guide = guide_legend(nrow = 4, byrow = TRUE)) +
    scale_linetype_discrete(name = "Model", labels = vLabel, 
                            guide = guide_legend(nrow = 4, byrow = TRUE)) +
    scale_x_date(date_breaks = "6 month", date_labels = "%b %Y") +
    scale_y_continuous(breaks = pretty_breaks(), labels = percent)
)

# - Save graph
dpi <-180
ggsave(g3, file=paste0(genFigPath, "LossRate-time.png"), width=1600/dpi, height=1000/dpi, dpi=dpi, bg="white")


# Loss severity graph

meanLoss_TruEnd <- mean(datCredit$LossSeverity, na.rm=T)
datCredit[LossSeverity > 1, LossSeverity := 1]

# - main graphs a) Overall LGD distribution
(g2 <- ggplot(datCredit, aes(x=LossSeverity)) + theme_bw() +
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


plotData <- melt(datCredit,measure.vars = c("LossRate_Real", "LossSeverity"),variable.name = "Type",value.name = "LossRate")
plotData[, Type := factor(Type,levels = c("LossRate_Real", "LossSeverity"),
                          labels = c("Empirical", "Loss Severity"))]
plotData[, FacetLabel := "Resolved defaults [cures/write-offs]"]

gOverlay <- ggplot(plotData, aes(x = LossRate)) + 
  theme_bw() + geom_histogram(
    aes(y = after_stat(density), fill = Type, colour = Type),
    alpha = 0.35,bins = round(2 * datCredit[, .N]^(1/3)), position = "identity") +
  labs(x = bquote({Realised~loss~rate~italic(L)}), y = "Histogram of loss rates" ) +
  theme(text = element_text(family = chosenFont),legend.position = "bottom",
        strip.background = element_rect(fill = "snow2", colour = "snow2"),
        strip.text = element_text(size = 8, colour = "gray50"),
        strip.placement = "outside",        
        strip.text.y.right = element_text(angle = 90)) +
  scale_x_continuous(breaks = pretty_breaks(), labels = scales::percent) +
  scale_colour_manual(values = c(vCol[1], vCol[2])) +
  scale_fill_manual(values   = c(vCol[1], vCol[2])) +
  facet_grid(FacetLabel ~., scales="free")

# - save plot
dpi <- 180
ggsave(gOverlay, file=paste0(genFigPath,"/ActvsExp_Loss_Severity.png"),width=1200/dpi, height=1000/dpi,dpi=dpi, bg="white")
