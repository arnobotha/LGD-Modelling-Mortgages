# ------ 1. Preliminaries
# --- 0. General
chosenFont <- "Cambria"
# - Confirm prepared datasets are loaded into memory
if (!exists('datCredit_train_CDH')) unpack.ffdf(paste0(genPath,"creditdata_train_CDH"), tempPath);gc()
if (!exists('datCredit_valid_CDH')) unpack.ffdf(paste0(genPath,"creditdata_valid_CDH"), tempPath);gc()

# - Use only default spells 
datCredit_train <- datCredit_train_CDH[!is.na(DefSpell_Key)]
datCredit_valid <- datCredit_valid_CDH[!is.na(DefSpell_Key)]

# - filter to maximum spell counter
datCredit_train <- datCredit_train[, .SD[which.max(DefSpell_Counter)], by = LoanID]
datCredit_valid <- datCredit_valid[, .SD[which.max(DefSpell_Counter)], by = LoanID]

# Identify where the loss rate is out of bounds and not feasible
datCredit_train <- datCredit_train[, OOB_Ind := ifelse(LossRate_Real < 0 | LossRate_Real > 1, 1,0)]
datCredit_valid <- datCredit_valid[, OOB_Ind := ifelse(LossRate_Real < 0 | LossRate_Real > 1, 1,0)]

# Subset to include only relevant data
datCredit_train <- subset(datCredit_train, OOB_Ind == 0)
datCredit_valid <- subset(datCredit_valid, OOB_Ind == 0)
datCredit <- rbind(datCredit_train,datCredit_valid)

# - subset for miniplot
datCredit_W <- subset(datCredit, DefSpellResol_Type_Hist=="WOFF")
# remove previous objects from memory
rm(datCredit_train_CDH, datCredit_valid_CDH); gc()


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


# - graphing parameters
vCol <- brewer.pal(10, "Paired")[c(8,6)]

# - Aesthetic engineering: Statistical Summaries
meanLoss_TruEnd <- mean(datCredit$LossRate_Real, na.rm=T)
MeanLoss_TruEnd_W <- mean(datCredit_W$LossRate_Real, na.rm=T)
mix_WC_TruEnd <- datCredit_W[, .N] / datCredit[, .N] # overall write-off probability given default of 18%

# - main graphs a) Overall LGD distribution
(g1 <- ggplot(datCredit, aes(x=LossRate_Real)) + theme_bw() +
    geom_histogram(aes(y=after_stat(density)), alpha=0.4, bins=round(2*datCredit[,.N]^(1/3)),
                   position="identity", fill=vCol[1], colour=vCol[1]) + 
    geom_density(linewidth=1, colour=vCol[1], linetype="dotted") + 
    geom_vline(xintercept=meanLoss_TruEnd, linewidth=0.6, colour=vCol[1], linetype="dashed") + 
    annotate(geom="text", x=meanLoss_TruEnd*0.8, y=20, family=chosenFont,
             label = paste0("Mean Loss: ", sprintf("%.1f", meanLoss_TruEnd*100), "%"), size=3, colour=vCol[1], angle=90) +     
    # facets & scale options
    labs(x=bquote({Realised~loss~rate~italic(L)}), 
         y="Histogram and density of resolved defaults [cures/write-offs]") + 
    theme(text=element_text(family=chosenFont),legend.position="bottom",
          strip.background=element_rect(fill="snow2", colour="snow2"),
          strip.text = element_text(size=8, colour="gray50"), 
          strip.text.y.right = element_text(angle=90)) + 
    scale_x_continuous(breaks=pretty_breaks(), label=percent)
)

# - miniplot for main graph | Write-offs only
(g2 <- ggplot(datCredit_W, aes(x=LossRate_Real)) + theme_bw() +
    geom_histogram(aes(y=after_stat(density)), alpha=0.4, bins=round(2*datCredit_W[,.N]^(1/3)), 
                   position="identity", fill=vCol[2], colour=vCol[2]) + 
    geom_density(linewidth=1, colour=vCol[2], linetype="dotted") + 
    geom_vline(xintercept=MeanLoss_TruEnd_W, linewidth=0.6, colour=vCol[2], linetype="dashed") + 
    annotate(geom="text", x=MeanLoss_TruEnd_W*0.93,  y=3, family=chosenFont,
             label = paste0("Mean Loss: ", sprintf("%.1f", MeanLoss_TruEnd_W*100), "%"), size=3, colour=vCol[2], angle=90) +     
    # facets & scale options
    labs(x="", y="", title=paste0("Write-offs only (", sprintf("%.0f", mix_WC_TruEnd*100), "%)")) + 
    theme(legend.position="none", text=element_text(size=12, family="Cambria"),
          #specific for plot-in-plot
          axis.text.y=element_text(margin=unit(c(0,0,0,0), "mm"), size=9),
          axis.text.x=element_text(margin=unit(c(0,0,0,0), "mm"), size=9),
          axis.ticks=element_blank(), axis.title.x=element_blank(), #axis.title.y=element_blank(),
          panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
          panel.background = element_rect(color="black", fill="white"),
          plot.background = element_rect(color="white"), plot.margin = unit(c(0,0,0,0),"mm"),
          plot.title = element_text(hjust=0.55,vjust=-10,margin=margin(t=-12))
    ) + 
    scale_x_continuous(breaks=pretty_breaks(), label=percent)
)

ymin <- diff(ggplot_build(g1)$layout$panel_params[[1]]$y.range) * 0.2
ymax <- max(ggplot_build(g1)$layout$panel_params[[1]]$y.range) * 0.95
(plot.full <- g1 + annotation_custom(grob = ggplotGrob(g2), xmin=0.1, xmax=0.9, ymin=ymin, ymax=ymax))

# - save plot
dpi <- 180
ggsave(plot.full, file=paste0(genFigPath,"/Actual_LGD.png"),width=1200/dpi, height=1000/dpi,dpi=dpi, bg="white")



datCredit <- datCredit[, LossRate_tweedie:= predict(modLR_tweedie, newdata=datCredit,type="response")]
datCredit[LossRate_tweedie > 1, LossRate_tweedie := 1]
meanLoss_TruEnd_tweedie <- mean(datCredit$LossRate_tweedie, na.rm=T)
(g1a <- ggplot(datCredit, aes(x=LossRate_tweedie)) + theme_bw() +
    geom_histogram(aes(y=after_stat(density)), alpha=0.4, bins=round(2*datCredit[,.N]^(1/3)),
                   position="identity", fill=vCol[1], colour=vCol[1]) + 
    geom_density(linewidth=1, colour=vCol[1], linetype="dotted") + 
    geom_vline(xintercept=meanLoss_TruEnd_tweedie, linewidth=0.6, colour=vCol[1], linetype="dashed") + 
    annotate(geom="text", x=meanLoss_TruEnd_tweedie*0.8, y=20, family=chosenFont,
             label = paste0("Mean Loss: ", sprintf("%.1f", meanLoss_TruEnd_tweedie*100), "%"), size=3, colour=vCol[1], angle=90) +     
    # facets & scale options
    labs(x=bquote({Realised~loss~rate~italic(L)}), 
         y="Histogram and density of resolved defaults [cures/write-offs]") + 
    theme(text=element_text(family=chosenFont),legend.position="bottom",
          strip.background=element_rect(fill="snow2", colour="snow2"),
          strip.text = element_text(size=8, colour="gray50"), 
          strip.text.y.right = element_text(angle=90)) + 
    scale_x_continuous(breaks=pretty_breaks(), label=percent)
)

metrics<-evalModel_onestage(datCredit,"LossRate_Real","LossRate_tweedie","tweedie",modLR_tweedie)

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

plotData <- melt(datCredit,measure.vars = c("LossRate_Real", "LossRate_tweedie"),variable.name = "Type",value.name = "LossRate")

plotData[, Type := factor(Type,levels = c("LossRate_Real", "LossRate_tweedie"),
  labels = c("Actual loss rate", "Compound Poisson-Gamma GLM loss rate"))]

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
ggsave(gOverlay, file=paste0(genFigPath,"/ActvsExp_onestage_tweedie.png"),width=1200/dpi, height=1000/dpi,dpi=dpi, bg="white")

datCredit <- datCredit[, LossRate_gaussian:= predict(modLR_gaussian, newdata=datCredit,type="response")]
datCredit_app <- datCredit[LossRate_gaussian >= 0]

meanLoss_TruEnd_gaussian <- mean(datCredit_app$LossRate_gaussian, na.rm=T)
(g1b <- ggplot(datCredit_app, aes(x=LossRate_gaussian)) + theme_bw() +
    geom_histogram(aes(y=after_stat(density)), alpha=0.4, bins=round(2*datCredit[,.N]^(1/3)),
                   position="identity", fill=vCol[1], colour=vCol[1]) + 
    geom_density(linewidth=1, colour=vCol[1], linetype="dotted") + 
    geom_vline(xintercept=meanLoss_TruEnd_gaussian, linewidth=0.6, colour=vCol[1], linetype="dashed") + 
    annotate(geom="text", x=meanLoss_TruEnd_gaussian*0.8, y=20, family=chosenFont,
             label = paste0("Mean Loss: ", sprintf("%.1f", meanLoss_TruEnd_gaussian*100), "%"), size=3, colour=vCol[1], angle=90) +     
    # facets & scale options
    labs(x=bquote({Realised~loss~rate~italic(L)}), 
         y="Histogram and density of resolved defaults [cures/write-offs]") + 
    theme(text=element_text(family=chosenFont),legend.position="bottom",
          strip.background=element_rect(fill="snow2", colour="snow2"),
          strip.text = element_text(size=8, colour="gray50"), 
          strip.text.y.right = element_text(angle=90)) + 
    scale_x_continuous(breaks=pretty_breaks(), label=percent)
)


# ---- Filter Gaussian predictions ----
datCredit_gauss <- datCredit[LossRate_gaussian >= 0, .(LossRate_gaussian)]
datCredit_real  <- datCredit[, .(LossRate_Real)]

# ---- Compute metrics for annotation ----
metrics <- evalModel_onestage(datCredit,
                              "LossRate_Real",
                              "LossRate_gaussian",
                              "gaussian",
                              modLR_gaussian)

# ---- Create annotation text ----
stats_text <- paste(
  "RMSE: ",          sprintf("%.1f%%", metrics$RMSE * 100), "\n",
  "MAE: ",           sprintf("%.1f%%", metrics$MAE * 100), "\n",
  "KS: ",            sprintf("%.1f%%", metrics$KS * 100), "\n",
  "KL: ",            sprintf("%.4f", metrics$KL), "\n",
  "JS: ",            sprintf("%.4f", metrics$JS), "\n",
  "Kendall's Tau: ", sprintf("%.2f", metrics$Kendalls_Tau), "\n",
  "Spearman's Rho: ",sprintf("%.2f", metrics$Spearmans_rho),
  sep = ""
)

plotData <- rbind(
  data.table(LossRate = datCredit_real$LossRate_Real, Type = "Actual loss rate"),
  data.table(LossRate = datCredit_gauss$LossRate_gaussian, Type = "Gaussian predicted"))
plotData[, Type := factor(Type, levels = c("Actual loss rate", "Gaussian predicted"))]

gOverlay <- ggplot(plotData, aes(x = LossRate)) + 
  theme_bw() +
  geom_histogram(
    aes(y = after_stat(density), fill = Type, colour = Type),alpha = 0.35,
    bins = round(2 * nrow(plotData)^(1/3)),position = "identity") +
  geom_density(aes(colour = Type),linewidth = 1,linetype = "dotted") +
  labs(x = bquote({Realised~loss~rate~italic(L)}),y = "Histogram and density of resolved defaults [cures/write-offs]") +
  annotate("label",x = 0.6,y = 80 * 0.95,label = stats_text,
    hjust = 0,vjust = 1,family = chosenFont,size = 4,fill = "white",
    colour = "black",label.size = 0.5) +
  theme(
    text = element_text(family = chosenFont),
    legend.position = "bottom",
    strip.background = element_rect(fill="snow2", colour="snow2"),
    strip.text = element_text(size=8, colour="gray50"),
    strip.text.y.right = element_text(angle=90)
  ) +
  scale_x_continuous(
    breaks = pretty_breaks(),
    labels = scales::percent
  ) +
  scale_colour_manual(values = c(vCol[1], vCol[2])) +
  scale_fill_manual(values   = c(vCol[1], vCol[2]))

# - save plot
dpi <- 180
ggsave(gOverlay, file=paste0(genFigPath,"/ActvsExp_onestage_gaussian.png"),width=1200/dpi, height=1000/dpi,dpi=dpi, bg="white")

