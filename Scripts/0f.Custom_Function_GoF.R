# ============================== SURVIVAL FUNCTIONS ==============================
# Defining various custom functions relating to the estimation, analysis, and
# graphing of residuals, as part of testing the goodness-of-fit of a given 
# Cox regresion model
# --------------------------------------------------------------------------------
# PROJECT TITLE: Default Survival Modelling
# SCRIPT AUTHOR(S): Bernard Scheepers (BS), Dr Arno Botha (AB), Marcel Muller (MM)
# VERSION: 2.0 (Apr-2025)
# ================================================================================





# ----------------- 1. Functions related to Cox-Snell residuals ------------------

# --- Function to calculate Cox-Snell residuals, as adjusted for censoring.
# Input: [cox]: A fitted Cox proportional hazard model; [vIDS]: vector of spell IDs; [vEvents]: vector of spell-level events
# Output: [cs]: Cox-Snell residuals
calc_CoxSnell_Adj <- function(model, vIDs, vEvents) {
  if (inherits(model, "coxph")) {
    # Continuous-time Cox
    vResid <- residuals(model, type="martingale", collapse=vIDs)
    vCS <- vEvents - vResid + log(2)*(1 - vEvents)  # adjustment for censored
  } else if (inherits(model, "glm")) {
    # Discrete-time Cox (logistic regression)
    p_hat <- predict(model, type="response")  # predicted probability of event
    vCS <- -log(1 - p_hat)                    # discrete-time Cox-Snell residual
  } else {
    stop("Model type not supported in calc_CoxSnell_Adj")
  }
  return(vCS)
}




# --- Function to calculate the Goodness-of-Fit (GoF) of a given Cox regression model
# This is achieved by calculating the degree of similarity between Cox-Snell residuals
# and a random unit exponential distribution. The similarity degree is summarised by
# using the complement of the Kolmogorov-Smirnov test statistic (1-KS), which becomes
# our similarity measure; higher values = greater similarity = better fit.
# Input: [cox]: A fitted Cox proportional hazard model; [data_train]: Training data;
# Output: [Stat]: The test statistic value (1 - KS) as a measure of goodness-of-fit
#         [KS_graph]: A graph that combines the Cox-Snell empirical cumulative distribution
#                     with the unit exponential distribution function.
GoF_CoxSnell_KS <- function(cox, data_train, data_valid, GraphInd=T, legPos=c(0.5,0.5),
                            panelTitle="", fileName=NA,fldLstRowInd="DefSpell_ExitInd",
                            fldEventInd="DefSpell_Event",fldSpellID="DefSpell_Key",
                            dpi=350, chosenFont="Cambria") {
  # - Testing conditions
  # cox <- coxDelinq; data_train <- datCredit_train_TFD; GraphInd<-T; legPos<-c(0.5,0.5)
  # fileName <- paste0(genFigPath, "TFD/KS_Test-CoxSnellResiduals_Exp", ".png");
  # fldSpellID="PerfSpell_Key"; data_valid <- datCredit_valid_TFD
  
  # --- Preliminaries
  # - Sanity checks
  if (GraphInd & is.na(fileName)) stop("File name not provided. Exiting ..")
  
  # - Data preparation
  # Subset last row per performing spell for Goodness-of-Fit (GoF) purposes
  datLstRow <- copy(data_train[get(fldLstRowInd)==1,])
  vLstRow_Events <- datLstRow[, get(fldEventInd)]
  
  # --- Calculate B-statistic (1-KS)
  
  # - Calculate adjusted Cox-Snell Residuals
  vCS <- calc_CoxSnell_Adj(cox, vIDs=data_train[[fldSpellID]], vEvents=vLstRow_Events)
  
  # - Initialize a unit exponential distribution
  vExp <- rexp(length(vCS),1)
  # - Perform the two-sample Kolmogorov-Smirnov test of distribution equality
  # H_0: cs and exp originates from the same distribution
  # NOTE: We only desire the KS test statistic in measuring distributional dissimilarity
  # Then, we subtract this from 1 in creating a coherent statistic; greater is better
  bStat <- 1 - round(suppressWarnings(ks.test(vCS, vExp))$statistic,4); names(bStat) <- "B-stat"
  
  # - Include Harrell's c
  conc <- concordance(cox,newdata=data_valid)
  
  # - Include AIC
  AIC <- round(AIC(cox))
  
  
  # --- Graphing
  # - Conditional execution towards creating a graphical output in accompanying main output (dissimilarity degree)
  if (GraphInd==T){
    
    # Calculate the empirical cumulative distribution of the obtained Cox-Snell residuals (cs)
    EmpDist <- ecdf(vCS)
    
    # Create a grid of x-values for plotting purposes
    x <- sort(unique(c(vCS, vExp)))
    
    # Calculate CDF-values for each observation at each x value
    y1 <- EmpDist(x)
    y2 <- pexp(x,1)
    
    # Find the maximum difference (D statistic)
    D_location <- which.max(abs(y1 - y2))
    
    # Create a data frame for plotting
    datGraph <- rbind( data.table(x=vCS,type="1_Cox-Snell"),
                       data.table(x=vExp,type="2_Unit_Exponential"))
    datSegment <- data.frame(x = x[D_location],xend = x[D_location],
                             y = y1[D_location],yend = y2[D_location],type="Difference")
    
    # - Aesthetic engineering
    chosenFont <- "Cambria"
    vCol <- brewer.pal(8,"Set1")[c(2,3)]
    vLabel <- c("1_Cox-Snell"=bquote("Adjusted Cox-Snell Residuals "*italic(r)^(CS)),
                "2_Unit_Exponential"="Unit Exponential")
    datGraph[, FacetLabel := panelTitle]
    
    # Plot the ECDFs with ggplot2
    gg <- ggplot(datGraph,aes(x=x,group=type)) + theme_minimal() + 
      theme(text=element_text(family=chosenFont),legend.position = "bottom",
            strip.background=element_rect(fill="snow2", colour="snow2"),
            strip.text=element_text(size=8, colour="gray50"), strip.text.y.right=element_text(angle=90)) +
      labs(x = bquote(italic(x)), y = bquote("Cumulative Distribution Function "*italic(F(x)))) +
      stat_ecdf(aes(color=type,linetype=type)) + 
      geom_segment(data=datSegment,aes(x = x, xend = xend, y = y, yend = yend),
                   linetype = "dashed", color = "black", 
                   arrow=arrow(type="closed", ends="both",length=unit(0.08,"inches"))) +
      annotate("label", x = x[D_location], y = y1[D_location],
               label = paste0("Harrell's c: ",sprintf("%.3f", conc$concordance*100),"%"),
               hjust = -0.1, vjust = 0.5, fill="white", alpha=0.6) +
      annotate("label", x = x[D_location], y = (y1[D_location] + y2[D_location]) / 2,
               label = paste0("KS-statistic: ", percent(1-bStat)),
               hjust = -0.1, vjust = 0.5, fill="white", alpha=0.6) +
      annotate("label", x = x[D_location], y = y2[D_location],
               label = paste0("AIC: ", comma(AIC)),
               hjust = -0.1, vjust = 0.5, fill="white", alpha=0.6) +
      # Scales and options
      facet_grid(FacetLabel ~ .) +   
      scale_color_manual(name = "", values = vCol, labels=vLabel) +
      scale_linetype_discrete(name = "",labels=vLabel) +
      scale_y_continuous(label=percent) + 
      scale_x_continuous(lim=c(NA,10))
    
    # Save figure
    ggsave(gg, file=fileName, width=1200/dpi, height=1000/dpi, dpi=dpi, bg="white")
    
    # Prepare return object
    retOb <- list(Stat = as.vector(bStat), KS_graph=gg)
  }else{
    retOb <- list(Stat = as.vector(bStat))
  }
  
  return(retOb)
}






# ----------------- 2. Functions for estimating Schoenfeld residuals -------------

# Function to graph the time dependent ROC curve and calculate the AUC.
# Input:  dat - Dataset containing the [Start], [Stop] and [Default_Ind] variables
#         cox - cox model
#         month - desired month to test ROC curve on.
#         span - % size of the neighborhood (will be symmetrical around the same point)
#         numDigits - rounding scheme applied to markers (specifically if the vectors memory consuming)
# Output: AUC - Area under the curve
#         ROC_graph - ggplot object for ROC curve

sfResiduals <- function(cox, dataset, var, legPos = c(50,1), legPosCat = c(0.9,0.1)){
  # Select relevant columns and convert to data.table
  dat <- dataset[, .(LoanID, Start, End, Default_Ind, Var=get(var))]
  
  # Add risk score and total risk score
  dat[, RiskScore := predict(cox, dataset, type = "risk")]
  dat[, TotalScore := sum(RiskScore), by = End]
  
  # Process numeric and categorical variables differently
  if (is.numeric(dat$Var)) {
    # Handling numeric Var
    dat[, RW_Val := Var * RiskScore / TotalScore]
    dat[, Exp_Val := sum(RW_Val), by = End]
    dat[, sfRes := Var - Exp_Val]
    #dat[, RW_V := var(sfRes), by=End]
    #dat[, ssfRes := sfRes/RW_V]
    p <- sfTest(cox)
    
    # Create a data frame for plotting
    datGraph <- dat[Default_Ind == 1,]
    segment_data <- data.frame(x = 1,xend = max(datGraph$End),y = 0,yend = 0)
    
    gg <- ggplot(datGraph,aes(x=End,y=sfRes)) + theme_minimal() + 
      theme(text = element_text(family="Cambria"),
            legend.position = "inside",
            legend.background = element_rect(fill="snow2", color="black", linetype="solid")) +
      geom_point(alpha=0.7, color = "cornflowerblue") + geom_smooth(method="loess", color="navy") +
      annotate("label",x=legPos[1], y=legPos[2], label = paste("p-value for ",var,": ", percent(p)),
               fill="grey", alpha=0.6) +
      geom_segment(data= segment_data, aes(x = x, xend = xend, y = y, yend = yend),
                   linetype = "dashed", color = "black") +
      labs(x = bquote("Default Time "*italic(T)), y = bquote("Schoenfeld Residuals "*italic(r)^(s)))
    
  } else if (is.character(dat$Var) || is.factor(dat$Var)) {
    # # Handling categorical Var
    Levels <- unique(dat$Var)
    # dat[, Var_Name_Level := as.character(Var)]  # Add level information
    # p <- sfTest(cox)
    # 
    # # Calculate RW_Val, Exp_Val, and residuals for all levels at once
    # dat[, RW_Val := (Var == Var_Name_Level) * RiskScore / TotalScore]
    # dat[, Exp_Val := sum(RW_Val), by = .(End, Var_Name_Level)]
    # dat[, sfRes := (Var == Var_Name_Level) - Exp_Val]
    # 
    if(length(Levels) == 2){
      r <- residuals(cox, type = "schoenfeld")
      r <- data.frame(End=as.numeric(names(r)), residuals = r )
      setnames(r, "residuals", paste0(var,Levels[2]))
      vCol <- brewer.pal(ncol(r)-1,"Set1")[1]
    }else{
      r <- residuals(cox,type="schoenfeld") %>% data.table()
      r <- cbind(End=as.numeric(rownames(r)),r) %>% as.data.table()
      vCol <- brewer.pal(ncol(r)-1,"Set1")
    }
    
    # Create a data frame for plotting
    datGraph <- pivot_longer(r,cols=starts_with(var),names_to=var, values_to = "sfRes")
    segment_data <- data.frame(x = 1,xend = max(datGraph$End),y = 0,yend = 0)
    #vLabel <- separate(unique(datGraph[,var]), strings, into=c("before", "after"), sep=var)$after
    
    
    gg <- ggplot(datGraph,aes(x=End, y=sfRes, color=get(var))) + theme_minimal() +
      theme(text = element_text(family="Cambria"),
            legend.position = "top",legend.background = element_rect(fill="snow2", color="black", linetype="solid")) +
      labs(x = bquote("Default Time "*italic(T)), y = bquote("Schoenfeld Residuals "*italic(r)^(s))) +
      geom_point(alpha=0.7) + geom_smooth(method="loess", se=FALSE) + facet_wrap( ~ get(var), scales="free_y") +
      geom_segment(data= segment_data, aes(x = x, xend = xend, y = y, yend = yend),
                   linetype = "dashed", color = "black") +
      scale_color_manual(name=var, values=vCol)
    
    # Save graph
    ggsave(gg, file=paste0(genFigPath, "TFD/Shoenfeld Residuals(",var,")",".png"), width=2550/dpi, height=2000/dpi, dpi=dpi, bg="white")
  }
  
  # Return final result
  print(gg)
  
  return(list(p_value = round(p,2),sumRes = sum(datGraph$sfRes)))
}

sfTest <- function(cox){
  ans <- cox.zph(cox)$table[1,"p"]
  return(ans)
}



# The cgd dataset from the survival package contains survival data from a clinical
# trial on patients with chronic granulomatous disease (CGD), a rare immune deficiency (recurrent event).
Test <- FALSE # Toggle for unit tests; Test <- T
if (Test){
  force(data(cgd,package="survival"))
  data(cgd) # Load data set
  # Lightly prepare data into a generic format that can span our eventual credit dataset as well
  dat <- as.data.table(cgd)[, .(id, tstart, tstop, status, sex, age, height, weight, inherit, enum, steroids, treat)] %>% 
    rename(ID=id, Start=tstart,End=tstop,Event_Ind=status)
  # dat <- survSplit(Surv(Start,End,Default_Ind) ~  .,data=cgd,cut=c(1:max(cgd$End)),
  #                 start="Start",end="End",event="Default_Ind") %>% as.data.table() # Apply the counting process
  dat[order(ID),Removed := ifelse(ID != shift(ID,type="lead"),1,0)]
  dat[is.na(Removed),Removed := TRUE]
  # --- Fit Cox Regression Model correctly, where observations are clustered around a given ID without assuming independence
  coxExample <- coxph(Surv(Start,End,Event_Ind) ~ weight + age + enum + steroids + treat,
                      data=dat, id=ID)
  summary(coxExample)
  
  r <- residuals(cox,type="schoenfeld")
  plot(names(r), r)
  
  sr <- residuals(cox,type="scaledsch")
  plot(names(sr),sr)
  abline(0,0, col="red")
  
  rm(cgd,cgd0,coxExample) 
}