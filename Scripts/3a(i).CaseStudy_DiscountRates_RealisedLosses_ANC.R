# ============================== Discount Rate Comparison ================================
# A case study for analysing the discount rates and subsequent realised losses 
# across a few calculation methods for a single loan with multiple default spells
# TruEnd-procedure applied.
# ---------------------------------------------------------------------------------------
# PROJECT TITLE: TruEnd-procedure
# SCRIPT AUTHOR(S): Dr Arno Botha
# ---------------------------------------------------------------------------------------
# -- Script dependencies:
#   - 0.Setup.R
#   - 1.Data_Import.R
#   - 2a.Data_Prepare_Credit_Basic.R
#   - 2b.Date_Prepare_Credit_Advanced.R
#   - 2c(i).Data_Prepare_Credit_TruEnd.R
#   - 2d(i).Data_Enrich_TruEnd.R
#   - 2e(i).Data_Exclusions_TruEnd.R
#
# -- Inputs:
#   - datCredit_real | Enhanced versions of input dataset (script 2e(i))
#
# -- Outputs:
#   - lookup | loan history (1 default spell only) for case study
#   - lookup2 | loan history (all history) for case study
#   - <analytics> | Graph showing influence of discount rate on actual LGD for a single loan
# =======================================================================================



# ------ Setup

# --- 1. Load TruEnd-treated data and create lookups
if (!exists('datCredit_real')) unpack.ffdf(paste0(genPath,"creditdata_final4-TruEnd"), tempPath)

# - Lookup: 1 default spell
lookupID <- datCredit_real[LossRate_Real < 0 & !is.na(DefSpell_Num) & DefSpellResol_Type_Hist != "Censored" & 
                             DefSpell_Counter==1, DefSpell_Key][1]
lookup <- subset(datCredit_real, DefSpell_Key == lookupID)
lookup <- subset(lookup, select=c("LoanID", "Date", "DefSpell_Key", "DefSpell_Num", "TimeInDefSpell","DefSpell_Age", 
                                  "DefSpellResol_Type_Hist", "Principal", "InterestRate_Nom", "Balance", "Instalment", "Receipt_Inf", "Arrears"))
# - Lookup: all history
lookupID2 <- substr(lookupID, 1, str_locate(pattern ='_',lookupID)[1]-1)
lookup2 <- subset(datCredit_real, LoanID == lookupID2)
lookup2 <- subset(lookup2, select=c("LoanID", "Date", "DefSpell_Key", "DefSpell_Num", "TimeInDefSpell","DefSpell_Age", 
                                    "DefSpellResol_Type_Hist", "Principal", "InterestRate_Nom", "Balance", "Instalment", "Receipt_Inf", "Arrears"))

# - save lookups to disk | LoanID: 138602
write_csv(lookup, paste0( genObjPath, "Example1_DefaultSpell.csv"))
write_csv(lookup2, paste0( genObjPath, "Example1_AllHistory.csv"))


# --- 2. Load example data
# NOTE: Execute from here if credit data is unavailable
if (!exists('lookup')) lookup <- read.csv("Example1_DefaultSpell.csv")
if (!exists('lookup2')) lookup2 <- read.csv("Example1_AllHistory.csv")



# ------ Define custom functions for calculating the Internal Rate of Return (IRR)

# - Custom function to calculate the present value of a series of cash flows [x], given 
# a vector of nominal interest rates [rate] that correspond to [x]
# NPV = \sum_{t=0}^{n}{x_t * (1+r_t)^{-t}}
calcNPV <- function(x, rate) { sum(x * (1+rate)^(-1*(0:(length(x)-1))), na.rm=T)}

# - Custom function for calculating the derivative of the NVP wrt rate (r): d(npv)/dr | "derivative" of the calcNPV()-function
# Step 1: For a single t-value: d/dr ( x_t * (1+r_t)^{-t} ) = x_t * d/dr ( (1+r_t)^{-t} )
# Step 2: Using chain rule on d/dr ( (1+r_t)^{-t} ):  -t * (1+r_t)^{-t-1} * d/dr (1+r_t) = -t * (1+r_t)^{-t-1} * (1)
# Step 3: Combine Steps 1-2: d/dr ( x_t * (1+r_t)^{-t} ) is therefore = x_t * (-t) * (1+r_t)^{-t-1}
# Step 4: Derivative of NPV is the sum of derivatives of each t-value, i.e., 
#   d(npv)/dr = \sum_{t=0}^{n}{   d/dr ( x_t * (1+r_t)^{-t} )  }
# Step 5: Substitute Step 3's result into Step 4: d(npv)/dr = \sum_{t=0}^{n}{ x_t * (-t) * (1+r_t)^{-t-1}  }
calcNPV_deriv <- function(x, rate) {
  sum(x * (-1*(0:(length(x)-1))) * (1+rate)^(-1*(1:length(x))), na.rm=T)
}

# - Custom function for calculating the second derivative of the NVP wrt rate (r): d(npv)/dr | "derivative" of the calcNPV_deriv()-function
# Similar process as to that of calcNPV_deriv():
# d2(npv)/dr2 = \sum_{t=0}^{n}{ x_t * (t) * (t+1) * (1+r_t)^{-t-2}  }
calcNPV_deriv2 <- function(x, rate) {
  sum(x * (0:(length(x)-1)) * (1:(length(x))) * (1+rate)^(-1*(2:(length(x)+1))), na.rm=T)
}

# - Custom function for calculating the internal rate of return
calcIRR <- function(x, irr_init = 0.1, tolerance=1e-7, max_iter=1000, method="Newton-Raphson") {
  # - Testing conditions
  # x <- c(-4115,1000,1100,1200); method <- "Newton-Raphson"
  
  irr <- irr_init # initial guess
  
  # - Execute based on given optimisation method
  if (method == "Newton-Raphson") {
    # - Newton-Raphson method | see https://en.wikipedia.org/wiki/Newton%27s_method
    
    for (i in 1:max_iter) {
      obj <- calcNPV(x,irr) # calculate objective function at current rate
      # Update rate
      irr <- irr - obj / calcNPV_deriv(x,irr)
      # Check convergence
      if (abs(obj) < tolerance) {
        cat(paste0("\nConverged after ", i, " iterations. IRR: ", sprintf("%.2f",irr*100), "%\n"))
        return(irr)
      }
    }
    cat("\nNewton-Raphson's method did not converge .. retrying with Halley's method.");
    calcIRR(x, irr_init, tolerance=tolerance, max_iter=max_iter, method="Halley")
    
  } else if (method == "Halley") {
    # - Hallye's method | see https://en.wikipedia.org/wiki/Halley%27s_method
    
    for (i in 1:max_iter) {
      
      obj <- calcNPV(x,irr) # calculate objective function at current rate
      deriv1 <- calcNPV_deriv(x,irr) # calculate 1st derivative of objective function
      deriv2 <- calcNPV_deriv2(x,irr) # calculate 2nd derivative of objective function
      
      # Update rate
      irr <- irr - (2*obj*deriv1) / (2*deriv1^2 - obj*deriv2)
      
      # Check convergence
      if (abs(obj) < tolerance) {
        cat(paste0("\nConverged after ", i, " iterations. IRR: ", sprintf("%.2f",irr*100), "%\n"))
        return(irr)
      }
    }
    warning("Halley's method did not converge"); return(irr)
  }
  else {stop("Specified method not implemented!")}
}

# - Unit tests
calcIRR(x=c(-4115,1000,1100,1200)); calcIRR(x=c(-4115,1000,1100,1200), method="Halley")
### RESULTS: Corresponds to IRR function in Excel and an online calculator, given the same cash flows



# ------ Analysis 1: Empirical case study on a single loan across calculated discount rate candidates

# -- Spell-level analysis
ggplot(lookup, aes(x=Date,y=Balance)) + theme_minimal() + geom_point() + geom_line()
ggplot(lookup, aes(x=Date,y=InterestRate_Nom)) + theme_minimal() + geom_point() + geom_line()
ggplot(lookup, aes(x=Date,y=Instalment)) + theme_minimal() + geom_point() + geom_line()
ggplot(lookup, aes(x=Date,y=Receipt_Inf)) + theme_minimal() + geom_point() + geom_line()

# -- Loan-level analysis
ggplot(lookup2, aes(x=Date,y=Balance)) + theme_minimal() + geom_point() + geom_line()
ggplot(lookup2, aes(x=Date,y=Instalment)) + theme_minimal() + geom_point() + geom_line()



# -- Candidate discount rates
# - Contract Rate itself, despite drawbacks (see Larney2023)
discRate1 <- lookup$InterestRate_Nom; mean(lookup$InterestRate_Nom, na.rm=T)
### RESULTS: mean: 8.75%

# - Internal rate of return on equating expected instalment vector to default balance
# NOTE: Last default spell only
discRate2 <- calcIRR(c(-lookup$Balance[1], lookup$Instalment), method="Halley")
### RESULTS: -22.05% IRR

# - Internal rate of return on equating expected instalment vector to principal (1st balance)
# NOTE: All history
discRate3 <- calcIRR(c(-lookup2$Balance[1], lookup2$Instalment), method="Halley")
### RESULTS: -0.35% IRR

# - Internal rate of return on equating expected instalment vector to principal (1st balance)
# NOTE: All history up to start of last default spell
lookup3 <- subset(lookup2, DefSpell_Num < max(lookup2$DefSpell_Num, na.rm=T))
discRate4 <- calcIRR(c(-lookup3$Balance[1], lookup3$Instalment), method="Halley")
### RESULTS: -0.8% IRR



# -- Calculate realised loss rate (actual LGD) using candidate discount rates

# - Contract Rate
# realised loss rate
lossRate1 <- 1 - calcNPV(lookup$Receipt_Inf, discRate1/12)/lookup$Balance[1]; sprintf("%.2f", lossRate1*100)
### RESULTS: -1.18% loss

# - IRR: Last default spell only
lossRate2 <- 1 - calcNPV(lookup$Receipt_Inf, discRate2/12)/lookup$Balance[1]; sprintf("%.2f", lossRate2*100)
### RESULTS: -33.69% loss

# - IRR: All history
lossRate3 <- 1 - calcNPV(lookup$Receipt_Inf, discRate3/12)/lookup$Balance[1]; sprintf("%.2f", lossRate3*100)
### RESULTS: -9.6% loss

# - IRR: All history up to start of last default spell
lossRate4 <- 1 - calcNPV(lookup$Receipt_Inf, discRate4/12)/lookup$Balance[1]; sprintf("%.2f", lossRate4*100)
### RESULTS: -10.05% loss


# -- Graphing results
datResults <- data.table(DiscountRate_Mean = c(mean(discRate1,na.rm=t), discRate2, discRate3, discRate4),
  LossRate = c(lossRate1, lossRate2, lossRate3, lossRate4))

ggplot(datResults, aes(x=DiscountRate_Mean, y=LossRate)) + theme_minimal() +
  geom_point() + geom_line()
### RESULTS: Seemingly linear, though this cannot be given financial mathematical theory
# I suspect the provided vector of candidate discount rates is too narrow
# Need to investigate this more broadly across all plausible discount rates \in [-1,1]




# ------ Analysis 2: Empirical case study on a single loan's LGD across hypothetical discount rates

# - Create vector of hypothetical discount rates
discRates <- seq(-1, 1, by=0.01)
# ensure discount rates include previous results
discRates <- sort(unique(c(discRates, datResults$DiscountRate_Mean)))

# - Calculate corresponding actual LGD-values (realised loss rate), given loan history
lossRates <- sapply(1:length(discRates), function(i,d) {
  1 - calcNPV(lookup$Receipt_Inf, d[i]/12)/lookup$Balance[1]
}, d=discRates)


# -- Graphing results
# - Create graphing object
datGraph <- data.table(DiscountRate = discRates, LossRate = lossRates)
minLGD <- min(datGraph$LossRate, na.rm=T)
minDiscRate <- min(datGraph$DiscountRate, na.rm=T)

# - Create annotation object
datAnnotate <- data.table(DiscountRate = datResults$DiscountRate_Mean,
                          LossRate = datResults$LossRate,
                          Type=c("b_ContractRate", "c_IRR_DefSpell", "d_IRR_AllHist", "e_IRR_AllHist_ExclLastSpell"),
                          Label= paste0("italic(l)==", sprintf("%.2f", datResults$LossRate*100), "*'%'")) 

# - Enrich graphing object with info from annotation object for graphing purposes
datGraph <- merge(datGraph, datAnnotate, by=c("DiscountRate", "LossRate"), all.x=T)
datGraph[is.na(Type), Type:= "a_Hypothetical"]

# - Set graphing parameters
vCol <- brewer.pal(8, "Set1")
vLabel <- c("b_ContractRate"=paste0("Contract Rate (mean: ", percent(mean(discRate1,na.rm=T), accuracy=0.01),")"), 
            "c_IRR_DefSpell"=paste0("IRR: Last default spell (", percent(discRate2, accuracy=0.01),")"),
            "d_IRR_AllHist"=paste0("IRR: All history (", percent(discRate3, accuracy=0.01),")"), 
            "e_IRR_AllHist_ExclLastSpell"=paste0("IRR: Exclude last default spell (", percent(discRate4,accuracy=0.01), ")") )
chosenFont <- "Cambria"
dpi <- 180

# - Create main graph
(g0 <- ggplot(datGraph, aes(x=DiscountRate, y=LossRate)) + theme_minimal() + 
  theme(text=element_text(family=chosenFont),legend.position="bottom") + 
  labs(title=paste0("Actual LGD for written-off LoanID ", lookup$LoanID[1]), 
       subtitle = paste0("Last default spell starting ", lookup$Date[1], " and ending ", max(lookup$Date,na.rm=T), 
                         ": ", lookup[,.N], " periods"),
       x=bquote("Discount rate (%) "*italic(r[d])), y=bquote("Realised loss rate (%) "*italic(l))) + 
  # Main graph: hypothetical discount rates
  geom_line(linewidth=0.5) + #geom_point(size=0.5) + 
  # Annotation: candidate discount rates calculated from loan data
  geom_segment(data=datAnnotate, aes(x=DiscountRate, xend=DiscountRate, y=LossRate, yend=minLGD, colour=Type), 
               linewidth=0.3, linetype="dotted") + 
  geom_segment(data=datAnnotate, aes(x=DiscountRate, xend=minDiscRate, y=LossRate, yend=LossRate, colour=Type), 
               linewidth=0.3, linetype="dotted") +     
  geom_point(data=datAnnotate, aes(x=DiscountRate, y=LossRate, colour=Type), size=1.5) + 
  geom_text(data=datAnnotate, aes(x=DiscountRate, y=LossRate, label=Label, colour=Type),
             check_overlap=T, size=2.5, nudge_y=0.15, parse=T) + 
  # Facets & scale options
  scale_color_manual(name="Discount rate method", values=vCol, label=vLabel) + 
  scale_x_continuous(label=percent) + scale_y_continuous(label=percent) + 
  guides(colour=guide_legend(nrow=2))
)

# - Save graph
ggsave(g0, file=paste0(genFigPath, "CaseStudy1-LossRates_DiscRates.png"), width=1200/dpi, height=1000/dpi, dpi=dpi, bg="white")

### RESULTS: As expected, the previous seeming linearity disappeared once the discount rate vector is sufficiently expanded
