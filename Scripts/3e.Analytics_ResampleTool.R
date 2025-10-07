# ============================ RESAMPLING SCHEMES FOR SURVIVAL MODELS - ACS ==================================
# A tool for implementing resampling on a survival (panel) dataset using advanced clustered
# sampling, with either random sampling or n-way stratified sampling. In the case of n-way stratified
# sampling, a frequency analysis is conducted where observations/ records from stratum who have sizes
# smaller than a specified threshold are excluded.
# ------------------------------------------------------------------------------------------------------------
# PROJECT TITLE: Loss Modelling (LGD) for FNB Mortgages
# SCRIPT AUTHOR(S): Dr Arno Botha, Marcel Muller, Mohammed Gabru

# DESCRIPTION:
# This ancillary & exploratory script implements a given subsampling- and resampling scheme using advanced
# clustered sampling. Either random sampling or n-way stratified sampling is applied as the inner sampling
# technique. For the latter a frequency analysis is conducted and stratum with sizes below a specified
# threshold are excluded. In addition to screening for time-dependent
# sampling bias, resolution rates defined for spell entry time is used to investigate the structure of the
# datasets. These graphs are inadequate when investigating the TTC-means though, and subsequent resolution
# rates for spell exit (stop) are with associated MAEs are also computed. These two measures,
# resolution rate for spell entry, and resolution rate for spell exit)
# give conclusive insight on the representatives of the chosen cross-validation scheme and chosen
# inner sampling technique.
# ------------------------------------------------------------------------------------------------------------
# -- Script dependencies:
#   - 0.Setup
#   - 2d.Data_Fusion

# -- Inputs:
#   - datCredit_real | Prepared from script 2d.Data_Fusion.
#
# -- Outputs:
#   - Frequency analysis of strata size graph (stratification specified as the inner sampling technique);
#   - Resolution rates for spell entry (for all default spells);
#   - Resolution rates for spell exit/ stop.
# ------------------------------------------------------------------------------------------------------------




# ------ 1. Preliminaries
# -- Confirm prepared dataset is loaded into memory
if (!exists('datCredit_real')) unpack.ffdf(paste0(genPath,"creditdata_final4a"), tempPath)

# - Max- and min date of each default spell (used as a stratifier and for identifying FALSE default spells)
datCredit_real[!is.na(DefSpell_Key), c("DefSpell_Min_Date","DefSpell_Max_Date") := as.list(range(Date, na.rm=TRUE)), by=list(DefSpell_Key)]
datCredit_real[is.na(DefSpell_Key), c("DefSpell_Min_Date","DefSpell_Max_Date") := as.list(c(NA,NA)), by=list(DefSpell_Key)]

# Sanity check - Should be TRUE
datCredit_real[!is.na(DefSpell_Key),.N] == datCredit_real[!is.na(DefSpell_Max_Date),.N] # TRUE, field created successfully
datCredit_real[!is.na(DefSpell_Key),.N] == datCredit_real[!is.na(DefSpell_Min_Date),.N] # TRUE, field created successfully

# - Creating a curing indicator for default spells (enables graphing of curing events in default spells)
datCredit_real[, Cured_Ind := ifelse(!is.na(DefSpell_Key) & DefSpellResol_Type_Hist=="Cured" & Date==DefSpell_Max_Date,1,0)] # | Reduce conditions to only the second and third (check feasibility)

# - Creating new spell resolution types for analytical purposes
datCredit_real <- datCredit_real %>% mutate(DefSpellResol_Type_Hist2 = case_when(DefSpellResol_Type_Hist=="Cured" ~ "Cured",
                                                                                 DefSpellResol_Type_Hist=="Censored" ~ "Censored",
                                                                                 DefSpellResol_Type_Hist=="WOFF" ~ "Written-off",
                                                                                 TRUE ~ NA))

datCredit_real <- datCredit_real %>% mutate(DefSpellResol_Type_Hist3 = case_when(DefSpellResol_Type_Hist=="WOFF" ~ "Written-off",
                                                                                 DefSpellResol_Type_Hist %in% c("Censored", "Cured") ~ "Other",
                                                                                 TRUE ~ NA))
### NOTE: [DefSpellResol_Type_Hist3] is exclusively used in graphing resolution rates over time given spell stop time as cohort-definition
# As such, there will only be a single right-censored cohort (last date).
# [SANITY CHECKS]: Any missing spells keys or mismatch between spell keys and associated resolution types?
datCredit_real[is.na(DefSpell_Key),.N] == datCredit_real[is.na(DefSpellResol_Type_Hist2),.N] # TRUE, field created successfully
datCredit_real[is.na(DefSpell_Key),.N] == datCredit_real[is.na(DefSpellResol_Type_Hist3),.N] # TRUE, field created successfully
# - Create a variable for the first observation date of a loan. This is done purely for graphing purposes
datCredit_real[, Date_First := Date[1], by=LoanID]
# [SANITY CHECK] Checking if the variable was created correctly
(check.1 <- datCredit_real[is.na(Date_First),.N] == 0) # Should be TRUE
cat(check.1 %?% 'SAFE: variable [Date_First] was successfully created.\n' %:% 'WARNING: variable [Date_First] was not successfully created!\n')


# - Field Names and other parameters
clusVar <- "LoanID" # for clustered random sampling on subject-level
clusVar_Spell <- "DefSpell_Key" # for clustered random sampling on spell-level within a subject's history
timeVar <- "Date" # calendar time variable over which main event is observed & predicted, used in strata analysis
counter <- "Counter" # subject-level row index variable, used in strata analysis
# - For stratification (optional)
stratifiers <- c("Date_First") # First variable should be of type "date" (and should ideally be [Date_First] for graphing purposes)
### NOTE: Assign stratifiers to NA if no stratifiers are desired. Good candidates include: "Event_Type", "LN_TPE", and "HasDefaulted_Ever"

# - Facet specification field names (for graphing purposes of the resolution rates)
resolType <- "DefSpellResol_Type_Hist2" # Performance spell resolution types
resolType_Val <- "Written-off" # Reference value in the performance spell resolution type field [resolType]
resolType2 <- "DefSpellResol_Type_Hist3" # for performance spell resolution rate; specific for the stopping time cohort 
### NOTE: assign [resolType2] to the same value of [resolType] if not interested in controlling the resolution rate facet for stop dates
resolType2_Val <- "Written-off" # Reference value for the main resolution type
### NOTE: Set to NA if not interested in creating additional facets for the performance spells using stopping time
spellNum <- "DefSpell_Num" # current spell number (integer value, expected to start at 1)
# - Collect all relevant variables together to be selected dynamically from dataset, as specified
selectionVar <- unique(c(clusVar, clusVar_Spell, timeVar, counter,
                         resolType, resolType2, stratifiers, spellNum)) # Variables to subset
selectionVar <- selectionVar[!is.na(selectionVar)] # Facilitating cases where the variables are left unspecified (specifically for use of no stratifiers)

# - Subset given dataset accordingly; a memory efficiency enhancement
datCredit <- subset(datCredit_real, select=selectionVar)

# - Subsampling parameters
smp_size <- 90000 # fixed size of downsampled set in terms of the number of unique loan accounts
# Implied sampling fraction for downsampling step
smp_perc <- smp_size/length(unique(datCredit[!is.na(get(clusVar_Spell)),get(clusVar)]))
cat("NOTE: A fixed sample size of", comma(smp_size), "is", percent(smp_perc, accuracy=0.1), "of all loans.\n")
### RESULTS: 90k constitutes 14% of all loans
confLevel <- 0.95 # Confidence interval parameter
# - Resampling, stratification, and other general parameters
smp_frac <- 0.7 # sampling fraction for resampling scheme
minStrata_size <- 0 # Minimum strata size specified for subsample
# - Clean up
rm(datCredit_real); gc()




# ------ 2. Subsampled resampling scheme: basic cross-validation with simple random sampling

# - Creating spell-level min/max date variables as stratifiers
datCredit[!is.na(get(clusVar)), c("timeVar_SpellMin","timeVar_SpellMax") := as.list(range(get(timeVar), na.rm=TRUE)), by=list(get(clusVar_Spell))]

# --- Downsample data into a set with a fixed size (using stratified sampling) before implementing resampling scheme
# - Set seed
set.seed(1, kind="Mersenne-Twister")

# - Training Key population
if (all(is.na(stratifiers))){ # - No stratifiers
  # Get unique subject IDs or keys from the full dataset
  datKeys <- unique(datCredit[!is.na(get(clusVar_Spell)), mget(c(clusVar))])
} else { # - Stratifiers
  # Get unique loan account IDs from the full dataset
  datKeys <- unique(datCredit[!is.na(get(clusVar_Spell)), mget(c(clusVar, stratifiers))])
}

# - Obtain the associated loan records in creating the subsampled dataset
datCredit_smp <- copy(datCredit[get(clusVar) %in% datKeys[, get(clusVar),]])

# --- Strata analysis and subsequent exclusions to ensure adherence to specified minimum strata size
if (all(!is.na(stratifiers))){ # - Conditional loop for strata
  selectionVar_smp <- c(clusVar, timeVar, stratifiers)
  # - Test for exclusions given violations in the minimum strata size, as provided
  datStrata_smp_min <- datCredit_smp[get(counter)==1, mget(selectionVar_smp)][, list(Freq = .N), by=stratifiers][Freq<minStrata_size,]
  cat(sum(datStrata_smp_min[,Freq]), "accounts of ", datCredit_smp[get(counter)==1,.N], "(", 
      sprintf("%.4f", sum(datStrata_smp_min[,Freq])/datCredit_smp[get(counter)==1,.N]*100), 
      "%) need to be excluded to ensure a minimum strata size of ", minStrata_size)
  
  # - Conditionally applying the exclusions
  if (sum(datStrata_smp_min[,Freq]) > 0){
    # Saving the number of records and the prior probability, in the subsampled dataset, for reporting
    datCredit_smp_old_n <- datCredit_smp[,.N]
    # Computing the prior probabilities of the performance spell resolution outcomes
    datCredit_smp_prior <- datCredit_smp[get(timeVar)==timeVar_SpellMin, get(resolType)] %>% table() %>% prop.table()
    datCredit_smp_prior <- datCredit_smp_prior[names(datCredit_smp_prior)[names(datCredit_smp_prior) == resolType_Val]][[1]]
    # Initiating a vector which will contain the exclusion IDs
    dat_keys_exc <- NA
    # Looping through the minimum strata dataset and building an exclusion condition (filter) for each row therein
    for (i in 1:datStrata_smp_min[,.N]){
      # Getting the type of class of each stratifier (used for building the ith condition)
      class_type <- sapply(datStrata_smp_min[,1:length(stratifiers)], function(x) {class(x[[1]])}) 
      
      excCond <- datStrata_smp_min[i,1:length(stratifiers)] # Getting the values of the ith minimum strata
      excCond <- data.table(Stratifier = colnames(excCond), # Building a dataset
                            Value = unname(t(excCond)), # Ensure that the column name is Value instead of Value.V1
                            Class = class_type)
      excCond[, Value.V1 := ifelse(Class %in% c("numeric", "Date"), 
                                   paste0("as.",Class,"(",'"',Value.V1,'"',")"), paste0('"', Value.V1, '"'))]
      excCond[, Condition := paste0(Stratifier, " == ", Value.V1, " & ")] # Adding an "and" operator to enable multiple conditions
      # Compiling the ith condition
      excCond2 <- parse(text = paste0(paste0(excCond$Condition, collapse = ""), counter,"==1"))
      # Add the excluded subject key to our list
      if (i==1) dat_keys_exc <- as.vector(datCredit_smp[eval(excCond2), get(clusVar)]) # set new list
      else dat_keys_exc <- c(dat_keys_exc, as.vector(datCredit_smp[eval(excCond2), get(clusVar)]) ) # append to existing list
    }
    
    # Applying the exclusions to the subsampled dataset
    datCredit_smp <- copy(datCredit_smp[!(get(clusVar) %in% dat_keys_exc),])
    
    cat(datCredit_smp_old_n-datCredit_smp[,.N], " observations removed (", 
        sprintf("%.4f", (datCredit_smp_old_n-datCredit_smp[,.N])/datCredit_smp_old_n*100), "% ) \n",
        "Prior probability = ", sprintf("%.4f", datCredit_smp_prior*100), "% comapred to ", 
        sprintf("%.4f", (datCredit_smp[get(timeVar)==timeVar_SpellMin, get(resolType)] %>% table() %>% prop.table())[[2]]*100), "%")
  }
  # [SANITY CHECK] Are there still violations in minimum strata sizes?
  datStrata_smp_min <- datCredit_smp[get(counter)==1, mget(selectionVar_smp)][, list(Freq = .N), by=stratifiers][Freq<minStrata_size,]
  if (NROW(datStrata_smp_min) > 0) cat(sum(datStrata_smp_min[,Freq]), "accounts of ", datCredit_smp[get(counter)==1,.N], "(", 
                                       sprintf("%.4f", sum(datStrata_smp_min[,Freq])/datCredit_smp[get(counter)==1,.N]*100), 
                                       "%) need to be excluded to ensure a minimum strata size of ", minStrata_size)
  # - Cleanup
  suppressWarnings( rm(datStrata_smp_min, datStrata_smp_min, datCredit_smp_old_n, datCredit_smp_prior, 
                       dat_keys_exc, class_type, excCond, excCond2))
}





# ------ 3. Apply a basic cross-validation clustered resampling scheme with possible n-way stratification

# - Use simple random sampling with the stratifiers to select the loan IDs that ought to be in the training dataset
set.seed(1, kind="Mersenne-Twister")
if (all(!is.na(stratifiers))){ # Stratifiers
  dat_train_keys <- datKeys %>% group_by(across(all_of(stratifiers))) %>% slice_sample(prop=smp_frac) %>% as.data.table() 
} else { # No stratifiers
  dat_train_keys <- datKeys %>% slice_sample(prop=smp_frac) %>% as.data.table()
}

# - Extract the entire loan histories into the training set for those randomly select subject IDs
datCredit_train <- copy(datCredit_smp[get(clusVar) %in% dat_train_keys[, get(clusVar)],])

# - Extract the entire loan histories into the validation set for those remaining subjects
datCredit_valid <- copy(datCredit_smp[!(get(clusVar) %in% dat_train_keys[, get(clusVar)]),])

# - [SANITY CHECKS]
# Can subsample be reconstituted?
check.1 <- datCredit_smp[,.N] == datCredit_train[,.N] + datCredit_valid[,.N]
# Does training set contain only first-time spells?
check.2 <- T # Irrelevant for this time definition, so assign default
# Does validation spell contain spell numbers other than 1?
check.3 <- T # Irrelevant for this time definition, so assign default
cat((check.1 %?% "SAFE: Training and validation datasets succcessfully reconstitute the subsampled dataset. \n" %:% 
       'WARNING: Training and validation datasets do not reconstitue the subsampled dataset. \n' ))
cat((check.2 %?% paste0("SAFE: Spells in the training dataset are selected as desired; Multi-spells? ", check.3, ".\n" ) %:%
       paste0("WARNING: Spells in the training dataset are not selected as desired; Multi-spells? ", check.3, ".\n" )))
cat((check.3 %?% paste0("SAFE: Spells in the validation dataset are selected as desired; Multi-spells? ", check.3, ".\n" ) %:%
       paste0("WARNING: Spells in the validation dataset are not selected as desired; Multi-spells? ", check.3, ".\n" )))

# - Clean up
suppressWarnings(rm(check.1, check.2, check.3, 
                    class_type, LoanID_FirstSpell))




# ------ 4. Strata frequency analysis on resampled datasets

# --- Preliminaries

# - Renaming certain columns within the datasets for ease of graphing
### NOTE: These names will be use throughout the rest of the script and will not be reverted given 
# the ancillary nature of this script
colnames(datCredit)[1:4] <- c("ClusVar", "clusVar_Spell", "timeVar", "Counter")
colnames(datCredit_smp)[1:4] <- c("ClusVar", "clusVar_Spell", "timeVar", "Counter")
colnames(datCredit_train)[1:4] <- c("ClusVar", "clusVar_Spell", "timeVar", "Counter")
colnames(datCredit_valid)[1:4] <- c("ClusVar", "clusVar_Spell", "timeVar", "Counter")

# - Merge datasets together for graphing purposes
datGraph <- rbind(datCredit[, Sample:="a_Full"],
                  datCredit_train[, Sample:="b_Train"],
                  datCredit_valid[, Sample:="c_Valid"])


# --- Conduct analysis only if there are stratifiers
if (all(!is.na(stratifiers))){
  # - Determine subsampling window given cross-sectional design
  StartDte <- min( datCredit_smp[!is.na(clusVar_Spell), timeVar], na.rm=T)
  EndDte <- max( datCredit_smp[!is.na(clusVar_Spell), timeVar], na.rm=T)
  maxDate <- EndDte %m+% months(-1) # A post-hoc filter, used for graphing purposes - left as the end of the sampling window
  minDate <- StartDte  %m+% months(1) # A post-hoc filter, used for graphing purposes - set as one month after the sampling window
  
  # - Aggregate data according to the same n-way stratified sampling technique used within subsampling/resampling scheme
  selectionVar_train <- c("ClusVar", "timeVar", stratifiers)
  datStrata <- copy(datCredit_train[Counter==1, ..selectionVar_train][, list(Freq = .N), by=stratifiers])
  
  # - Aesthetics engineering
  datStrata[, Facet_label := "Strata Frequency Analysis"]
  
  # - Create summaries for annotations within graph
  datStrata_aggr <- datStrata[, list(StratumSize_N = .N, StratumSize_Min = min(Freq,na.rm=T), StratumSize_Mean = mean(Freq,na.rm=T),
                                     StratumSize_SD = sd(Freq,na.rm=T))]
  
  datStrata_aggr[, StrataSize_Margin := qnorm(1-(1-confLevel)/2) * datStrata_aggr$StratumSize_SD / sqrt(datStrata_aggr$StratumSize_N)]
  
  # - Graphing parameters
  chosenFont <- "Cambria"; dpi <- 340
  vCol <- brewer.pal(8, "Dark2")
  vFill <- brewer.pal(8, "Set2")
  x_pos <- median(as.vector(data.table(datStrata[,..stratifiers])[,1])[[1]]) + years(5)
  
  # - Create graph to evidence minimum strata sizes
  (g1 <- ggplot(datStrata[get(stratifiers[1])>=minDate & get(stratifiers[1])<=maxDate,], aes(x=get(stratifiers[1]), y=Freq)) + theme_minimal() + 
      labs(x=bquote("Date "*italic(t)), y=bquote("Proporionate volume (%) of stratifiers within "*italic(D[T])~"("*.(round(datCredit_train[,.N]/1000))*"k)")) + 
      theme(text=element_text(family=chosenFont),legend.position = "bottom",
            axis.text.x=element_text(angle=90), #legend.text=element_text(family=chosenFont), 
            strip.background=element_rect(fill="snow2", colour="snow2"),
            strip.text=element_text(size=8, colour="gray50"), strip.text.y.right=element_text(angle=90)) +
      # Creating conditional geoms based on the number of stratification variables used
      {if (length(stratifiers)>1){
        # main bar graph
        list(geom_bar(position="stack", stat="identity", aes(colour=get(stratifiers[-1]), fill=get(stratifiers[-1]))),
             # scale options
             scale_colour_manual(name=stratifiers[-1], values=vCol),
             scale_fill_manual(name=stratifiers[-1], values=vFill))
      } else {
        # main bar graph
        list(geom_bar(position="stack", stat="identity", colour=vCol[1], fill=vFill[1]))
      }} +
      # annotations
      annotate("text", x=x_pos, y=Inf, size=3, hjust=0.5, vjust=4, family=chosenFont,
               label=paste0(datStrata_aggr$StratumSize_N, " total strata \nwith a mean cell size of ", 
                            comma(datStrata_aggr$StratumSize_Mean, accuracy=0.1),
                            " ± ", sprintf("%.1f", datStrata_aggr$StrataSize_Margin), " \nand a minimum size of ", 
                            sprintf("%.0f", datStrata_aggr$StratumSize_Min))) + 
      # Rest of the facet & scale options
      facet_grid(Facet_label ~ .) + 
      scale_y_continuous(breaks=pretty_breaks(), label=comma) + 
      scale_x_date(date_breaks=paste0(6, " month"), date_labels = "%b %Y"))
  
  # - Save graph
  ggsave(g1, file=paste0(genFigPath, "/StrataDesign_Train_", round(datCredit_smp[,.N]/1000),"k.png"), width=2550/dpi, height=2000/dpi, dpi=dpi, bg="white")
  
  # --- Clean up
  rm(datStrata, datStrata_aggr, selectionVar_train, vCol, vFill, chosenFont, g1)
  
  ### CONCLUSION: Minimum strata sizes constantly violated. We will therefore amend the stratification design to 1-way
}




# ------ 5. Spell resolution rates over time across resampled datasets | Spell entry time (t_e)

# - Check representatives | dataset-level proportions should be similar
datCredit[timeVar==timeVar_SpellMin, get(resolType)] %>% table() %>% prop.table()
datCredit_train[timeVar==timeVar_SpellMin, get(resolType)] %>% table() %>% prop.table()
datCredit_valid[timeVar==timeVar_SpellMin, get(resolType)] %>% table() %>% prop.table()

# --- Merge datasets together for graphing purposes, subset necessary fields, and rename columns for graphing ease
datGraph <- rbind(datCredit[, Sample:="a_Full"],
                  datCredit_train[, Sample:="b_Train"],
                  datCredit_valid[, Sample:="c_Valid"]) %>%
  subset(!is.na(clusVar_Spell), select = c("clusVar_Spell", "timeVar", "timeVar_SpellMin", "timeVar_SpellMax", resolType, "Sample"))
colnames(datGraph) <- c("clusVar_Spell", "timeVar", "timeVar_SpellMin", "timeVar_SpellMax", "Spell_Resol", "Sample")

# - Setting some aggregation parameters, purely to facilitate graphing aesthetics
StartDte <- min(datCredit_smp$timeVar, na.rm=T)
EndDte <- max(datCredit_smp$timeVar, na.rm=T)
maxDate <- EndDte # A post-hoc filter, used for graphing purposes - left as the end of the sampling window
minDate <- StartDte + month(1) # A post-hoc filter, used for graphing purposes - set as one month after the sampling window

# - Fixing to spell entry-time, we aggregate to monthly level and observe the time series up to given point
datAggr_cohorts <- merge(datGraph[timeVar==timeVar_SpellMin, list(Sum_Total = .N), by=list(Sample,timeVar)],
                         datGraph[timeVar==timeVar_SpellMin, list(Sum_Resol = .N), by=list(Sample,timeVar,Spell_Resol)],
                         by=c("Sample", "timeVar"))[timeVar >= minDate & timeVar <= maxDate,]
datAggr_cohorts[, Prop := Sum_Resol/Sum_Total]


# --- Aesthetic engineering
# - Aggregate by spell type towards creating the prior probabilities, and rename columns accordingly
datFacetsProp <- datGraph[timeVar==timeVar_SpellMin, .(Prior = .N/ datGraph[timeVar==timeVar_SpellMin,.N]), keyby=.(Spell_Resol)]
resolDef_levels <- unique(datFacetsProp$Spell_Resol) # Get the unique factors of the spell resolution type (used as facets)

# Enrich aggregated dataset with prior probabilities
datAggr_cohorts <- merge(datAggr_cohorts, datFacetsProp, by="Spell_Resol")
datAggr_cohorts[, Facet:=paste0('"', Spell_Resol, ' (', sprintf("%.2f", Prior*100), '%)"')] # Facetting purposes

# - Pivot aggregated dataset to wider based on the combination field of Sample & Spell_Resol
datAggr_cohorts2 <- datAggr_cohorts %>% pivot_wider(id_cols = c(timeVar), names_from = c(Sample, Spell_Resol), values_from = Prop) %>% data.table()

# - Number of annotations "sets" to create and positioning in graph space
anno_n <- length(resolDef_levels)
x_pos <- min(datCredit$timeVar) + round((max(datCredit$timeVar) - min(datCredit$timeVar))/2) # x-position for the annotation

# -- Creating the annotation dataset
# Basic design
datAnnotate <- data.table(MAE = rep(0,anno_n*3), Mean_EventRate = rep(0, anno_n*3),
                          stdError_EventRate = rep(0, anno_n*3), margin_EventRate = rep(0, anno_n*3),
                          Spell_Resol = unlist(lapply(resolDef_levels, function(x){rep(x,3)})),
                          Dataset = rep(c("A-B","A-C","B-C"), anno_n),
                          Label = rep(c(paste0("'MAE between '*italic(A[t])*' and '*italic(B[t])*'"),
                                        paste0("'MAE between '*italic(A[t])*' and '*italic(C[t])*'"),
                                        paste0("'MAE between '*italic(B[t])*' and '*italic(C[t])*'")),
                                      anno_n),
                          x = rep(x_pos,anno_n*3), y = rep(Inf,anno_n*3),
                          vjust = c(7,8,9,3,4,5,-1,0,1), hjust=c(0.25,0.25,0.25,0.5,0.5,0.5,0.5,0.5,0.5))
# Get the column names to help compute the MAEs
vCols <- colnames(datAggr_cohorts2)

# - Calculate MAEs between resolution rates from different samples over time and across facets
for (i in 1:anno_n){
  # i <- 1 # testing condition
  # Get 
  vFull <- subset(datAggr_cohorts2, select=vCols[vCols %in% paste0("a_Full_", resolDef_levels[i])])[[1]]
  vTrain <- subset(datAggr_cohorts2, select=vCols[vCols %in% paste0("b_Train_", resolDef_levels[i])])[[1]]
  vValid <- subset(datAggr_cohorts2, select=vCols[vCols %in% paste0("c_Valid_", resolDef_levels[i])])[[1]]
  
  datAnnotate[i*3-2, MAE := mean(abs(vFull - vTrain), na.rm = T)] # MAE between the full- and training dataset
  datAnnotate[i*3-1, MAE := mean(abs(vFull - vValid), na.rm = T)] # MAE between the full- and validation dataset
  datAnnotate[i*3, MAE := mean(abs(vTrain- vValid), na.rm = T)] # MAE between the training- and validation dataset
}
# Expanding the label field with the MAEs
datAnnotate[, Label := paste0(Label, " = ", sprintf("%.4f",MAE*100), "%'")]

# - Enrich annotation object with facet labels to ensure positioning across facets is correct
datAnnotate <- merge(datAnnotate, unique(datAggr_cohorts[, .(Spell_Resol, Facet)]), by="Spell_Resol")

# - Graphing parameters
chosenFont <- "Cambria"; dpi <- 340
vCol <- brewer.pal(9, "Set1")
vLabel <- c("a_Full"=expression(italic(A)[t]*": Full set "*italic(D)),
            "b_Train"=bquote(italic(B)[t]*": Training set "*italic(D)[italic(T)]~"("*.(round(datCredit_train[,.N]/1000))*"k)"),
            "c_Valid"=bquote(italic(C)[t]*": Validation set "*italic(D)[italic(V)]~"("*.(round(datCredit_valid[,.N]/1000))*"k)"))


# --- Create graph
(g2 <- ggplot(datAggr_cohorts, aes(x=timeVar, y=Prop)) + theme_minimal() + 
    labs(x=bquote("Default spell cohorts (mmmccyy): entry time "*italic(t[e])), y=bquote("Resolution rate (%) of type "*~italic(kappa))) +
    theme(text=element_text(family=chosenFont),legend.position = "bottom",
          axis.text.x=element_text(angle=90), legend.text=element_text(family=chosenFont), 
          strip.background=element_rect(fill="snow2", colour="snow2"),
          strip.text=element_text(size=8, colour="gray50"), strip.text.y.right=element_text(angle=90)) + 
    # main line graph with overlaid points
    geom_line(aes(colour=Sample, linetype=Sample)) + 
    geom_point(aes(colour=Sample, shape=Sample), size=1) + 
    # facets
    facet_wrap(Facet~., labeller = label_parsed, scales = "free", nrow=length(resolDef_levels), strip.position="right") + 
    #annotations
    geom_text(data=datAnnotate, aes(x=x, y=y, hjust=hjust, vjust=vjust+7, label = Label), family=chosenFont, size=3, parse=T) + 
    # scale options
    scale_colour_manual(name=bquote("Sample "*italic(bar(D))), values=vCol, labels=vLabel) + 
    scale_shape_discrete(name=bquote("Sample "*italic(bar(D))), labels=vLabel) + 
    scale_linetype_discrete(name=bquote("Sample "*italic(bar(D))), labels=vLabel) + 
    scale_y_continuous(breaks=pretty_breaks(), label=percent) + 
    scale_x_date(date_breaks=paste0(6, " month"), date_labels = "%b %Y"))

# - Save graph
dpi <- 200
ggsave(g2, file=paste0(genFigPath, "ResolutionRates_Def_te_resample_", 
                       round(datCredit_smp[,.N]/1000),"k.png"), width=1200/(dpi), height=1800/(dpi), dpi=dpi, bg="white")

# --- Create graph only for censored
(g2a <- ggplot(datAggr_cohorts[Spell_Resol=="Censored" ,], aes(x=timeVar, y=Prop)) + theme_minimal() + 
    labs(x=bquote("Default spell cohorts (mmmccyy): entry time "*italic(t[e])), y=bquote("Resolution rate (%) of type "*italic(kappa)==1)) +
    theme(text=element_text(family=chosenFont),legend.position = "bottom",
          axis.text.x=element_text(angle=90), #legend.text=element_text(family=chosenFont), 
          strip.background=element_rect(fill="snow2", colour="snow2"),
          strip.text=element_text(size=8, colour="gray50"), strip.text.y.right=element_text(angle=90)) + 
    # main line graph with overlaid points
    geom_line(aes(colour=Sample, linetype=Sample)) + 
    geom_point(aes(colour=Sample, shape=Sample), size=1) + 
    # facets
    facet_wrap(Facet~., labeller = label_parsed, scales = "free", strip.position="right") + 
    #annotations
    geom_text(data=datAnnotate[Spell_Resol=="Censored", ], aes(x=x, y=y, hjust=hjust, vjust=vjust, label = Label), family=chosenFont, size=3, parse=T) + 
    # scale options
    scale_colour_manual(name=bquote("Sample "*italic(bar(D))), values=vCol, labels=vLabel) + 
    scale_shape_discrete(name=bquote("Sample "*italic(bar(D))), labels=vLabel) + scale_linetype_discrete(name=bquote("Sample "*italic(bar(D))), labels=vLabel) + 
    scale_y_continuous(breaks=pretty_breaks(), label=percent) + 
    scale_x_date(date_breaks=paste0(6, " month"), date_labels = "%b %Y"))

# - Save graph
dpi <- 200
ggsave(g2a, file=paste0(genFigPath, "ResolutionRates_Def_te_resample_Censored", 
                        round(datCredit_smp[,.N]/1000),"k.png"), width=1200/(dpi), height=1000/(dpi), dpi=dpi, bg="white")
# --- Create graph only for cured
(g2b <- ggplot(datAggr_cohorts[Spell_Resol=="Cured",], aes(x=timeVar, y=Prop)) + theme_minimal() + 
    labs(x=bquote("Default spell cohorts (mmmccyy): entry time "*italic(t[e])), y=bquote("Resolution rate (%) of type "*italic(kappa)==1)) +
    theme(text=element_text(family=chosenFont),legend.position = "bottom",
          axis.text.x=element_text(angle=90), #legend.text=element_text(family=chosenFont), 
          strip.background=element_rect(fill="snow2", colour="snow2"),
          strip.text=element_text(size=8, colour="gray50"), strip.text.y.right=element_text(angle=90)) + 
    # main line graph with overlaid points
    geom_line(aes(colour=Sample, linetype=Sample)) + 
    geom_point(aes(colour=Sample, shape=Sample), size=1) + 
    # facets
    facet_wrap(Facet~., labeller = label_parsed, scales = "free", strip.position="right") + 
    #annotations
    geom_text(data=datAnnotate[Spell_Resol=="Cured", ], aes(x=x, y=y, hjust=hjust, vjust=vjust, label = Label), family=chosenFont, size=3, parse=T) + 
    # scale options
    scale_colour_manual(name=bquote("Sample "*italic(bar(D))), values=vCol, labels=vLabel) + 
    scale_shape_discrete(name=bquote("Sample "*italic(bar(D))), labels=vLabel) + scale_linetype_discrete(name=bquote("Sample "*italic(bar(D))), labels=vLabel) + 
    scale_y_continuous(breaks=pretty_breaks(), label=percent) + 
    scale_x_date(date_breaks=paste0(6, " month"), date_labels = "%b %Y"))

# - Save graph
dpi <- 200
ggsave(g2b, file=paste0(genFigPath, "ResolutionRates_Def_te_resample_Cured", 
                        round(datCredit_smp[,.N]/1000),"k.png"), width=1200/(dpi), height=1000/(dpi), dpi=dpi, bg="white")
# Produce graph focusing only on written-off component
(g2c <- ggplot(datAggr_cohorts[Spell_Resol=="Written-off" ,], aes(x=timeVar, y=Prop)) + theme_minimal() + 
    labs(x=bquote("Default spell cohorts (mmmccyy): entry time "*italic(t[e])), y=bquote("Resolution rate (%) of type "*italic(kappa)==1)) +
    theme(text=element_text(family=chosenFont),legend.position = "bottom",
          axis.text.x=element_text(angle=90), #legend.text=element_text(family=chosenFont), 
          strip.background=element_rect(fill="snow2", colour="snow2"),
          strip.text=element_text(size=8, colour="gray50"), strip.text.y.right=element_text(angle=90)) + 
    # main line graph with overlaid points
    geom_line(aes(colour=Sample, linetype=Sample)) + 
    geom_point(aes(colour=Sample, shape=Sample), size=1) + 
    # facets
    facet_wrap(Facet~., labeller = label_parsed, scales = "free", strip.position="right") + 
    #annotations
    geom_text(data=datAnnotate[Spell_Resol=="Written-off", ], aes(x=x, y=y, hjust=hjust, vjust=vjust, label = Label), family=chosenFont, size=3, parse=T) + 
    # scale options
    scale_colour_manual(name=bquote("Sample "*italic(bar(D))), values=vCol, labels=vLabel) + 
    scale_shape_discrete(name=bquote("Sample "*italic(bar(D))), labels=vLabel) + scale_linetype_discrete(name=bquote("Sample "*italic(bar(D))), labels=vLabel) + 
    scale_y_continuous(breaks=pretty_breaks(), label=percent) + 
    scale_x_date(date_breaks=paste0(6, " month"), date_labels = "%b %Y"))

# - Save graph
dpi <- 200
ggsave(g2c, file=paste0(genFigPath, "ResolutionRates_Def_te_resample_WOff", 
                        round(datCredit_smp[,.N]/1000),"k.png"), width=1200/(dpi), height=1000/(dpi), dpi=dpi, bg="white")

# - Cleanup
suppressWarnings( rm(datGraph, datAggr_cohorts, datAnnotate, resolPerf_levels, chosenFont, g1, g2,g2a,g2b,g2c,
                     vCol, vLabel, colnames, datAggr_cohorts2, datFacetsProp) )




# ------ 6. Spell resolution rates over time across resampled datasets | Spell stop time (t_s)

# - Check representatives | dataset-level proportions should be similar
datCredit[timeVar==timeVar_SpellMax, get(resolType)] %>% table() %>% prop.table()
datCredit_train[timeVar==timeVar_SpellMax, get(resolType)] %>% table() %>% prop.table()
datCredit_valid[timeVar==timeVar_SpellMax, get(resolType)] %>% table() %>% prop.table()

# --- Merge datasets together for graphing purposes, subset necessary fields, and rename columns for graphing ease
datGraph <- rbind(datCredit[, Sample:="a_Full"],
                  datCredit_train[, Sample:="b_Train"],
                  datCredit_valid[, Sample:="c_Valid"]) %>%
  subset(!is.na(clusVar_Spell), select = c("clusVar_Spell", "timeVar", "timeVar_SpellMin", "timeVar_SpellMax", resolType, resolType2, "Sample"))
colnames(datGraph) <- c("clusVar_Spell", "timeVar", "timeVar_SpellMin", "timeVar_SpellMax", "Spell_Resol", "Spell_Resol2", "Sample")

# - Setting some aggregation parameters, purely to facilitate graphing aesthetics
StartDte <- min(datCredit_smp$timeVar, na.rm=T)
EndDte <- max(datCredit_smp$timeVar, na.rm=T)
maxDate <- EndDte # A post-hoc filter, used for graphing purposes - left as the end of the sampling window
minDate <- StartDte #+ month(1) # A post-hoc filter, used for graphing purposes - set as one month after the sampling window

# - Fixing to spell stop-time, we aggregate to monthly level and observe the time series up to given point
datAggr_cohorts <- merge(datGraph[timeVar==timeVar_SpellMax, list(Sum_Total = .N), by=list(Sample,timeVar)],
                         datGraph[timeVar==timeVar_SpellMax, list(Sum_Resol = .N), by=list(Sample,timeVar,Spell_Resol2)],
                         by=c("Sample", "timeVar"))[timeVar >= minDate & timeVar <= maxDate,]
datAggr_cohorts[, Prop := Sum_Resol/Sum_Total]


# --- Aesthetic engineering
# - Aggregate by spell type towards creating the prior probabilities, and rename columns accordingly
datFacetsProp <- datGraph[timeVar==timeVar_SpellMax, .(Prior = .N/ datGraph[timeVar==timeVar_SpellMax,.N]), keyby=.(Spell_Resol2)]
resolDef_levels <- unique(datFacetsProp$Spell_Resol2) # Get the unique factors of the spell resolution type (used as facets)

# Enrich aggregated dataset with prior probabilities
datAggr_cohorts <- merge(datAggr_cohorts, datFacetsProp, by="Spell_Resol2")
datAggr_cohorts[, Facet:=paste0('"', Spell_Resol2, ' (', sprintf("%.2f", Prior*100), '%)"')] # Facetting purposes

# - Pivot aggregated dataset to wider based on the combination field of Sample & Spell_Resol
datAggr_cohorts2 <- datAggr_cohorts %>% pivot_wider(id_cols = c(timeVar), names_from = c(Sample, Spell_Resol2), values_from = Prop) %>% data.table()

# - Number of annotations "sets" to create and positioning in graph space
anno_n <- length(resolDef_levels)
x_pos <- min(datCredit$timeVar) + round((max(datCredit$timeVar) - min(datCredit$timeVar))/2) # x-position for the annotation

# -- Creating the annotation dataset
# Basic design
datAnnotate <- data.table(MAE = rep(0,anno_n*4), Mean_EventRate = rep(0, anno_n*4),
                          stdError_EventRate = rep(0, anno_n*4), margin_EventRate = rep(0, anno_n*4),
                          Spell_Resol2 = unlist(lapply(resolDef_levels, function(x){rep(x,4)})),
                          Dataset = rep(c("A-B","A-C","B-C", "B"), anno_n),
                          Label = rep(c(paste0("'MAE between '*italic(A[t])*' and '*italic(B[t])*'"),
                                        paste0("'MAE between '*italic(A[t])*' and '*italic(C[t])*'"),
                                        paste0("'MAE between '*italic(B[t])*' and '*italic(C[t])*'"), 
                                        paste0("'TTC-mean '*E(italic(B[t]))*'")), anno_n),
                          x = rep(x_pos,anno_n*4), y = rep(Inf, anno_n*4),
                          vjust = c(21,22,23,22,1,2,3,4.5), hjust=rep(0.5, anno_n*4))
# Get the column names to help compute the MAEs
vCols <- colnames(datAggr_cohorts2)

# - Calculate MAEs between resolution rates from different samples over time and across facets
for (i in 1:anno_n){
  # i <- 1 # testing condition
  # Get 
  vFull <- subset(datAggr_cohorts2, select=vCols[vCols %in% paste0("a_Full_", resolDef_levels[i])])[[1]]
  vTrain <- subset(datAggr_cohorts2, select=vCols[vCols %in% paste0("b_Train_", resolDef_levels[i])])[[1]]
  vValid <- subset(datAggr_cohorts2, select=vCols[vCols %in% paste0("c_Valid_", resolDef_levels[i])])[[1]]
  
  datAnnotate[i*4-3, MAE := mean(abs(vFull - vTrain), na.rm = T)] # MAE between the full- and training dataset
  datAnnotate[i*4-2, MAE := mean(abs(vFull - vValid), na.rm = T)] # MAE between the full- and validation dataset
  datAnnotate[i*4-1, MAE := mean(abs(vTrain- vValid), na.rm = T)] # MAE between the training- and validation dataset
  datAnnotate[i*4,   mean_EventRate := mean(vTrain, na.rm=T)]
  datAnnotate[i*4,   stdError_EventRate := sd(vTrain, na.rm=T)/ sqrt(length(vTrain))]
}

# - Expanding the label field with the MAEs
datAnnotate[, Label2 := paste0(Label, " = ", sprintf("%.4f",MAE*100), "%'")]
# Override the label for the TTC-mean rates
datAnnotate[seq(from=4, to=4*anno_n, by=4), 
            Label2 := paste0(Label, " = ", sprintf("%.2f", mean_EventRate*100) , "% ±", 
                             sprintf("%.3f", margin_EventRate*100), "%'")]

# - Enrich annotation object with facet labels to ensure positioning across facets is correct
datAnnotate <- merge(datAnnotate, unique(datAggr_cohorts[, .(Spell_Resol2, Facet)]), by="Spell_Resol2")

# - Graphing parameters
chosenFont <- "Cambria"; dpi <- 340
vCol <- brewer.pal(9, "Set1")
vLabel <- c("a_Full"=expression(italic(A)[t]*": Full set "*italic(D)),
            "b_Train"=bquote(italic(B)[t]*": Training set "*italic(D)[italic(T)]~"("*.(round(datCredit_train[,.N]/1000))*"k)"),
            "c_Valid"=bquote(italic(C)[t]*": Validation set "*italic(D)[italic(V)]~"("*.(round(datCredit_valid[,.N]/1000))*"k)"))


# --- Create graph: Multi-facets
(g4 <- ggplot(datAggr_cohorts, aes(x=timeVar, y=Prop)) + theme_minimal() + 
    labs(x=bquote("Default spell cohorts (mmmccyy): stop time "*italic(t[s])), y=bquote("Resolution rate (%) of type "*~italic(kappa))) +
    theme(text=element_text(family=chosenFont),legend.position = "bottom",
          axis.text.x=element_text(angle=90), legend.text=element_text(family=chosenFont), 
          strip.background=element_rect(fill="snow2", colour="snow2"),
          strip.text=element_text(size=8, colour="gray50"), strip.text.y.right=element_text(angle=90)) + 
    # main line graph with overlaid points
    geom_line(aes(colour=Sample, linetype=Sample)) + 
    geom_point(aes(colour=Sample, shape=Sample), size=1) + 
    # facets
    facet_wrap(Facet~., labeller = label_parsed, scales = "free", nrow=length(resolDef_levels), strip.position="right") + 
    #annotations
    geom_text(data=datAnnotate, aes(x=x, y=y, hjust=hjust, vjust=vjust+1, label = Label2), family=chosenFont, size=3, parse=T) + 
    # scale options
    scale_colour_manual(name=bquote("Sample "*italic(bar(D))), values=vCol, labels=vLabel) + 
    scale_shape_discrete(name=bquote("Sample "*italic(bar(D))), labels=vLabel) + 
    scale_linetype_discrete(name=bquote("Sample "*italic(bar(D))), labels=vLabel) + 
    scale_y_continuous(breaks=pretty_breaks(), label=percent) + 
    scale_x_date(date_breaks=paste0(6, " month"), date_labels = "%b %Y"))

# - Save graph
dpi <- 180
ggsave(g4, file=paste0(genFigPath, "ResolutionRates_Def_ts_resample_", 
                       round(datCredit_smp[,.N]/1000),"k.png"), width=1200/(dpi), height=1600/(dpi), dpi=dpi, bg="white")

# --- Create graph: Single-facet
if (!is.na(resolType2_Val)){
  
  # - Cap dates for graphing purposes
  sDateMax <- max(datAggr_cohorts$timeVar) %m-% months(1)
  (g5 <- ggplot(datAggr_cohorts[Spell_Resol2==resolType2_Val & timeVar <= sDateMax,], aes(x=timeVar, y=Prop)) + theme_minimal() + 
      labs(x=bquote("Default spell cohorts (mmmccyy): stop time "*italic(t[s])), y=bquote("Resolution rate (%) of type "*italic(kappa)==1)) +
      theme(text=element_text(family=chosenFont),legend.position = "bottom",
            axis.text.x=element_text(angle=90), #legend.text=element_text(family=chosenFont), 
            strip.background=element_rect(fill="snow2", colour="snow2"),
            strip.text=element_text(size=8, colour="gray50"), strip.text.y.right=element_text(angle=90)) + 
      # main line graph with overlaid points
      geom_line(aes(colour=Sample, linetype=Sample)) + 
      geom_point(aes(colour=Sample, shape=Sample), size=1) + 
      # facets
      facet_wrap(Facet~., labeller = label_parsed, scales = "free", strip.position="right") + 
      #annotations
      geom_text(data=datAnnotate[Spell_Resol2==resolType2_Val, ], aes(x=x, y=y, hjust=hjust, vjust=vjust, label = Label2), family=chosenFont, size=3, parse=T) + 
      # scale options
      scale_colour_manual(name=bquote("Sample "*italic(bar(D))), values=vCol, labels=vLabel) + 
      scale_shape_discrete(name=bquote("Sample "*italic(bar(D))), labels=vLabel) + scale_linetype_discrete(name=bquote("Sample "*italic(bar(D))), labels=vLabel) + 
      scale_y_continuous(breaks=pretty_breaks(), label=percent) + 
      scale_x_date(date_breaks=paste0(6, " month"), date_labels = "%b %Y"))
  
  # Save graph
  dpi <- 200
  ggsave(g5, file=paste0(genFigPath, "ResolutionRates_Def_ts_resample_Single_Facet-", 
                         round(datCredit_smp[,.N]/1000),"k.png"), width=1200/dpi, height=1000/dpi, dpi=dpi, bg="white")
  
}

# - Cleanup
rm(datAnnotate, resolDef_levels, chosenFont, vCol, vLabel, datAggr_cohorts, 
   datAggr_cohorts2, maxDate, minDate, datFacetsProp)

# --- Cleanup
suppressWarnings(rm(g1, g2, g3, g4, g5, datCredit, datCredit_smp, stratifiers, clusVar, Counter, timeVar, End_Dte,
                    Start_Dte, datGraph, datCredit_train, datCredit_valid, 
                    dat_train_keys, datKeys, datKeys_sampled))