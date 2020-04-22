rm(list = ls())
#################################################################################
#
# Modified PHAC S(E)IR model script with n age classes to estimate the number of
# COVID-19 cases (infected, hospitalized, etc).
#
#==================================================================================
#
#
# Author of the modifications: Maikol Diasparra, Claude Nadeau and Joel Barnes
#
# Date: 25 MAR 2020
# Last update: 8 APR 2020 
# Version: 1.0.1
#
# Modification objective: to extend the model to get the estimates by age-groups
# Original script provided by PHAC Analysts: Antoinette Ludwig & Erin Rees.
#
# But the script can be used for n age classes (e.g.age groups: 1=under 20, 2=20-59, 3=60-69, 4=70-79, 5=80+)
# Also, the differential equations are no longer hardcoded but rather built from the excel representation of the flow chart.
#
#     
# 
#################################################################################


#==========================================================================
#  packages
#==========================================================================

package_names <- c("janitor","readxl","dplyr","deSolve","tidyr","ggplot2", "ggpubr", "tidyverse") # , "viridis") 
load_packages <- lapply(package_names, require, character.only = TRUE)


### User input parameters
WDir <- "C:/Users/maiko/Downloads/SEIR_Model"             # working directory **** NO TRAILING /   akin to choose.dir()  ****
WDir <- "C:/Users/Cloud/Desktop/WORK/PHAC/SEIR-Claude/v7" # working directory **** NO TRAILING /   akin to choose.dir()  ****
WDir <- "C:/Users/Cloud/Desktop/WORK/PHAC/SEIR-PHAC snapsnots/Untitled_Message(From Antoinette April 20)" # working directory **** NO TRAILING /   akin to choose.dir()  ****
#WDir <- choose.dir() # this does not generate a trailing slash or backslash
cat(WDir)  # show folder chosen
setwd(WDir)
getwd()    # show working directory

source("UtilitiesChunks.R")
source("SEIR.n.Age.Classes and friends.R")



 
# BEGIN 3) parameter sweep to replicate 24 scenarios

# Take any of those 3 files. It does not matter for the sweep/search
 file_name = "BluePrint4 Conceptual model_V4_20200415V2(5agegrp)_SC1.xls" # lambda = 0.5 ... 0.5, 0.5  delta = 0.3 ... 0.3, 0.3
 file_name = "BluePrint4 Conceptual model_V4_20200415V2(5agegrp)_SC4.xls" # lambda = 0.5 ... 0.5, 0.8  delta = 0.3 ... 0.3, 0.3
 file_name = "BluePrint4 Conceptual model_V4_20200415V2(5agegrp)_SC5.xls" # lambda = 0.5 ... 0.5, 0.5  delta = 0.3 ... 0.3, 0.4

 
 # Take lazy or not lazy sheet.  It does not matter
 sheet_names.not.lazy = list(initial.conditions="Initial conditions",parms.1d="Parameters by Age",parms.2d="Parameters by Age x Age",model.flow="Model Specs (not lazy)",auxiliary.vars="Intermediate calculations")
 sheet_names.lazy     = list(initial.conditions="Initial conditions",parms.1d="Parameters by Age",parms.2d="Parameters by Age x Age",model.flow="Model Specs (lazy)" ,auxiliary.vars="Intermediate calculations")
 
 
 # baseline/template
 results.baseline = SEIR.n.Age.Classes(file_name,sheet_names.not.lazy)[c("input.info","input.info.verbatim")] # all you really need
 results.baseline = SEIR.n.Age.Classes(file_name,sheet_names.not.lazy)
 

# file_name   = results.baseline$input.info.verbatim$file.name
 sheet_names_for_sweep = results.baseline$input.info           # Can use either of those two lines ... in theory (not tested)
 sheet_names_for_sweep = results.baseline$input.info.verbatim  # Can use either of those two lines ... in theory (not tested)
 
 baseline.parms.1d = results.baseline$input.info$parms.1d  # data frame of 1d parameters
 baseline.parms.2d = results.baseline$input.info$parms.1d  # data frame of 1d parameters
 
 delta.candidates  = seq(0.3,0.8,0.1) # 0.3 to 0.8 by 0.1 steps
 lambda.candidates = seq(0.5,0.8,0.1) # 0.5 to 0.8 by 0.1 steps  
 
 lower.bound.last.time.interval = max(baseline.parms.1d$tmin)
 list.sweep = list() #        store results in list  ... 
 df.sweep = c()      # ... or store results in data.frame
 for(this.delta in delta.candidates)
   for(this.lambda in lambda.candidates)
   {
      this.label = paste("delta=",this.lambda,"lambda=",this.lambda)
      cat("\n Doing",this.label)
      
      parms.1d = baseline.parms.1d  # data frame of 1d parameters
      subset(parms.1d,tmin == lower.bound.last.time.interval)[c("delta","lambda")] = c(this.delta,this.lambda)
      sheet_names_for_sweep$parms.1d = parms.1d # altered data.frame goes in sheet_names_for_sweep
      
      this.result = SEIR.n.Age.Classes(file_name,sheet_names_for_sweep) 
      
      list.sweep[[this.label]] = this.result$solution
      this.result$solution$this.label = this.label
      df.sweep = rbind(df.sweep,this.result$solution)
   }
 range(list.sweep$"t_latency multiplied by 1" - results$solution) # same      as expected
 range(list.sweep[[ 3 ]]                      - results$solution) # same      as expected
 range(list.sweep[[ 2 ]]                      - results$solution) # different as expected
 
 extract.df = subset(df.sweep,this.label == "t_latency multiplied by 1")
 extract.df$this.label = c()
 range(extract.df - results$solution)  # same      as expected
 
 # quick crack at plotting. Can plot much better ... but good enough for proof of concept
 plot(df.sweep$time , df.sweep$S2,type="l") 
 # plot(subset(df.sweep,time<50)$time , subset(df.sweep,time<50)$S2,type="l") 

 
# END 3) parameter sweep to replicate 24 scenarios
 
 
 

 
 
 
 
 
 
 # BEGIN 4) parameter sweep to match with old model ... let us consider old SC1 (lambda = 0.5, delta=0.3) 
 
 # Take any of those 3 files. It does not matter for the sweep/search
 file_name = "BluePrint4 Conceptual model_V4_20200415V2(5agegrp)_SC1.xls" # lambda = 0.5 ... 0.5, 0.5  delta = 0.3 ... 0.3, 0.3
# file_name = "BluePrint4 Conceptual model_V4_20200415V2(5agegrp)_SC4.xls" # lambda = 0.5 ... 0.5, 0.8  delta = 0.3 ... 0.3, 0.3
# file_name = "BluePrint4 Conceptual model_V4_20200415V2(5agegrp)_SC5.xls" # lambda = 0.5 ... 0.5, 0.5  delta = 0.3 ... 0.3, 0.4
 
 
 # Take lazy or not lazy sheet.  It does not matter
 sheet_names.not.lazy = list(initial.conditions="Initial conditions",parms.1d="Parameters by Age",parms.2d="Parameters by Age x Age",model.flow="Model Specs (not lazy)",auxiliary.vars="Intermediate calculations")
 sheet_names.lazy     = list(initial.conditions="Initial conditions",parms.1d="Parameters by Age",parms.2d="Parameters by Age x Age",model.flow="Model Specs (lazy)" ,auxiliary.vars="Intermediate calculations")
 
 
 # baseline/template
 results.baseline = SEIR.n.Age.Classes(file_name,sheet_names.not.lazy)[c("input.info","input.info.verbatim")] # all you really need
 results.baseline = SEIR.n.Age.Classes(file_name,sheet_names.not.lazy)
 
 
 # file_name   = results.baseline$input.info.verbatim$file.name
 sheet_names_for_sweep = results.baseline$input.info           # Can use either of those two lines ... in theory (not tested)
 sheet_names_for_sweep = results.baseline$input.info.verbatim  # Can use either of those two lines ... in theory (not tested)
 
 baseline.parms.1d = results.baseline$input.info$parms.1d  # data frame of 1d parameters
 baseline.parms.2d = results.baseline$input.info$parms.2d  # data frame of 2d parameters

 

 
 # Define multipliers
 multipliers <- exp(seq(from = -0.5, to = 0.5, by = 0.5))
 
 # Apply multipliers to candidates
 # NOTE: this is the only place where you name the parameters
 # NOTE: use the parameter name as it appears in the relevant Excel spreadsheet
# candidates <- tibble(  # will not work if have different number of candidates for Cgg and Cgq (for instance)
  candidates <- list(  
   Cgg = multipliers,
   Cgq = multipliers,
   lambda = multipliers,
   beta = multipliers,
   sigma = multipliers
 )
  
  candidates <- list(  
    Cgg = multipliers,
    Cgq = multipliers,
    lambda = 1,
    beta = 1,
    sigma = 1
  )  
 
 # Compute all possible candidate by multiplier combinations
 multiplier_combos <- expand.grid(candidates, KEEP.OUT.ATTRS = FALSE)
 dim(multiplier_combos) # check size of sweep you are about to do  (number of scenarios , number of parameters)
 
 tmin.alter.scope = 40:55
 list.sweep = list() #        store results in list  ... 
 df.sweep = c()      # ... or store results in data.frame
 outcomes.summary.df = c()
 
 for(i in 1:nrow(multiplier_combos)) {
   row <- multiplier_combos[i,]
   this.label <- paste0(names(row), ".multiplier= ", row, collapse = " , ")
   cat("\n Doing",this.label)
   
   parms.1d = baseline.parms.1d  # data frame of 1d parameters to be altered
   parms.2d = baseline.parms.2d  # data frame of 2d parameters to be altered
   
   # Modify the parameter values at the specified tmin.alter.scope
   for(parameter in names(row)) {
     if(parameter %in% names(parms.1d)) {
       parms.1d[parms.1d$tmin %in% tmin.alter.scope, parameter] = subset(parms.1d,tmin %in% tmin.alter.scope)[[parameter]] * row[[parameter]]
     } else if(parameter %in% names(parms.2d)) {
       parms.2d[parms.2d$tmin %in% tmin.alter.scope, parameter] =subset(parms.2d,tmin %in% tmin.alter.scope)[[parameter]] * row[[parameter]]
     } else {
       stop(paste(parameter,"was not found in any parameter sheet"))# Parameter was not found in any parameter sheet
     }
   }
   
   sheet_names_for_sweep$parms.1d = parms.1d # altered data.frame goes in sheet_names_for_sweep
   sheet_names_for_sweep$parms.2d = parms.2d # altered data.frame goes in sheet_names_for_sweep
   
   this.result = SEIR.n.Age.Classes(file_name,sheet_names_for_sweep) 
   
   # Add on other time series like cumulative incidence
   this.result$solution = Add.Other.Outcomes (this.result$solution,parms.1d,"third argument currently inoperant")
   
   # Add on univariate stuff like maxI or maxI.time to outcomes.summary.df
   univariate.chunk = Outcomes.Summary(this.result$solution)
   univariate.chunk$etiquette = this.label # not sure if this is useful to keep
   for(parameter in names(row)) 
     univariate.chunk[[paste0(parameter, ".multiplier")]] <- row[[parameter]]
   
   outcomes.summary.df = rbind(outcomes.summary.df,univariate.chunk)
     
   list.sweep[[this.label]] = this.result$solution
   
   # Add this.label and the parameter multpliers to the this.result$solution data frame
   this.result$solution$etiquette <- this.label
   for(parameter in names(row)) {
     this.result$solution[[paste0(parameter, ".multiplier")]] <- row[[parameter]]
   }
   
   df.sweep = rbind(df.sweep,this.result$solution)
 }
 
 
 names(list.sweep)
 table(df.sweep$etiquette)
 table(df.sweep$Cgg.multiplier,df.sweep$Cgq.multiplier)


 
 # quick crack at plotting. Can plot much better ... but good enough for proof of concept
 plot(df.sweep$time , df.sweep$S,type="l") 
 plot(outcomes.summary.df$maxI, outcomes.summary.df$maxI.time)
 # plot(subset(df.sweep,time<50)$time , subset(df.sweep,time<50)$S2,type="l") 
 
 
 # END 4) parameter sweep to match with old model ... let us consider old SC1 (lambda = 0.5, delta=0.3) 
 
 
 
 