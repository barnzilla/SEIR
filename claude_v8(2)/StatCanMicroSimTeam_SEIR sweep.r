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
WDir <- "C:/Users/Cloud/Desktop/WORK/PHAC/SEIR-Claude/v8"
WDir <- "c:/users/joel/google drive/github/seir/claude_v8(2)"# working directory **** NO TRAILING /   akin to choose.dir()  ****

#WDir <- choose.dir() # this does not generate a trailing slash or backslash
cat(WDir)  # show folder chosen
setwd(WDir)
getwd()    # show working directory

source("UtilitiesChunks.R")
source("SEIR.n.Age.Classes and friends.R")


 
 
# Sweeping BluePrint4 wrt Cgg , Cgq , lambda , beta , sigma
# Sweeping BluePrint4 wrt Cgg , Cgq , lambda , beta , sigma
 
# BEGIN User inputs
# BEGIN User inputs

  #  User specifies file_name, sheet_names, lower.bound, upper.boud, n.repeat, racine, use.this.operation and tmin.alter.scope
   
  file_name = "BluePrint4 Conceptual model_V4_20200415V2(5agegrp)_SC1 - April20.xls" # lambda = 0.5 ... 0.5, 0.5  delta = 0.3 ... 0.3, 0.3
 #  file_name = "BluePrint4 Conceptual model_V4_20200415V2(5agegrp)_SC1.xls" # lambda = 0.5 ... 0.5, 0.5  delta = 0.3 ... 0.3, 0.3
  sheet_names = list(initial.conditions="Initial conditions",parms.1d="Parameters by Age",parms.2d="Parameters by Age x Age",model.flow="Model Specs (not lazy)",auxiliary.vars="Intermediate calculations")
   
   if(FALSE)  # DH's initial systematic (non random) search
   {
      # NOTE: this is the only place where you name the parameters
      # NOTE: use the parameter name as it appears in the relevant Excel spreadsheet
      
      multipliers <- exp(seq(from = -0.15, to = 0.15, by = 0.1))
      upper.bound <- list(  
         Cgg = multipliers,
         Cgq = multipliers,
         lambda = multipliers,
         beta = multipliers,
         sigma = multipliers
      )
      lower.bound = upper.bound 
      n.repeat = 1 # repetitions above and beyond the explosion of upper.bound, lower.bound
      racine = 42
      
      use.this.operation = c("overwrite","add","multiply")[3]
      tmin.alter.scope = 40:55
   }
      

   if(TRUE)  # random search
   {
      # NOTE: this is the only place where you name the parameters
      # NOTE: use the parameter name as it appears in the relevant Excel spreadsheet
      
      center = c(Cgg=0.25,Cgq=0.0,lambda=-0.75,beta=-0.25,sigma=-0.75) # Joel solution 5-8 with Cgq set to 0  (exp(Cgq) = 1).
      half.range = 0.4 + 0*center
      half.range["beta"] = 0
      lower.bound = as.list( exp(center - half.range ) ) 
      upper.bound = as.list( exp(center + half.range ) )
     
      n.repeat = 300 # repetitions above and beyond the expansion of upper.bound, lower.bound
      racine = 42
      
      use.this.operation = c("overwrite","add","multiply")[3]
      tmin.alter.scope = 40:55
   }
   

# END User inputs
# END User inputs
   
 
  
 
 # USER HAS NOTHING TO CHANGE BELOW
set.seed(racine)
 # Compute all possible candidate by multiplier combinations
 #multiplier_combos <- expand.grid(candidates, KEEP.OUT.ATTRS = FALSE)
 lower.bound.expanded = expand.grid(lower.bound, KEEP.OUT.ATTRS = FALSE)
 upper.bound.expanded = expand.grid(upper.bound, KEEP.OUT.ATTRS = FALSE)
 if(any(dim(lower.bound.expanded) != dim(upper.bound.expanded) ))
    stop('dimension mismatch')
 if(any(colnames(lower.bound.expanded) != colnames(upper.bound.expanded) ))
    stop('column name mismatch')
 lower.bound.expanded = lower.bound.expanded[rep(seq(nrow(lower.bound.expanded)) , n.repeat ),]
 upper.bound.expanded = upper.bound.expanded[rep(seq(nrow(upper.bound.expanded)) , n.repeat ),]
 
 parms.to.try = (upper.bound.expanded - lower.bound.expanded) * runif(prod(dim(upper.bound.expanded)))
 parms.to.try = lower.bound.expanded + parms.to.try
 dim(parms.to.try) # check size of sweep you are about to do  (number of scenarios , number of parameters)
 
 operation_list = list(overwrite = function(current,new)         {0*current+new        } ,
                       add       = function(current,increment  ) {  current+increment  } ,
                       multiply  = function(current,mult_factor) {  current*mult_factor} )
 operation_func = operation_list[[use.this.operation]]
 operation.label = c(overwrite="",add=".add",multiply=".multiplier")[use.this.operation]
 
 # baseline/template
 # results.baseline = SEIR.n.Age.Classes(file_name,sheet_names)[c("input.info","input.info.verbatim")] # all you really need
 results.baseline = SEIR.n.Age.Classes(file_name,sheet_names)
 
 
 sheet_names_for_sweep = results.baseline$input.info           # Can use either of those two lines ... in theory (not tested)
 sheet_names_for_sweep = results.baseline$input.info.verbatim  # Can use either of those two lines ... in theory (not tested)
 
 baseline.parms.1d = results.baseline$input.info$parms.1d  # data frame of 1d parameters
 baseline.parms.2d = results.baseline$input.info$parms.2d  # data frame of 2d parameters
 
 list.sweep = list() #        store results in list  ... 
 df.sweep = c()      # ... or store results in data.frame
 outcomes.summary.df = c()
 
 for(i in 1:nrow(lower.bound.expanded)) {
   row <- parms.to.try[i,]
  #this.label <- paste0(names(row),       ".multiplier= ", row, collapse = " , ")
   this.label <- paste0(names(row), operation.label, "= ", row, collapse = " , ")
   
   
   cat("\n Doing",i,"of",nrow(parms.to.try),"simulations :",this.label)
   
   parms.1d = baseline.parms.1d  # data frame of 1d parameters to be altered
   parms.2d = baseline.parms.2d  # data frame of 2d parameters to be altered
   
   # Modify the parameter values at the specified tmin.alter.scope
   for(parameter in names(row)) {
     if(parameter %in% names(parms.1d)) {
       parms.1d[parms.1d$tmin %in% tmin.alter.scope, parameter] = operation_func(subset(parms.1d,tmin %in% tmin.alter.scope)[[parameter]] , row[[parameter]] )
     } else if(parameter %in% names(parms.2d)) {
       parms.2d[parms.2d$tmin %in% tmin.alter.scope, parameter] = operation_func(subset(parms.2d,tmin %in% tmin.alter.scope)[[parameter]] , row[[parameter]] )
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
     univariate.chunk[[paste0(parameter, operation.label)]] <- row[[parameter]]
   
   outcomes.summary.df = rbind(outcomes.summary.df,univariate.chunk)
     
   list.sweep[[this.label]] = this.result$solution
   
   # Add this.label and the parameter multpliers to the this.result$solution data frame
   this.result$solution$etiquette <- this.label
   for(parameter in names(row)) {
     this.result$solution[[paste0(parameter, operation.label)]] <- row[[parameter]]
   }
   
   df.sweep = rbind(df.sweep,this.result$solution)
 }
 
 
 names(list.sweep)[1:2]
 names(df.sweep)
 table(df.sweep$etiquette)[1:2]
 


 
 # quick crack at plotting. Can plot much better ... but good enough for proof of concept
 plot(parms.to.try$Cgg,parms.to.try$Cgq)  
 plot(df.sweep$time , df.sweep$S,type="l") 
 plot(outcomes.summary.df$maxI, outcomes.summary.df$maxI.time)
 # plot(subset(df.sweep,time<50)$time , subset(df.sweep,time<50)$S2,type="l") 
 
 
 # Create a scenario grouping variable (factor)
 lookup <- tibble(long = unique(outcomes.summary.df$etiquette), short = 1:length(unique(outcomes.summary.df$etiquette)))
 outcomes.summary.df$scenario <- factor(sapply(outcomes.summary.df$etiquette, function(x) lookup$short[x == lookup$long])) 
 
 library("plotly")
 
 # Print scatter plot
 ggplotly(ggplot(outcomes.summary.df, aes(x = maxI, y = maxI.time)) +
    geom_point(aes(color = scenario), size = 0.25) +
    ggtitle(paste0("Time by maximum incidence, based on 125 scenarios")) +
    xlab("Maximum incidence (individuals)") +
    ylab("Time (days)") +
    theme_minimal() +
    theme(
       plot.title = element_text(size = 12),
       axis.title.x = element_text(size = 12),
       axis.title.y = element_text(size = 12),
       legend.text = element_text(size = 12),
       legend.title = element_blank()
    ), width = 900, height = 614)
 
 # Print data table
 datatable(outcomes.summary.df %>% select(-c(Simulation, etiquette)) %>% mutate(maxI = as.integer(maxI), Cgg.multiplier = round(Cgg.multiplier, 3), Cgq.multiplier = round(Cgq.multiplier, 3), lambda.multiplier = round(lambda.multiplier, 3), beta.multiplier = round(beta.multiplier, 3), sigma.multiplier = round(sigma.multiplier, 3)) %>% arrange(desc(maxI.time, maxI)), 
           extensions = c("Buttons", "Scroller"), 
           options = list(
              pageLength = 50, 
              dom = "Bfrtip", 
              buttons = c("colvis", "copy", "csv", "excel", "pdf"), 
              deferRender = TRUE, 
              searchDelay = 500,
              initComplete = JS(
                 "function(settings, json) {",
                 "$(this.api().table().header()).css({'background-color': '#fff', 'color': '#111'});",
                 "}"
              ),
              columnDefs = list(
                 list(visible = FALSE, targets = c())
              )
           ), 
           rownames = FALSE,
           escape = FALSE,
           editable = TRUE
 )
 