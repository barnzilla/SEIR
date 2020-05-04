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
WDir <- "c:/users/joel/google drive/github/seir/claude_v8(4)" # working directory **** NO TRAILING /   akin to choose.dir()  ****

#WDir <- choose.dir() # this does not generate a trailing slash or backslash
cat(WDir)  # show folder chosen
setwd(WDir)
getwd()    # show working directory

source("UtilitiesChunks.R")
source("SEIR.n.Age.Classes and friends.R")


# Sweeping BluePrint4 wrt Cgg , Cgq , lambda , sigma  (but not beta)
# Sweeping BluePrint4 wrt Cgg , Cgq , lambda , sigma  (but not beta)
 
# BEGIN User inputs
# BEGIN User inputs

# BEGIN    covid.target = list(donnees= data.frame , time.ranges = data.frame )
   target_file= paste0(WDir,"/RE__Update_on_SEIR_model_sensitivity_analysis/CDN covid epi data (cleaner file from file called Comparison with observed data).xls")
   target.df =  as.data.frame.from.tbl( readxl::read_excel(target_file, sheet ="Canadian data" ) )
   target.df$cumI = target.df$`Cumulative cases`
   target.df$D    = target.df$Deaths
   
   covid.targets=list(donnees=target.df[,c("time","cumI","D")] )
   covid.targets$time.ranges = data.frame(lower.bound=20 +10*seq(5))
   covid.targets$time.ranges$upper.bound = pmin( covid.targets$time.ranges$lower.bound + 9 , 75)
   
  #covid.targets=list(donnees=target.df[,c("time","cumI")] )             # only interested in cumI
  #covid.targets$time.ranges = data.frame(lower.bound=75,upper.bound=75) # only interested in day 75
# END    covid.target = list(donnees= data.frame , time.ranges = data.frame )

 # BEGIN MODEL WORKBOOK  -->   file_name and  sheet_names
 #  User specifies file_name, sheet_names, lower.bound, upper.boud, n.repeat, racine, use.this.operation and tmin.alter.scope

 # file_name = "BluePrint4 Conceptual model_V4_20200415V2(5agegrp)_SC1.xls"           # File misnamed.  There is only 1 age group in here. Date is prior to April 20.
 # file_name = "BluePrint4 Conceptual model_V4_20200415V2(5agegrp)_SC1 - April20.xls" # File misnamed.  There is only 1 age group in here
 # file_name = "BluePrint4 Conceptual model_V4_20200415V2(5agegrp)_SC1 - April23.xls" # dummy 5 age group counterpart of "...(1agegrp)_SC1 - April23.xls" 
   file_name = "BluePrint4 Conceptual model_V4_20200415V2(1agegrp)_SC1 - April23.xls" # From PHAC"s misnamed "...(1agegrp)_SC1.xls" shared on April 23
   file_name = "BluePrint4 Conceptual model_V4_20200415V2(1agegrp)_SC1 - April23 - MaxTime222.xls"
   
  sheet_names = list(initial.conditions="Initial conditions",parms.1d="Parameters by Age",parms.2d="Parameters by Age x Age",model.flow="Model Specs (not lazy)",auxiliary.vars="Intermediate calculations")
  # END MODEL WORKBOOK  -->   file_name and  sheet_names 
  
  # BEGIN Sweep specs --> upper.bound , lower.bound, n.repeat, racine, use.this.transformation, use.this.operation, tmin.alter.scope
  if(FALSE)  # toy with SC1 (lambda= 0.3 , delta  = 0.4)
  {
     # NOTE: this is the only place where you name the parameters
     # NOTE: use the parameter name as it appears in the relevant Excel spreadsheet
     
     # Make sure to use v1 for S --> L transitions in sheet "Model Specs (not lazy)" in workbook  "... (1agegrp)_SC1 - April23.xls"
     
     lower.bound = list( lambda= 0.3 , delta  = 0.4 )  # lambda first ==> inner loop will be lambda just like PHAC
     upper.bound = lower.bound
     
     n.repeat = 1 # repetitions above and beyond the expansion of upper.bound, lower.bound
     racine = 42
     
     use.this.transformation = function(x) {x} # need to provide a function like exp here
     use.this.operation = c("overwrite","add","multiply")[1]
     tmin.alter.scope = 81
  }
  
  if(FALSE)  # Code replicating PHAC results in SEIR_results_1agegrpSC1_12 Figformated.xlsx
  {
     # NOTE: this is the only place where you name the parameters
     # NOTE: use the parameter name as it appears in the relevant Excel spreadsheet
     
     # Make sure file_name is " ... (1agegrp)_SC1 - April23.xls"
     # Make sure to use v1 for S --> L transitions in sheet "Model Specs (not lazy)" in workbook  "... (1agegrp)_SC1 - April23.xls"
    
     lower.bound = list( lambda=seq(0.3,0.7,0.2) , delta  = seq(0.4,0.7,0.1) )  # lambda first ==> inner loop will be lambda just like PHAC
     upper.bound = lower.bound
     
     n.repeat = 1 # repetitions above and beyond the expansion of upper.bound, lower.bound
     racine = 42
     
     use.this.transformation = function(x) {x} # need to provide a function like exp here
     use.this.operation = c("overwrite","add","multiply")[1]
     tmin.alter.scope = 81
  }
  
   if(FALSE)  # DH's initial systematic (non random) search
   {
      # NOTE: this is the only place where you name the parameters
      # NOTE: use the parameter name as it appears in the relevant Excel spreadsheet
      
      multipliers <- exp(seq(from = -0.15, to = 0.15, by = 0.1))
      upper.bound <- list(  
         Cgg = multipliers,
         Cgq = multipliers,
         lambda = multipliers,
        # beta = multipliers,
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
      
      center = c(Cgg=0.25,Cgq=0.0,lambda=-0.75,sigma=-0.75) # Joel solution 5-8 with Cgq set to 0  (exp(Cgq) = 1).
      center = c(Cgg=0   ,Cgq=0  ,lambda=0    ,sigma=0) 
      half.range = 0.1 + 0*center
     #half.range["beta"] = 0
      lower.bound = as.list( center - half.range )  
      upper.bound = as.list( center + half.range ) 
     
      n.repeat = 5
      n.repeat = 100 # repetitions above and beyond the expansion of upper.bound, lower.bound
      racine = 42
      
      use.this.transformation = exp # need to provide a function like exp here
      use.this.operation = c("overwrite","add","multiply")[3]
      tmin.alter.scope = 40:55
   }
   
  # END Sweep specs --> upper.bound , lower.bound, n.repeat, racine, use.this.transformation, use.this.operation, tmin.alter.scope
  
# END User inputs
# END User inputs
   
 

 
# USER HAS NOTHING TO CHANGE BELOW
 
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
 parms.to.try = use.this.transformation(parms.to.try)
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
 list.sweep.ever.from.inflows = list.sweep.ever.from.outflows = list.sweep
   df.sweep.ever.from.inflows =   df.sweep.ever.from.outflows =   df.sweep  # later maybe
 outcomes.summary.df = c()
 ever.been.here.info= list(ever.been.here.from.inflows=0         , ever.been.here.from.outflows=0         ) # numeric   --> do not bypass
 ever.been.here.info= list(ever.been.here.from.inflows="Not done", ever.been.here.from.outflows="Not done") # character -->        bypass
 set.seed(racine)
 
 for(i in 1:nrow(lower.bound.expanded)) {
   row <-  parms.to.try[i,] 
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
   summary.template = data.frame(etiquette = this.label)
   for(parameter in names(row)) 
      summary.template[[paste0(parameter, operation.label)]] <- row[[parameter]]
   summary.chunk = Outcomes.Summary(this.result$solution,covid.targets)
   summary.chunk = cbind(summary.template,summary.chunk)
  # summary.chunk$etiquette = this.label # not sure if this is useful to keep
  # for(parameter in names(row)) 
   #  summary.chunk[[paste0(parameter, operation.label)]] <- row[[parameter]]
   
   outcomes.summary.df = rbind(outcomes.summary.df,summary.chunk)
     
   # Get ever.been.here info
   if(!is.character(ever.been.here.info$ever.been.here.from.inflows))
     ever.been.here.info = ever.been.here(this.result) # this.result has been altered above but should not cause harm
   
   #Update list.sweep and df.sweep
   list.sweep[[this.label]] = this.result$solution
   list.sweep.ever.from.inflows [[this.label]] = ever.been.here.info$ever.been.here.from.inflows
   list.sweep.ever.from.outflows[[this.label]] = ever.been.here.info$ever.been.here.from.outflows
      
   # Add this.label and the parameter multpliers to the this.result$solution data frame
   this.result$solution$etiquette <- this.label
   for(parameter in names(row)) {
     this.result$solution[[paste0(parameter, operation.label)]] <- row[[parameter]]
   }
   
   df.sweep = rbind(df.sweep,this.result$solution)
 }
 rownames(outcomes.summary.df) = c()
 
 names(list.sweep)[1:2]
 names(df.sweep)
 table(df.sweep$etiquette)[1:2]
 names(outcomes.summary.df)
 
# verbose.save("list.sweep_hr.2_S1")

 
 # quick crack at plotting. Can plot much better ... but good enough for proof of concept
 plot(parms.to.try$Cgg,parms.to.try$Cgq)  
 plot(df.sweep$time , df.sweep$S,type="l") 
 plot(outcomes.summary.df$maxI.time, outcomes.summary.df$maxI)
 plot(outcomes.summary.df$maxI.time, outcomes.summary.df$cumI.75days)
 plot(outcomes.summary.df$`GOF cumI days 50-59`,outcomes.summary.df$`GOF cumI days 60-69`)
 interesting = identify(outcomes.summary.df$`GOF cumI days 50-59`,outcomes.summary.df$`GOF cumI days 60-69`)
 interesting
 # plot(subset(df.sweep,time<50)$time , subset(df.sweep,time<50)$S2,type="l") 
 
 verbose.save("list.sweep")          # creates file "This file contains an R object called list.sweep.SavedFromR". Use load() to read back 
 verbose.save("df.sweep")            # creates file "This file contains an R object called   df.sweep.SavedFromR". Use load() to read back 
 verbose.save("outcomes.summary.df") #
 
 # BEGIN Check if successful in replicating PHAC results in SEIR_results_1agegrpSC1_12 Figformated.xlsx
 if(FALSE)
 {
    # BEGIN Read 12 different sheets in PHAC's SEIR_results_1agegrpSC1_12 Figformated.xlsx workbook
    SC1_12_file= paste0(WDir,"/RE__Update_on_SEIR_model_sensitivity_analysis/SEIR_results_1agegrpSC1_12 Figformated.xlsx")
    PHAC=list()
    for(k in 1:12)
       PHAC[[k]] = as.data.frame.from.tbl( readxl::read_excel(SC1_12_file, sheet =paste0("SEIR_results_1agegrpSC",k) ) ) 
    # END Read 12 different sheets in PHAC's SEIR_results_1agegrpSC1_12 Figformated.xlsx workbook
    
    # Compare with results of "Code replicating PHAC results ..."
    common.col = intersect(names(list.sweep[[1]]),names(PHAC[[1]]))
    for(k in 1:12)
         print( range(list.sweep[[k]][,common.col] - PHAC[[k]][,common.col] ) )
    
#    [1] -5.587935e-08  6.332994e-08
#    [1] -5.587935e-08  5.215406e-08
#    [1] -7.078052e-08  5.587935e-08
#    [1] -5.215406e-08  8.940697e-08
#    [1] -7.078052e-08  5.215406e-08
#    [1] -5.215406e-08  7.450581e-08
#    [1] -390.3934  390.4064             # weird difference
#    [1] -0.7411783  0.7418581           # weird difference but very small
#    [1] -1.415610e-07  2.440065e-07
#    [1] -5.215406e-08  6.519258e-08
#    [1] -5.215406e-08  5.587935e-08
#    [1] -5.215406e-08  5.215406e-08    
    
 }
 # END Check if successful in replicating PHAC results in SEIR_results_1agegrpSC1_12 Figformated.xlsx