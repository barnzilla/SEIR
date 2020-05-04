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
#WDir <- choose.dir() # this does not generate a trailing slash or backslash
cat(WDir)  # show folder chosen
setwd(WDir)
getwd()    # show working directory

source("UtilitiesChunks.R")
source("SEIR.n.Age.Classes.R")



# BEGIN 1) 1 age group using lazy or not


file_name   = file.path(WDir,"BluePrint4 Conceptual model_V4_20200415V2(1agegrp).xls")

sheet_names.not.lazy = list(initial.conditions="Initial conditions",parms.1d="Parameters by Age",parms.2d="Parameters by Age x Age",model.flow="Model Specs (not lazy)",auxiliary.vars="Intermediate calculations")
sheet_names.lazy     = list(initial.conditions="Initial conditions",parms.1d="Parameters by Age",parms.2d="Parameters by Age x Age",model.flow="Model Specs (lazy)"    ,auxiliary.vars="Intermediate calculations")

results.1age.not.lazy = SEIR.n.Age.Classes(file_name,sheet_names.not.lazy)
results.1age.lazy     = SEIR.n.Age.Classes(file_name,sheet_names.lazy )
 
range(results.1age.not.lazy$solution - results.1age.lazy$solution)  # -0.0001689714  0.0001858233  --> tiny difference


# END 1)  1 age group using lazy = FALSE or TRUE
 
 
# BEGIN 2) 5 age groups using lazy or not and using beta or beta_mat

 # Same file_name and sheet_names as for 1).  These are reproduced below anyway 
 
 file_name   = file.path(WDir,"BluePrint4 Conceptual model_V4_20200415V2(5agegrp).xls")
 
 sheet_names.not.lazy = list(initial.conditions="Initial conditions",parms.1d="Parameters by Age",parms.2d="Parameters by Age x Age",model.flow="Model Specs (not lazy)",auxiliary.vars="Intermediate calculations")
 sheet_names.lazy     = list(initial.conditions="Initial conditions",parms.1d="Parameters by Age",parms.2d="Parameters by Age x Age",model.flow="Model Specs (lazy)" ,auxiliary.vars="Intermediate calculations")
 
 # Make sure that expression involving beta are   activated (and those involving beta_mat are inactivated)
 results.5age.betavec.not.lazy = SEIR.n.Age.Classes(file_name,sheet_names.not.lazy)
 results.5age.betavec.lazy     = SEIR.n.Age.Classes(file_name,sheet_names.lazy )
 
 # Make sure that expression involving beta are inactivated (and those involving beta_mat are   activated)
 results.5age.betamat.not.lazy = SEIR.n.Age.Classes(file_name,sheet_names.not.lazy )
 results.5age.betamat.lazy     = SEIR.n.Age.Classes(file_name,sheet_names.lazy  )
 
 # Check impact of beta versus beta_mat  -->   tiny differences
 range(results.5age.betamat.not.lazy$solution - results.5age.betavec.not.lazy$solution) # -0.04878081  0.02353204
 range(results.5age.betamat.lazy$solution     - results.5age.betavec.lazy$solution    ) # -0.01425387  0.02816876
 
 # Check impact of TRUE versus FALSE     -->   tiny differences
 range(results.5age.betamat.not.lazy$solution - results.5age.betamat.lazy$solution) # -0.1782229  0.3644418
 range(results.5age.betavec.not.lazy$solution - results.5age.betavec.lazy$solution) # -0.2160088  0.4413207
 
 # Compare 1 age group results with 5 age groups
 results.1age  = results.1age.not.lazy       # 2 options here
 results.5ages = results.5age.betamat.lazy   # 4 options here
 for(this.box in setdiff(colnames(results.1age$solution),c( "time","N_tot" )) )
 {
   somme = apply(results.5ages$solution[,paste0(this.box,1:5)],1,sum)
   cat("\n",this.box,"\t", range(results.1age$solution[,this.box] - somme) )   # fairly good agreement between results.1age and results.5ages
 }
 
 #View( results.5age.betavec.not.lazy$differential.eqns.func  ) # take a look at equations used
 
# END 2) 5 age groups using lazy or not and using beta or beta_mat
 
 
 
 
# BEGIN 3) parameter sweep with model by 5 age groups
 
 results = results.5age.betavec.not.lazy
 file_name   = results$input.info.verbatim$file.name
 sheet_names = results$input.info           # Can use either of those two lines ... in theory (not tested)
 sheet_names = results$input.info.verbatim  # Can use either of those two lines ... in theory (not tested)

 sheet_names_for_sweep = sheet_names       
 
 baseline.parms.1d = results$input.info$parms.1d  # data frame of 1d parameters
 list.sweep = list() #        store results in list  ... 
 df.sweep = c()      # ... or store results in data.frame
 for(log.fudge.factor in seq(-0.5,0.5,0.25))
 {
    cat("\n Doing log.fudge.factor=",log.fudge.factor)
    this.label = paste("t_latency multiplied by",exp(log.fudge.factor))
    parms.1d = baseline.parms.1d  # data frame of 1d parameters
    parms.1d$t_latency = parms.1d$t_latency * exp(log.fudge.factor) # alter t_latency as per parameter sweep
    sheet_names_for_sweep$parms.1d = parms.1d
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

 
# END 3) parameter sweep with model by 5 age groups
 
 
 
 
# BEGIN 4)  Get ever.been.here using inflows and outflows approaches
 
 results = results.5age.betavec.not.lazy
 range(diff(results$solution$time)) # check that times are in increments of 1 (feature implicitly used below)
 differential.eqns.func=results$differential.eqns.func
 
 these.are.not.parms = c("tmin",     "tmax" ,  "agegrp", "isim" ) # actaully want to keep "ragegrp",  "cagegrp"
 inflows  = c()
 outflows = c()
 for(tt in results$solution$time) # [1:4]
 {
   #BEGIN get list.parm.1d.2d  (all parameters stored in a list  )
   df.parm.1d = results$input.info$parms.1d
   df.parm.2d = results$input.info$parms.2d
   df.parm.1d = subset(df.parm.1d ,overlap.length(tt,tt+1,df.parm.1d$tmin,df.parm.1d$tmax) > 0 )
   df.parm.2d = subset(df.parm.2d ,overlap.length(tt,tt+1,df.parm.2d$tmin,df.parm.2d$tmax) > 0 )
   list.parm.1d.2d = smooch.parms.df.into.list(df.parm.1d,df.parm.2d,these.are.not.parms)
   #END get list.parm.1d.2d  (all parameters stored in a list  )
  
   #BEGIN list.compartments  ... will contain vectors D, R, S, ... at time tt
   list.compartments = list() # will contain vectors D, R, S, ....
   n_agegrp = ncol(results$input.info$initial.conditions) - 1 # number of age groups
   mat1row.compartments = data.matrix(subset(results$solution,time == tt))
   for(k in results$input.info$initial.conditions$NAME)
     list.compartments[[k]] = mat1row.compartments[1,paste0(k,1:n_agegrp)]
   #END list.compartments  ... will contain vectors D, R, S, ... at time tt
  # browser()
   inflows  = rbind(inflows  , differential.eqns.func(list.compartments,list.parm.1d.2d,flow.multiplier =list(inflow = 1,outflow=0))[[1]] )
   outflows = rbind(outflows , differential.eqns.func(list.compartments,list.parm.1d.2d,flow.multiplier =list(inflow = 0,outflow=1))[[1]] )
 }
 colnames(inflows ) = gsub("derivative.","",colnames(inflows ))
 colnames(outflows) = gsub("derivative.","",colnames(outflows))
 solution.lean = results$solution[,setdiff(names(results$solution),c("time","N_tot")) ]
 

 ever.been.here.from.inflows = rbind(solution.lean[1,],inflows[-nrow(inflows),])
 ever.been.here.from.inflows = as.data.frame( apply(ever.been.here.from.inflows,2,cumsum)  )
 ever.been.here.from.inflows$time = results$solution$time
 
 ever.been.here.from.outflows = rbind(0*solution.lean[1,],outflows[-nrow(outflows),])
 ever.been.here.from.outflows = as.data.frame( apply(ever.been.here.from.outflows,2,cumsum)  )
 ever.been.here.from.outflows = ever.been.here.from.outflows[,colnames(solution.lean)] + solution.lean
 ever.been.here.from.outflows$time = results$solution$time
 
  # Warning.  Dynosaur plots crossing ahead
 temps = ever.been.here.from.outflows$time
 box = "S2"
 box = "Lg2"
 matplot(temps,cbind(ever.been.here.from.inflows[,box],ever.been.here.from.outflows[,box] ),type="l",ylab=box)
 matplot(temps,cbind(ever.been.here.from.inflows[,box],ever.been.here.from.outflows[,box],solution.lean[,box]),type="l",ylab=box)


 
# END 4)  Get ever.been.here using inflows and outflows approaches
 
# BEGIN 5)  Get ever.been.here using absorbing states (naive and mirror approaches)
 
# END 5)  Get ever.been.here using absorbing states (naive and mirror approaches)
 
