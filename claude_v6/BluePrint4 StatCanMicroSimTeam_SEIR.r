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

package_names <- c("janitor","readxl","dplyr","deSolve","tidyr","ggplot2", "ggpubr", "tidyverse", "viridis") 
load_packages <- lapply(package_names, require, character.only = TRUE)


### User input parameters
WDir <- "C:/Users/maiko/Downloads/SEIR_Model"             # working directory **** NO TRAILING /   akin to choose.dir()  ****
WDir <- "C:/Users/Cloud/Desktop/WORK/PHAC/SEIR-Claude/v6" # working directory **** NO TRAILING /   akin to choose.dir()  ****
WDir <- "c:/users/joel/google drive/github/seir/claude_v6"
#WDir <- choose.dir() # this does not generate a trailing slash or backslash
cat(WDir)  # show folder chosen
setwd(WDir)
getwd()    # show working directory

source("UtilitiesChunks.R")
source("SEIR.n.Age.Classes.R")



# BEGIN 1) 1 age group using scale.rate.to.size = FALSE or TRUE


file_name   = file.path(WDir,"BluePrint4 Conceptual model_V4_20200415V2(1agegrp).xls")

sheet_names.FALSE = list(initial.conditions="Initial conditions",parms.1d="Parameters by Age",parms.2d="Parameters by Age x Age",model.flow="Model Specs (use with FALSE)",auxiliary.vars="Intermediate calculations")
sheet_names.TRUE  = list(initial.conditions="Initial conditions",parms.1d="Parameters by Age",parms.2d="Parameters by Age x Age",model.flow="Model Specs (use with TRUE)" ,auxiliary.vars="Intermediate calculations")
sheet_names.FALSE$scale.rate.to.size = FALSE  # reminder that sheet_names$model.flow is constructed in such a way that it calls for scale.rate.to.size = FALSE
sheet_names.TRUE$scale.rate.to.size  = TRUE   # reminder that sheet_names$model.flow is constructed in such a way that it calls for scale.rate.to.size = TRUE

 results.1age.FALSE = SEIR.n.Age.Classes(file_name,sheet_names.FALSE,  scale.rate.to.size = FALSE )
 results.1age.TRUE  = SEIR.n.Age.Classes(file_name,sheet_names.TRUE ,  scale.rate.to.size = TRUE  )
 
 range(results.1age.FALSE$solution - results.1age.TRUE$solution)  # -0.0001689714  0.0001858233  --> tiny difference


# END 1)  1 age group using scale.rate.to.size = FALSE or TRUE
 
 
# BEGIN 2) 5 age groups using scale.rate.to.size = FALSE or TRUE and using beta or beta_mat
 # make sure 'room_left_softflag*( Issq / t_severe_Hosp)'  is used/activated in 'Model Specs'.  Activate the alternative below it. 
 # Same file_name and sheet_names as for 1).  These are reproduced below anyway 
 
 file_name   = file.path(WDir,"BluePrint4 Conceptual model_V4_20200415V2(5agegrp).xls")
 
 sheet_names.FALSE = list(initial.conditions="Initial conditions",parms.1d="Parameters by Age",parms.2d="Parameters by Age x Age",model.flow="Model Specs (use with FALSE)",auxiliary.vars="Intermediate calculations")
 sheet_names.TRUE  = list(initial.conditions="Initial conditions",parms.1d="Parameters by Age",parms.2d="Parameters by Age x Age",model.flow="Model Specs (use with TRUE)" ,auxiliary.vars="Intermediate calculations")
 sheet_names.FALSE$scale.rate.to.size = FALSE  # reminder that sheet_names$model.flow is constructed in such a way that it calls for scale.rate.to.size = FALSE
 sheet_names.TRUE$scale.rate.to.size  = TRUE   # reminder that sheet_names$model.flow is constructed in such a way that it calls for scale.rate.to.size = TRUE
 
 # Make sure that expression involving beta are   activated (and those involving beta_mat are inactivated)
 results.5age.betavec.FALSE = SEIR.n.Age.Classes(file_name,sheet_names.FALSE,  scale.rate.to.size = FALSE )
 results.5age.betavec.TRUE  = SEIR.n.Age.Classes(file_name,sheet_names.TRUE ,  scale.rate.to.size = TRUE  )
 
 # Make sure that expression involving beta are inactivated (and those involving beta_mat are   activated)
 results.5age.betamat.FALSE = SEIR.n.Age.Classes(file_name,sheet_names.FALSE,  scale.rate.to.size = FALSE )
 results.5age.betamat.TRUE  = SEIR.n.Age.Classes(file_name,sheet_names.TRUE ,  scale.rate.to.size = TRUE  )
 
 # Check impact of beta versus beta_mat  -->   tiny differences
 range(results.5age.betamat.FALSE$solution - results.5age.betavec.FALSE$solution) # -0.04878081  0.02353204
 range(results.5age.betamat.TRUE$solution  - results.5age.betavec.TRUE$solution)  # -0.01425387  0.02816876
 
 # Check impact of TRUE versus FALSE     -->   tiny differences
 range(results.5age.betamat.FALSE$solution - results.5age.betamat.TRUE$solution) # -0.1782229  0.3644418
 range(results.5age.betavec.FALSE$solution - results.5age.betavec.TRUE$solution) # -0.2160088  0.4413207
 
 # Compare 1 age group results with 5 age groups
 results.1age  = results.1age.FALSE          # 2 options here
 results.5ages = results.5age.betamat.TRUE   # 4 options here
 for(this.box in setdiff(colnames(results.1age$solution),c( "time","N_tot" )) )
 {
   somme = apply(results.5ages$solution[,paste0(this.box,1:5)],1,sum)
   cat("\n",this.box,"\t", range(results.1age$solution[,this.box] - somme) )   # fairly good agreement between results.2 and results.5ages
 }
 
 
 #View( results.5age.betavec.FALSE$differential.eqns.func  ) # take a look at equations used
# END 2) 5 age groups using scale.rate.to.size = FALSE or TRUE and using beta or beta_mat
 


   # outdated stuff below. Joel to fix/update this. 
   # outdated stuff below. Joel to fix/update this. 
   # outdated stuff below. Joel to fix/update this. 

 
# continue on below with listOut as before but should consider using results$solution
   
results = results.5age.betamat.TRUE  
listOut = results$listOut.to.be.decomissioned  
nagegrp = ncol(results$input.info$initial.conditions)-1


# Merge the data
big_out <- bind_rows(listOut, .id = "column_label") %>% distinct(time, .keep_all= TRUE)
xx <- yy <- df <- df2 <- NULL
for (p in 1: nagegrp){
  if (nagegrp>1){varsc<-names(big_out)[grepl(p,names(big_out))]}else{varsc<-names(big_out)}
  df <-big_out %>% 
    select(one_of(varsc))
  xx <- df %>%
    select_at(vars(starts_with("L"))) %>% 
    rowSums()
  yy <-df %>% 
    select_at(vars(starts_with("I"))) %>% 
    rowSums()
  df2 <- cbind(xx,yy)
  big_out <- cbind(big_out,df2)
  names(big_out)[c(dim(big_out)[2]-1,dim(big_out)[2])]<-c(paste0(c("L_tot","I_tot"),p))
  varsc<-names(big_out)[grepl(p,names(big_out))]
}

write.csv(big_out,paste0(WDir,paste0("SEIR_results_",paste0(nagegrp,"agegrp.csv"))), row.names = FALSE)

# Checking the results 
Cond_1<-table(big_out<0)["TRUE"]
if(is.na(Cond_1)==FALSE){print("Warning: At least one value is negative")}
if(abs(big_out$N_tot[dim(big_out)[1]]-big_out$N_tot[1])>10**(-6)){print("Warning: the total number of individuals in the system has changed over time")}

#==========================================================================
#  Graphics 
#==========================================================================

# to set a time limit (in days)
time_limit <- 365.25 # or max(big_out$time)+1

# Labels of age groups
if (nagegrp==1){lookup0 <- c("all age groups")}
if (nagegrp==5){lookup0 <- c("< 20 year-olds", "20- to 59-year-olds", "60- to 69-year-olds", "70-79-year-olds", "80+ year-olds")}

# Variables of interest for plots
if (nagegrp > 1){
  variables_of_interest <- as.vector(sapply(c("S","L_tot","I_tot","R","D"), function(x) paste0(x, 1:nagegrp)))
}else{
  variables_of_interest <- c("S","L_tot1","I_tot1","R","D")
}

big_out_graphs <- big_out %>%
  select(c("time", variables_of_interest)) %>%
  filter(time < time_limit) 


get_plot <- function(data, age_group,lookup=lookup0) {
  
  if (nagegrp>1){
    data_subset <- filter(data, meta_key %in% paste0(c("S","L_tot","I_tot","R","D"), age_group))
  }else{data_subset <- filter(data, meta_key %in% c("S","L_tot1","I_tot1","R","D"))}
  
  data_subset$meta_key <- factor(data_subset$meta_key)
  
  data_subset$meta_key <- factor(data_subset$meta_key, levels = levels(data_subset$meta_key), labels = c("Susceptible", "Latent", "Infected", "Recovered", "Dead"))
  
  # Output the plot
  ggplot(data_subset, aes(x = time, y = meta_value)) + 
    geom_line(aes(color = meta_key), size = 0.55) +
    ggtitle(paste0("SEIR model, age group ", lookup[age_group])) +
    xlab("Time (days)") +
    ylab("N (individuals)") +
    scale_color_discrete(name = "") +
    theme_minimal() +
    theme(
      plot.title = element_text(size = 10),
      axis.title.x = element_text(size = 10),
      axis.title.y = element_text(size = 10),
      legend.title = element_blank()
    )
}

# Reshape SEIR model output from wide to long format
big_out_long <- gather(big_out_graphs, key = meta_key, value = meta_value, 2:ncol(big_out_graphs), factor_key = TRUE)
# Create a time series plot for each age group and add it to a list object
plots <- lapply(1:nagegrp, FUN = get_plot, data = big_out_long)
# Output the plots in a panel
ggarrange(plotlist = plots, ncol = 2, nrow = ceiling(length(plots)/ 2), common.legend = TRUE) # two plots by row
#ggarrange(plotlist = plots, ncol = 1, nrow = length(plots), common.legend = TRUE)            # one plot by row

