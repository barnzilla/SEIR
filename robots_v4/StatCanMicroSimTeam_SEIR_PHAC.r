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
install_packages <- lapply(package_names, FUN = function(x) if(! require(x, character.only = TRUE)) install.packages(x))
load_packages <- lapply(package_names, require, character.only = TRUE)

### User input parameters
WDir <- "C:/Users/maiko/Downloads/SEIR_Model"             # working directory **** NO TRAILING /   akin to choose.dir()  ****
WDir <- "C:/Users/Cloud/Desktop/WORK/PHAC/SEIR-Claude/v4" # working directory **** NO TRAILING /   akin to choose.dir()  ****
WDir <- choose.dir() # this does not generate a trailing slash or backslash
cat(WDir)  # show folder chosen
setwd(WDir)

source("UtilitiesChunks.R")
source("SEIR.n.Age.Classes.R")



file_name   = file.path(WDir,"covid19_input_sheet_v4(1agegrp - old names).xls") # not tested
file_name   = file.path(WDir,"covid19_input_sheet_v4(5agegrp - new names) - lean.xls")
file_name   = file.path(WDir,"covid19_input_sheet_v4(5agegrp - old names) - lean.xls")
file_name   = file.path(WDir,"covid19_input_sheet_v4(5agegrp - new names).xls")
file_name   = file.path(WDir,"covid19_input_sheet_v4(5agegrp - old names).xls")

sheet_names_v1 = list(initial.conditions="Initial conditions",parms.1d="Parameters by Age",parms.2d="Parameters by Age x Age",model.flow="Model Specs v1",auxiliary.vars="Intermediate calculations")
sheet_names_v2 = list(initial.conditions="Initial conditions",parms.1d="Parameters by Age",parms.2d="Parameters by Age x Age",model.flow="Model Specs v2",auxiliary.vars="Intermediate calculations")

# note that 3rd argument (i.e. seq(0,1499,1) here) is currently inoperant
results.sheet.v1.bad  = SEIR.n.Age.Classes(file_name,sheet_names_v1,seq(0,1499,1) ) # scale.rate.to.size  FALSE by default now
results.sheet.v1.good = SEIR.n.Age.Classes(file_name,sheet_names_v1,seq(0,1499,1), scale.rate.to.size=TRUE )
results.sheet.v2.good = SEIR.n.Age.Classes(file_name,sheet_names_v2,seq(0,1499,1) ) # scale.rate.to.size  FALSE by default now
results.sheet.v2.bad  = SEIR.n.Age.Classes(file_name,sheet_names_v2,seq(0,1499,1), scale.rate.to.size=TRUE )

results = results.sheet.v2.good
#View( results$differential.eqns.func  ) # take a look at equations used
#range(diff(results$solution$time)) # check if times are in increment of 1
#plot   (results$solution$time,results$solution$S4,type="l") # plotting without ggplot2 just like a dynosaur.
#plot   (results$solution$time[1:50],results$solution$S4[1:50],type="l",xlab="time") # plotting without ggplot2 just like a dynosaur.
#matplot(results$solution$time[1:50],results$solution[1:50,paste0("S",1:5)],type="l",xlab="time")


if(FALSE) # example of a parameter sweep
{
  sheet_names_for_sweep = sheet_names_v2        # provide sheet names used in results (results.sheet.v2.good)
  baseline.parms.1d = results$input.info$parms.1d  # data frame of 1d parameters
  list.sweep = list() #        store results in list  ... 
  df.sweep = c()      # ... or store results in data.frame
  for(log.fudge.factor in seq(-0.5,0.5,0.25))
  {
    this.label = paste("sigma multiplied by",exp(log.fudge.factor))
    parms.1d = baseline.parms.1d  # data frame of 1d parameters
    parms.1d$sigma = parms.1d$sigma * exp(log.fudge.factor) # alter sigma as per parameter sweep
    sheet_names_for_sweep$parms.1d = parms.1d
    this.result = SEIR.n.Age.Classes(file_name,sheet_names_for_sweep,seq(0,1499,1) ) 
    list.sweep[[this.label]] = this.result$solution
    this.result$solution$this.label = this.label
    df.sweep = rbind(df.sweep,this.result$solution)
    
  }
  range(list.sweep$"sigma multiplied by 1" - results$solution) # same      as expected
  range(list.sweep[[ 3 ]]                  - results$solution) # same      as expected
  range(list.sweep[[ 2 ]]                  - results$solution) # different as expected
  
  extract.df = subset(df.sweep,this.label == "sigma multiplied by 1")
  extract.df$this.label = c()
  range(extract.df - results$solution)
  
  plot(df.sweep$time,df.sweep$S4,type="l") # quick crack at plotting. Can do much better ...
}






# continue on below with listOut as before but should consider using results$solution
listOut = results$listOut.to.be.decomissioned  
listOut = results$solution 
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

# Compute incidence
for(i in 1:nagegrp) {
  v <- c()
  I_tot <- big_out %>% select(all_of(paste0("I_tot", i)))
  I_tot <- I_tot[,1]
  L_tot <- big_out %>% select(all_of(paste0("L_tot", i)))
  L_tot <- L_tot[,1]
  for(j in 1:nrow(big_out)) {
    if(j == 1) {
      v[j] <- I_tot[1]
    } else {
      v[j] <- L_tot[j - 1] * 0.27027 
    }
  }
  big_out[[paste0("IncI", i)]] <- assign(paste0("IncI", i), v)
} 


View(big_out)

sum(big_out$I_smis1, big_out$I_ssis1)
sum(big_out$I_smis2, big_out$I_ssis2)

# Compute Isolated
for(i in 1:nagegrp) {
  I_smis <- unname(unlist(big_out %>% select(all_of(paste0("I_smis", i)))))
  I_ssis <- unname(unlist(big_out %>% select(all_of(paste0("I_ssis", i)))))
  v <- c()
  for(j in 1:nrow(big_out)) {
    v[j] <- I_smis[j] + I_ssis[j]
  }
  big_out[[paste0("Isolat", i)]] <- assign(paste0("Isolat", i), v)
} 

