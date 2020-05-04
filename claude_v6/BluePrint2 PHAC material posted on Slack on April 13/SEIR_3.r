



rm(list = ls())


library(deSolve)
library(ggplot2)
library(tidyr)
library(dplyr)
library(tidyverse)
library(matrixStats)
library(readxl)
library(janitor)
source("SEIR_util.r")


getwd()
setwd("~/Projects/covid_19/model")
G_FN <- "input_sheet_tst.xlsx"



####################################
#
read_input <- function(fn = G_FN) {
  param_df <- readxl::read_excel(G_FN, sheet = "param")
  param_df$typ <- "param"
  init_df <- readxl::read_excel(G_FN, sheet = "init")
  init_df$typ <- "state"
  mdl_df <- rbind(param_df, init_df)
  return(mdl_df)
}
####################################



run_mdl <- function(fn = G_FN, overrides = NULL){
  
  mdl_df <- read_input(fn = fn)
  
  
  for (nm in names(overrides)){
    mdl_df %>% filter(name == nm ) %>% 
      mutate()
    
    mdl_df <- 
      mdl_df %>% 
      mutate(value = replace(value, name == nm, overrides[[nm]]))
  } 
  
  
  mdl_lst <- NULL
  #mdl_lst <- as.list(mdl_df$value) %>% setNames(mdl_df$name)
  
  state_df <- mdl_df %>% filter(typ == "state") 
  state_vec <- state_df$value %>% set_names(state_df$name)
  state_vec %>% length()
  
  param_df <- mdl_df %>% filter(typ == "param") 
  param_lst <- as.list(param_df$value) %>% setNames(param_df$name)
  

  Times <- 1:param_lst$max_time_steps
  
  out <- deSolve::ode(y = state_vec, 
                      times = Times, 
                      func = seir, 
                      parms = param_lst
  ) %>% as_tibble() %>% 
    mutate_all(as.numeric) %>% 
    calc_agg()
  
  
  out <- out %>% cbind(param_df %>% select(name, value) %>% pivot_wider(names_from = name, values_from = value)) %>% as_tibble()
   
  out_delta <-
    out %>% 
    calc_all_Ds(D_to_calc = D_to_update, ignore_out = TRUE, ignore_in = FALSE) 
    
  
  out <-cbind(out, out_delta) %>% as_tibble()
    
  return(out)
}


##################################
#
calc_agg_pattern <- function(df, pattern = "^L", fun = rowSums){
  df %>% select(matches(pattern)) %>% fun
}



#################################
#
calc_agg <- function(df = out, 
                     t = "time", 
                     cols = df  %>% select(-t) %>% colnames(),
                     fun = rowSums,
                     fun_nm = "rowSums",
                     col_prefix = "AGG",
                     sep = "_" 
                    ){
  
  df$AGG_TOTAL_PEOPLE <- df %>% select(-t) %>% rowSums() 
  df[[paste(col_prefix, "L", fun_nm, sep = sep)]] <- df %>% calc_agg_pattern("^L", fun = fun)
  df[[paste(col_prefix, "I", fun_nm, sep = sep)]] <- df %>% calc_agg_pattern("^I", fun = fun)
  df[[paste(col_prefix, "S", fun_nm, sep = sep)]] <- df %>% calc_agg_pattern("^S", fun = fun)
  df[[paste(col_prefix, "R", fun_nm, sep = sep)]] <- df %>% calc_agg_pattern("^R", fun = fun)
  df[[paste(col_prefix, "D", fun_nm, sep = sep)]] <- df %>% calc_agg_pattern("^D", fun = fun)
  df[[paste(col_prefix, "H", fun_nm, sep = sep)]] <- df %>% calc_agg_pattern("^H", fun = fun)
  
  return(df)
}


#################################
##
find_peaks <- function(df = out, 
                       t = "time", 
                       cols = df  %>% select(-t) %>% colnames()
){
  
  df %>% 
    select(t, cols) %>% 
    pivot_longer(cols = -time) %>%
    group_by(name) %>% 
    slice(which.max(value)) %>% 
    arrange(time)
}




########################################
# Run the model and get the output.
out <- run_mdl()


library(xlsx)
out$AGG_TOTAL_PEOPLE

###############################33
#output the peaks of any column
out %>% find_peaks() 


#####################################3
#get the people that go through a given box
out %>% select(time, matches("^CHANGE_")) %>% 
  pivot_longer (matches("^CHANGE_")) %>% 
  group_by(name) %>%
  summarise(value = sum(value))



# Show the death rate, hospitalization and Severe symptoms
out %>% 
  select(time, Hg, D, Issq) %>% 
  pivot_longer(cols = -time) %>%
  ggplot(aes(x = time, y = value, color = name)) + geom_line(size = 1.5) + theme_bw()


out$AGG


out %>% select(matches("^AGG_"), time) %>% select(-AGG_TOTAL_PEOPLE) %>%
  pivot_longer(cols = matches("^AGG_")) %>% 
  ggplot(aes(x = time, y = value, color = name)) + 
  geom_line(size = 1.5) + 
  #scale_y_log10() + 
  #annotation_logticks(sides = "l")+
  labs(title = "Major desease compartments against time", y = "Number of People", x = "Time") + 
  theme_bw()

  ggplotly(p)

