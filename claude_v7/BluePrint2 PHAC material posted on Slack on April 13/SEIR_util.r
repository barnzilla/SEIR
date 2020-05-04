



library(deSolve)
library(ggplot2)
library(tidyr)
library(dplyr)
library(tidyverse)
library(matrixStats)
library(readxl)
library(janitor)


plist <- function(mdl_lst){
  pp("*** Dumping List ***")
  for (nm in names(mdl_lst)){
    pp(nm, "=", mdl_lst[[nm]])
  }
}

pp <- function(...){
  paste0(...) %>% print()
}


################################3
#
run_Q_equation <- function(func_to_run = Q_Lg_Iag, 
                           mdl_lst = mdl_lst, 
                           box_size = NULL){
  needed <- 
    func_to_run %>% 
    formals() %>% 
    list() %>% 
    unlist() %>% 
    names
  
  if(needed == "mdl_lst"){
    return(func_to_run(mdl_lst))
  }
  
  if (! is.null(box_size)){
    mdl_lst[["box_size"]] <- mdl_lst[[box_size]]
    
  }
  
  
  invoke(.f = func_to_run, 
         .x = mdl_lst[needed]
         )
}



###############################
# runs the given Q equation once for the ith row in the df
#
run_function_ith_row <- function(irow, func_to_run, df){
  arguments <- df %>% slice(irow) %>% as.list()
  
  result <- invoke(.f = func_to_run, 
                   .x = arguments
  )
  return(result)
}

###############################
# runs the given Q equation once for the ith row in the df
# TODO: maybe
# run_function_ith_row_mdl_list <- function(irow, func_to_run, df){
#   mdl_lst <- df %>% slice(irow) %>% as.list()
#   
#   func_to_run(mdl_lst)
# 
#   return(result)
# }


#run_Q_equation_many_times(df, Q_Lg_Iag)
# run_Q_equation_for_each_row(df, Q_Sg_Lg)
# run_Q_equation_for_each_row(df, Q_Sg_Lq)
# run_Q_equation_for_each_row(df, Q_Lg_Iag)
###############################
# runs the given Q equation once for each row in in the df passed in
#
run_Q_equation_for_each_row <- function(df,
                                      func_to_run =Q_Lg_Iag){
  needed <- 
    func_to_run %>% 
    formals() %>% 
    list() %>% 
    unlist() %>% 
    names

  
  ret_vec <- NULL

  if(needed == "mdl_lst"){
    ret_vec = c()
    for (irow in 1:nrow(df)){
      mdl_lst <- df %>% slice(irow) %>% as.list()
      #plist(mdl_lst)
      ret_vec <- c(ret_vec,func_to_run(mdl_lst))
    }
    
    
  }else{
  
    df <- df %>% select(needed)
    ret_vec <- 
      lapply(X = 1:nrow(df), 
           FUN = run_function_ith_row, 
           func_to_run = func_to_run, 
           df = df 
           ) %>% unlist()
    
  }
  return(ret_vec)
}


##########################
# Leave the Susceptible population
Q_Sg_Lg <- function(trans_rate,  #transmission rate
                    frac_contact_trace, # % exposed id’d in contact tracing
                    Cgg, Cgq, 
                    Iag, Ismg, Iaq, 
                    Ismq, Issq,
                    Sg){
  trans_rate * (1 - frac_contact_trace) * (Cgg * (Iag + Ismg) + Cgq * (Iaq + Ismq +Issq)) * Sg
}



##########################
# Leave the Susceptible population
Q_Sg_Lq <- function(trans_rate, #transmission rate
                    frac_contact_trace, # % exposed id’d in contact tracing
                     Cgg, 
                     Cgq, 
                     Iag, 
                     Ismg, 
                     Iaq, 
                    Ismq, 
                    Issq,
                    Sg
                     ){
  trans_rate * (frac_contact_trace) * (Cgg * (Iag + Ismg) +  Cgq * (Iaq + Ismq +Issq)) * Sg
}



##########################
#
#  Leave the Latent box for the Infected a-symptomatic 
# Same for both Infected a-symptomatic General and Quarintined
#
Q_L_Ia <- function(t_latency,
                   box_size
                   ){
  (1.0/t_latency )*box_size
}
Q_Lg_Iag <- function(mdl_lst){  run_Q_equation(Q_L_Ia, mdl_lst, box_size = "Lg")   }
Q_Lq_Iaq <- function(mdl_lst){  run_Q_equation(Q_L_Ia, mdl_lst, box_size = "Lq")   }

##########################
#
#  Leave the Infected a-symptomatic for recovery
# Same for both Infected a-symptomatic General and Quarintined
#
Q_Ia_R <- function(f_symp,   #% a-symptomatic who will develop symptoms
                  t_Ia_R, # duration of a-symptomatic period before recovery
                  box_size
                      ){
  ((1 - f_symp) / t_Ia_R ) * box_size
}
Q_Iag_R <- function(mdl_lst){  run_Q_equation(Q_Ia_R, mdl_lst, box_size = "Iag")   }
Q_Iaq_R <- function(mdl_lst){  run_Q_equation(Q_Ia_R, mdl_lst, box_size = "Iaq")   }




##########################
#   the a-symptomatic population for mild symptimatic, general population
#
Q_Ia_Ismg <- function(f_symp,   # % a-symptomatic who will develop symptoms
                     f_mild,  # % symptomatic that get mild symptoms
                     f_mild_iso, #  % mild cases go in isolation
                     t_presymp, # days pre-symptomatic infectious period 
                     t_diag, # days from onset of symptoms to diagnostic 
                     box_size
                    ){
  (f_symp * f_mild * (1- f_mild_iso) / (t_presymp + t_diag)) * box_size
}
Q_Iag_Ismg <- function(mdl_lst){  run_Q_equation(Q_Ia_Ismg, mdl_lst, box_size = "Iag")   }
Q_Iaq_Ismg <- function(mdl_lst){  run_Q_equation(Q_Ia_Ismg, mdl_lst, box_size = "Iaq")   }






##########################
# Leave the a-symptomatic population for mild symptimatic,  quarintined
#
Q_Ia_Ismq <- function(f_symp,   # % a-symptomatic who will develop symptoms
                     f_mild,  # % symptomatic that get mild symptoms
                     f_mild_iso, #  % mild cases go in isolation
                     t_presymp, # days pre-symptomatic infectious period 
                     t_diag, # days from onset of symptoms to diagnostic 
                     box_size
){
  (f_symp * f_mild * (f_mild_iso) / (t_presymp + t_diag)) * box_size
}
Q_Iag_Ismq <- function(mdl_lst){  run_Q_equation(Q_Ia_Ismq, mdl_lst, box_size = "Iag")   }
Q_Iaq_Ismq <- function(mdl_lst){  run_Q_equation(Q_Ia_Ismq, mdl_lst, box_size = "Iaq")   }



##########################
# Leave the a-symptomatic population for severe symptimatic,  quarintined
#
Q_Ia_Issq <- function(f_symp,   # % a-symptomatic who will develop symptoms
                      f_mild,  # % symptomatic that get mild symptoms
                      t_presymp, # days pre-symptomatic infectious period 
                      t_diag, # days from onset of symptoms to diagnostic 
                      box_size
){
  (f_symp * (1-f_mild)  / (t_presymp + t_diag)) * box_size
}
Q_Iag_Issq <- function(mdl_lst){  run_Q_equation(Q_Ia_Issq, mdl_lst, box_size = "Iag")   }
Q_Iaq_Issq <- function(mdl_lst){  run_Q_equation(Q_Ia_Issq, mdl_lst, box_size = "Iaq")   }


##########################
# Leave the MILD-symptomatic population for RECOVERY
#
Q_Ism_R <- function(t_mild_symptomatic,
                    box_size){
  (1.0/t_mild_symptomatic) * box_size
}
Q_Ismg_R <- function(mdl_lst){  run_Q_equation(Q_Ism_R, mdl_lst, box_size = "Ismg")   }
Q_Ismq_R <- function(mdl_lst){  run_Q_equation(Q_Ism_R, mdl_lst, box_size = "Ismq")   }



##########################
# Leave the severe - symptomatic population for RECOVERY
#
# YAY!! We like this one!!!
#
Q_Issq_R <- function(f_severe_death,# percentage of severe case dying
                    t_severe_recovery,
                    Issq
                    ){
  ((1-f_severe_death) / t_severe_recovery ) * Issq
}


##########################
# Leave the severe - symptomatic population for Death
Q_Issq_D <- function(f_severe_death, # percentage of severe case dying
                     t_severe_Death, # duration of the symptomatic period for Severe cases before dyin
                     Issq
                    ){
  ((f_severe_death) / t_severe_Death ) * Issq
}

####################################
#
Q_Issq_Hg <- function(t_severe_Hosp,
                      Issq,
                      Hg,
                      Hg_cap
){
    Qtmp <- (1 / t_severe_Hosp) * Issq
    Hg_new <- min(Hg + Qtmp , Hg_cap)
    new_delta_Hg = Hg_new - Hg
    
    if(new_delta_Hg > Hg_cap){
      pp("t_severe_Hosp = ", t_severe_Hosp)
      pp("Issq = ", Issq)
      pp("Hg = ", Hg)
      pp("Hg_cap = ", Hg_cap)
      pp("Qtmp = ", Qtmp)
      pp("Hg_new = ", Hg_new)
      pp("new_delta_Hg = ", new_delta_Hg)
      assertthat::assert_that(FALSE)
    }
    return(new_delta_Hg)
}


####################################
#
Q_Hg_R <- function(f_severe_death,
                   f_Hg_saved,
                   t_Hg,
                   Hg
                   ){
  ((1-(f_Hg_saved*f_severe_death)) / t_Hg ) * Hg
}

####################################
#
Q_Hg_D <- function(f_severe_death,
                   f_Hg_saved,
                   t_Hg,
                   Hg
                   ){
  ((f_Hg_saved*f_severe_death) / t_Hg ) * Hg
}

#############################################################
###################### End Q Eqs start D Eqs#################
#############################################################



################################
# Run the vector of equations eqs with lapply
# pass mdl_lst to each one, and sum() the results 
add_Q_eq <- function(eqs = NULL, mdl_lst = mdl_lst){
  ret_val = 0
  
  if (is.null(eqs)){
    return(ret_val)
  }
  if (is.na(eqs)){
    return(ret_val)
  }  
  
  if (length(eqs) == 0){
    return(ret_val)
  }  
  if (length(eqs) == 1){
    ret_val <- run_Q_equation(eqs, mdl_lst = mdl_lst)
    
    if(ret_val < 0 ){
      plist(eqs)
      plist(ret_val)
      plist(mdl_lst)
      assertthat::assert_that(FALSE)
    }
    
  }else{
    ret_vals <- 
    lapply(X = eqs,
          FUN = run_Q_equation,
          mdl_lst = mdl_lst
             ) %>% 
      unlist()
    
    if(min(ret_vals) < 0 ){
      plist(eqs)
      plist(ret_vals)
      plist(mdl_lst)
      assertthat::assert_that(FALSE)
    }
    
    ret_val = sum(ret_vals)
  }
  
  return(ret_val)
}


#############################333
#  given a list of functions that calculate all the ins and all the outs
#  returns a delta on the box.
delta_all <- function(Q_ins = NULL, Q_outs = NULL , mdl_lst = mdl_lst, ignore_in = FALSE, ignore_out = FALSE){
  Q_in_val <- 0
  Q_out_val <- 0
  
  if ( ! is.null(Q_ins) & ignore_in == FALSE){
    Q_in_val = Q_in_val + add_Q_eq(Q_ins, mdl_lst)
  }
  
  if ( ! is.null(Q_outs) & ignore_out == FALSE){
    Q_out_val = Q_out_val + add_Q_eq(Q_outs, mdl_lst)
  }  
  
  return(Q_in_val - Q_out_val)
}






################################
# delta in S
D_Sg <- function(mdl_lst, ...){
  delta_all(Q_outs = c(Q_Sg_Lg, Q_Sg_Lq), mdl_lst = mdl_lst, ...)
}



#######################
# Change in the Latent General population
D_Lg <- function(mdl_lst, ...){
  delta_all(Q_ins = Q_Sg_Lg, 
            Q_outs = Q_Lg_Iag, 
            mdl_lst = mdl_lst, 
            ...
            )
}
#######################
# Change in the Latent quarintine
D_Lq <- function(mdl_lst, ...){
  delta_all(Q_ins = Q_Sg_Lq, 
            Q_outs = Q_Lq_Iaq, 
            mdl_lst = mdl_lst, ...)
}



#######################
# Change in the a-symptomatic general population
D_Iag <- function(mdl_lst, ...){
  delta_all(Q_ins = Q_Lg_Iag, 
            Q_outs = c(Q_Iag_R , Q_Iag_Ismg, Q_Iag_Ismq, Q_Iag_Issq), 
            mdl_lst = mdl_lst, ...)
}


#######################
# Change in the a-symptomatic Quarintine population
D_Iaq <- function(mdl_lst, ...){
  delta_all(Q_ins = Q_Lq_Iaq, 
            Q_outs = c(Q_Iaq_R , Q_Iaq_Ismg, Q_Iaq_Ismq, Q_Iaq_Issq), 
            mdl_lst = mdl_lst, ...)
}

#######################33
# Change in mild symptoms of the general population
D_Ismg <- function(mdl_lst, ...){
  delta_all(Q_ins = c(Q_Iag_Ismg, Q_Iaq_Ismg), 
            Q_outs = Q_Ismg_R, 
            mdl_lst = mdl_lst, ...)  
}
#######################33
# Change in mild symptoms of the quarintine population
D_Ismq <- function(mdl_lst, ...){
  delta_all(Q_ins = c(Q_Iag_Ismq, Q_Iaq_Ismq), 
            Q_outs = Q_Ismq_R, 
            mdl_lst = mdl_lst, ...)  
}



###########################
# change in the severe symptops of Quarintine
D_Issq <- function(mdl_lst, ...){
  delta_all(Q_ins = c(Q_Iaq_Issq, Q_Iag_Issq), 
            Q_outs = c(Q_Issq_D, Q_Issq_R, Q_Issq_Hg), 
            mdl_lst = mdl_lst, ...)   
}


D_D <- function(mdl_lst, ...){
  ret_val <- 
  delta_all(Q_ins = c(Q_Issq_D, Q_Hg_D), 
            mdl_lst = mdl_lst, ...)   
  
  
   if (ret_val < 0){
     plist()
     assertthat::assert_that(FALSE)
   }
  
  return(ret_val)
}

D_R <- function(mdl_lst, ...){
  delta_all(Q_ins = c(Q_Iag_R, Q_Iaq_R, Q_Ismg_R, Q_Ismq_R, Q_Issq_R, Q_Hg_R),
            mdl_lst = mdl_lst, ...)   
}


D_Hg <- function(mdl_lst, ...){
  delta_all(Q_ins = Q_Issq_Hg,
            Q_outs = c(Q_Hg_D , Q_Hg_R),
            mdl_lst = mdl_lst, ...) 
}






####################################
# Equations to run to updatee deltas
D_to_update <- c(Sg = D_Sg, 
                 D = D_D, 
                 Lg = D_Lg, 
                 Lq = D_Lq,
                 Iag = D_Iag, 
                 Iaq = D_Iaq,
                 Ismg = D_Ismg, 
                 Ismq = D_Ismq, 
                 Issq = D_Issq,
                 Hg = D_Hg,
                 R = D_R )


######################################
# Given a DF of model states and a delta function,
# Run the delta function for each row in the data frame
# returns a vector of size nrow(df)
#
# optinally ignore all that comes in ore all that comes out with 
#
# ignore_out = TRUE
# ignore_in = TRUE
run_D_each_row <- function(df = out, func_to_run = D_Sg, ignore_out , ignore_in ) {
  ret_vec <- c()
  for (irow in 1:nrow(df)){
    curr_mdl_lst <- df %>% slice(irow)  %>% as.list()
    ret_vec <- ret_vec %>% c(func_to_run(curr_mdl_lst, ignore_out = ignore_out, ignore_in = ignore_in)    )
  }
  
  return(ret_vec)
  #lapply(X = 1:nrow(df), FUN = func_to_run, df = df, ...)
  # curr_mdl_lst <- df %>% slice(20)  %>% as.list()
  # curr_mdl_lst %>% length()
  # curr_mdl_lst %>% unlist()
  # func_to_run(mdl_lst, ...)
}



##############################################################
#
# Calculate a lot of delta "Either in or out", for every timestep
#
#
#out %>% calc_all_Ds(D_to_calc = D_to_update, ignore_out = FALSE, ignore_in = TRUE)
#out %>% calc_all_Ds(D_to_calc = D_to_update, ignore_out = TRUE, ignore_in = FALSE)
#out %>% calc_all_Ds(D_to_calc = D_to_update, ignore_out = TRUE, ignore_in = TRUE)
#out %>% calc_all_Ds(D_to_calc = D_to_update, ignore_out = FALSE, ignore_in = FALSE)
calc_all_Ds <- function(df, D_to_calc, prefix = "CHANGE", sep = "_", ignore_out = TRUE, ignore_in = FALSE, ...){
  
  ret_lst <- list()
  ret_df <- tibble(irownum = 1:nrow(df))
  for (iD in 1:length(D_to_calc)){
    nm <- names(D_to_calc[iD])
    new_nm = paste(prefix, nm, sep = sep)
    if (ignore_out == FALSE)
      new_nm <- new_nm %>% paste("out", sep = sep)
    
    if (ignore_in == FALSE)
      new_nm <- new_nm %>% paste("in", sep = sep)
    
    
    ret_df[[new_nm]] <-  run_D_each_row(df = df, 
                                        func_to_run = D_to_calc[[iD]], 
                                        ignore_out = ignore_out, 
                                        ignore_in = ignore_in
                                        )
    
  }

  return(ret_df %>% select(-irownum))
  
}


#########################################333333
#
# SEIR Model
#
seir <- function(time, state_vec, param_lst) {
  mdl_lst <- c(param_lst, state_vec)
  
  
  
  D_lst <- lapply(D_to_update, run_Q_equation, mdl_lst= mdl_lst)
  
  D_lst %>% length()
  ret_lst <- D_lst[names(state_vec)] 
  ret_lst %>% length()
  ret_lst %>% unlist()
  return(ret_lst %>% unlist() %>% list())
}


