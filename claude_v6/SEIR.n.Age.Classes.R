
SEIR.n.Age.Classes = function(file.name, sheets.names, differential.eqns.func=NULL, scale.rate.to.size=FALSE)
{
  #sheets.names is list(parms.1d,  parms.2d,  initial.conditions,  model.flow,  auxiliary.vars)
  
  #==========================================================================
  #   Initial conditions and parameters
  #==========================================================================
  #browser()
  
  time_stuff   = sheets.names$parms.1d            # 1 dimensional parameters
  time_stuff_m = sheets.names$parms.2d            # 2 dimensional parameters
  input_stuff  = sheets.names$initial.conditions  # initial values/conditions
  
  if(!is.data.frame(time_stuff))
    time_stuff   <- as.data.frame.from.tbl( readxl::read_excel(file.name, sheet = time_stuff) )  # 1 dimensional parameters
  
  if(!is.data.frame(time_stuff_m))  
    time_stuff_m <- as.data.frame.from.tbl( readxl::read_excel(file.name, sheet = time_stuff_m) ) # 2 dimensional parameters
  
  if(!is.data.frame(input_stuff)) 
    input_stuff  <- as.data.frame.from.tbl( readxl::read_excel(file.name, sheet = input_stuff) ) # initial values/conditions
  
  nagegrp <- length(unique(time_stuff$agegrp))        # number of age groups
  
  nrow_   <- dim(time_stuff)[1]/nagegrp
  
  time_stuff <- dplyr::arrange(time_stuff, tmin, agegrp) # sort by tmin agegrp
  time_stuff <- time_stuff %>%
    mutate(isim = rep(1:nrow_, each=nagegrp)) 
  
  time_stuff_m <- arrange(time_stuff_m, tmin, cagegrp, ragegrp) # sort by tmin cagegrp  ragegrp
  time_stuff_m <- time_stuff_m %>%
    mutate(isim = rep(1:nrow_, each = nagegrp*nagegrp))
  #===================================================================
  # Initial values (components)
  #===================================================================
  #initial values
  input_stuff_age_columns = setdiff(colnames(input_stuff), "NAME")
  init_list <- list()
  for(k in input_stuff$NAME){
    init_list[[k]] <- as.matrix( subset(input_stuff, NAME == k)[,input_stuff_age_columns] )
  }
  
  #===================================================================
  # Build function eval.differential.eqns.func (if not provided)
  #===================================================================
  model_flows_tmp = auxiliary.vars = "Not used since was argument differential.eqns.func was provided"
  eval.differential.eqns.func = differential.eqns.func
  if(is.null(eval.differential.eqns.func))
  {
    model_flows_tmp = sheets.names$model.flow
    auxiliary.vars  = sheets.names$auxiliary.vars
 
    
    if(!is.data.frame(model_flows_tmp))
      model_flows_tmp  <- as.data.frame.from.tbl( readxl::read_excel(file.name, sheet = model_flows_tmp) )  # arrows in flowchart
    
    if(!is.data.frame(auxiliary.vars))
      auxiliary.vars   <- as.data.frame.from.tbl( readxl::read_excel(file.name, sheet = auxiliary.vars ) )  # auxiliary variables in equations
    
    model_flows_tmp = subset( model_flows_tmp,!is.na(expression) & activation ==1)
    model_flows = model_flows_tmp     # keep model_flows_tmp for output. Work on model_flows from this point on
    
    # BEGIN Build character vector differential.eqns.char
    model_flows$multiplier = model_flows$From
    if(!scale.rate.to.size)
      model_flows$multiplier = NA
    
    model_flows = model_flows %>%   
      mutate(fromstar = ifelse(is.na(multiplier),"",paste0(From,"*(")) ,
             closing.parenthesis = ifelse(is.na(multiplier),"",")") ,
             expression = paste0(fromstar,expression,closing.parenthesis) )
    
    boxes = unique(c(model_flows$From,model_flows$To))
    outflows=rep("#####$$$$$$$$$$$!@@@@@@@@",length(boxes))
    names(outflows) = boxes
    inflows=outflows
    
    for(k in seq(nrow(model_flows)))
    {
      this.expression = model_flows$expression[k]
      outflows[model_flows$From[k]] = paste(outflows[model_flows$From[k]],"+", this.expression)
      inflows [model_flows$To[k]  ] = paste( inflows[model_flows$To[k]  ],"+", this.expression)
    }
    differential.eqns.char = paste("(",inflows,") - (",outflows,")")
    differential.eqns.char = gsub(  "#####$$$$$$$$$$$!@@@@@@@@ +","" ,differential.eqns.char ,fixed=TRUE)
    differential.eqns.char = gsub("( #####$$$$$$$$$$$!@@@@@@@@ )","0",differential.eqns.char ,fixed=TRUE)
    differential.eqns.char = paste0("derivative.",boxes,"=",differential.eqns.char)
    
    rm(model_flows,boxes,inflows,outflows,this.expression) # clean up intermediate objects no longer required
    
    # END Build character vector differential.eqns.char
    
    
    # BEGIN Build function eval.differential.eqns.func
    
    auxiliary.vars.assignment = paste(auxiliary.vars$variable,"=",auxiliary.vars$expression)
    out.char = paste0("out=c(", paste0("derivative.", paste(names(init_list), collapse=", derivative.")),")")
    
    #function.char = c(auxiliary.vars.assignment,differential.eqns)
    within.with.part.char = paste(c(auxiliary.vars.assignment,differential.eqns.char,out.char,"list(out)"),collapse="\n") # note ";" is important here
    
    within.with.part.char.pretty = gsub(";","\n",within.with.part.char)  # icitte delete
    
    function.char = "function(list.inits, list.parms){" 
    with.char =   " with(as.list(c(list.inits, list.parms)),{"
    
    
    eval.differential.eqns.func = paste(c(function.char,with.char,within.with.part.char,"})" ,"}"),collapse="\n" ) # code of eval.differential.eqns.func
    # cat(eval.differential.eqns.func) # quick look at how it look like
    eval.differential.eqns.func = eval(parse(text =  eval.differential.eqns.func ))
    
    #eval.differential.eqns.func = eval(parse(text= "function(X) {\n cos(X)\n}" ))
    #eval.differential.eqns.func = eval(parse(text= "function(X) { cos(X)}" ))
    #eval.differential.eqns.func (pi)  # gives -1 as it should
    
    rm(auxiliary.vars.assignment, out.char, within.with.part.char, function.char, with.char) # clean up 
    # END Build function eval.differential.eqns.func
  }
  
  
  #==========================================================================
  #  Main routine
  #==========================================================================
  # The SEIR model with N age classes
  #
  SEIR.n.Age.Classes.within.loop <- function( time=NULL, age.parms = NULL, age.age.parms = NULL,list.inits = NULL, not.parms=  c("tmin", "tmax", "agegrp", "cagegrp", "ragegrp", "isim"))
  {
    nage = nrow(age.parms)
    
    if (is.null(age.parms))
      stop("undefined 'age.parms'")
    
    if (is.null(age.age.parms))
      stop("undefined 'age.age.parms'")
    
    if (is.null(time))
      stop("undefined 'time'")
    
    if (is.null(list.inits))
      stop("undefined 'list.inits'")
    
    
    list.parms <- list()
    for(k in setdiff(names(age.parms), not.parms )){
      list.parms[[k]] <- age.parms[,k]
    }
    for(k in setdiff(names(age.age.parms), not.parms ))
    {
      temp<- array(NA, c(nage,nage))
      temp[cbind(age.age.parms$cagegrp, age.age.parms$ragegrp)] <- age.age.parms[,k]
      list.parms[[k]] <- temp
      if(any(is.na(temp)))
        stop(paste0(k," matrix has some missing entries"))
    }
    
    ### to write the system of differential equations
    calculate_derivatives <- function(time, vec.inits, list.parms, names.inits) {
      
     
      iota <- seq(length(vec.inits)/length(names.inits)) 
      list.inits <- list()
      for(k in names.inits){
        if (length(iota) > 1) {
          list.inits[[k]] <- vec.inits[paste0(k, iota)] 
        }else{list.inits[[k]] <- vec.inits[k] }
      }
      
      eval.differential.eqns.func(list.inits, list.parms)
      
      
    } #end of function calculate_derivatives
    
    ###---------------------------------------------------------------------------------
    ### Solver for Ordinary Differential Equations (ODE), Switching Automatically
    ### Between Stiff and Non-stiff Methods
    ###---------------------------------------------------------------------------------
    
    output <-  lsoda(y = unlist(list.inits),
                     times = time,
                     func =  calculate_derivatives,
                     parms = list.parms,
                     names.inits = names(list.inits))
    
    return(output)
  }  # END  of function SEIR.n.Age.Classes.within.loop
  
  ################################################################
  #        To run the example
  ################################################################
  
  excluded_names <- c("tmin", "tmax","agegrp","cagegrp","ragegrp","isim")
  sprintf("S(E)IR model script to estimate the number of COVID-19 cases")
  sprintf("Number of age groups considered: %s", nagegrp)
  sprintf("Components:")
  sprintf( names(init_list) )
  sprintf("Parameters that change with age (age-groups):")
  sprintf( setdiff(colnames(time_stuff  ), excluded_names) )
  sprintf("Parameters that change with age and contact with others (age-groups x age-groups):")
  sprintf( setdiff(colnames(time_stuff_m), excluded_names) )
  sprintf("...Computing ... ")
  
  nSim <- max(time_stuff$isim)
  listOut <- list()
  previous.tmax <- 0
  out<-NULL
  df.out = c()
  
  for(i in seq(1, nSim, 1))
  {
    parameter.by.age     <- subset(time_stuff  , isim == i)
    parameter.by.age.age <- subset(time_stuff_m, isim == i)
    
    tmin <- unique(c(parameter.by.age$tmin, parameter.by.age.age$tmin))
    tmax <- unique(c(parameter.by.age$tmax, parameter.by.age.age$tmax)) 
    
    if(length(tmin)>1 || length(tmax)>1 || tmin>=tmax )
      stop(paste0("Unexpected pattern in tmin, tmax for interval ", i))
    
    tt <- seq(0, tmax - tmin, by = 1)
    
    if(tmin != previous.tmax)
      stop(paste(interval.label , "\n  Interval lower bound not equal to previous interval upper bound"))
    
    previous.tmax <- tmax
    out <- SEIR.n.Age.Classes.within.loop( time=tt,
                                           age.parms = parameter.by.age,
                                           age.age.parms = parameter.by.age.age,
                                           list.inits = init_list)
    
    out <- as.data.frame(out)
    # ode/lsoda Output diagnostic #######################
    #diagn <- diagnostics.deSolve(out)
    
    out$time <- seq(tmin,tmax,1) 
    out_for_init <- out %>%
      slice(nrow(out)) %>%
      pivot_longer(-time)
    init <- out_for_init$value
    names(init) <- out_for_init$name     
    
    rowns <- names(select(out,-c(time)))
    out <- out %>%
      mutate(N_tot = rowSums(.[rowns]))  # Total number of individuals 
    
    
    #updating the initial values  
    for(k in 1:length(init_list)){
      init_list[[k]][1:nagegrp] <- init[seq(nagegrp*(k-1)+1,nagegrp*k)] 
    }
    
    
    # Add outputs to the list
    df.out = rbind(df.out,out[-nrow(out),])
    listOut[[i]] <- out
  } #end for(i in seq(1, nSim, 1))
  resultat = list(listOut.to.be.decomissioned = listOut , solution=df.out, differential.eqns.func = eval.differential.eqns.func )
  resultat$input.info = list(parms.1d=time_stuff, parms.2d=time_stuff_m, initial.conditions=input_stuff,
                             auxiliary.vars=auxiliary.vars,model.flow=model_flows_tmp)
  resultat
} #end of SEIR.n.Age.Classes function
