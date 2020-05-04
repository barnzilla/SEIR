
SEIR.n.Age.Classes = function(file.name, sheets.names, differential.eqns.func=NULL)
{
  #sheets.names is list(parms.1d,  parms.2d,  initial.conditions,  model.flow,  auxiliary.vars)
  
  input.info.verbatim = sheets.names
  input.info.verbatim$file.name = file.name
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
    
    lazy.range=range(model_flows$lazy)
    if(is.na(lazy.range[1]))
      stop("\nstyle (lazy or not) must be specified for all transitions")
    if(diff(lazy.range)>0 || !(lazy.range[1] %in% c(0,1)) )  # may be relaxed later
      stop("\nstyle (lazy or not) should be all zeros or all ones")
    
    if(lazy.range[1] == 0)  # not lazy
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
    differential.eqns.char = paste("(flow.multiplier$inflow)*(",inflows,") + (flow.multiplier$outflow)*(",outflows,")")
    differential.eqns.char = gsub(  "#####$$$$$$$$$$$!@@@@@@@@ +","" ,differential.eqns.char ,fixed=TRUE)
    differential.eqns.char = gsub("( #####$$$$$$$$$$$!@@@@@@@@ )","0",differential.eqns.char ,fixed=TRUE)
    differential.eqns.char = paste0("derivative.",boxes,"=",differential.eqns.char)
    
    #if( any(grepl("#####$$$$$$$$$$$!@@@@@@@",differential.eqns.char)) )  browser()

    
    rm(model_flows,boxes,inflows,outflows,this.expression) # clean up intermediate objects no longer required
    
    # END Build character vector differential.eqns.char
    
    
    # BEGIN Build function eval.differential.eqns.func
    
    auxiliary.vars.assignment = paste(auxiliary.vars$variable,"=",auxiliary.vars$expression)
    compartments =   names(init_list) # e.g.  S D L
    compartments.age = c ( t( outer( compartments,seq(nagegrp),paste0) ) ) # e.g. S1 S2 S3 S4 S5 D1 D2 ...
    out.char      = paste0("out=c(", paste0("derivative.", paste(compartments    , collapse=", derivative.")),")")
    names.out.char= paste0("out=c(", paste0("derivative.", paste(compartments.age, collapse=",derivative." )),")") 
    names.out.char = gsub(",","','",names.out.char ,fixed=TRUE) # replace , by ','
    names.out.char = gsub("(","('" ,names.out.char ,fixed=TRUE) # replace ( by ('
    names.out.char = gsub(")","')" ,names.out.char ,fixed=TRUE) # replace ) by ')
    names.out.char = gsub("out=","names(out)=",names.out.char ,fixed=TRUE)
    
    #function.char = c(auxiliary.vars.assignment,differential.eqns)
    within.with.part.char = paste(c(auxiliary.vars.assignment,differential.eqns.char,out.char,names.out.char,"list(out)"),collapse="\n") # First tried ";\n" but realised that "\n" is sufficient
    
    function.char = "function(list.inits, list.parms,flow.multiplier=list(inflow=1,outflow=-1)){" 
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
    
   if(i < nSim)
      out = out[-nrow(out),]
    # Add outputs to the list
    df.out = rbind(df.out,out)
    listOut[[i]] <- out
  } #end for(i in seq(1, nSim, 1))
  resultat = list( solution=df.out, differential.eqns.func = eval.differential.eqns.func ) # ,listOut.to.be.decomissioned = listOut 
  resultat$input.info = list(parms.1d=time_stuff, parms.2d=time_stuff_m, initial.conditions=input_stuff,
                             auxiliary.vars=auxiliary.vars,model.flow=model_flows_tmp)
  resultat$input.info.verbatim = input.info.verbatim
  resultat
} #end of SEIR.n.Age.Classes function


smooch.parms.df.into.list = function(df.parms.1d,df.parms.2d,these.are.not.parms)
{ # takes 1d and 2d parms in data.frame format and makes a list.
  # for 2d parms, they are in skinny format in input data frame, but are square matrices in the output list.
  list.parm.1d = as.list(df.parms.1d [,setdiff(colnames(df.parms.1d),these.are.not.parms)]) # list rather than data.frame
  list.parm.2d = as.list(df.parms.2d [,setdiff(colnames(df.parms.2d),these.are.not.parms)]) # list rather than data.frame
  
  # BEGIN fix list.parm.2d which is not quite what we want.  We want list of matrices, not vectors.
  list.parm.2d.tmp = list.parm.2d 
  list.parm.2d = list()
  matrix.template = array(NA,c(1,1)*max(list.parm.2d.tmp$ragegr))
  matrix.indices = cbind(list.parm.2d.tmp$ragegrp,list.parm.2d.tmp$cagegrp)
  for(k in setdiff(names(list.parm.2d.tmp),c("ragegrp" , "cagegrp")) )
  {
    list.parm.2d[[k]] = matrix.template
    list.parm.2d[[k]] [matrix.indices] = list.parm.2d.tmp[[k]]
  }  
  # END fix list.parm.2d which is not quite what we want.  We want list of matrices.
  as.list(c(list.parm.1d,list.parm.2d))
}



Add.Other.Outcomes = function(solution.df,parms.1d,definition.list)
{  # definition.list not used.  Could be something like list(L_tot = "L*", Quarant = c("Lq" ,"Iq_pres" ,"Iaq_r") , Hosp = "Iss_hosp")
  
  # Add other outcomes
  # Compute L_tot
  solution.df[["L_tot"]] <- solution.df %>% select_at(vars(starts_with("L"))) %>% rowSums()
  
  # Compute I_tot
  solution.df[["I_tot"]] <- solution.df %>% select_at(vars(starts_with("I"))) %>% rowSums()
  
  # Compute Incidence  (number of new cases each day, over time [incidence])
  IncI <- c()
  
  for(j in 1:nrow(solution.df)) { 
    if(j == 1) {  
      IncI[j] <- solution.df$I_tot[1]   
    } else {  
      sigma <- parms.1d %>% filter(solution.df$time[j] > tmin & solution.df$time[j] <= tmax) %>% select(sigma)
      IncI[j] <- solution.df$L_tot[j - 1] * sigma
    } 
  }    
  solution.df[["IncI"]] <- unlist(IncI)
  
  # Compute cumulative incidence
  solution.df[["cumI"]] <- cumsum(IncI)
  
  # Compute Hospitalized
  solution.df[["Hosp"]] <- solution.df$Iss_hosp
  
  # Compute Quarantined
  solution.df[["Quarant"]] <- solution.df$Lq + solution.df$Iq_pres + solution.df$Iaq_r
  
  # Compute Isolated
  solution.df[["Isolat"]] <- solution.df$Ism_iso + solution.df$Iss_isohome
  
  solution.df
}

Outcomes.Summary = function(solution.df,targets)  # content to be expanded
{
  maxI = max(solution.df$I_tot)
  
  sommaire=data.frame(maxI=maxI,maxI.time = subset(solution.df,I_tot==maxI)$time)
  sommaire$cumI.75days = subset(solution.df,time==75)$cumI
  sommaire$total.deaths = max(solution.df$D)
  
  if(length(setdiff( colnames(targets$donnees) , colnames(solution.df)  ) )>0 )
    stop("\nAll variables in targets$donnees should be in solution.df")
  
  Ynames = setdiff(colnames(targets$donnees),"time")
  
  for(k in seq(nrow(targets$time.ranges)))
  {
    time.span = targets$time.ranges[k,]
    time.span.char = paste(time.span,collapse="-")
    model = subset(solution.df     , time.span$lower.bound <= time & time <=  time.span$upper.bound)[,Ynames,drop=F]
    dat   = subset(targets$donnees , time.span$lower.bound <= time & time <=  time.span$upper.bound)[,Ynames,drop=F]
    erreur = apply(abs(model-dat),2,sum)
    names(erreur) = paste("GOF",names(erreur),"days",time.span.char) # e.g. "GOF cumI days 60-69" "GOF D days 60-69"
    sommaire = cbind(sommaire, t(as.data.frame(erreur)) )
  } 
  
  sommaire
}


ever.been.here = function(SEIR.object)  
{
  n_agegrp = ncol(SEIR.object$input.info$initial.conditions) - 1 # number of age groups
  agegrp.suffix = ""
  if(n_agegrp > 1)
    agegrp.suffix = 1:n_agegrp
  
 # SEIR.object = SEIR.object.5age.betavec.not.lazy
  range(diff(SEIR.object$solution$time)) # check that times are in increments of 1 (feature implicitly used below)
  differential.eqns.func=SEIR.object$differential.eqns.func
  
  these.are.not.parms = c("tmin",     "tmax" ,  "agegrp", "isim" ) # actaully want to keep "ragegrp",  "cagegrp"
  inflows  = c()
  outflows = c()
  for(tt in SEIR.object$solution$time) # [1:4]
  {
    cat("\nicitte ",tt)
    #BEGIN get list.parm.1d.2d  (all parameters stored in a list  )
    df.parm.1d = SEIR.object$input.info$parms.1d
    df.parm.2d = SEIR.object$input.info$parms.2d
    df.parm.1d = subset(df.parm.1d ,overlap.length(tt,tt+1,df.parm.1d$tmin,df.parm.1d$tmax) > 0 )
    df.parm.2d = subset(df.parm.2d ,overlap.length(tt,tt+1,df.parm.2d$tmin,df.parm.2d$tmax) > 0 )
    list.parm.1d.2d = smooch.parms.df.into.list(df.parm.1d,df.parm.2d,these.are.not.parms)
    #END get list.parm.1d.2d  (all parameters stored in a list  )
    
   # browser()
    #BEGIN list.compartments  ... will contain vectors D, R, S, ... at time tt
    list.compartments = list() # will contain vectors D, R, S, ....
    mat1row.compartments = data.matrix(subset(SEIR.object$solution,time == tt))
    for(k in SEIR.object$input.info$initial.conditions$NAME)
      list.compartments[[k]] = mat1row.compartments[1,paste0(k,agegrp.suffix)]
    #END list.compartments  ... will contain vectors D, R, S, ... at time tt
    # browser()
    inflows  = rbind(inflows  , differential.eqns.func(list.compartments,list.parm.1d.2d,flow.multiplier =list(inflow = 1,outflow=0))[[1]] )
    outflows = rbind(outflows , differential.eqns.func(list.compartments,list.parm.1d.2d,flow.multiplier =list(inflow = 0,outflow=1))[[1]] )
  }
  browser()
  colnames(inflows ) = gsub("derivative.","",colnames(inflows ))
  colnames(outflows) = gsub("derivative.","",colnames(outflows))
  solution.lean = SEIR.object$solution[,setdiff(names(SEIR.object$solution),c("time","N_tot")) ]
  
  
  ever.been.here.from.inflows = rbind(solution.lean[1,],inflows[-nrow(inflows),])
  ever.been.here.from.inflows = as.data.frame( apply(ever.been.here.from.inflows,2,cumsum)  )
  ever.been.here.from.inflows$time = SEIR.object$solution$time
  
  ever.been.here.from.outflows = rbind(0*solution.lean[1,],outflows[-nrow(outflows),])
  ever.been.here.from.outflows = as.data.frame( apply(ever.been.here.from.outflows,2,cumsum)  )
  ever.been.here.from.outflows = ever.been.here.from.outflows[,colnames(solution.lean)] + solution.lean
  ever.been.here.from.outflows$time = SEIR.object$solution$time
  
  list(ever.been.here.from.inflows=ever.been.here.from.inflows, ever.been.here.from.outflows=ever.been.here.from.outflows)
}

if(FALSE)
{
  # Compare 1 age group results with 5 age groups
  results.1age  = "provide some object"          
  results.5ages = "provide some object"  
  mat.range = c()
  for(this.box in setdiff(colnames(results.1age$solution),c( "time","N_tot" )) )
  {
    somme = apply(results.5ages$solution[,paste0(this.box,1:5)],1,sum)
    this.range = range((results.1age$solution[,this.box] - somme)/(1e-9+pmax(results.1age$solution[,this.box] , somme)) ) 
    mat.range = rbind(mat.range,this.range)
    cat("\n",this.box,"\t", this.range)   # fairly good agreement between results.2 and results.5ages
  }
  matplot(seq(nrow(mat.range)),mat.range)
}
