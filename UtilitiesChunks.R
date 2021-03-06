# This a subset of \\hird1/Saslibe$/SAS_user_defined_macros/Utilities.R (aka S:/SAS_user_defined_macros/Utilities.R)


fetch.package =function(nom_du_package,try.this=c("nothing","load","install"),verbose=FALSE)
{
  if(verbose)
    cat(paste("\n try.this =",try.this[1]))
  
  if(try.this[1] =="load")
    try( suppressPackageStartupMessages(library(nom_du_package,character.only=TRUE,quietly=TRUE)) )#,warn.conflicts=FALSE)
  
  if(try.this[1] =="install")    # This part needs more work
  {
    #   browser()
    cat("\ntTrying to install ",nom_du_package)
    #install.packages(nom_du_package)
    install.packages( pkgs = nom_du_package  )   #,repos = "file:////fld6filer/packagerepo-depotprogiciel/miniCRAN"
    found = !grepl("no package called",try(find.package(nom_du_package)) )
    if(found)
    {
      suppressPackageStartupMessages(library(nom_du_package,character.only=TRUE,quietly=TRUE))#,warn.conflicts=FALSE)
      cat("\nInstalled and loaded ",nom_du_package)
    }
    else 
      cat("\nCould not install ",nom_du_package ) # stop(paste("\nCould not install",nom_du_package) )      
  }
  
  # found=require(nom_du_package,character.only=TRUE,quietly=TRUE,warn.conflicts=FALSE)
  found=  paste("package:",nom_du_package,sep="") %in% search()
  
  if(!found && length(try.this)>1)
    found = fetch.package(nom_du_package,try.this[-1])  # recursive call
  
  
  found
}

as.data.frame.from.tbl = function(data.frame.ish,rename=NULL) # rename is vector or data.frame with at least $var.names.in
{ 
  if (!fetch.package("dplyr"))
    stop("package dplyr not found")  # needed for dplyr::is.tbl
  
  #browser()
  #BEGIN make rename = data.frame(var.names.in=..., var.names.out=..., colClass=...)
  if(is.null(rename))
    rename = names(data.frame.ish) 
  
  if(!is.data.frame(rename))
    rename = data.frame(var.names.in=rename,var.names.out=rename,stringsAsFactors = FALSE)
  
  if(!any(names(rename)=="var.names.out")) 
    rename$var.names.out=rename$var.names.in
  
  if(!any(names(rename)=="colClass"))
    rename$colClass='unspecified'
  #END   make rename = data.frame(var.names.in=..., var.names.out=...)
  
  this.is.tbl = dplyr::is.tbl(data.frame.ish)
  
  simple.df=data.frame(iota...seq=seq(nrow(data.frame.ish)))
  for(k in 1:nrow(rename))
  {
    if(this.is.tbl)
    {
      if(rename$colClass[k] == "character"  ) simple.df[,rename$var.names.out[k]] = as.character( data.frame.ish[,rename$var.names.in[k]][[1]] )
      if(rename$colClass[k] == "numeric"    ) simple.df[,rename$var.names.out[k]] = as.numeric  ( data.frame.ish[,rename$var.names.in[k]][[1]] ) 
      if(rename$colClass[k] == "unspecified") simple.df[,rename$var.names.out[k]] =             ( data.frame.ish[,rename$var.names.in[k]][[1]] )      
    }
    else
      simple.df[,rename$var.names.out[k]] =  data.frame.ish[,rename$var.names.in[k]]  
  }
  
  simple.df$iota...seq = NULL   
  simple.df
}
