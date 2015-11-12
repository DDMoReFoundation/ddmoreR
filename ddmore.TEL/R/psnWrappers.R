
#' VPC.PsN
#' Performs a VPC (Visual Predictive Check) of a given model using PsN.
#' @param model An object of class \linkS4class{mogObj} or an MDL file. Will be
#' 		  passed on directly to execute()
#' @param command A string with the vpc command. Default is vpc. 
#' @param samples An integer indicating the number of samples to run. Must be at least 20.
#' @param seed An integer with a random seed to pass to vpc.
#' @param vpcOptions (Optional) String containing any PsN vpc options
#'   	  except -samples, -seed, and currently unsupported file options.
#' @param plot (Optional) Logical dictating if a default Xpose plot should
#'        be displayed upon successful job completion. Default is true.
#' @param subfolder (Optional) Specify the name of a subfolder, within the directory
#'        containing the model file, in which to store the results. Default
#'        is a timestamped folder with prefix vpc_ .
#' @param ...
#' @return An object of class \linkS4class{StandardOutputObject} for simulated 
#' 		   observations
#' 
#' @author Kajsa Harling
#' @export
VPC.PsN <- function(model, command="vpc", samples, seed, vpcOptions="", plot=TRUE, subfolder=paste0("vpc_",format(Sys.time(), "%Y%b%d%H%M%S")), ...) {

  vpccommand <- paste0(command," --samples=", samples," --seed=", seed, " ", vpcOptions)
  
  #TODO cannot handle collect=FALSE
  #TODO loading SO can take a long time, boolean option importSO to execute() would be nice
  
  outputObject <- execute(model, target="PsNgeneric", addargs=vpccommand, subfolder=subfolder, ...)
  
  if (plot) {
    resultsDir <- .resolveResultsDirectory(model, subfolder);
    
    vpctab <- .findResultFile(resultsDir,'^vpctab[[:digit:]]*$')
    vpc.info <- .findResultFile(resultsDir,"^vpc_results\\.csv$")
    logfile <- .findResultFile(resultsDir,"\\.psn\\.log$")
    
    results <- list(SO=outputObject,vpc.info=vpc.info,logFile=logfile,vpctab=vpctab,folder=file_path_as_absolute(resultsDir))
    
    if (!is.null(vpctab) && !is.null(vpc.info)){
      #TODO special types of VPC
      library("xpose4")
      plot <-xpose.VPC(vpc.info=results$vpc.info,vpctab=results$vpctab)
      print(plot)
    } else {
      if (!is.null(logfile)){
        warning('vpc failed, could not find vpctab file for plotting. Check error messages in ',results$logFile)
      } else {
        warning('vpc failed, could not find vpctab file for plotting in ',resultsDir)
      }
    }
  }
  outputObject
}

#' bootstrap.PsN
#' Runs a PsN bootstrap of a given model.
#' @param model An object of class \linkS4class{mogObj} or an MDL file. Will be
#' 		  passed on directly to execute()
#' @param command A string with the bootstrap command. Default is bootstrap. 
#' @param samples An integer indicating the number of samples to run.
#' @param seed An integer with a random seed to pass to bootstrap.
#' @param bootstrapOptions (Optional) String containing any PsN bootstrap options
#'   	  except -samples.
#' @param plot (Optional) Logical dictating if default Xpose plots should
#'        be displayed upon successful job completion. Default is true.
#' @param subfolder (Optional) Specify the name of a subfolder, within the directory
#'        containing the model file, in which to store the results. Default
#'        is a timestamped folder with prefix bootstrap_ .
#' @param ...
#' @return An object of class \linkS4class{StandardOutputObject}
#' 
#' @author Kajsa Harling
#' @param ...
#' @export
bootstrap.PsN <- function(model, command="bootstrap", samples, seed, bootstrapOptions="", plot=TRUE, subfolder=paste0("bootstrap_",format(Sys.time(), "%Y%b%d%H%M%S")), ...) {
  bootstrapcommand <- paste0(command," --samples=", samples," --seed=", seed, " ", bootstrapOptions)
  
  outputObject <- execute(model, target="PsNgeneric", addargs=bootstrapcommand, subfolder=subfolder, ...)
  
  if (plot) {
    resultsDir <- .resolveResultsDirectory(model, subfolder);
    
    rawresults <- .findResultFile(resultsDir,"raw_results_.*\\.csv$")
    incl.ids.file <- .findResultFile(resultsDir,"^included_individuals1\\.csv$")
    logfile <- .findResultFile(resultsDir,"\\.psn\\.log$")
    
    results <- list(SO=outputObject,rawresults=rawresults,incl.ids.file=incl.ids.file,logFile=logfile,folder=file_path_as_absolute(resultsDir))
    
    library("xpose4")
    if(packageVersion("xpose4")<"4.5.0") { 
      warning("xpose4 version must be 4.5.0 or later for bootstrap plot")
    } else {
      #TODO check output object errors before doing things here
      if ( (!is.null(results$rawresults)) && (!is.null(results$incl.ids.file)) ){
        plots<-boot.hist(results.file=results$rawresults,incl.ids.file=results$incl.ids.file)
        print(plots)
      }else{
        warning('could not find bootstrap results file for plotting in ',resultsDir)
      }
    }
  }
  outputObject
}

#' SSE.PsN
#' Performs SSE (Stochastic Simulation and Estimation) on a given model.
#' @param model An object of class \linkS4class{mogObj} or an MDL file. Will be
#' 		  passed on directly to execute()
#' @param command A string with the sse command. Default is sse. 
#' @param samples An integer indicating the number of samples to run. Must be at least 2.
#' @param seed An integer with a random seed to pass to sse. 
#' @param sseOptions (Optional) String containing any PsN sse options
#'   	  except -samples, -seed and currently unsupported file options.
#' @param subfolder (Optional) Specify the name of a subfolder, within the directory
#'        containing the model file, in which to store the results. Default
#'        is a timestamped folder with prefix sse_ .
#' @param ...
#' @return An object of class \linkS4class{StandardOutputObject} for simulations and 
#' 		   possibly estimations
#' 
#' @author Kajsa Harling
#' @export
SSE.PsN <- function(model, command="sse", samples, seed, sseOptions="", subfolder=paste0("sse_",format(Sys.time(), "%Y%b%d%H%M%S")), ...) {
  ssecommand <- paste0(command," --samples=", samples," --seed=", seed, " ", sseOptions)
  
  #TODO loading SO can take a long time, boolean option importSO to execute() would be nice
  #TODO If collect is set to false we do not get result files back, and no SO object. Cannot handle that here
  
  multiple <- (samples > 1)
  outputObject <- execute(model, target="PsNgeneric", addargs=ssecommand, subfolder=subfolder, importMultipleSO=multiple, ...)
  
  outputObject
}

#' sim.PsN
#' Simulate with a given model (via the PsN script nca)
#' @param model An object of class \linkS4class{mogObj} or an MDL file. Will be
#' 		  passed on directly to execute()
#' @param samples An integer indicating the number of samples to run. Must be at least 20.
#' @param dv (Optional) String indicating name of dependent variable. Default is DV.
#' @param idv (Optional) String indicating name of independent variable. Default is TIME.
#' @param columns (Optional) String or vector of strings containing extra columns to append. Default is none.
#' @param allColumns (Optional) Logical dictating if all columns input should be output. Default is false.
#' @param rawresFile (Optional) String indicating name of file if simulating with uncertainty. Default is none.
#' @param rawresFile (Optional) Integer indicating lines to skip in rawres file. Default is 1.
#' @param subfolder (Optional) Specify the name of a subfolder, within the directory
#'        containing the model file, in which to store the results. Default
#'        is a timestamped folder with prefix sim_.
#' @return An object of class \linkS4class{StandardOutputObject}
#' 
#' @author Gunnar Yngman
#' @export
sim.PsN <- function(model, samples, dv="DV", idv="TIME", columns="", allColumns=FALSE, rawresFile="", rawresOffset=1, subfolder=paste0("sim_",format(Sys.time(), "%Y%b%d%H%M%S"))) {
  # Options for columns to include
  ncaOptions <- ifelse(allColumns, " --include_all_columns", " ")
  if (!any(columns == "")) {
    ncaOptions <- paste0(ncaOptions, " --columns=", paste(columns, collapse=","))
  }

  # Dependent and independent variable options
  ncaOptions <- paste0(ncaOptions, " --dv=", dv)
  ncaOptions <- paste0(ncaOptions, " --idv=", idv)

  # Rawres file options
  extraFile <- NULL
  if (rawresFile != "") {
    ncaOptions <- paste0(ncaOptions, " --rawres_input=", rawresFile)
    ncaOptions <- paste0(ncaOptions, " --offset_rawres=", rawresOffset)
    extraFile <- rawresFile
  }

  # Final command including mandatory number of samples
  ncacommand <- paste0("nca --samples=", samples, ncaOptions)

  #TODO loading SO can take a long time, boolean option importSO to execute() would be nice
  #TODO If collect is set to false we do not get result files back, and no SO object. Cannot handle that here

  outputObject <- execute(model, target="PsNgeneric", addargs=ncacommand, subfolder=subfolder, importMultipleSO=FALSE, extraInputFiles=extraFile)

  outputObject
}

#Helper function to find file with pattern in resultsDir 
#Return string with absolute path file name, or NULL if no file matching pattern found, or if more than one found
.findResultFile <- function(resultsDir,pattern) {
  file <- grep(pattern,dir(resultsDir),fixed=FALSE,invert=FALSE,value=TRUE)
  if (length(file)==1){
    result <- file_path_as_absolute(file.path(resultsDir,file[1]))
  } else {
    result <- NULL
  }
  result
}

