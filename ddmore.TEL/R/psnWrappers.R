
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

#' ncasim.PsN
#' Simulate a number of samples with a given model (internally using the PsN script nca)
#' @param model An object of class \linkS4class{mogObj} or an MDL file. Will be
#' 		    passed on directly to execute().
#' @param samples An integer indicating the number of samples to simulate.
#'        Must be at least 20.
#' @param seed A string or numeric with a random seed for the Perl pseudo-random number generator.
#' @param dv (Optional) String indicating name of dependent variable. Default is DV.
#' @param idv (Optional) String indicating name of independent variable. Default is TIME.
#' @param vars (Optional) String or string vector containing extra variables
#'        to append simulation output. Can be combined with inputVars. Default is none.
#' @param inputVars (Optional) Logical dictating if all variables input in model
#'        also should append simulation output. Default is false (only id, idv and dv).
#' @param rawresFile (Optional) String indicating path of PsN raw results file,
#'        e.g. from an earlier bootstrap execution, if simulating with uncertainty
#'        (see PsN documentation). Default is none.
#' @param rawresOffset (Optional) Integer indicating lines to skip (excluding header)
#'        in raw results file (if used). Default is 1.
#' @param subfolder (Optional) Specify the name of a subfolder, within the directory
#'        containing the model file, in which to store the results. Default
#'        is a timestamped folder with prefix ncasim_.
#' @param ncaOptions (Optional) String containing extra PsN nca options to append
#'        (see PsN nca documentation). Default is none.
#' @param extraInputFiles (Optional) String or string vector of paths, either
#'        absolute or relative (to the model file), to any additional files to be
#'        included in the execution. Default is none or, if used, value of rawresFile.
#' @return An object of class \linkS4class{StandardOutputObject}
#' 
#' @author Gunnar Yngman
#' @export
ncasim.PsN <- function(model, samples, seed, dv="DV", idv="TIME", vars="", inputVars=FALSE, rawresFile="", rawresOffset=1, subfolder=paste0("ncasim_",format(Sys.time(), "%Y%b%d%H%M%S")), ncaOptions="", extraInputFiles="", ...) {
  # Simple argument validation
  if (!(is.numeric(samples) && length(samples) == 1 && samples >= 20)) stop("Argument 'samples' is not single numeric and >=20")
  if (!((is.character(seed) || is.numeric(seed)) && length(seed) == 1)) stop("Argument 'seed' is not single numeric or string")
  if (!(is.character(dv) && length(dv) == 1)) stop("Argument 'dv' is not single string")
  if (!(is.character(idv) && length(idv) == 1)) stop("Argument 'idv' is not single string")
  if (!(is.character(vars) && !(any(vars == "") && length(vars) > 1))) stop("Argument 'vars' is not single string or string vector")
  if (!(is.logical(inputVars) && length(inputVars) == 1)) stop("Argument 'inputVars' is not single logical")
  if (!(is.character(rawresFile) && length(rawresFile) == 1)) stop("Argument 'rawresFile' is not single string")
  if (!(is.numeric(rawresOffset) && length(rawresOffset) == 1 && rawresOffset >= 0)) stop("Argument 'rawresOffset' is not single numeric and >=0")
  if (!(is.character(subfolder) && length(subfolder) == 1)) stop("Argument 'subfolder' is not single string")
  if (!(is.character(ncaOptions) && length(ncaOptions) == 1)) stop("Argument 'ncaOptions' is not single string")
  if (!(is.character(extraInputFiles) && !(any(extraInputFiles == "") && length(extraInputFiles) > 1))) stop("Argument 'extraInputFiles' is not single string or string vector")
  
  # Options for variables to include
  ncaCommand <- ifelse(inputVars, " --include_all_columns", "")
  if (!any(vars == "")) {
    ncaCommand <- paste0(ncaCommand, " --columns=", paste(vars, collapse=","))
  }

  # Options for dependent and independent variables
  ncaCommand <- paste0(ncaCommand, " --dv=", dv)
  ncaCommand <- paste0(ncaCommand, " --idv=", idv)

  # Options for rawres/extra input files
  if(any(extraInputFiles == "")) extraInputFiles <- NULL
  if (rawresFile != "") {
    ncaCommand <- paste0(ncaCommand, " --rawres_input=", rawresFile)
    ncaCommand <- paste0(ncaCommand, " --offset_rawres=", rawresOffset)
    if(is.null(extraInputFiles)) extraInputFiles <- rawresFile
  }

  # Final command including mandatory number of samples, seed and optional extra options
  ncaCommand <- paste0("nca --samples=", samples, " --seed=", seed, ncaCommand, " ", ncaOptions)

  #TODO loading SO can take a long time, boolean option importSO to execute() would be nice
  #TODO If collect is set to false we do not get result files back, and no SO object. Cannot handle that here

  outputObject <- execute(model, target="PsNgeneric", addargs=ncaCommand, subfolder=subfolder, importMultipleSO=FALSE, extraInputFiles=extraInputFiles, ...)

  outputObject
}

#' sim.PsN
#' Simulate a number of samples with a given model (internally using the PsN script sse),
#' keeping the standard table output format for a complete population of the SO.
#' @param model An object of class \linkS4class{mogObj} or an MDL file. Will be
#' 		    passed on directly to execute().
#' @param samples An integer indicating the number of samples to simulate.
#'        Must be at least 20.
#' @param seed A string or numeric with a random seed for the Perl pseudo-random number generator.
#' @param vars (Optional) String or string vector containing extra variables
#'        to append simulation output. Default is none.
#' @param rawresFile (Optional) String indicating path of PsN raw results file,
#'        e.g. from an earlier bootstrap execution, if simulating with uncertainty
#'        (see PsN documentation). Default is none.
#' @param rawresOffset (Optional) Integer indicating lines to skip (excluding header)
#'        in raw results file (if used). Default is 1.
#' @param subfolder (Optional) Specify the name of a subfolder, within the directory
#'        containing the model file, in which to store the results. Default
#'        is a timestamped folder with prefix sim_.
#' @param sseOptions (Optional) String containing extra PsN sse options to append
#'        (see PsN sse documentation). No estimation options allowed. Default is none.
#' @param extraInputFiles (Optional) String or string vector of paths, either
#'        absolute or relative (to the model file), to any additional files to be
#'        included in the execution. Default is none or, if used, value of rawresFile.
#' @return An object of class \linkS4class{StandardOutputObject}
#' 
#' @author Gunnar Yngman
#' @export
sim.PsN <- function(model, samples, seed, vars="", rawresFile="", rawresOffset=1, subfolder=paste0("sim_",format(Sys.time(), "%Y%b%d%H%M%S")), sseOptions="", extraInputFiles="", ...) {
  # Simple argument validation
  if (!(is.numeric(samples) && length(samples) == 1 && samples >= 1)) stop("Argument 'samples' is not single numeric and >=1")
  if (!((is.character(seed) || is.numeric(seed)) && length(seed) == 1)) stop("Argument 'seed' is not single numeric or string")
  if (!(is.character(vars) && !(any(vars == "") && length(vars) > 1))) stop("Argument 'vars' is not single string or string vector")
  if (!(is.character(rawresFile) && length(rawresFile) == 1)) stop("Argument 'rawresFile' is not single string")
  if (!(is.numeric(rawresOffset) && length(rawresOffset) == 1 && rawresOffset >= 0)) stop("Argument 'rawresOffset' is not single numeric and >=0")
  if (!(is.character(subfolder) && length(subfolder) == 1)) stop("Argument 'subfolder' is not single string")
  if (!(is.character(sseOptions) && length(sseOptions) == 1)) stop("Argument 'sseOptions' is not single string")
  if (!(is.character(extraInputFiles) && !(any(extraInputFiles == "") && length(extraInputFiles) > 1))) stop("Argument 'extraInputFiles' is not single string or string vector")
	
	# Check for (illegal) estimation options
	if (grepl("-alternative_models|-estimate_simulation|-add_models|-random_estimation_inits|-out_filter|-recompute", sseOptions)) stop("Argument 'sseOptions' contains estimation options")
  
	# --no-estimate_simulation and --keep_tables are necessary (for simulation and nmoutput2so merge)
	sseCommand <- " --no-estimate_simulation --keep_tables "
	
  # Options for variables to include
  if (!any(vars == "")) {
    sseCommand <- paste0(sseCommand, " --append_columns=", paste(vars, collapse=","))
  }

  # Options for rawres/extra input files
  if(any(extraInputFiles == "")) extraInputFiles <- NULL
  if (rawresFile != "") {
    sseCommand <- paste0(sseCommand, " --rawres_input=", rawresFile)
    sseCommand <- paste0(sseCommand, " --offset_rawres=", rawresOffset)
    if(is.null(extraInputFiles)) extraInputFiles <- rawresFile
  }

  # Final command including mandatory number of samples, seed and optional extra options
  sseCommand <- paste0("sse --samples=", samples, " --seed=", seed, sseCommand, " ", sseOptions)

  #TODO loading SO can take a long time, boolean option importSO to execute() would be nice
  #TODO If collect is set to false we do not get result files back, and no SO object. Cannot handle that here

  outputObject <- execute(model, target="PsNgeneric", addargs=sseCommand, subfolder=subfolder, importMultipleSO=FALSE, extraInputFiles=extraInputFiles, ...)

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

