
#' VPC.PsN
#' Performs a VPC (Visual Predictive Check) of a given model using PsN.
#' @param model An object of class \linkS4class{mogObj} or an MDL file. Will be
#' 		  passed on directly to estimate()
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
#' @return A list with an object of class \linkS4class{StandardOutputObject} for simulated 
#' 		   observations and a set of file names: vpc.info and 
#' 		   vpctab to be used for plotting with Xpose, and logFile in the case of run failure
#' 
#' @author Kajsa Harling
#' @export
VPC.PsN <- function(model, command="vpc", samples, seed, vpcOptions="", plot=TRUE, subfolder=paste0("vpc_",format(Sys.time(), "%Y%b%d%H%M%S")), ...) {

	vpccommand <- paste0(command," --samples=", samples," --seed=", seed, " ", vpcOptions)

	#TODO cannot handle collect=FALSE
	#TODO loading SO can take a long time, boolean option importSO to estimate would be nice
	#2015-01-14 importSO does not yet parse SimulatedProfiles

	outputObject <- estimate(model, target="PsNgeneric", subfolder=subfolder, addargs=vpccommand, ...)
    resultsDir <- .resolveResultsDirectory(model, subfolder);

	vpctab <- grep('^vpctab[[:digit:]]*$',dir(resultsDir),fixed=FALSE,invert=FALSE,value=TRUE)

	if (length(vpctab)==1){
	   results <- list(SO=outputObject,vpc.info=file_path_as_absolute(file.path(resultsDir,"vpc_results.csv")),vpctab=file_path_as_absolute(file.path(resultsDir,vpctab[1])),folder=file_path_as_absolute(resultsDir))
	   if (plot) {
	   	  #TODO special types of VPC
	   	  library("xpose4")
	   	  plot <-xpose.VPC(vpc.info=results$vpc.info,vpctab=results$vpctab)
       	  print(plot)
		}
	} else {
	  #we can only get here if psngeneric connector sets exit 0 even if return status of vpc command was non-zero
	  logfile <- grep('.psn.log$',dir(resultsDir),fixed=FALSE,invert=FALSE,value=TRUE)
	  if (length(logfile)==1){
	  	 results <- list(SO=outputObject,folder=file_path_as_absolute(resultsDir),logFile=file_path_as_absolute(file.path(resultsDir,logfile[1])))
	  	 warning('vpc failed, could not find vpctab file. Check error messages in ',results$logFile)
	  } else {
	  	 results <- list(SO=outputObject)
	  	 warning('vpc failed, could not find vpctab file in ',resultsDir)
	  }
	}
	results
}

#' bootstrap.PsN
#' Runs a PsN bootstrap of a given model.
#' @param model An object of class \linkS4class{mogObj} or an MDL file. Will be
#' 		  passed on directly to estimate()
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
#' @return A list with an object of class \linkS4class{StandardOutputObject}.
#' 
#' @author Kajsa Harling
#' @param ...
#' @export
bootstrap.PsN <- function(model, command="bootstrap", samples, seed, bootstrapOptions="", plot=TRUE, subfolder=paste0("bootstrap_",format(Sys.time(), "%Y%b%d%H%M%S")), ...) {
	bootstrapcommand <- paste0(command," --samples=", samples," --seed=", seed, " ", bootstrapOptions)

	#2015-01-14 importSO does not yet parse Bootstrap PercentilesCI
	outputObject <- estimate(model, target="PsNgeneric", addargs=bootstrapcommand, subfolder=subfolder, ...)

    resultsDir <- .resolveResultsDirectory(model, subfolder);
    
	if (plot) {
	   library("xpose4")
	   if(packageVersion("xpose4")<"4.5.0") { 
			cat("xpose4 version must be 4.5.0 or later for bootstrap plot")
		} else {
		  rawresults <- grep('^raw_results_[[:alnum:]]+.csv$',dir(resultsDir),fixed=FALSE,invert=FALSE,value=TRUE)
		  bootresults <- grep('bootstrap_results.csv',dir(resultsDir),fixed=TRUE,invert=FALSE,value=TRUE)
		  #TODO check output object errors before doing things here
		  if (length(rawresults)==1 && length(bootresults)==1 ){
	   	  	 plots<-boot.hist(results.file=file.path(resultsDir,rawresults[1]),incl.ids.file=file.path(resultsDir,"included_individuals1.csv"))
       	  	 print(plots)
			} else {
	  		  #we can only get here if psngeneric connector sets exit 0 even if return status of bootstrap command was non-zero
	  		  logfile <- grep('.psn.log$',dir(resultsDir),fixed=FALSE,invert=FALSE,value=TRUE)
	  		  if (length(logfile)==1) {	
	  	 	  	 warning('could not find bootstrap results in ',resultsDir,'. Check error messages in ',file.path(resultsDir,logfile[1]))
	  			} else {
	  	 	  	 warning('could not find bootstrap results in ',resultsDir)
	  			}

			}
	    }
	}
	list(SO=outputObject)
}

#' SSE.PsN
#' Performs SSE (Stochastic Simulation and Estimation) on a given model.
#' @param model An object of class \linkS4class{mogObj} or an MDL file. Will be
#' 		  passed on directly to estimate()
#' @param command A string with the sse command. Default is sse. 
#' @param samples An integer indicating the number of samples to run. Must be at least 2.
#' @param seed An integer with a random seed to pass to sse. 
#' @param sseOptions (Optional) String containing any PsN sse options
#'   	  except -samples, -seed and currently unsupported file options.
#' @param subfolder (Optional) Specify the name of a subfolder, within the directory
#'        containing the model file, in which to store the results. Default
#'        is a timestamped folder with prefix sse_ .
#' @param ...
#' @return A list with an object of class \linkS4class{StandardOutputObject} for simulations and 
#' 		   possibly estimations, and a set of file names: rawresults and 
#' 		   sseresults to be used for plotting with Xpose, and logFile in the case of run failure
#' 
#' @author Kajsa Harling
#' @export
SSE.PsN <- function(model, command="sse", samples, seed, sseOptions="", subfolder=paste0("sse_",format(Sys.time(), "%Y%b%d%H%M%S")), ...) {
	ssecommand <- paste0(command," --samples=", samples," --seed=", seed, " ", sseOptions)

	#TODO loading SO can take a long time, boolean option importSO to estimate would be nice
	#TODO If collect is set to false we do not get result files back, and no SO object. Cannot handle that here
	#2015-01-14 importSO can only handle -samples=1 -no-est in order to get only one block in SO, otherwise exits with error. SimulatedProfiles not read

	outputObject <- estimate(model, target="PsNgeneric", subfolder=subfolder, addargs=ssecommand, ...)

    resultsDir <- .resolveResultsDirectory(model, subfolder);

	sseresultsfile <- grep('sse_results.csv',dir(resultsDir),fixed=TRUE,invert=FALSE,value=TRUE)

	#this will not exist if only doing simulation
	rawresultsfile <- grep('^raw_results_[[:alnum:]]+.csv$',dir(resultsDir),fixed=FALSE,invert=FALSE,value=TRUE)

	if (length(sseresultsfile)==1){
		if (length(rawresultsfile)==1){
	   	   results <- list(SO=outputObject,sseresults=file_path_as_absolute(file.path(resultsDir,"sse_results.csv")),
		   		   rawresults=file_path_as_absolute(file.path(resultsDir,rawresultsfile[1])),folder=file_path_as_absolute(resultsDir))
		} else {
	   	   results <- list(SO=outputObject,sseresults=file_path_as_absolute(file.path(resultsDir,"sse_results.csv")),folder=file_path_as_absolute(resultsDir))
		}
	} else {
	  #we can only get here if psngeneric connector sets exit 0 even if return status of sse command was non-zero
	  logfile <- grep('.psn.log$',dir(subfolder),fixed=FALSE,invert=FALSE,value=TRUE)
	  if (length(logfile)==1){
	  	 resuls <- list(SO=outputObject,folder=file_path_as_absolute(resultsDir),logFile=file_path_as_absolute(file.path(resultsDir,logfile[1])))
	  	 warning('sse failed, could not find sse_results file. Check error messages in ',results$logFile)
	  } else {
	  	 results <- list(SO=outputObject)
	  	 warning('sse failed, could not find sse_results file in ',resultsDir)
	  }
	}
	results
}

