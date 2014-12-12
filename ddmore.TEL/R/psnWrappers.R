
#' VPC.PsN
#' Performs a VPC (Visual Predictive Check) of a given model using PsN.
#' @param model An object of class \linkS4class{mogObj} or an MDL file. Will be
#' 		  passed on directly to estimate()
#' @param command A string with the vpc command. Default is vpc. 
#' @param samples An integer indicating the number of samples to run. Must be at least 20.
#' @param vpcOptions (Optional) String containing any PsN vpc options
#'   	  except -samples and currently unsupported file options.
#' @param plot (Optional) Logical dictating if a default Xpose plot should
#'        be displayed upon successful job completion. Default is true.
#' @param subfolder (Optional) Specify the name of a subfolder, within the directory
#'        containing the model file, in which to store the results. Default
#'        is a timestamped folder with prefix vpc_ .
#' @param clearUp (Optional) Logical dictating if the job working directory should
#'        be deleted upon successful job completion. Default is false, since this
#'        directory may contain useful information in the event that a job failed
#'        to execute successfully.
#' @param ...
#' @return A list of file names: vpc.info and vpctab to be used for plotting with Xpose, and logFile in the case of run failure
#' 
#' @author Kajsa Harling
#' @export
VPC.PsN <- function(model, command="vpc", samples, vpcOptions="", plot=TRUE, subfolder=paste0("vpc_",format(Sys.time(), "%Y%b%d%H%M%S")),clearUp=FALSE,  ...) {
	vpccommand <- paste0(command," --samples=", samples, " ", vpcOptions)
	submission <- estimate(model, target="PsNgeneric", collect=FALSE, subfolder=subfolder, addargs=vpccommand, ...)
	outputFolder <- file.path(submission$sourceDirectory, subfolder)
	submission <- TEL.importFiles(submission, target=outputFolder, clearUp=clearUp)

	vpctab <- grep('^vpctab[[:digit:]]*$',dir(outputFolder),fixed=FALSE,invert=FALSE,value=TRUE)
	if (length(vpctab)==1){
	   resultFiles <- list(vpc.info=file.path(outputFolder,"vpc_results.csv"),vpctab=file.path(outputFolder,vpctab[1]),folder=outputFolder)
	   if (plot) {
	   	  #TODO special types of VPC
	   	  library("xpose4")
	   	  plot <-xpose.VPC(vpc.info=resultFiles$vpc.info,vpctab=resultFiles$vpctab)
       	  print(plot)
		}
	} else {
	  #we can only get here if psngeneric connector sets exit 0 even if return status of vpc command was non-zero
	  logfile <- grep('.psn.log$',dir(outputFolder),fixed=FALSE,invert=FALSE,value=TRUE)
	  if (length(logfile)==1){
	  	 resultFiles <- list(folder=outputFolder,logFile=file.path(outputFolder,logfile[1]))
	  	 warning('vpc failed, could not find vpctab file. Check error messages in ',resultFiles$logFile)
	  } else {
	  	 resultFiles <- list(folder=outputFolder)
	  	 warning('vpc failed, could not find vpctab file in ',outputFolder)
	  }
	}	   		   
	resultFiles
}

#' bootstrap.PsN
#' Runs a PsN bootstrap of a given model.
#' @param model An object of class \linkS4class{mogObj} or an MDL file. Will be
#' 		  passed on directly to estimate()
#' @param command A string with the bootstrap command. Default is bootstrap. 
#' @param samples An integer indicating the number of samples to run.
#' @param bootstrapOptions (Optional) String containing any PsN bootstrap options
#'   	  except -samples.
#' @param plot (Optional) Logical dictating if default Xpose plots should
#'        be displayed upon successful job completion. Default is true.
#' @param subfolder (Optional) Specify the name of a subfolder, within the directory
#'        containing the model file, in which to store the results. Default
#'        is a timestamped folder with prefix bootstrap_ .
#' @param ...
#' @return The results from executing the MDL file, and adding bootstrap results, in the form of an object of
#'         class \linkS4class{StandardOutputObject}.
#' 
#' @author Kajsa Harling
#' @param ...
#' @export
bootstrap.PsN <- function(model, command="bootstrap", samples, bootstrapOptions="", plot=TRUE, subfolder=paste0("bootstrap_",format(Sys.time(), "%Y%b%d%H%M%S")), ...) {
	bootstrapcommand <- paste0(command," --samples=", samples, " ", bootstrapOptions)
	outputObject <- estimate(model, target="PsNgeneric", addargs=bootstrapcommand, subfolder=subfolder, ...)
	if (plot) {
	   library("xpose4")
	   if(packageVersion("xpose4")<"4.5.0") { 
			cat("xpose4 version must be 4.5.0 or later for bootstrap plot")
		} else {
		  rawresults <- grep('^raw_results_[[:alnum:]]+.csv$',dir(subfolder),fixed=FALSE,invert=FALSE,value=TRUE)
		  bootresults <- grep('bootstrap_results.csv',dir(subfolder),fixed=TRUE,invert=FALSE,value=TRUE)
		  #TODO check output object errors before doing things here
		  if (length(rawresults)==1 && length(bootresults)==1 ){
	   	  	 plots<-boot.hist(results.file=file.path(subfolder,rawresults[1]),incl.ids.file=file.path(subfolder,"included_individuals1.csv"))
       	  	 print(plots)
			} else {
	  		  #we can only get here if psngeneric connector sets exit 0 even if return status of bootstrap command was non-zero
	  		  logfile <- grep('.psn.log$',dir(subfolder),fixed=FALSE,invert=FALSE,value=TRUE)
	  		  if (length(logfile)==1) {	
	  	 	  	 warning('could not find bootstrap results in ',subfolder,'. Check error messages in ',file.path(subfolder,logfile[1]))
	  			} else {
	  	 	  	 warning('could not find bootstrap results in ',subolder)
	  			}

			}
	    }
	}
	outputObject	   		   
}

#' SSE.PsN
#' Performs SSE (Stochastic Simulation and Estimation) on a given model.
#' @param model An object of class \linkS4class{mogObj} or an MDL file. Will be
#' 		  passed on directly to estimate()
#' @param command A string with the sse command. Default is sse. 
#' @param samples An integer indicating the number of samples to run. Must be at least 2.
#' @param sseOptions (Optional) String containing any PsN sse options
#'   	  except -samples and currently unsupported file options.
#' @param subfolder (Optional) Specify the name of a subfolder, within the directory
#'        containing the model file, in which to store the results. Default
#'        is a timestamped folder with prefix sse_ .
#' @param clearUp (Optional) Logical dictating if the job working directory should
#'        be deleted upon successful job completion. Default is false, since this
#'        directory may contain useful information in the event that a job failed
#'        to execute successfully.
#' @param ...
#' @return A list of file names: rawresults and sseresults to be used for plotting with Xpose, and logFile in the case of run failure
#' 
#' @author Kajsa Harling
#' @export
SSE.PsN <- function(model, command="sse", samples, sseOptions="", subfolder=paste0("sse_",format(Sys.time(), "%Y%b%d%H%M%S")), clearUp=FALSE, ...) {
	ssecommand <- paste0(command," --samples=", samples, " ", sseOptions)
	submission <- estimate(model, target="PsNgeneric", collect=FALSE, subfolder=subfolder, addargs=ssecommand, ...)
	outputFolder <- file.path(submission$sourceDirectory, subfolder)
	submission <- TEL.importFiles(submission, target=outputFolder, clearUp=clearUp)
	rawresults <- grep('^raw_results_[[:alnum:]]+.csv$',dir(outputFolder),fixed=FALSE,invert=FALSE,value=TRUE)
	sseresults <- grep('sse_results.csv',dir(outputFolder),fixed=TRUE,invert=FALSE,value=TRUE)
	if (length(sseresults)==1){
	   resultFiles <- list(sseresults=file.path(outputFolder,"sse_results.csv"),rawresults=file.path(outputFolder,rawresults[1]),folder=outputFolder)
	} else {
	  #we can only get here if psngeneric connector sets exit 0 even if return status of sse command was non-zero
	  logfile <- grep('.psn.log$',dir(outputFolder),fixed=FALSE,invert=FALSE,value=TRUE)
	  if (length(logfile)==1){
	  	 resultFiles <- list(folder=outputFolder,logFile=file.path(outputFolder,logfile[1]))
	  	 warning('sse failed, could not find sse_results file. Check error messages in ',resultFiles$logFile)
	  } else {
	  	 resultFiles <- list(folder=outputFolder)
	  	 warning('vpc failed, could not find sse_results file in ',outputFolder)
	  }
	}	   		   
	resultFiles

}

