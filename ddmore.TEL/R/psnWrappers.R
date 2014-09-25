
# Internal generic function to be called by all PsN variants to make the call to the framework
.execute.PsN.command <- function(modelfile, HOST='localhost', PORT='9010', command, args="", subfolder=format(Sys.time(), "%Y%b%d%H%M%S"), collect=TRUE, clearUp=FALSE, ...) {
	
	# Strip off leading path and obtain NONMEM .ctl file from the provided .mdl (or .ctl) file; this is what PsN will execute upon
	control_file_without_path <- paste0(file_path_sans_ext(tail(strsplit(file_path_as_absolute(modelfile), '/')[[1]], n=1)), ".ctl")
	
	fullCommand <- paste(command, shQuote(control_file_without_path), args)
	cat(paste(paste("PsN command is:", fullCommand), "\n"))
	
	workingDirectory <- TEL.prepareWorkingFolder(modelfile)
	
	outputObject <- submit.job("cmdline.execute", workingDirectory, modelfile, HOST, PORT, addargs=fullCommand)
	
	submitResponse = outputObject$ret
	
	if (submitResponse[[1]] == 0) {
		
		if (collect) {
			
			outputObject <- TEL.poll(outputObject, HOST=HOST, PORT=PORT) 
			
			outputObject$resultsDir = file.path(outputObject$sourceDirectory, subfolder)
			
			if (outputObject$status == "COMPLETED") {
				
				outputObject <- TEL.import(outputObject, target=outputObject$resultsDir, clearUp=clearUp)
	
			} else { # outputObject$status != "COMPLETED"
				stop(paste(c("Execution of model ", outputObject$modelFile, " failed.\n  The contents of the working directory ", outputObject$workingDirectory, " may be useful for tracking down the cause of the failure."), sep=""))
			}
		
		}

	} else {
		stop("Submission of execution request was unsuccessful.")
	}
	
}

#' execute.PsN
#' Estimates parameters using PsN Execute.
#' @param modelfile
#' @param HOST
#' @param PORT
#' @param command
#' @param addargs
#' @param ...
#' @export
execute.PsN <- function(modelfile, HOST='localhost', PORT='9010', command="execute-3.6.2.bat", addargs="", ...) {
	outputObject <- .execute.PsN.command(modelfile, HOST, PORT, command, addargs)
}

#' VPC.PsN
#' Performs a VPC (Visual Predictive Check) of a given model using PsN.
#' @param modelfile
#' @param HOST
#' @param PORT
#' @param command
#' @param nsamp
#' @param seed
#' @param addargs
#' @param cleanup
#' @param ...
#' @export
VPC.PsN <- function(modelfile, HOST='localhost', PORT='9010', command="vpc-3.6.2.bat", nsamp, seed, addargs="", cleanup=T, ...) {
	args <- paste0("--samples=", nsamp, " --seed=", seed, " ", addargs)
	outputObject <- .execute.PsN.command(modelfile, HOST, PORT, command, args)
}

#' bootstrap.PsN
#' Performs bootstrapping to generate new datasets from the dataset of a given model.
#' @param modelfile
#' @param HOST
#' @param PORT
#' @param command
#' @param nsamp
#' @param seed
#' @param addargs
#' @param cleanup
#' @param ...
#' @export
bootstrap.PsN <- function(modelfile, HOST='localhost', PORT='9010', command="bootstrap-3.6.2.bat", nsamp, seed, addargs="", cleanup=T, ...) {
	args <- paste0("--samples=", nsamp, " --seed=", seed, " ", addargs)
	outputObject <- .execute.PsN.command(modelfile, HOST, PORT, command, args)
}

#' SSE.PsN
#' Performs SSE (Stochastic Simulation and Estimation) on a given model.
#' @param modelfile
#' @param HOST
#' @param PORT
#' @param command
#' @param nsamp
#' @param seed
#' @param addargs
#' @param cleanup
#' @param ...
#' @export
SSE.PsN <- function(modelfile, HOST='localhost', PORT='9010', command="sse-3.6.2.bat", nsamp, seed, addargs="", cleanup=T, ...) {
	args <- paste0("--samples=", nsamp, " --seed=", seed, " ", addargs)
	outputObject <- .execute.PsN.command(modelfile, HOST, PORT, command, args)
}

