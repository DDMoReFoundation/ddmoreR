
# Internal generic function to be called by all PsN variants to make the call to the framework
.execute.PsN.command <- function(modelfile, HOST='localhost', PORT='9010', command, args="", subfolder=format(Sys.time(), "%Y%b%d%H%M%S"), collect=TRUE, clearUp=FALSE, ...) {
	
	
	#prepareWorkingFolder copies lst-files TODO remove that and move to individual wrappers where needed
	workingDirectory <- TEL.prepareWorkingFolder(modelfile)

	fullCommand <- paste(command, args)
	cat(paste(paste("PsN command is:", fullCommand), "\n"))
	
	outputObject <- submit.job("psn.generic", workingDirectory, modelfile, HOST, PORT, addargs=fullCommand)
	
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
execute.PsN <- function(modelfile, HOST='localhost', PORT='9010', command="execute", addargs="", ...) {
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
VPC.PsN <- function(modelfile, HOST='localhost', PORT='9010', command="vpc", nsamp, seed, addargs="", cleanup=T, ...) {
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
bootstrap.PsN <- function(modelfile, HOST='localhost', PORT='9010', command="bootstrap", nsamp, seed, addargs="", cleanup=T, ...) {
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
SSE.PsN <- function(modelfile, HOST='localhost', PORT='9010', command="sse", nsamp, seed, addargs="", cleanup=T, ...) {
	args <- paste0("--samples=", nsamp, " --seed=", seed, " ", addargs)
	outputObject <- .execute.PsN.command(modelfile, HOST, PORT, command, args)
}

