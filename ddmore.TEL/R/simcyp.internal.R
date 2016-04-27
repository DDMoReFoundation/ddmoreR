####################################################################################################
#' Perform a Simcyp execution
#'
#' Prepares and submits a Simcyp execution. This includes creation and population of the execution
#' file and any necessary parsing / copying / deleting of resulting files. All results are placed
#' in a folder within the user's working directory. Some executions leave no resulting files (i.e.
#' when \code{executionArgs} contains either of the names 'q' or 'u').
#'
#' @param executionArgs A list of named values which will be converted to command line 
#' arguments for the Simcyp Console application. E.g. 
#' \code{list(p = "SIM-HEALTH_VOLUNTEERS", n = 25)} would become the command line arguments 
#' \code{-p=SIM-HEALTHY_VOLUNTEERS -n=25}.
#' 
#' @param embedDataInSO TRUE if data from the Simcyp outputs should be embedded within 
#' the resulting Standard Output, FALSE if the outputs should simply be referenced as external files
#' within the Standard Output.
#' 
#' @param importSO TRUE if the resulting Standard Output file should be loaded into memory and
#' returned as an object of type \code{\link{SOObject}} upon successful execution. FALSE if NULL 
#' should be returned.
#' 
#' @param outputIDs A list of identifiers indicating the types of outputs to generate. Each 
#' identifier should be one of the items in \code{\link{simcyp.getOutputIDs}} or a numeric value 
#' which is the position of an ID within this list. Unused if \code{executionArgs} contains either 
#' of the names 'q' or 'u'.
#'
#' @return An object of type \code{\link{SOObject}} if; a) execution is successful, b) 
#' \code{executionArgs} doesn't contain either of the names 'q' or 'u', and c) \code{importSO} is 
#' TRUE. Otherwise NULL is returned.
#'
#' @seealso \code{\link{SOObject}}
#' ~seealso \code{\link{simcyp.getOutputIDs}}
#'
#' @examples \dontrun{
#'
#' # Execute a query which returns a list of output IDs.
#' simcyp.execute(list(q = NULL), FALSE, FALSE, NULL)
#'
#' # Execute a query which returns a list of population IDs.
#' simcyp.execute(list(u = NULL), FALSE, FALSE, NULL)
#'
#' # Execute a simulation which returns a SOObject of the resulting large SO file, and generates all 
#' # possible Simcyp optional outputs.
#' simcyp.execute(list(w = "C:/Path/To/Workspace.wksz"), TRUE, TRUE, NULL)
#'
#' # Execute a population generation which returns NULL, generates a small SO file, and generates 
#' # possible Simcyp optional outputs.
#' simcyp.execute(list(p = "SIM-JAPANESE", n = 20), FALSE, FALSE, simcyp.getOutputIDs())
#' # SOObject
#'
#' # Execute a multiple populations generation which returns NULL, generates a small SO file, and 
#' # generates only 2 Simcyp optional outputs.
#' ids <- simcyp.getOutputIDs()
#' simcyp.execute(
#' 		list(p = "SIM-JAPANESE,SIM-HEALTHY_VOLUNTEERS", n = 20,20), 
#' 		FALSE, 
#' 		FALSE, 
#' 		c(ids[[1]], ids[[12]]))
#' }
#'
#' @author Craig Lewin (Simcyp)
simcyp.execute <- function(executionArgs, embedDataInSO, importSO, outputIDs)
{	
	if (is.null(executionArgs) | is.list(executionArgs) == FALSE | length(executionArgs) == 0)
	{
		stop("'executionArgs' parameter must be a list containing at least 1 argument")
	}
	
	workingDirectory <- getwd()
	tempDirectory <- tempdir()
	isOutputIDsQuery <- is.na(match("u", names(executionArgs))) == FALSE
	isPopulationIDsQuery <- is.na(match("q", names(executionArgs))) == FALSE
	isSimulation <- is.na(match("w", names(executionArgs))) == FALSE
	isSinglePopulationGeneration <- FALSE
	isMultiplePopulationsGeneration <- FALSE
	isPopulationGeneration <- is.na(match("p", names(executionArgs))) == FALSE
	
	if (isPopulationGeneration)
	{
		isSinglePopulationGeneration <- str_detect(executionArgs$p, ",") == FALSE
		isMultiplePopulationsGeneration <- str_detect(executionArgs$p, ",")
	}
	
	if (isSimulation)
	{
		file.copy(executionArgs$w, file.path(tempDirectory, basename(executionArgs$w)))
	}
	
	# Perform the execution in a temporary folder to avoid residual files appearing in the user's
	# working directory.
	setwd(tempDirectory)
	
	if (isOutputIDsQuery | isPopulationIDsQuery)
	{
		# Execution file isn't necessary for a query job but it is required by the execute function, 
		# so just create an empty dummy file to use.
		if (isOutputIDsQuery)
		{
			executionFile <- "outputIDs.config"
		}
		
		if (isPopulationIDsQuery)
		{
			executionFile <- "populationIDs.config"
		}
		
		if (file.create(executionFile) == FALSE)
		{
			setwd(workingDirectory)
			stop(paste0("Failed to create temporary execution file: ", executionFile))
		}
	}
	else
	{
		# Execution file is needed, so create and populate it.
		if (isSimulation)
		{
			name <- file_path_sans_ext(basename(executionArgs$w))
			name <- gsub("_", "-", name)
			name <- gsub(" ", "-", name)
			
			executionFile <- paste0(name, ".config")
		}
		
		if (isSinglePopulationGeneration)
		{
			name <- str_sub(executionArgs$p, 5)
			name <- gsub("_", "-", name)
					
			executionFile <- paste0(name, ".config")
		}
		
		if (isMultiplePopulationsGeneration)
		{
			numPops <- switch(str_count(executionArgs$p, ","), "Two", "Three", "Four")
			executionFile <- paste0(numPops, "-Populations.config")
		}
		
		if (file.create(executionFile))
		{			
			if (length(outputIDs) > 1) 
			{
				for (outputID in sapply(outputIDs, simcyp.validateOutputID))
				{
					write(sprintf("%s=%s+", "otable_yes", outputID), executionFile, append = TRUE)
				}
			} 
			else 
			{
				setwd(workingDirectory)
				stop("The outputIDs parameter should be a list containing multiple IDs")
			}
			
			write(sprintf("%s=%s", "#embed_data", embedDataInSO), executionFile, append = TRUE)
			write(sprintf("%s=%s", "log_file", "simcyp_log.txt"), executionFile, append = TRUE)
			write(sprintf("%s=%s", "status_file", "simcyp_status.txt"), executionFile, append = TRUE)
			write(sprintf("%s=%s", "control_file", "simcyp_control.txt"), executionFile, append = TRUE)
		}
		else
		{
			setwd(workingDirectory)
			stop(paste0("Failed to create execution file: ", executionFile))
		}
	}
	
	addargs <- ""
	executionArgs$c <- executionFile
	subfolder <- sprintf("%s_SIMCYP_%s.out", executionFile, format(Sys.time(), "%H%M%S"))
	
	for (argName in names(executionArgs))
	{
		argValue <- executionArgs[[argName]]
		
		if (is.null(argValue))
		{
			addargs <- paste(addargs, paste0("-", argName), collapse = " ")
		}
		else
		{
			addargs <- paste(addargs, sprintf("-%s=\"%s\"", argName, argValue), collapse = " ")
		}
	}
	
	addargs <- sub("^\\s+", "", addargs) # Remove leading whitespace
	extraInputFiles <- NULL
	
	if (isSimulation)
	{
		extraInputFiles <- basename(executionArgs$w)
		message("\n-- Simulate using workspace file")
	}
	else if (isSinglePopulationGeneration)
	{
		message("\n-- Generate single population")
	}
	else if (isMultiplePopulationsGeneration)
	{
		message("\n-- Generate multiple populations")
	}
	else if (isOutputIDsQuery)
	{
		message("\n-- Get available output IDs")
	}
	else if (isPopulationIDsQuery)
	{
		message("\n-- Get available population IDs")
	}
		
	result <- ddmore:::execute(
		x = executionFile,
		target = "simcyp",
		addargs = addargs,
		subfolder = subfolder,
		extraInputFiles = extraInputFiles,
		importSO = importSO)

	if (isOutputIDsQuery | isPopulationIDsQuery)
	{		
		result <- simcyp.getIDsFromOutput(file.path(getwd(), subfolder, "stdout.txt"))
		
		# Delete all files from the temprorary folder.
		unlink(executionFile, TRUE, TRUE)
		unlink(subfolder, TRUE, TRUE)
	}
	else
	{
		# Move the standard output and error files into the 'txt' folder.
		txtFolder <- file.path(subfolder, "txt")
		stdoutFile <- file.path(subfolder, "stdout.txt")
		stderrFile <- file.path(subfolder, "stderr.txt")
		
		dir.create(txtFolder, FALSE)
		
		if (file.exists(txtFolder) &
			file.exists(stdoutFile) &
			file.exists(stderrFile))
		{
			file.rename(stdoutFile, file.path(txtFolder, basename(stdoutFile)))
			file.rename(stderrFile, file.path(txtFolder, basename(stderrFile)))
		}
		
		# Move all results from the temporary folder into the user's working directory.
		file.rename(subfolder, file.path(workingDirectory, subfolder))
		
		# Clean up residual files in the temporary folder.
		unlink(executionFile, TRUE, TRUE)
		
		if (isSimulation)
		{
			unlink(basename(executionArgs$w), TRUE, TRUE)
		}
	}
	
	setwd(workingDirectory)
	
	return(result)
}


####################################################################################################
#' Extract IDs from the primary Simcyp Console output file
#'
#' Extracts IDs from the primary output file generated by the Simcyp Console (typically 
#' 'simcyp.output'). Used by \code{\link{simcyp.getPopulationIDs}} and 
#' \code{\link{simcyp.getOutputIDs}}.
#'
#' @param outputFile The primary output file to parse IDs from.
#' 
#' @return A list of IDs extracted from the output file.
#' 
#' @seealso \code{\link{simcyp.getPopulationIDs}}
#' @seealso \code{\link{simcyp.getOutputIDs}}
#'
#' @examples
#' simcyp.getIDsFromOutput()
#'
#' @author Craig Lewin (Simcyp)
simcyp.getIDsFromOutput <- function(outputFile) 
{
	if (file.exists(outputFile)) 
	{
		ids <- list()
		
		for (line in readLines(outputFile))
		{
			if (str_detect(line, '\t'))
			{
				suppressWarnings(
					id <- as.integer(str_sub(line, 1, str_locate(line, '\t')[1, 2] - 1)))
				
				if (is.na(id) == FALSE & id > 0)
				{
					ids <- c(ids, str_sub(line, str_locate(line, '\t')[1, 1] + 1))
				}
			}
		}
		
		return(ids)
	}
	
	return(NULL)
}


####################################################################################################
#' Perform Simcyp execution preprocessing
#'
#' Performs some preprocessing prior to execution of \code{\link{simcyp.simulate}},
#' \code{\link{simcyp.generateSinglePopulation}}, or 
#' \code{\link{simcyp.generateMultiplePopulations}}. Avoids having to execute additional tasks (to
#' acquire IDs) mid-execution by performing the tasks beforehand instead. 
#'
#' @examples
#' simcyp.preprocess()
#'
#' @author Craig Lewin (Simcyp)
simcyp.preprocess <- function()
{
	simcyp.getPopulationIDs()
	simcyp.getOutputIDs()
}


####################################################################################################
#' Validate an output ID
#'
#' Checks that a given parameter can be parsed to a valid output ID and returns the parsed ID if 
#' so. This function is used by \code{\link{simcyp.generateSinglePopulation}},
#' \code{\link{simcyp.generateMultiplePopulations}}, and
#' \code{\link{simcyp.simulate}} for parameter validation.
#'
#' @param outputID The ID to try and parse / validate. Can be the numeric position of an ID within
#' its parent list or the name of the ID.
#'
#' @return The parsed ID
#'
#' @examples
#' simcyp.validateOutputID(1)
#' simcyp.validateOutputID("DEMOGRAPHIC")
#' simcyp.validateOutputID("demographic")
#' simcyp.validateOutputID("profiles") #ERROR
#' simcyp.validateOutputID(50) #ERROR
#'
#' @author Craig Lewin (Simcyp)
simcyp.validateOutputID <- function(outputID) 
{
	if (is.numeric(outputID)) 
	{
		allOutputIDs <- simcyp.getOutputIDs()
		
		if (outputID < 1 | outputID > length(allOutputIDs)) 
		{
			stop(sprintf(
				paste0("The acceptable range for specifying an output ID numerically is 1-%i ",
					"(positions of the output IDs retrieved via the simcyp.getOutputIDs ",
					"function)"),
				length(allOutputIDs)))
		}
		
		return(allOutputIDs[[as.integer(outputID)]])
	} 
	else if (is.character(outputID)) 
	{
		allOutputIDs <- simcyp.getOutputIDs()
		
		if (is.na(match(tolower(outputID), tolower(allOutputIDs)))) 
		{
			stop(paste0("When specifying an output ID by name it must match one of the IDs ",
				"retrieved via the simcyp.getOutputIDs function"))
		}
		
		return(toupper(outputID))
	} 
	else 
	{
		stop(paste0("The output ID must either be an ID found in the list retrieved via the ",
			"simcyp.getOutputIDs function or the numeric index of one such ID within this ",
			"list"))
	}
}


####################################################################################################
#' Validate a population ID
#'
#' Checks that a given parameter can be parsed to a valid population ID and returns the parsed ID if 
#' so. This function is used by \code{\link{simcyp.generateSinglePopulation}},
#' \code{\link{simcyp.generateMultiplePopulations}}, and
#' \code{\link{simcyp.simulate}} for parameter validation.
#'
#' @param populationID The ID to try and parse / validate. Can be the numeric position of an ID 
#' within its parent list or the name of the ID.
#'
#' @return The parsed ID
#'
#' @examples
#' simcyp.validatePopulationID(1)
#' simcyp.validatePopulationID("SIM-HEALTHY_VOLUNTEERS")
#' simcyp.validatePopulationID("sim-healthy_volunteers")
#' simcyp.validatePopulationID("sim-aliens") #ERROR
#' simcyp.validatePopulationID(50) #ERROR
#'
#' @author Craig Lewin (Simcyp)
simcyp.validatePopulationID <- function(populationID) 
{
	if (is.numeric(populationID)) 
	{
		allPopulationIDs <- simcyp.getPopulationIDs()
		
		if (populationID < 1 | populationID > length(allPopulationIDs)) 
		{
			stop(sprintf(
				paste0("The acceptable range for specifying a population ID numerically is 1-%i ",
					"(positions of the population IDs retrieved via the simcyp.getPopulationIDs ",
					"function)"),
				length(allPopulationIDs)))
		}
		
		return(allPopulationIDs[[as.integer(populationID)]])
	} 
	else if (is.character(populationID)) 
	{
		allPopulationIDs <- simcyp.getPopulationIDs()
		
		if (is.na(match(tolower(populationID), tolower(allPopulationIDs)))) 
		{
			stop(paste0("When specifying a population ID by name it must match one of the IDs ",
				"retrieved via the simcyp.getPopulationIDs function"))
		}
		
		return(toupper(populationID))
	} 
	else 
	{
		stop(paste0("The population ID must either be an ID found in the list retrieved via the ",
			"simcyp.getPopulationIDs function or the numeric index of one such ID within this ",
			"list"))
	}
}


####################################################################################################
#' Validate a population size
#'
#' Checks that a given parameter is a valid population size. This function is used by 
#' \code{\link{simcyp.generateSinglePopulation}} and
#' \code{\link{simcyp.generateMultiplePopulations}} for parameter validation.
#'
#' @param populationSize The population size to validate. Should be a numeric value.
#'
#' @return The population size
#'
#' @examples
#' simcyp.validatePopulationSize(0)
#' simcyp.validatePopulationSize(1)
#' simcyp.validatePopulationSize("100") #ERROR
#' simcyp.validatePopulationSize(10000) #ERROR
#'
#' @author Craig Lewin (Simcyp)
simcyp.validatePopulationSize <- function(populationSize) 
{
	if (is.numeric(populationSize)) 
	{
		if (populationSize < 0 | populationSize > 5000) 
		{
			stop(paste0("Population size must be 0 (for population representative) or greater (up ",
							"to a maximum of 5000)"))
		}
		
		return(as.integer(populationSize))
	} 
	else 
	{
		stop("Population size should be numeric (acceptable range is 0-5000)")
	}
}