####################################################################################################
#' Create global environment objects
#'
#' Initialises any global objects which are used by other Simcyp functions. Removed via the
#' \code{\link{simcyp.destroyGlobals}} function.
#' 
#' @param newConfigFile A logical value which determines if a new temporary config file name is
#' retrieved. This name should only be set once at the beginning of a Simcyp execution.
#'
#' @seealso \code{\link{simcyp.destroyGlobals}}
#'
#' @examples 
#' simcyp.createGlobals()
#' 
#' # Simcyp work...
#' 
#' simcyp.destroyGlobals()
#'
#' @author Craig Lewin (Simcyp)
simcyp.createGlobals <- function()
{
	configFile <- file.path(getwd(), "simcyp-console.config")
	configFileName <- file_path_sans_ext(basename(configFile))
	
	unlink(configFile, TRUE, TRUE)
	file.create(configFile)
	
	assign("simcyp.input.config", configFile, globalenv())
	
	assign("simcyp.input.config.key.logFolder", "log_dir", globalenv())
	assign("simcyp.input.config.key.logFile", "log_file", globalenv())
	assign("simcyp.input.config.key.signalFile", "control_file", globalenv())
	assign("simcyp.input.config.key.statusFile", "status_file", globalenv())
	assign("simcyp.input.config.key.includedOutput", "otable_yes", globalenv())
	assign("simcyp.input.config.key.embedData", "#embed_data", globalenv())
	
	assign("simcyp.output.file.standardOutput", paste0(configFileName, ".SO.xml"), globalenv())
	assign("simcyp.output.file.combineArchive", paste0(configFileName, ".CA.omex"), globalenv())
	
	assign("simcyp.output.folder.fis", ".fis", globalenv())
	assign("simcyp.output.folder.csv", "csv", globalenv())
	assign("simcyp.output.folder.lua", "lua", globalenv())
	assign("simcyp.output.folder.txt", "txt", globalenv())
	assign("simcyp.output.folder.tmp", "tmp", globalenv())
	
	assign("simcyp.executionCommand.simulate", "simulation", globalenv())
	assign("simcyp.executionCommand.generateSinglePopulation", "single population generation", globalenv())
	assign("simcyp.executionCommand.generateMultiplePopulations", "multiple populations generation", globalenv())
	assign("simcyp.executionCommand.getPopulationIds", "population IDs retrieval", globalenv())
	assign("simcyp.executionCommand.getOutputIds", "output IDs retrieval", globalenv())
}


####################################################################################################
#' Destroy global environment objects
#'
#' Removes any global objects which were used by other Simcyp functions. Created via the
#' \code{\link{simcyp.createGlobals}} function.
#'
#' @seealso \code{\link{simcyp.createGlobals}}
#'
#' @examples 
#' simcyp.createGlobals()
#' 
#' # Simcyp work...
#' 
#' simcyp.destroyGlobals()
#'
#' @author Craig Lewin (Simcyp)
simcyp.destroyGlobals <- function()
{
	rm(list = ls(pattern = "^simcyp\\.input\\..+$"))
	rm(list = ls(pattern = "^simcyp\\.output\\..+$"))
	rm(list = ls(pattern = "^simcyp\\.executionCommand\\..+$"))
}


####################################################################################################
#' Execute a Simcyp command
#'
#' Prepares a job for submission including populating the execution file, submits the job, monitors 
#' the progress, and then returns the results. Any files generated as part of the execution are 
#' moved into a unique subdirectory within the working directory.
#'
#' @param executionCommand The type of Simcyp command to be executed. Should be one of the
#' \code{simcyp.executionCommand.*} objects defined in \code{\link{simcyp.preprocess}}.
#'
#' @param executionParameters A list of named values which will be converted to command line 
#' arguments for the Simcyp Console application. E.g. 
#' \code{list(p = "SIM-HEALTH_VOLUNTEERS", n = 25)} would become the command line arguments 
#' \code{-p=SIM-HEALTHY_VOLUNTEERS -n=25}.
#'
#' @param silentMode TRUE if messages should be output in the console during the execution of the 
#' job to acquire output IDs, FALSE if no output should be displayed.
#'
#' @return An object of type \code{\link{SOObject}} if successful and the command isn't a query 
#' command, TRUE if successful and the command is a query command, NULL otherwise.
#'
#' @seealso \code{\link{simcyp.preprocess}}
#' @seealso \code{\link{simcyp.isQueryCommand}}
#' @seealso \code{\link{SOObject}}
#'
#' @examples \dontrun{
#'
#' # Execute the query job to retrieve population IDs silently.
#' simcyp.execute(simcyp.executionCommand.getPopulationIds, list(q = NULL), TRUE)
#' # TRUE
#'
#' # Execute the query job to retrieve population IDs silently.
#' simcyp.execute(simcyp.executionCommand.getOutputIds, list(u = NULL), TRUE)
#' # TRUE
#'
#' # Execute a simulation.
#' simcyp.execute(simcyp.executionCommand.simulate, list(w = "C:/Path/To/Workspace.wksz"))
#' # SOObject
#'
#' # Execute a population generation.
#' simcyp.execute(simcyp.executionCommand.generateSinglePoulation, 
#' 				  list(p = "SIM-JAPANESE", n = 20))
#' # SOObject
#'
#' # Execute a multiple population generation.
#' simcyp.execute(simcyp.executionCommand.generateMultiplePopulations, 
#' 				  list(p = "SIM-JAPANESE,SIM-HEALTHY_VOLUNTEERS", n = 20,20))
#' # SOObject
#' }
#'
#' @author Craig Lewin (Simcyp)
simcyp.execute <- function(executionCommand,
						   executionParameters,
						   silentMode = FALSE) 
{
	executionCommands <- sapply(ls(globalenv(), pattern = "^simcyp\\.executionCommand\\.\\w+$"),
								function(o) get(o, globalenv()))
	isQueryCommand <- simcyp.isQueryCommand(executionCommand)

	if (is.na(match(executionCommand, executionCommands))) 
	{
		stop(paste0("Unexpected Simcyp execution type - acceptable values are; ", 
					paste(executionCommands, collapse = ", ")))
	}

	if (silentMode == FALSE) 
	{
		title <- sprintf("# Start of Simcyp execution (%s) #", executionCommand)

		message('\n', rep('#', nchar(title)))
		message(title)
		message(rep('#', nchar(title)))
	}

	# Execution file initialisation
	if (silentMode == FALSE) 
	{
		message("Initialising execution file:")
	}

	if (isQueryCommand == FALSE) 
	{
		# Populate the config file with general information
		simcyp.setConfigFileValue(simcyp.input.config.key.logFile, "simcyp_log.txt")
		simcyp.setConfigFileValue(simcyp.input.config.key.signalFile, "simcyp_control.txt")
		simcyp.setConfigFileValue(simcyp.input.config.key.statusFile, "simcyp_status.txt")

		executionParameters$c <- simcyp.input.config
	}

	if (silentMode == FALSE) 
	{
		if (executionCommand == simcyp.executionCommand.simulate) 
		{
			# Running a simulation
			message(paste0("\tWorkspace: ", executionParameters$w))
		} 
		else if (executionCommand == simcyp.executionCommand.generateSinglePopulation) 
		{
			# Generating a single population
			if (executionParameters$n == 0) 
			{
				message(sprintf("\tPopulation: %s (population representative)", 
								executionParameters$p))
			} 
			else if (executionParameters$n == 1) 
			{
				message(sprintf("\tPopulation: %s (1 individual)", executionParameters$p))
			} 
			else 
			{
				message(sprintf("\tPopulation: %s (%i individuals)", 
								executionParameters$p, 
								executionParameters$n))
			}
		} 
		else if (executionCommand == simcyp.executionCommand.generateMultiplePopulations) 
		{
			# Generating multiple populations
			message("\tPopulations:")

			popIds <- str_split(executionParameters$p, ',')[[1]]
			popSizes <- str_split(executionParameters$n, ',')[[1]]

			for (i in 1:length(popIds)) 
			{
				popId <- popIds[i]
				popSize <- as.integer(popSizes[i])

				if (popSize == 0) 
				{
					message(sprintf("\t\t%s (population representative)", popId))
				} 
				else if (popSize == 1) 
				{
					message(sprintf("\t\t%s (1 individual)", popId))
				} 
				else 
				{
					message(sprintf("\t\t%s (%s individuals)", popId, popSize))
				}
			}
		} 
		else if (isQueryCommand) 
		{
			message(paste0("\tExecution file not required for this type of Simcyp execution - ",
						   "using a dummy file"))
		}
	}
	
	if (executionCommand == simcyp.executionCommand.simulate)
	{
		workspace <- executionParameters$w
	}

	# Format the execution parameters into command line arguments expected by the Simcyp Console
	formatExecutionParameter <- function(name) 
	{
		ifelse(isQueryCommand, 
			   paste0('-', name), 
			   sprintf('-%s="%s"', name, executionParameters[[name]]))
	}
	executionParameters <- paste(sapply(names(executionParameters), 
										formatExecutionParameter), 
								 collapse = ' ')

	# Create and submit the job
	executionType <- "simcyp"
	executionFile <- simcyp.input.config
	workingDirectory <- getwd()

	if (silentMode == FALSE) 
	{
		message("Initialising job:")
	}

	job <- createFISJob(executionType, executionFile, workingDirectory, executionParameters)
	job <- DDMORE.submitJob(job)

	if (silentMode == FALSE) 
	{
		message(paste0("\tID: ", job@id))
		message(paste0("\tWorking directory: ", workingDirectory))
		message(paste0("\tExecution type: ", executionType))
		message(paste0("\tExecution file: ", executionFile))
		message(paste0("\tExecution parameters: ", executionParameters))

		message("Monitoring progress:")
	}

	success <- simcyp.monitorProgress(job@id, silentMode)

	if (silentMode == FALSE) 
	{
		message("Finalising:")
	}
	
	# Move all of the resulting files into a new folder within the working directory.
	outputsPath <- file.path(
		workingDirectory, 
		sprintf(
			"%s_SIMCYP_%s.out", 
			basename(simcyp.input.config), 
			format(Sys.time(), "%H%M%S")))

	simcyp.moveResults(outputsPath)
	
	file.copy(simcyp.input.config, file.path(outputsPath, basename(simcyp.input.config)))
	file.remove(simcyp.input.config)
	
	if (executionCommand == simcyp.executionCommand.simulate)
	{
		file.copy(workspace, file.path(outputsPath, basename(workspace)))
	}

	assign("simcyp.output.folder.root", outputsPath, globalenv())

	if (silentMode == FALSE) 
	{
		message(paste0("\tResults directory: ", outputsPath))
	}

	if (success) 
	{
		if (isQueryCommand) 
		{
			result <- TRUE
		} 
		else 
		{
			soPath <- file.path(outputsPath, simcyp.output.file.standardOutput)

			if (silentMode == FALSE) 
			{
				message(paste0("\tLoading Standard Output file: ", soPath))
			}

			result <- NULL #LoadSOObject(soPath)
		}
	} 
	else 
	{
		result <- NULL
	}

	if (silentMode == FALSE) 
	{
		title <- sprintf("# End of Simcyp execution (%s) #", executionCommand)

		message(rep('#', nchar(title)))
		message(title)
		message(rep('#', nchar(title)), '\n')
	}
	
	if (isQueryCommand == FALSE)
	{
		simcyp.destroyGlobals()
	}
	
	return(result)
}

####################################################################################################
#' Retrieve a value from the config / execution file
#'
#' Reads the contents of the config file (which is also the execution file) and searches the lines 
#' (each line is structured as <key>=<value>) for a specific <key> so that its corresponding <value> 
#' can be extracted.
#'
#' @param configFileKey The <key> part of a line in the config file to retrieve the <value> for.
#'
#' @return The <value> associated with the given <key>.
#'
#' @examples
#' simcyp.getConfigFileValue(simcyp.input.config.key.signalFile)
#'
#' @author Craig Lewin (Simcyp)
simcyp.getConfigFileValue <- function(configFileKey) 
{
	if (file.exists(simcyp.input.config)) 
	{
		configText <- readChar(simcyp.input.config, file.info(simcyp.input.config)$size)

		if (str_detect(configText, configFileKey)) 
		{
			# The config file has been updated by the prepare handler script to include the data 
			# we're looking for. Find the <key>=<value> line where the <key> matches and extract 
			# the corresponding <value>.
			startIndex <- str_locate(configText, configFileKey)[1, 2] + 2
			configText <- str_sub(configText, startIndex)
			endIndex <- str_locate(configText, '\n')[1, 1] - 1

			if (is.na(endIndex)) 
			{
				value <- str_trim(configText)
			} 
			else 
			{
				value <- str_trim(str_sub(configText, 0, endIndex))
			}

			if (is.na(value) == FALSE) 
			{
				return(value)
			}
		}
	}

	return(NULL)
}


####################################################################################################
#' Check if a an execution command is a query
#'
#' Checks if a given execution command is considered to be a query command or not. A query command 
#' is one of those which retrieves IDs and does not generate any populations or run a simulation.
#'
#' @param executionCommand The execution command which should be checked to determine if it is a 
#' query command. Should be one of the \code{simcyp.executionCommand.*} objects defined in 
#' \code{\link{simcyp.preprocess}}.
#'
#' @return TRUE if the given execution command is considered to be a query, otherwise FALSE.
#'
#' @seealso \code{\link{simcyp.preprocess}}
#'
#' @examples
#' simcyp.isQueryCommand(simcyp.executionCommand.getPopulationIds)
#' simcyp.isQueryCommand(simcyp.executionCommand.simulate)
#'
#' @author Craig Lewin (Simcyp)
simcyp.isQueryCommand <- function(executionCommand) 
{
	if (executionCommand == simcyp.executionCommand.getPopulationIds)
	{
		return(TRUE)
	}
	
	if (executionCommand == simcyp.executionCommand.getOutputIds)
	{
		return(TRUE)
	}

	return(FALSE)
}


####################################################################################################
#' Monitor the progress of a Simcyp job
#'
#' Monitors the progress of a Simcyp job from start to end and reports information to the console if 
#' not running in silent mode.
#'
#' @param jobId The unique identifier of the \code{\link{FISJob}} being monitored.
#'
#' @param silentMode TRUE if messages should be output in the console during the execution of the 
#' job to acquire output IDs, FALSE if no output should be displayed.
#'
#' @return TRUE if the final job status is "COMPLETED", otherwise FALSE.
#'
#' @seealso \code{\link{FISJob}}
#'
#' @examples \dontrun{
#'
#' fisJob <- createFISJob(...)
#' fisJob <- DDMORE.submitJob(job)
#' success <- simcyp.monitorProgress(fisJob@@id, FALSE)
#' }
#'
#' @author Craig Lewin (Simcyp)
simcyp.monitorProgress <- function(jobId, silentMode) 
{
	jobStatus <- ""
	simStatus <- ""
	simProgress <- ""
	simStatusCheckTime <- Sys.time()
	simStatusFile <- NULL

	while (jobStatus != "COMPLETED" & jobStatus != "FAILED" & jobStatus != "CANCELLED") {
		# Job status
		job <- DDMORE.getJob(jobId)

		if (job@status != jobStatus) 
		{
			jobStatus <- job@status

			if (silentMode == FALSE) 
			{
				message(paste0("\tJob status: ", jobStatus))
			}
		}

		if (silentMode == FALSE) 
		{
			# Try to extract the location of the Simcyp Console status file from the execution file 
			# if it hasn't already been retrieved. This information may not be initially available 
			# because it is added to the execution file by the prepare handler script.
			if (is.null(simStatusFile)) 
			{
				simcypStatusDirectory <- simcyp.getConfigFileValue(simcyp.input.config.key.logFolder)

				if (is.null(simcypStatusDirectory) == FALSE) 
				{
					simcypStatusFileName <- simcyp.getConfigFileValue(simcyp.input.config.key.signalFile)

					if (is.null(simcypStatusFileName) == FALSE) 
					{
						simStatusFile <- file.path(simcypStatusDirectory, simcypStatusFileName)
					}
				}
			}

			# Read the Simcyp status file if we know its location and it has been modified since the 
			# last time it was read. Only checked every 30 seconds (used to show the status of long
			# simulations)
			if (is.null(simStatusFile) == FALSE) 
			{
				if (file.exists(simStatusFile) & simStatusCheckTime < (Sys.time() - 30)) 
				{
					nullFunc <- function(p) { return(NULL) }
					simcypStatusInfo <- tryCatch(file.info(simStatusFile),
												 warning = nullFunc,
												 error = nullFunc)

					if (is.null(simcypStatusInfo) == FALSE) 
					{
						if (simStatusCheckTime < simcypStatusInfo$mtime) 
						{
							simStatusCheckTime <- simcypStatusInfo$mtime

							simcypStatusFileContent <- 
								tryCatch(readChar(simStatusFile, file.info(simStatusFile)$size),
										 warning = nullFunc,
										 error = nullFunc)

							if (is.null(simcypStatusFileContent) == FALSE) 
							{
								if (length(simcypStatusFileContent) > 0) 
								{
									statusParts <- str_split(simcypStatusFileContent, ',')[[1]]

									if (length(statusParts) > 1) 
									{
										if (length(statusParts) > 2) 
										{
											# Contains simulation progress as well as a status
											newSimcypStatus <- statusParts[2]
											newSimcypProgress <- sprintf(paste0(": %s%% complete ",
																				"(~%s minutes ",
																				"remaining)"),
																		 statusParts[3],
																		 statusParts[4])
										} 
										else 
										{
											# Just a status
											newSimcypStatus <- statusParts[2]
											newSimcypProgress <- ""
										}

										if (newSimcypStatus != simStatus |
											newSimcypProgress != simProgress) 
										{
											simStatus <- newSimcypStatus
											simProgress <- newSimcypProgress

											# Currently only reporting the simulation progress (for 
											# longer simulations)
											if (simStatus == "SIMULATING") 
											{
												message(paste0("\tSimcyp status: ",
															   simStatus,
															   simProgress))
											}
										}
									}
								}
							}
						}
					}
				}
			}
		}

		Sys.sleep(1)
	}

	return(jobStatus == "COMPLETED")
}


####################################################################################################
#' Move the results files
#'
#' Moves all files resulting from the execution of a job into the specified location.
#'
#' @param newLocation The directory into which output files will be moved.
#'
#' @examples \dontrun{
#'
#' simcyp.moveResults("C:/Path/To/New/Location")
#' }
#'
#' @author Craig Lewin (Simcyp)
simcyp.moveResults <- function(newLocation) 
{
	if (dir.create(newLocation)) 
	{
		outputs <- sapply(ls(globalenv(), pattern = "^simcyp\\.output\\..+$"),
						  function(o) get(o, globalenv()))
		outputs <- sapply(outputs, function(o) paste0("(^", gsub("\\.", "\\\\.", o), "$)"))
		outputs <- list.files(getwd(), paste(outputs, collapse = "|"), TRUE, TRUE, FALSE)
		
		sapply(outputs, function(f) file.copy(f, newLocation, TRUE, TRUE))
		unlink(outputs, TRUE, TRUE)
	}
}


####################################################################################################
#' Parse the primary Simcyp Console output file
#'
#' Extracts IDs from the primary output file generated by the Simcyp Console (typically 
#' 'simcyp.output'). Used by \code{\link{simcyp.getPopulationIds}} and 
#' \code{\link{simcyp.getOutputIds}}.
#'
#' @return A list of IDs extracted from the output file.
#'
#' @examples
#' simcyp.parseOutputFile()
#'
#' @author Craig Lewin (Simcyp)
simcyp.parseOutputFile <- function() 
{
	potentialFiles <- list.files(simcyp.output.folder.root, "stdout\\.txt", TRUE, TRUE, TRUE)
	
	if (length(potentialFiles) > 0)
	{
		outputFile <- potentialFiles[[1]]
		
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

	stop(paste0("Failed to retrieve IDs: '", outputFile, "' not found"))
}


####################################################################################################
#' Perform preprocessing steps common to Simcyp functions
#'
#' Retrieves population IDs and Output IDs and persists them in memory to avoid subsequent lengthy 
#' jobs to retrieve them during execution. Creates the config file (which is also the execution 
#' file) and populates it with a single entry to determine what is put into a resulting Standard 
#' Output file.
#'
#' @param embedDataInStandardOutput TRUE if data from the Simcyp outputs should be embedded within 
#' the resulting Standard Output, FALSE if the outputs should simply be referenced as external files 
#' within the Standard Output.
#'
#' @examples
#' # Large SO file size.
#' simcyp.preprocess(TRUE)
#'
#' # Small SO file size.
#' simcyp.preprocess(FALSE)
#'
#' @author Craig Lewin (Simcyp)
simcyp.preprocess <- function(embedDataInSO, outputIds) 
{	
	simcyp.createGlobals()
	
	simcyp.getPopulationIds()
	simcyp.getOutputIds()
	
	if (is.logical(embedDataInSO)) 
	{
		simcyp.setConfigFileValue(simcyp.input.config.key.embedData, embedDataInSO)
	} 
	else 
	{
		stop(paste0("The embedDataInStandardOutput parameter must be a logical value"))
	}
			
	if (is.null(outputIds)) 
	{
		for (outputId in simcyp.getOutputIds()) 
		{
			simcyp.setConfigFileValue(simcyp.input.config.key.includedOutput, paste0(outputId, "+"))
		}
	} 
	else if (is.list(outputIds) | is.vector(outputIds)) 
	{
		for (outputId in outputIds) 
		{
			simcyp.setConfigFileValue(simcyp.input.config.key.includedOutput, paste0(outputId, "+"))
		}
	} 
	else 
	{
		stop("The outputIds parameter should be a list or vector of IDs")
	}
}


####################################################################################################
#' Add an entry to the config / execution file
#'
#' Appends a <key>=<value> line to the config file (which is also the execution file). Does not 
#' check if the <key> already exists in the file.
#'
#' @param configFileKey The <key> part of the new line to append.
#' 
#' @param configFileValue The <value> part of the new line to append.
#'
#' @examples
#' simcyp.setConfigFileValue(simcyp.input.config.key.embedData, TRUE)
#' simcyp.setConfigFileValue(simcyp.input.config.key.signalFile, "simcyp.signal")
#'
#' @author Craig Lewin (Simcyp)
simcyp.setConfigFileValue <- function(configFileKey, configFileValue) 
{
	write(sprintf("%s=%s", configFileKey, configFileValue), simcyp.input.config, append = TRUE)
}


####################################################################################################
#' Validate a population ID
#'
#' Checks that a given parameter can be parsed to a valid population ID and returns the parsed ID if 
#' so. This function is used by \code{\link{simcyp.generateSinglePopulation}},
#' \code{\link{simcyp.generateMultiplePopulations}}, and
#' \code{\link{simcyp.simulate}} for parameter validation.
#'
#' @param populationId The ID to try and parse / validate. Can be the numeric position of an ID 
#' within its parent list or the name of the ID.
#'
#' @return The parsed ID
#'
#' @examples
#' simcyp.validatePopulationId(1)
#' simcyp.validatePopulationId("SIM-HEALTHY_VOLUNTEERS")
#' simcyp.validatePopulationId("sim-healthy_volunteers")
#' simcyp.validatePopulationId("sim-aliens") # ERROR
#' simcyp.validatePopulationId(50) # ERROR
#'
#' @author Craig Lewin (Simcyp)
simcyp.validatePopulationId <- function(populationId) 
{
	if (is.numeric(populationId)) 
	{
		allPopulationIds <- simcyp.getPopulationIds(TRUE)

		if (populationId < 1 | populationId > length(allPopulationIds)) 
		{
			stop(sprintf(paste0("The acceptable range for specifying a population ID numerically ",
								"is 1-%i (positions of the population IDs retrieved via the ",
								"simcyp.getPopulationIds function)"),
						 length(allPopulationIds)))
		}

		return(allPopulationIds[[as.integer(populationId)]])
	} 
	else if (is.character(populationId)) 
	{
		allPopulationIds <- simcyp.getPopulationIds(TRUE)

		if (is.na(match(tolower(populationId), tolower(allPopulationIds)))) 
		{
			stop(paste0("When specifying a population ID by name it must match one of the IDs ",
						"retrieved via the simcyp.getPopulationIds function"))
		}

		return(toupper(populationId))
	} 
	else 
	{
		stop(paste0("The population ID must either be an ID found in the list retrieved via the ",
					"simcyp.getPopulationIds function or the numeric index of one such ID within ",
					"this list"))
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
#' simcyp.validatePopulationSize("100")
#' simcyp.validatePopulationSize(10000)
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
