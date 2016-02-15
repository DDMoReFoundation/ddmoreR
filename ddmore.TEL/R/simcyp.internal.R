####################################################################################################
#' Execute a Simcyp command
#'
#' Prepares a job for submission including populating the execution file, submits the job, monitors 
#' the progress, and then returns the results. Any files generated as part of the execution are 
#' moved into a unique subdirectory within the working directory.
#'
#' @param executionCommand The type of Simcyp command to be executed. Should be one of the items in 
#' \code{\link{simcyp.getCommonValues}()$executionCommands}.
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
#' @seealso \code{\link{simcyp.getCommonValues}}
#' @seealso \code{\link{simcyp.isQueryCommand}}
#' @seealso \code{\link{SOObject}}
#'
#' @examples \dontrun{
#'
#' commands <- simcyp.getCommonValues()$executionCommands
#'
#' # Execute the query job to retrieve population IDs silently.
#' simcyp.execute(commands$getPopIds, list(q = NULL), TRUE)
#' # TRUE
#'
#' # Execute the query job to retrieve population IDs silently.
#' simcyp.execute(commands$getOutputIds, list(u = NULL), TRUE)
#' # TRUE
#'
#' # Execute a simulation.
#' simcyp.execute(commands$simulate, list(w = "C:/Path/To/Workspace.wksz"))
#' # SOObject
#'
#' # Execute a population generation.
#' simcyp.execute(commands$generateSinglePop, list(p = "SIM-JAPANESE", n = 20))
#' # SOObject
#'
#' # Execute a multiple population generation.
#' simcyp.execute(commands$generateMultiPops, 
#' 				  list(p = "SIM-JAPANESE,SIM-HEALTHY_VOLUNTEERS", n = 20,20))
#' # SOObject
#' }
#'
#' @author Craig Lewin (Simcyp)
simcyp.execute <- function(executionCommand,
						   executionParameters,
						   silentMode = FALSE) 
{
	simcypCommonVals <- simcyp.getCommonValues()
	executionCommands <- simcypCommonVals$executionCommands
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
		keys <- simcypCommonVals$configFileKeys
		vals <- simcypCommonVals$configFileValues
		
		simcyp.setConfigFileValue(keys$logFile, vals$logFile)
		simcyp.setConfigFileValue(keys$signalFile, vals$signalFile)
		simcyp.setConfigFileValue(keys$statusFile, vals$statusFile)

		executionParameters$c <- simcypCommonVals$configFile
	}

	if (silentMode == FALSE) 
	{
		if (executionCommand == executionCommands$simulate) 
		{
			# Running a simulation
			message(paste0("\tWorkspace: ", executionParameters$w))
		} 
		else if (executionCommand == executionCommands$generateSinglePop) 
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
		else if (executionCommand == executionCommands$generateMultiPops) 
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
	executionFile <- simcypCommonVals$configFile
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

	# Move all of the resulting files into a new folder within the working directory (so that files 
	# aren't overwritten when subsequent jobs are executed). Don't do this for query commands 
	# because the files are deleted after use.
	if (isQueryCommand == FALSE) 
	{
		if (silentMode == FALSE) 
		{
			message("Finalising:")
		}

		outputsPath <- file.path(workingDirectory, 
								 sprintf("%s-results-%s",
										 names(executionCommands)[match(executionCommand, 
														 				executionCommands)],
							 			 job@id))

		simcyp.moveResults(outputsPath)

		simcyp.getResultsDirectory.result <<- outputsPath

		if (silentMode == FALSE) 
		{
			message(paste0("\tResults directory: ", outputsPath))
		}
	}

	if (success) 
	{
		if (isQueryCommand) 
		{
			result <- TRUE
		} 
		else 
		{
			soPath <- file.path(outputsPath, simcypCommonVals$standardOutputFileName)

			if (silentMode == FALSE) 
			{
				message(paste0("\tLoading Standard Output file: ", soPath))
			}

			result <- LoadSOObject(soPath)
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
}

####################################################################################################
#' Retrieve a value from the config / execution file
#'
#' Reads the contents of the config file (which is also the execution file) and searches the lines 
#' (each line is structured as <key>=<value>) for a specific <key> so that its corresponding <value> 
#' can be extracted.
#'
#' @param configFileKey The <key> part of a line in the config file to retrieve the <value> for. 
#' Should be one of the items in \code{\link{simcyp.getCommonValues()}$configFileKeys}.
#'
#' @return The <value> associated with the given <key>.
#'
#' @seealso \code{\link{simcyp.getCommonValues}}
#'
#' @examples
#' simcyp.getConfigFileValue(simcyp.getCommonValues()$configFileKeys$embedDataInSO)
#'
#' @author Craig Lewin (Simcyp)
simcyp.getConfigFileValue <- function(configFileKey) 
{
	simcypCommonVals <- simcyp.getCommonValues()

	if (file.exists(simcypCommonVals$configFile)) 
	{
		configText <- readChar(simcypCommonVals$configFile, 
							   file.info(simcypCommonVals$configFile)$size)

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
#' Get Simcyp-specific constants
#'
#' Retrieves a list of constant values for use by multiple Simcyp-specific R functions within the 
#' DDMoRe R package.
#'
#' @return A list of common values used by Simcyp-specific R functions.
#'
#' @examples
#' simcypConsts <- simcyp.getCommonValues()
#' cmd <- simcypConsts$executionCommands$simulate
#'
#' @author Craig Lewin (Simcyp)
simcyp.getCommonValues <- function() 
{
	return(list(configFile = file.path(getwd(), "simcyp.config"),
				outputFile = file.path(getwd(), "simcyp.output"),
				fisFolder = file.path(getwd(), ".fis"),
				dataFolder = file.path(getwd(), "data"),
				scriptsFolder = file.path(getwd(), "scripts"),
				logsFolder = file.path(getwd(), "logs"),
				standardOutputFileName = "simcyp_standard_output.xml",
				configFileKeys = list(logDir = "log_dir",
									  logFile = "log_file",
									  signalFile = "control_file",
									  statusFile = "status_file",
									  includedOutput = "otable_yes",
									  embedDataInSO = "#embed_data"),
				configFileValues = list(logFile = "simcyp.log",
										signalFile = "simcyp.signal",
										statusFile = "simcyp.status"),
				executionCommands = list(simulate = "simulation",
										 generateSinglePop = "single population generation",
										 generateMultiPops = "multiple populations generation",
										 getPopIds = "population IDs retrieval",
										 getOutputIds = "output IDs retrieval")))
}


####################################################################################################
#' Check if a an execution command is a query
#'
#' Checks if a given execution command is considered to be a query command or not. A query command 
#' is one of those which retrieves IDs and does not generate any populations or run a simulation.
#'
#' @param executionCommand The execution command which should be checked to determine if it is a 
#' query command. Should be one of the items in 
#' \code{\link{simcyp.getCommonValues()}$executionCommands}.
#'
#' @return TRUE if the given execution command is considered to be a query, otherwise FALSE.
#'
#' @seealso \code{\link{simcyp.getCommonValues}}
#'
#' @examples
#' commands <- simcyp.getCommonValues()$executionCommands
#'
#' simcyp.isQueryCommand(commands$getPopIds)
#' simcyp.isQueryCommand(commands$simulate)
#'
#' @author Craig Lewin (Simcyp)
simcyp.isQueryCommand <- function(executionCommand) 
{
	simcypCommonVals <- simcyp.getCommonValues()
	
	if (executionCommand == simcypCommonVals$executionCommands$getPopIds)
	{
		return(TRUE)
	}
	
	if (executionCommand == simcypCommonVals$executionCommands$getOutputIds)
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
	simcypCommonVals <- simcyp.getCommonValues()
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
				configKeys <- simcypCommonVals$configFileKeys
				simcypStatusDirectory <- simcyp.getConfigFileValue(configKeys$logDir)

				if (is.null(simcypStatusDirectory) == FALSE) 
				{
					simcypStatusFileName <- simcyp.getConfigFileValue(configKeys$statusFile)

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
#' simcyp.MoveResults("C:/Path/To/New/Location")
#' }
#'
#' @author Craig Lewin (Simcyp)
simcyp.moveResults <- function(newLocation) 
{
	if (dir.create(newLocation)) 
	{
		simcypCommonVals <- simcyp.getCommonValues()

		outputFiles <- list.files(getwd(), "*.*", full.names = TRUE, recursive = FALSE)
		outputFiles <- outputFiles[file.info(outputFiles)$isdir == FALSE]
		outputFiles <- outputFiles[toupper(file_ext(outputFiles)) != "WKSZ"]
		outputFiles <- outputFiles[toupper(file_ext(outputFiles)) != "WKSX"]
		file.copy(outputFiles, newLocation)
		unlink(outputFiles, TRUE, TRUE)

		if (file.exists(simcypCommonVals$fisFolder)) 
		{
			file.copy(simcypCommonVals$fisFolder, newLocation, recursive = TRUE)
			unlink(simcypCommonVals$fisFolder, TRUE, TRUE)
		}

		if (file.exists(simcypCommonVals$dataFolder)) 
		{
			file.copy(simcypCommonVals$dataFolder, newLocation, recursive = TRUE)
			unlink(simcypCommonVals$dataFolder, TRUE, TRUE)
		}

		if (file.exists(simcypCommonVals$scriptsFolder)) 
		{
			file.copy(simcypCommonVals$scriptsFolder, newLocation, recursive = TRUE)
			unlink(simcypCommonVals$scriptsFolder, TRUE, TRUE)
		}

		if (file.exists(simcypCommonVals$logsFolder)) 
		{
			file.copy(simcypCommonVals$logsFolder, newLocation, recursive = TRUE)
			unlink(simcypCommonVals$logsFolder, TRUE, TRUE)
		}
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
	simcypCommonVals <- simcyp.getCommonValues()

	if (file.exists(simcypCommonVals$outputFile)) 
	{
		# Get only the lines which begin with an integer greater than 0 followed by a tab
		lines <- readLines(simcypCommonVals$outputFile)
		lines <- lines[str_detect(lines, '\t')]
		lines <- lines[as.integer(str_sub(lines, 0, str_locate(lines, '\t')[1, 2])) > 0]

		return(lapply(lines, function(line) { str_sub(line, str_locate(line, '\t')[1, 1] + 1) }))
	}

	return(NULL)
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
	simcypCommonVals <- simcyp.getCommonValues()

	simcyp.getPopulationIds()
	simcyp.getOutputIds()

	if (file.exists(simcypCommonVals$configFile)) 
	{
		if (unlink(simcypCommonVals$configFile, TRUE, TRUE) != 0) 
		{
			stop("Failed to delete existing execution file in ", getwd())
		}
	}

	if (file.create(simcypCommonVals$configFile)) 
	{
		if (is.logical(embedDataInSO)) 
		{
			simcyp.setConfigFileValue(simcypCommonVals$configFileKeys$embedDataInSO, embedDataInSO)
		} 
		else 
		{
			stop(paste0("The embedDataInStandardOutput parameter must be a logical value"))
		}	
				
		if (is.null(outputIds)) 
		{
			for (outputId in simcyp.getOutputIds()) 
			{
				simcyp.setConfigFileValue(simcypCommonVals$configFileKeys$includedOutput, outputId)
			}
		} 
		else if (is.list(outputIds) | is.vector(outputIds)) 
		{
			for (outputId in outputIds) 
			{
				simcyp.setConfigFileValue(simcypCommonVals$configFileKeys$includedOutput, outputId)
			}
		} 
		else 
		{
			stop("The outputIds parameter should be a list or vector of IDs")
		}
	} 
	else 
	{
		stop("Failed to create execution file in ", getwd())
	}
}


####################################################################################################
#' Add an entry to the config / execution file
#'
#' Appends a <key>=<value> line to the config file (which is also the execution file). Does not 
#' check if the <key> already exists in the file.
#'
#' @param configFileKey The <key> part of the new line to append. Should be one of the items in 
#' \code{\link{simcyp.getCommonValues()}$configFileKeys}.
#' 
#' @param configFileValue The <value> part of the new line to append. Possibly one of the items in 
#' \code{\link{simcyp.getCommonValues()}$configFileValues}.
#'
#' @seealso \code{\link{simcyp.getCommonValues}}
#'
#' @examples
#' simcyp.setConfigFileValue(simcyp.getCommonValues()$configFileKeys$embedDataInSO, TRUE)
#'
#' @author Craig Lewin (Simcyp)
simcyp.setConfigFileValue <- function(configFileKey, configFileValue) 
{
	write(sprintf("%s=%s", configFileKey, configFileValue),
		  simcyp.getCommonValues()$configFile,
		  append = TRUE)
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
