####################################################################################################
#' Generate multiple Simcyp populations
#'
#' Generates between 1 and 4 Simcyp populations of specific types and sizes and then returns the 
#' resulting Standard Output loaded into memory (\code{\link{SOObject}}).
#'
#' @param populationIds A collection of identifiers indicating the types of populations to generate. 
#' Each identifier should be one of the items in \code{\link{simcyp.getPopulationIds}} or a numeric 
#' value which is the position of an ID within this collection. A maximum of 4 IDs are accepted - 
#' any subsequent IDs will be ignored.
#'
#' @param populationSizes A collection of the number of individuals to generate for each given 
#' population. Each item must be a numeric value in the range 0-5000 where 0 indicates that a 
#' population representative should be used. A single numeric value may also be used if the same 
#' population size is desired for all of the populations. A maximum of 4 sizes are accepted - any 
#' subsequent sizes will be ignored.
#'
#' @param embedDataInStandardOutput TRUE if data from the Simcyp outputs should be embedded within 
#' the resulting Standard Output, FALSE if the outputs should simply be referenced as external files
#' within the Standard Output.
#' 
#' @param outputIds A collection of identifiers indicating the types of outputs to generate. Each 
#' identifier should be one of the items in \code{\link{simcyp.getOutputIds}} or a numeric value 
#' which is the position of an ID within this collection. If omitted or NULL, all possible outputs 
#' will be generated.
#'
#' @return An object of type \code{\link{SOObject}} if successful, otherwise NULL.
#'
#' @seealso \code{\link{simcyp.getPopulationIds}}
#' @seealso \code{\link{simcyp.getOutputIds}}
#' @seealso \code{\link{SOObject}}
#'
#' @examples
#' # Generates 20 individuals of the healthy Chinese population, 70 individuals of the healthy 
#' # volunteers population and produces a large SO.
#' so1 <- simcyp.generateSinglePopulation(c("SIM-CHINESE_HEALTHY", "SIM-HEALTHY_VOLUNTEERS"),
#' 										  c(20, 70))
#'
#' # Generates 10 individuals for each of the healthy Chinese, healthy volunteers, japanese, and 
#' # obese populations and produces a small SO.
#' so2 <- simcyp.generateSinglePopulation(c(0, "SIM-HEALTHY_VOLUNTEERS", "SIM-JAPANESE", 11),
#'   									  10,
#'   									  FALSE)
#'
#' @author Craig Lewin (Simcyp)
#'
#' @export
simcyp.generateMultiplePopulations <- function(populationIds,
			 								   populationSizes = c(0),
											   embedDataInStandardOutput = TRUE,
											   outputIds = NULL)
{
	simcyp.preprocess(embedDataInStandardOutput, outputIds)

	if (length(populationIds) > 1) 
	{
		popIds <- paste(head(sapply(populationIds, simcyp.validatePopulationId), 4), collapse = ',')
	} 
	else 
	{
		stop(paste0("The populationIds parameter should be a list containing multiple IDs - to ",
					"generate a single population use simcyp.generateSinglePopulation instead"))
	}
	
	if (length(populationSizes) == 1 & is.numeric(populationSizes)) 
	{
		numSizes <- min(length(populationIds), 4)
		validSizes <- rep(simcyp.validatePopulationSize(populationSizes), numSizes)
		popSizes <- paste(validSizes, collapse = ",")
	} 
	else if (length(populationSizes) > 1) 
	{
		popSizes <- paste(head(sapply(populationSizes, 
									  simcyp.validatePopulationSize), 
							   4), 
					      collapse = ',')
	}
	else 
	{
		stop(paste0("The populationSizes parameter should either be a single integer which ",
					"determines a population size for all populations or a list of integers ","
					identifying the individual population sizes for each population identified ",
					"by the populationIds list"))
	}
	
	cmd <- simcyp.getCommonValues()$executionCommands$generateMultiPops
	params <- list(p = popIds, n = popSizes)

	return(simcyp.execute(cmd, params))
}


####################################################################################################
#' Generate a Simcyp population
#'
#' Generates a Simcyp population of a specific type and size and then returns the resulting Standard 
#' Output loaded into memory (\code{\link{SOObject}}).
#'
#' @param populationId The identifier indicating the type of population to generate. Should be one 
#' of the items in \code{\link{simcyp.getPopulationIds}} or a numeric value which is the position of 
#' an ID within this collection.
#'
#' @param populationSize The number of individuals to generate for the given population. Must be a 
#' numeric value in the range 0-5000 where 0 indicates that a population representative should be 
#' used.
#'
#' @param embedDataInStandardOutput TRUE if data from the Simcyp outputs should be embedded within 
#' the resulting Standard Output, FALSE if the outputs should simply be referenced as external files 
#' within the Standard Output.
#' 
#' @param outputIds A collection of identifiers indicating the types of outputs to generate. Each 
#' identifier should be one of the items in \code{\link{simcyp.getOutputIds}} or a numeric value 
#' which is the position of an ID within this collection. If omitted or NULL, all possible outputs 
#' will be generated.
#'
#' @return An object of type \code{\link{SOObject}} if successful, otherwise NULL.
#'
#' @seealso \code{\link{simcyp.getPopulationIds}}
#' @seealso \code{\link{SOObject}}
#'
#' @examples
#' # Generates a single population representative of the healthy Chinese population and produces a 
#' # large SO.
#' so1 <- simcyp.generateSinglePopulation("SIM-CHINESE_HEALTHY", 0)
#'
#' # Generates 25 individuals of the healthy volunteers population and produces a small SO.
#' so2 <- simcyp.generateSinglePopulation(7, 25, FALSE)
#'
#' @author Craig Lewin (Simcyp)
#'
#' @export
simcyp.generateSinglePopulation <- function(populationId,
											populationSize = 0,
											embedDataInStandardOutput = TRUE,
											outputIds = NULL) 
{
	simcyp.preprocess(embedDataInStandardOutput, outputIds)

	if (length(populationId) > 1) 
	{
		stop(paste0("The populationId parameter should be single value, not a list - to generate ",
					"multiple populations use the simcyp.generateMultiplePopulations function ",
					"instead"))
	}

	if (length(populationSize) > 1) 
	{
		stop(paste0("The populationSize parameter should be single value, not a list - to ",
					"generate multiple populations use the simcyp.generateMultiplePopulations ",
					"function instead"))
	}

	cmd <- simcyp.getCommonValues()$executionCommands$generateSinglePop
	popId <- simcyp.validatePopulationId(populationId)
	popSize <- simcyp.validatePopulationSize(populationSize)
	params <- list(p = popId, n = popSize)
	
	return(simcyp.execute(cmd, params))
}


####################################################################################################
#' Get dosing data from a Standard Output file
#' 
#' Retrieves the dosing section from a Standard Output file.
#' 
#' @param soFile The path to a Standard Output file from which to extract dosing data. If omitted,
#' the Standard Output file generated by the latest Simcyp job will be used. 
#' 
#' @return A data.frame object.
#' 
#' @seealso \code{\link{data.frame}}
#' 
#' @examples
#' dosing <- simcyp.getDosingFromSO()
#' 
#' # View the dosing headers.
#' names(dosing)
#' 
#' @author Craig Lewin (Simcyp)
#'
#' @export
simcyp.getDosingFromSO <- function(soFile = file.path(simcyp.getResultsDirectory(), 
													  "simcyp_standard_output.xml"))
{
	so <- xmlParse(soFile)
	dosing <- xmlRoot(so)[["SOBlock"]][["Simulation"]][["SimulationBlock"]][["Dosing"]]
	definition <- dosing[["Definition"]]
	table <- dosing[["Table"]]
	data <- data.frame()
	
	for (column in xmlChildren(definition))
	{
		attrs <- xmlAttrs(column)		
		colNum <- as.integer(attrs[["columnNum"]])			
		values <- xmlSApply(table, function(r) { xmlValue(xmlChildren(r)[[colNum]]) })
		
		data[1:length(values), attrs[["columnId"]]] <- as.character(values)
	}
	
	return(data)
}


####################################################################################################
#' Get the headers from a Simcyp CSV file
#' 
#' Retrieves the headers from a Simcyp CSV file.
#' 
#' @param file The name of the CSV file from which the headers should be retrieved.
#' @param folder An absolute folder path in which the specified file is located. If omitted, uses 
#' the latest results directory of a Simcyp job.
#' 
#' @return A character list of headers.
#' 
#' @examples
#' # View the available headers in the ConcProfiles file.
#' simcyp.getHeadersFromCSV("ConcProfiles")
#' 
#' @author Craig Lewin (Simcyp)
#'
#' @export
simcyp.getHeadersFromCSV <- function(file, folder = file.path(simcyp.getResultsDirectory(), "data"))
{
	fileName <- basename(file_path_sans_ext(file))
	file <- file.path(folder, paste0(fileName, ".csv"))
	
	if (file.exists(file))
	{
		return(names(read.csv(file)))
	}
	else
	{
		stop(paste0("File not found: ", file))
	}
}


####################################################################################################
#' Get Simcyp output IDs
#'
#' Retrieves a list of IDs representing available outputs following a simulation 
#' (\code{\link{simcyp.simulate}}) or the generation of populations 
#' (\code{\link{simcyp.generateSinglePopulation}} or 
#' \code{\link{simcyp.generateMultiplePopulations}}).
#'
#' When this function is called for the first time, a job is created and submitted to detect all 
#' available Simcyp outputs and the results file from the completion of this job is parsed to 
#' acquire the IDs. The return value of this function is persisted so that subsequent calls in the 
#' same session do not require a repeated job submission and thus are much quicker.
#'
#' @param silentMode TRUE if messages should be output in the console during the execution of the 
#' job to acquire output IDs, FALSE if no output should be displayed.
#'
#' @return A list of IDs representing Simcyp outputs.
#'
#' @seealso \code{\link{simcyp.simulate}}
#' @seealso \code{\link{simcyp.generateSinglePopulation}}
#' @seealso \code{\link{simcyp.generateMultiplePopulations}}
#'
#' @examples
#' # Get the output IDs and display progress to the console.
#' simcyp.getOutputIds()
#'
#' # Get the output IDs silently (no console display of job progress).
#' simcyp.getOutputIds(TRUE)
#'
#' @author Craig Lewin (Simcyp)
#'
#' @export
simcyp.getOutputIds <- function(silentMode = FALSE) 
{
	if (exists("simcyp.getOutputIds.result") == FALSE) 
	{
		oldwd <- getwd()
		tempwd <- file.path(oldwd, "getOutputIds-result")

		if (dir.create(tempwd)) 
		{
			setwd(tempwd)
			
			cmd <- simcyp.getCommonValues()$executionCommands$getOutputIds
			params <- list(u = NULL)

			simcyp.execute(cmd, params, silentMode)

			simcyp.getOutputIds.result <<- simcyp.parseOutputFile()

			setwd(oldwd)

			unlink(tempwd, TRUE, TRUE)
		} 
		else 
		{
			return(NULL)
		}
	}

	return(simcyp.getOutputIds.result)
}


####################################################################################################
#' Get Simcyp population IDs
#'
#' Retrieves a list of IDs representing populations available for use with the
#' \code{\link{simcyp.generateSinglePopulation}} and 
#' \code{\link{simcyp.generateMultiplePopulations}} functions.
#'
#' When this function is called for the first time, a job is created and submitted to detect all 
#' available Simcyp populations and the results file from the completion of this job is parsed to 
#' acquire the IDs. The return value of this function is persisted so that subsequent calls in the 
#' same session do not require a repeated job submission and thus are much quicker.
#'
#' @param silentMode TRUE if messages should be output in the console during the execution of the 
#' job to acquire output IDs, FALSE if no output should be displayed.
#'
#' @return A list of IDs representing Simcyp populations.
#'
#' @seealso \code{\link{simcyp.generateSinglePopulation}}
#' @seealso \code{\link{simcyp.generateMultiplePopulations}}
#'
#' @examples
#' # Get the population IDs and display progress to the console.
#' simcyp.getPopulationIds()
#'
#' # Get the population IDs silently (no console display of job progress).
#' simcyp.getPopulationIds(TRUE)
#'
#' @author Craig Lewin (Simcyp)
#'
#' @export
simcyp.getPopulationIds <- function(silentMode = FALSE) 
{
	if (exists("simcyp.getPopulationIds.result") == FALSE) 
	{
		oldwd <- getwd()
		tempwd <- file.path(oldwd, "getPopIds-result")

		if (dir.create(tempwd)) 
		{
			setwd(tempwd)
			
			cmd <- simcyp.getCommonValues()$executionCommands$getPopIds
			params <- list(q = NULL)

			simcyp.execute(cmd, params, silentMode)

			simcyp.getPopulationIds.result <<- simcyp.parseOutputFile()

			setwd(oldwd)

			unlink(tempwd, TRUE, TRUE)
		} 
		else 
		{
			return(NULL)
		}
	}

	return(simcyp.getPopulationIds.result)
}


####################################################################################################
#' Get the location of the most recent results
#' 
#' Gets the absolute path to the folder which contains the files for the most recent Simcyp job (any 
#' of \code{\link{simcyp.simulate}}, \code{\link{simcyp.generateSinglePopulation}}, or 
#' \code{\link{simcyp.generateMultiplePopulations}}).
#' 
#' @return An absolute path to the results directory.
#' 
#' @seealso \code{\link{simcyp.simulate}}
#' @seealso \code{\link{simcyp.generateSinglePopulation}}
#' @seealso \code{\link{simcyp.generateMultiplePopulations}}
#' 
#' @examples
#' list.files(simcyp.getResultsDirectory(), include.dirs = TRUE)
#' 
#' @author Craig Lewin (Simcyp)
#'
#' @export
simcyp.getResultsDirectory <- function()
{
	if (exists("simcyp.getResultsDirectory.result")) 
	{
		if (file.exists(simcyp.getResultsDirectory.result)) 
		{
			return(simcyp.getResultsDirectory.result)
		}
	}
	
	warning("Results directory not found")
	
	return(getwd())
}


####################################################################################################
#' Get the values from a Simcyp CSV file column
#' 
#' Retrieves values from a column of a Simcyp CSV file.
#' 
#' @param header The header of the column from which the values should be retrieved.
#' @param file The name of the CSV file from which the values should be retrieved.
#' @param folder An absolute folder path in which the specified file is located. If omitted, uses 
#' the latest results directory of a Simcyp job.
#' 
#' @return A character list of values.
#' 
#' @seealso \code{\link{simcyp.getHeadersFromCSV}}
#' 
#' @examples
#' # View the available headers in the Demographic file.
#' simcyp.getHeadersFromCSV("Demographic")
#' 
#' heights <- simcyp.getValuesFromCSV("Demographic", "Height")
#' 
#' @author Craig Lewin (Simcyp)
#'
#' @export
simcyp.getValuesFromCSV <- function(header, 
									file, 
									folder = file.path(simcyp.getResultsDirectory(), "data"))
{	
	fileName <- basename(file_path_sans_ext(file))
	file <- file.path(folder, paste0(fileName, ".csv"))
	
	if (file.exists(file))
	{
		fileContents <- read.csv(file)
		
		if (is.na(match(header, names(fileContents))))
		{
			stop(sprintf("'%s' not found in '%s'", header, file))
		}
		else
		{
			return(as.character(fileContents[[header]][-1]))
		}
	}
	else
	{
		stop(paste0("File not found: ", file))
	}
}


####################################################################################################
#' Run a Simcyp simulation using a workspace
#'
#' Runs a Simcyp simulation using the provided workspace and then returns the resulting Standard 
#' Output loaded into memory (\code{\link{SOObject}}). Simulation progress is reported every 30 
#' seconds in the console so that long simulations can be monitored.
#'
#' @param workspaceFile The path to a Simcyp workspace file to be used for a simulation. This path 
#' should either be absolute or relative to the current working directory set via 
#' \code{\link{setwd}}
#'
#' @param embedDataInStandardOutput TRUE if data from the Simcyp outputs should be embedded within 
#' the resulting Standard Output, FALSE if the outputs should simply be referenced as external 
#' files within the Standard Output.
#' 
#' @param outputIds A list of identifiers indicating the types of outputs to generate. Each 
#' identifier should be one of the items in \code{\link{simcyp.getOutputIds}} or a numeric value 
#' which is the position of an ID within this list.
#'
#' @return An object of type \code{\link{SOObject}} if successful, otherwise NULL.
#'
#' @seealso \code{\link{SOObject}}
#' @seealso \code{\link{setwd}}
#'
#' @examples \dontrun{
#'
#' # Using an absolute path and producing a large SO.
#' soObj <- simcyp.simulate("C:/Path/To/Workspace.wksz", TRUE)
#'
#' # Using a relative path and producing a small SO.
#' soObj <- simcyp.simulate("files/Workspace.wksz", FALSE)
#' }
#'
#' @author Craig Lewin (Simcyp)
#'
#' @export
simcyp.simulate <- function(workspaceFile,
			 				embedDataInStandardOutput = TRUE,
			 				outputIds = NULL) 
{
	simcyp.preprocess(embedDataInStandardOutput, outputIds)

	if (is.character(workspaceFile)) 
	{
		if (file.exists(workspaceFile) == FALSE) 
		{
			stop("The specified workspace file does not exist")
		}

		pathLength <- str_length(workspaceFile)
		pathExt <- tolower(str_sub(workspaceFile, pathLength - 4, pathLength))

		if (pathExt != ".wksx" & pathExt != ".wksz") 
		{
			stop(paste0("The workspace parameter should be the path to a Simcyp workspace file ",
						"(.wksx or .wksz)"))
		}
	} 
	else 
	{
		stop(paste0("The workspace parameter should be the path to a Simcyp workspace file (.wksx ",
					"or .wksz)"))
	}

	cmd <- simcyp.getCommonValues()$executionCommands$simulate
	params <- list(w = workspaceFile)
	
	return(simcyp.execute(cmd, params))
}