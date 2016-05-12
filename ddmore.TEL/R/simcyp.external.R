####################################################################################################
#' Generate multiple Simcyp populations
#'
#' Generates between 1 and 4 Simcyp populations of specific types and sizes.
#'
#' @param populationIDs A collection of identifiers indicating the types of populations to generate.
#' Each identifier should be one of the items in \code{\link{simcyp.getPopulationIDs}} or a numeric 
#' value which is the position of an ID within this collection. A maximum of 4 IDs are accepted - 
#' any subsequent IDs will be ignored.
#'
#' @param populationSizes A collection of the number of individuals to generate for each given 
#' population. Each item must be a numeric value in the range 0-5000 where 0 indicates that a 
#' population representative should be used. A single numeric value may also be used if the same 
#' population size is desired for all of the populations. A maximum of 4 sizes are accepted - any
#' subsequent sizes will be ignored.
#'
#' @param embedDataInSO TRUE if data from the Simcyp outputs should be embedded within 
#' the resulting Standard Output, FALSE if the outputs should simply be referenced as external files
#' within the Standard Output.
#' 
#' @param importSO TRUE if the resulting Standard Output file should be loaded into memory and
#' returned as an object of type \code{\link{SOObject}} upon successful execution. FALSE if NULL 
#' should be returned.
#' 
#' @param outputIDs A collection of identifiers indicating the types of outputs to generate. Each 
#' identifier should be one of the items in \code{\link{simcyp.getOutputIDs}} or a numeric value 
#' which is the position of an ID within this collection. If omitted, all possible outputs will be 
#' generated.
#'
#' @return An object of type \code{\link{SOObject}} if successful, otherwise NULL.
#'
#' @seealso \code{\link{simcyp.getPopulationIDs}}
#' @seealso \code{\link{simcyp.getOutputIDs}}
#' @seealso \code{\link{SOObject}}
#'
#' @examples
#' # Generates 20 individuals of the healthy Chinese population, 70 individuals of the healthy 
#' # volunteers population and produces a large SO.
#' so1 <- simcyp.generateMultiplePopulations(
#' 		c("SIM-CHINESE_HEALTHY", "SIM-HEALTHY_VOLUNTEERS"),
#' 		c(20, 70))
#'
#' # Generates 10 individuals for each of the healthy Chinese, healthy volunteers, japanese, and 
#' # obese populations and produces a small SO.
#' so2 <- simcyp.generateMultiplePopulations(
#' 		c(0, "SIM-HEALTHY_VOLUNTEERS", "SIM-JAPANESE", 11),
#'   	10,
#'   	FALSE)
#'
#' @author Craig Lewin (Simcyp)
#'
#' @export
simcyp.generateMultiplePopulations <- function(
	populationIDs,
	populationSizes = c(25),
	embedDataInSO = TRUE,
	importSO = TRUE,
	outputIDs = simcyp.getOutputIDs())
{
	simcyp.preprocess()
	
	if (length(populationIDs) > 1) 
	{
		popIDs <- paste(head(sapply(populationIDs, simcyp.validatePopulationID), 4), collapse = ',')
	} 
	else 
	{
		stop(paste0("The populationIDs parameter should be a list containing multiple IDs - to ",
			"generate a single population use simcyp.generateSinglePopulation instead"))
	}
	
	if (length(populationSizes) == 1 & is.numeric(populationSizes)) 
	{
		numSizes <- min(length(populationIDs), 4)
		validSizes <- rep(simcyp.validatePopulationSize(populationSizes), numSizes)
		popSizes <- paste(validSizes, collapse = ",")
	} 
	else if (length(populationSizes) > 1) 
	{
		popSizes <- 
			paste(head(sapply(populationSizes, simcyp.validatePopulationSize), 4), collapse = ',')
	}
	else 
	{
		stop(paste0("The populationSizes parameter should either be a single integer which ",
			"determines a population size for all populations or a list of integers identifying ",
			"the individual population sizes for each population identified by the populationIDs ",
			"list"))
	}
	
	if (any(popSizes == 0))
	{
		stop(paste0("Population sizes must be greater then 0 - population representative is not ",
			"valid when generating multiple populations"))
	}
	
	if (is.logical(embedDataInSO) == FALSE)
	{
		stop("'embedDataInSO' should be a logical value")
	}
	
	if (is.logical(importSO) == FALSE)
	{
		stop("'importSO' should be a logical value")
	}
	
	return(simcyp.execute(list(p = popIDs, n = popSizes), embedDataInSO, importSO, outputIDs))
}


####################################################################################################
#' Generate a Simcyp population
#'
#' Generates a Simcyp population of a specific type and size.
#'
#' @param populationID The identifier indicating the type of population to generate. Should be one 
#' of the items in \code{\link{simcyp.getPopulationIDs}} or a numeric value which is the position of 
#' an ID within this collection.
#'
#' @param populationSize The number of individuals to generate for the given population. Must be a 
#' numeric value in the range 0-5000 where 0 indicates that a population representative should be 
#' used.
#'
#' @param embedDataInSO TRUE if data from the Simcyp outputs should be embedded within 
#' the resulting Standard Output, FALSE if the outputs should simply be referenced as external files
#' within the Standard Output.
#' 
#' @param importSO TRUE if the resulting Standard Output file should be loaded into memory and
#' returned as an object of type \code{\link{SOObject}} upon successful execution. FALSE if NULL 
#' should be returned.
#' 
#' @param outputIDs A collection of identifiers indicating the types of outputs to generate. Each 
#' identifier should be one of the items in \code{\link{simcyp.getOutputIDs}} or a numeric value 
#' which is the position of an ID within this collection. If omitted, all possible outputs will be 
#' generated.
#'
#' @return An object of type \code{\link{SOObject}} if successful, otherwise NULL.
#'
#' @seealso \code{\link{simcyp.getPopulationIDs}}
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
simcyp.generateSinglePopulation <- function(
	populationID,
	populationSize = 0,
	embedDataInSO = TRUE,
	importSO = TRUE,
	outputIDs = simcyp.getOutputIDs())
{
	simcyp.preprocess()
	
	if (length(populationID) > 1) 
	{
		stop("The populationID parameter should be single value, not a list - to generate ",
			"multiple populations use the simcyp.generateMultiplePopulations function instead")
	}
	
	if (length(populationSize) > 1) 
	{
		stop(paste0("The populationSize parameter should be single value, not a list - to ",
			"generate multiple populations use the simcyp.generateMultiplePopulations function ",
			"instead"))
	}
	
	popID <- simcyp.validatePopulationID(populationID)
	popSize <- simcyp.validatePopulationSize(populationSize)
	
	return(simcyp.execute(list(p = popID, n = popSize), embedDataInSO, importSO, outputIDs))
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
#' @return A list of IDs representing Simcyp outputs.
#'
#' @seealso \code{\link{simcyp.simulate}}
#' @seealso \code{\link{simcyp.generateSinglePopulation}}
#' @seealso \code{\link{simcyp.generateMultiplePopulations}}
#'
#' @examples
#' # Print the available output IDs.
#' print(simcyp.getOutputIDs())
#'
#' @author Craig Lewin (Simcyp)
#'
#' @export
simcyp.getOutputIDs <- function() 
{
	if (exists("simcyp.outputIDs") == FALSE) 
	{
		outputIDs <- simcyp.execute(list(u = NULL), FALSE, FALSE, NULL)
		
		assign("simcyp.outputIDs", outputIDs, globalenv())
	}
	
	return(simcyp.outputIDs)
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
#' @return A list of IDs representing Simcyp populations.
#'
#' @seealso \code{\link{simcyp.generateSinglePopulation}}
#' @seealso \code{\link{simcyp.generateMultiplePopulations}}
#'
#' @examples
#' # Print the available population IDs.
#' print(simcyp.getPopulationIDs())
#'
#' @author Craig Lewin (Simcyp)
#'
#' @export
simcyp.getPopulationIDs <- function() 
{
	if (exists("simcyp.populationIDs") == FALSE) 
	{
		populationIDs <- simcyp.execute(list(q = NULL), FALSE, FALSE, NULL)
		
		assign("simcyp.populationIDs", populationIDs, globalenv())
	}
	
	return(simcyp.populationIDs)
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
simcyp.simulate <- function(
	workspaceFile, 
	embedDataInSO = TRUE, 
	importSO = TRUE, 
	outputIDs = simcyp.getOutputIDs())
{
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
	
	simcyp.preprocess()
	
	return(simcyp.execute(list(w = workspaceFile), embedDataInSO, importSO, outputIDs))
}