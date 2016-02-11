################################################################################
#' Generate multiple Simcyp populations
#'
#' Generates between 1 and 4 Simcyp populations of specific types and sizes and
#' then returns the resulting Standard Output loaded into memory
#' (\code{\link{SOObject}}).
#'
#' @param populationIds A list of identifiers indicating the types of
#' populations to generate. Each identifier should be one of the items in
#' \code{\link{simcyp.getPopulationIds}} or a numeric value which is the
#' position of an ID within this list.
#'
#' @param populationSizes A list of the number of individuals to generate for
#' each given population. Each item must be a numeric value in the range 0-5000
#' where 0 indicates that a population representative should be used. A single
#' numeric value may also be used if the same population size is desired for all
#' of the populations.
#'
#' @param embedDataInStandardOutput TRUE if data from the Simcyp outputs should
#' be embedded within the resulting Standard Output, FALSE if the outputs
#' should simply be referenced as external files within the Standard Output.
#'
#' @return An object of type \code{\link{SOObject}} if successful, otherwise
#' NULL.
#'
#' @seealso \code{\link{simcyp.getPopulationIds}}
#' @seealso \code{\link{SOObject}}
#'
#' @examples
#' # Generates 20 individuals of the healthy Chinese population, 70 individuals
#' # of the healthy volunteers population and produces a large SO
#' so1 <- simcyp.generateSinglePopulation(
#'   list("SIM-CHINESE_HEALTHY", "SIM-HEALTHY_VOLUNTEERS"),
#'   list(20, 70)
#' )
#'
#' # Generates 10 individuals for each of the healthy Chinese, healthy
#' # volunteers, japanese, and obese populations and produces a small SO
#' so2 <- simcyp.generateSinglePopulation(
#'   list(0, "SIM-HEALTHY_VOLUNTEERS", "SIM-JAPANESE", 11),
#'   10,
#'   FALSE
#' )
#'
#' @author Craig Lewin (Simcyp)
#'
#' @export
simcyp.generateMultiplePopulations <- function(
  populationIds = list("SIM-HEALTHY_VOLUNTEERS"),
  populationSizes = list(0),
  embedDataInStandardOutput = TRUE
) {
  simcyp.preprocess(embedDataInStandardOutput)

  if (is.list(populationIds)) {
    popIds <- paste(
      sapply(
        populationIds,
        simcyp.validatePopulationId
      ),
      collapse = ','
    )
  } else {
    stop(
      paste0(
        "The populationIds parameter should be a list - to generate a single",
        "population use simcyp.generateSinglePopulation instead"
      )
    )
  }

  if (is.list(populationSizes)) {
    popSizes <- paste(
      sapply(
        populationSizes,
        simcyp.validatePopulationSize
      ),
      collapse = ','
    )
  } else if (is.numeric(populationSizes)) {
    popSizes <- paste(
      rep(
        simcyp.validatePopulationSize(populationSizes),
        length(populationIds)
      ),
      collapse = ","
    )
  } else {
    stop(
      paste0(
        "The populationSizes parameter should either be a single integer which",
        "determines a population size for all populations or a list of ",
        "integers identifying the individual population sizes for each ",
        "population identified by the populationIds list"
      )
    )
  }

  return(
    simcyp.execute(
      simcyp.getConstants()$executionCommands$generateMultiPops,
      list(p = popIds, n = popSizes)
    )
  )
}


################################################################################
#' Generate a Simcyp population
#'
#' Generates a Simcyp population of a specific type and size and then returns
#' the resulting Standard Output loaded into memory (\code{\link{SOObject}}).
#'
#' @param populationId The identifier indicating the type of population to
#' generate. Should be one of the items in \code{\link{simcyp.getPopulationIds}}
#' or a numeric value which is the position of an ID within this list.
#'
#' @param populationSize The number of individuals to generate for the given
#' population. Must be a numeric value in the range 0-5000 where 0 indicates
#' that a population representative should be used.
#'
#' @param embedDataInStandardOutput TRUE if data from the Simcyp outputs should
#' be embedded within the resulting Standard Output, FALSE if the outputs
#' should simply be referenced as external files within the Standard Output.
#'
#' @return An object of type \code{\link{SOObject}} if successful, otherwise
#' NULL.
#'
#' @seealso \code{\link{simcyp.getPopulationIds}}
#' @seealso \code{\link{SOObject}}
#'
#' @examples
#' # Generates a single population representative of the healthy Chinese
#' # population and produces a large SO
#' so1 <- simcyp.generateSinglePopulation("SIM-CHINESE_HEALTHY", 0)
#'
#' # Generates 25 individuals of the healthy volunteers population and produces
#' # a small SO
#' so2 <- simcyp.generateSinglePopulation(7, 25, FALSE)
#'
#' @author Craig Lewin (Simcyp)
#'
#' @export
simcyp.generateSinglePopulation <- function(
  populationId = "SIM-HEALTHY_VOLUNTEERS",
  populationSize = 0,
  embedDataInStandardOutput = TRUE
) {
  simcyp.preprocess(embedDataInStandardOutput)

  if (is.list(populationId)) {
    stop(
      paste0(
        "The populationId parameter should be single value, not a list - ",
        "to generate multiple populations use the ",
        "simcyp.generateMultiplePopulations function instead"
      )
    )
  }

  if (is.list(populationSize)) {
    stop(
      paste0(
        "The populationSize parameter should be single value, not a list - to ",
        "generate multiple populations use the ",
        "simcyp.generateMultiplePopulations function instead"
      )
    )
  }

  return(
    simcyp.execute(
      simcyp.getConstants()$executionCommands$generateSinglePop,
      list(
        p = simcyp.validatePopulationId(populationId),
        n = simcyp.validatePopulationSize(populationSize)
      )
    )
  )
}


################################################################################
#' Get Simcyp output IDs
#'
#' Retrieves a list of IDs representing available outputs following a simulation
#' (\code{\link{simcyp.simulate}}) or the generation of populations
#' (\code{\link{simcyp.generateSinglePopulation}} or
#' \code{\link{simcyp.generateMultiplePopulations}}).
#'
#' When this function is called for the first time, a job is created and
#' submitted to detect all available Simcyp outputs and the results file from
#' the completion of this job is parsed to acquire the IDs. The return value of
#' this function is persisted so that subsequent calls in the same session do
#' not require a repeated job submission and thus are much quicker.
#'
#' @param silentMode TRUE if messages should be output in the console during
#' the execution of the job to acquire output IDs, FALSE if no output should be
#' displayed.
#'
#' @return A list of IDs representing Simcyp outputs.
#'
#' @seealso \code{\link{simcyp.simulate}}
#' @seealso \code{\link{simcyp.generateSinglePopulation}}
#' @seealso \code{\link{simcyp.generateMultiplePopulations}}
#'
#' @examples
#' # Get the output IDs and display progress to the console
#' simcyp.getOutputIds()
#'
#' # Get the output IDs silently (no console display of job progress)
#' simcyp.getOutputIds(TRUE)
#'
#' @author Craig Lewin (Simcyp)
#'
#' @export
simcyp.getOutputIds <- function(silentMode = FALSE) {
  if (exists("simcyp.getOutputIds.result") == FALSE) {
    oldwd <- getwd()
    tempwd <- file.path(oldwd, "getOutputIds-result")

    if (dir.create(tempwd)) {
      setwd(tempwd)

      simcyp.execute(
        simcyp.getConstants()$executionCommands$getOutputIds,
        list(u = NULL),
        silentMode
      )

      simcyp.getOutputIds.result <<- simcyp.parseOutputFile()

      setwd(oldwd)

      unlink(tempwd, TRUE, TRUE)
    } else {
      return(NULL)
    }
  }

  return(simcyp.getOutputIds.result)
}


################################################################################
#' Get Simcyp population IDs
#'
#' Retrieves a list of IDs representing populations available for use with the
#' \code{\link{simcyp.generateSinglePopulation}} and
#' \code{\link{simcyp.generateMultiplePopulations}} functions.
#'
#' When this function is called for the first time, a job is created and
#' submitted to detect all available Simcyp populations and the results file
#' from the completion of this job is parsed to acquire the IDs. The return
#' value of this function is persisted so that subsequent calls in the same
#' session do not require a repeated job submission and thus are much quicker.
#'
#' @param silentMode TRUE if messages should be output in the console during
#' the execution of the job to acquire output IDs, FALSE if no output should be
#' displayed.
#'
#' @return A list of IDs representing Simcyp populations.
#'
#' @seealso \code{\link{simcyp.generateSinglePopulation}}
#' @seealso \code{\link{simcyp.generateMultiplePopulations}}
#'
#' @examples
#' # Get the population IDs and display progress to the console
#' simcyp.getPopulationIds()
#'
#' # Get the population IDs silently (no console display of job progress)
#' simcyp.getPopulationIds(TRUE)
#'
#' @author Craig Lewin (Simcyp)
#'
#' @export
simcyp.getPopulationIds <- function(silentMode = FALSE) {
  if (exists("simcyp.getPopulationIds.result") == FALSE) {
    oldwd <- getwd()
    tempwd <- file.path(oldwd, "getPopIds-result")

    if (dir.create(tempwd)) {
      setwd(tempwd)

      simcyp.execute(
        simcyp.getConstants()$executionCommands$getPopIds,
        list(q = NULL),
        silentMode
      )

      simcyp.getPopulationIds.result <<- simcyp.parseOutputFile()

      setwd(oldwd)

      unlink(tempwd, TRUE, TRUE)
    } else {
      return(NULL)
    }
  }

  return(simcyp.getPopulationIds.result)
}


################################################################################
#' Run a Simcyp simulation using a workspace
#'
#' Runs a Simcyp simulation using the provided workspace and then returns the
#' resulting Standard Output loaded into memory (\code{\link{SOObject}}).
#' Simulation progress is reported every 30 seconds in the console so that long
#' simulations can be monitored.
#'
#' @param workspaceFile The path to a Simcyp workspace file to be used for a
#' simulation. This path should either be absolute or relative to the current
#' working directory set via \code{\link{setwd}}
#'
#' @param embedDataInStandardOutput TRUE if data from the Simcyp outputs should
#' be embedded within the resulting Standard Output, FALSE if the outputs
#' should simply be referenced as external files within the Standard Output.
#'
#' @return An object of type \code{\link{SOObject}} if successful, otherwise
#' NULL.
#'
#' @seealso \code{\link{SOObject}}
#' @seealso \code{\link{setwd}}
#'
#' @examples \dontrun{
#'
#' # Using an absolute path and producing a large SO
#' soObj <- simcyp.simulate("C:/Path/To/Workspace.wksz", TRUE)
#'
#' # Using a relative path and producing a small SO
#' soObj <- simcyp.simulate("files/Workspace.wksz", FALSE)
#' }
#'
#' @author Craig Lewin (Simcyp)
#'
#' @export
simcyp.simulate <- function(workspaceFile, embedDataInStandardOutput = TRUE) {
  simcyp.preprocess(embedDataInStandardOutput)

  if (is.character(workspaceFile)) {
    if (file.exists(workspaceFile) == FALSE) {
      stop("The specified workspace file does not exist")
    }

    pathLength <- str_length(workspaceFile)
    pathExt <- tolower(str_sub(workspaceFile, pathLength - 4, pathLength))

    if (pathExt != ".wksx" & pathExt != ".wksz") {
      stop(
        paste0(
          "The workspace parameter should be the path to a Simcyp workspace ",
          "file (.wksx or .wksz)"
        )
      )
    }
  } else {
    stop(
      paste0(
        "The workspace parameter should be the path to a Simcyp workspace ",
        "file(.wksx or .wksz)"
      )
    )
  }

  return(
    simcyp.execute(
      simcyp.getConstants()$executionCommands$simulate,
      list(w = workspaceFile)
    )
  )
}
