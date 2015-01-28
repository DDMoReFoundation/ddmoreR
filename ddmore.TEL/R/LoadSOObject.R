#' LoadSOObject
#'
#'   Parse in the SO Object of a PharmML .xml file and return it as a new StandardOutputObject.
#'
#' @param file The relative path to the .xml file. 
#' 
#' @return Returns a newly created StandardOutputObject class populated with all entries in the SO section of the PharmML file.  
#' 
#' @export
#' @include StandardOutputObject.R xmlParsers.R 
LoadSOObject <- function(file) {

  # Error Checking
  stopifnot(class(file) == "character" & file.exists(file))

  # Set working directory to that specified in file 
  old.wd <- getwd()
  dir <- dirname(file)
  f.name <- basename(file)
  setwd(dir)

  # Generate Blank SO object 
  SOObject <- createSOObject()

  # Get a reference to the root node in the xml doc
  root <- xmlRoot(xmlTreeParse(f.name))

  # Fetch List of SOBlock elements
  SOBlockList = root[names(root) == "SOBlock"]

  # Assumes only one SOBlock for now!
  stopifnot(length(SOBlockList) == 1)

  ### Future for loop to start here if multiple SO blocks

  # Fetch all Components of the SO object that are defined
  SOChildren <- xmlChildren(SOBlockList[[1]])

  # Error Checking of unexpected elements
  expectedTags = c("ToolSettings", "RawResults", "TaskInformation", "Estimation", 
                  "Simulation")
  unexpected = setdiff(names(SOChildren), expectedTags)
  if (length(unexpected) != 0) {
    warning(paste("The following unexpected elements were detected in the PharmML SO.", 
              paste(unexpected, collapse="\n      "), sep="\n      "))
  }

  # Error checking of expected XML structure + Parser Execution
  if ("ToolSettings" %in% names(SOChildren)){
    SOObject <- ParseToolSettings(SOObject, SOChildren[["ToolSettings"]])
  } else {
    warning("ToolSettings element not detected in PharmML. Skipping...")
  }

  if ("RawResults" %in% names(SOChildren)){
    SOObject <- ParseRawResults(SOObject, SOChildren[["RawResults"]])
  } else {
    warning("RawResults element not detected in PharmML. Skipping...")
  }

  if ("TaskInformation" %in% names(SOChildren)){
    SOObject <- ParseTaskInformation(SOObject, SOChildren[["TaskInformation"]])
  } else {
    warning("TaskInformation element not detected in PharmML. Skipping...")
  }

  if ("Estimation" %in% names(SOChildren)){

      # Error Checking of unexpected elements in Estimation Block
      expectedTags = c("PopulationEstimates", "PrecisionPopulationEstimates", 
        "IndividualEstimates", "PrecisionIndividualEstimates", "Residuals", 
        "Predictions", "Likelihood")
      unexpected = setdiff(names(SOChildren[["Estimation"]]), expectedTags)
      if (length(unexpected) != 0) {
        warning(paste("The following unexpected elements were detected in the Estimation block of the PharmML SO.", 
              paste(unexpected, collapse="\n      "), sep="\n      "))
      }

      if ("PopulationEstimates" %in% names(SOChildren[["Estimation"]])){
        SOObject <- ParsePopulationEstimates(SOObject, SOChildren[["Estimation"]][["PopulationEstimates"]])
      } else {
        warning("PopulationEstimates element not detected in PharmML. Skipping...")
      }

      if ("PrecisionPopulationEstimates" %in% names(SOChildren[["Estimation"]])){
        SOObject <- ParsePrecisionPopulationEstimates(SOObject, SOChildren[["Estimation"]][["PrecisionPopulationEstimates"]])
      } else {
        warning("PrecisionPopulationEstimates element not detected in PharmML. Skipping...")
      }

      if ("IndividualEstimates" %in% names(SOChildren[["Estimation"]])){
        SOObject <- ParseIndividualEstimates(SOObject, SOChildren[["Estimation"]][["IndividualEstimates"]])
      } else {
        warning("IndividualEstimates element not detected in PharmML. Skipping...")
      }

      if ("PrecisionIndividualEstimates" %in% names(SOChildren[["Estimation"]])){
        SOObject <- ParsePrecisionIndividualEstimates(SOObject, SOChildren[["Estimation"]][["PrecisionIndividualEstimates"]])
      } else {
        warning("PrecisionIndividualEstimates element not detected in PharmML. Skipping...")
      }

      if ("Residuals" %in% names(SOChildren[["Estimation"]])){
        SOObject <- ParseResiduals(SOObject, SOChildren[["Estimation"]][["Residuals"]])
      } else {
        warning("Residuals element not detected in PharmML. Skipping...")
      }

      if ("Predictions" %in% names(SOChildren[["Estimation"]])){
        SOObject <- ParsePredictions(SOObject, SOChildren[["Estimation"]][["Predictions"]])
      } else {
        warning("Predictions element not detected in PharmML. Skipping...")
      }

      if ("Likelihood" %in% names(SOChildren[["Estimation"]])){
        SOObject <- ParseLikelihood(SOObject, SOChildren[["Estimation"]][["Likelihood"]])
      } else {
        warning("Likelihood element not detected in PharmML. Skipping...")
      }

  } else {
    warning("Estimation element not detected in PharmML. Skipping...")
  }

  if ("Simulation" %in% names(SOChildren)){

    # Error Checking of unexpected elements of Simulation node
    expectedTags = c("Description", "OriginalDataset", "SimulationBlock")
    unexpected = setdiff(names(SOChildren[["Simulation"]]), expectedTags)
    if (length(unexpected) != 0) {
      warning(paste("The following unexpected elements were detected in the Simulation block of the PharmML SO.", 
            paste(unexpected, collapse="\n      "), sep="\n      "))
    }

    # Parse the Simulation node
    SOObject <- ParseSimulation(SOObject, SOChildren[["Simulation"]])

  } else {
    warning("Simulation element not detected in PharmML. Skipping...")
  }

















  # Run validation functions on S4 Class and subclasses
  validObject(SOObject)
  validObject(SOObject@RawResults)
  validObject(SOObject@Estimation)
  validObject(SOObject@Simulation)
  validObject(SOObject@OptimalDesign)

  # Reset Working directory 
  setwd(old.wd)

  return(SOObject)
}