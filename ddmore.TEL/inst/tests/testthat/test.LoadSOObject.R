library("ddmore")
library("XML")
require("methods")

testSlotsNotEmpty <- function(S4class, slotnames) {

  for (slotname in slotnames) {

  SOslot = slot(S4class, slotname)
  
  expect_true(
    length(SOslot) > 0 , 
    info = paste("Slot", slotname, "should not be empty"), )
  }
  
}

context("Loading in SOObjects from Handcoded PharmMLSO Version 0.1")

test_that("PharmML SO fills expected slots in Estimation", {

  # Clear workspace. 
  rm(list=ls())
  
  data.path = system.file("tests/data/PharmMLSO/HandCoded/warfarin_PK_ODE_SO_FULL-v0_1.xml",  
  		package = "ddmore")
        
  # Load in SO
  SOObject = LoadSOObject(data.path)
  
  # Test is S4
  expect_true(isS4(SOObject), info = "Object is S4", )
  
  # Test Slots have been populated #
  # ------------------------------ #
  
  # ToolSettings
  expect_true(length(SOObject@ToolSettings) > 0 , info = "Object is not empty", )
  
  # RawResults
  testSlotsNotEmpty(SOObject@RawResults, c("DataFiles", "GraphicsFiles"))
  
  #TaskInformation
  expect_false(
    all(sapply(SOObject@TaskInformation$Messages, is.null)), 
    info = "All Message slots should not be empty", )
  
  expect_true(
    length(names(SOObject@TaskInformation)[names(SOObject@TaskInformation) != "Messages"]) > 0, 
    info = "'Messages' should be the only child element present")
  
  # Estimates
  slotnames = c("PopulationEstimates", "PrecisionPopulationEstimates",
    "IndividualEstimates", "PrecisionIndividualEstimates",
    "Residuals", "Predictions", "Likelihood")
  testSlotsNotEmpty(SOObject@Estimation, slotnames)

  # Simulation 
  slotnames = c("SimulationBlock")
  testSlotsNotEmpty(SOObject@Simulation, slotnames)

  # Model Diagnostic
  slotnames = c("DiagnosticPlotsIndividualParams", "DiagnosticPlotsStructuralModel")
  testSlotsNotEmpty(SOObject@ModelDiagnostic, slotnames)
   
})


#-----------------------------------------------------------------------
context("Loading in SOObjects from machine generated PharmMLSO Version 0.2")
#-----------------------------------------------------------------------

test_that("PharmML SO returns expected slots when running a bootstrap task", {

  # Clear workspace.
  rm(list=ls())

  data.path = system.file("tests/data/PharmMLSO/MachineGenerated/bootstrap.SO.xml",  
                          package = "ddmore")
  
  # Load in SO
  SOObject = LoadSOObject(data.path)

  # Test is S4
  expect_true(isS4(SOObject), info = "Object is S4", )

  # Test Slots have been populated #
  # ------------------------------ #
  # Commented out tests are yet to be populated in the task generated data 

  # ToolSettings
#    expect_true(length(SOObject@ToolSettings) > 0 , info = "Object is not empty", )

  # RawResults
  testSlotsNotEmpty(SOObject@RawResults, "DataFiles")

  #TaskInformation
#   expect_false(
#     all(sapply(SOObject@TaskInformation$Messages, is.null)), 
#     info = "All Message slots should not be empty", )
# 
#   expect_true(
#     length(names(SOObject@TaskInformation)[names(SOObject@TaskInformation) != "Messages"]) > 0, 
#     info = "'Messages' should be the only child element present")

  # Estimates
#   slotnames = c("PopulationEstimates", "PrecisionPopulationEstimates",
#     "IndividualEstimates", "PrecisionIndividualEstimates",
#     "Residuals", "Predictions", "Likelihood")
  slotnames = c("PopulationEstimates", "PrecisionPopulationEstimates")
  testSlotsNotEmpty(SOObject@Estimation, slotnames)

#   # Simulation 
#   slotnames = c("SimulationBlock")
#   testSlotsNotEmpty(SOObject@Simulation, slotnames)
# 
#   # Model Diagnostic
#   slotnames = c("DiagnosticPlotsIndividualParams", "DiagnosticPlotsStructuralModel")
#   testSlotsNotEmpty(SOObject@ModelDiagnostic, slotnames)

})


test_that("PharmML SO returns expected slots when running a simulation task", {
  
  # Clear workspace.
  rm(list=ls())
  
  data.path = system.file("tests/data/PharmMLSO/MachineGenerated/run1.SO.xml",  
                          package = "ddmore")
  
  # Load in SO
  SOObject = LoadSOObject(data.path)
  
  # Test is S4
  expect_true(isS4(SOObject), info = "Object is S4", )
  
  # Test Slots have been populated #
  # ------------------------------ #
  # Commented out tests are yet to be populated in the task generated data 
  
  # ToolSettings
  #    expect_true(length(SOObject@ToolSettings) > 0 , info = "Object is not empty", )
  
  # RawResults
  testSlotsNotEmpty(SOObject@RawResults, "DataFiles")
  
  # TaskInformation
  expect_false(
      all(sapply(SOObject@TaskInformation$Messages, is.null)), 
      info = "All Message slots should not be empty", )
  
  expect_true(
      length(names(SOObject@TaskInformation)[names(SOObject@TaskInformation) != "Messages"]) > 0, 
      info = "'Messages' should be the only child element present")
  
  # Estimates
  #   slotnames = c("PopulationEstimates", "PrecisionPopulationEstimates",
  #     "IndividualEstimates", "PrecisionIndividualEstimates",
  #     "Residuals", "Predictions", "Likelihood")
#   testSlotsNotEmpty(SOObject@Estimation, slotnames)
  
  # Simulation 
  slotnames = c("SimulationBlock")
  testSlotsNotEmpty(SOObject@Simulation, slotnames)
  
  expect_true(
    length(SOObject@Simulation@SimulationBlock) == 20, 
    info = "Simulation Block should contain 20 separate simulations.")
   
  #   # Model Diagnostic
  #   slotnames = c("DiagnosticPlotsIndividualParams", "DiagnosticPlotsStructuralModel")
  #   testSlotsNotEmpty(SOObject@ModelDiagnostic, slotnames)
  
})


# TODO: Commented out due to incomplete machine generated data available for SO version 0.2    


# test_that("PharmML SO fills expected slots in Estimation", {

#   # Clear workspace.
#   rm(list=ls())
  
#   data.path = system.file("tests/data/PharmMLSO/MachineGenerated/pheno.SO.xml",  
#   		package = "ddmore")
        
#   # Load in SO
#   SOObject = LoadSOObject(data.path)
  
#   # Test is S4
#   expect_true(isS4(SOObject), info = "Object is S4", )
  
#   # Test Slots have been populated #
#   # ------------------------------ #
  
#   # ToolSettings
#   expect_true(length(SOObject@ToolSettings) > 0 , info = "Object is not empty", )
  
#   # RawResults
#   testSlotsNotEmpty(SOObject@RawResults, c("DataFiles", "GraphicsFiles"))
  
#   #TaskInformation
#   expect_false(
#     all(sapply(SOObject@TaskInformation$Messages, is.null)), 
#     info = "All Message slots should not be empty", )
  
#   expect_true(
#     length(names(SOObject@TaskInformation)[names(SOObject@TaskInformation) != "Messages"]) > 0, 
#     info = "'Messages' should be the only child element present")
  
#   # Estimates
#   slotnames = c("PopulationEstimates", "PrecisionPopulationEstimates",
#     "IndividualEstimates", "PrecisionIndividualEstimates",
#     "Residuals", "Predictions", "Likelihood")
#   testSlotsNotEmpty(SOObject@Estimation, slotnames)

#   # Simulation 
#   slotnames = c("SimulationBlock")
#   testSlotsNotEmpty(SOObject@Simulation, slotnames)

#   # Model Diagnostic
#   slotnames = c("DiagnosticPlotsIndividualParams", "DiagnosticPlotsStructuralModel")
#   testSlotsNotEmpty(SOObject@ModelDiagnostic, slotnames)
   
# })





#-------------------------------------------------------------------------------------------------
context("Loading an empty, i.e. no SOBlocks, PharmML SO Version 0.1. Checking an Error is raised")
#-------------------------------------------------------------------------------------------------

test_that("Error is raised on passing an empty PharmML SO.", {

	soXmlFilePath = system.file("tests/data/PharmMLSO/HandCoded/emptySO_v0_1.xml", 
	package = "ddmore")

	# Single SO version
	expect_error(LoadSOObject(soXmlFilePath), 
		"Error in LoadSOObject\\(soXmlFilePath\\) : \\n  PharmML parsing aborted")

	# Multiple SO Version
	expect_error(LoadSOObjects(soXmlFilePath), 
		"Error in LoadSOObjects\\(soXmlFilePath\\) : \\n  PharmML parsing aborted")

})


context("Loading multiple SOBlocks contained within a PsN SSE PharmML SO")

test_that("List of 20 SO objects is parsed correctly from LoadSOObjects", {

  # Clear workspace. 
  rm(list=ls())

  soXmlFilePath = system.file("tests/data/PharmMLSO/MachineGenerated/PsN/Warfarin-ODE-latest-sse.SO.xml", package = "ddmore")

  # Load in SO
  SOObjects = LoadSOObjects(soXmlFilePath)  
		
  # Checking list structure
	expect_true(is.list(SOObjects), info="LoadSOObjects should return a List")
	expect_equal(length(SOObjects), 20, info="Expected 20 objects in list returned from LoadSOObjects")
	
  # Checking element properties
  expect_true(isS4(SOObjects[[1]]), info="SOObject should be an S4 class")
  expect_true(length(SOObjects[[1]]@RawResults) > 0, info="RawResults slot should not be empty")
  expect_equal(length(SOObjects[[1]]@RawResults@DataFiles), 2, info="RawResults@DataFiles slot should have two entries")

  # Checking that distinct SOObjects have been returned in the list
  df1a = SOObjects[[1]]@RawResults@DataFiles[[1]][["path"]]
  df1b = SOObjects[[1]]@RawResults@DataFiles[[2]][["path"]]
  df2a = SOObjects[[2]]@RawResults@DataFiles[[1]][["path"]]
  df2b = SOObjects[[2]]@RawResults@DataFiles[[2]][["path"]]
  
  expect_false(df1a == df2a, info="First data files of SOObjects 1 and 2 should be different")
  expect_false(df1b == df2b, info="Second data files of SOObjects 1 and 2 should be different")

})


context("Loading a PharmML SO with empty Matrix elements")


test_that("Expected Estimation::PrecisionPopulationEstimates::MLE::StandardError to be populated", {

  # Clear workspace. 
  rm(list=ls())

  soXmlFilePath = system.file("tests/data/PharmMLSO/HandCoded/Warfarin-ODE-latest-Monolix-EmptyMatrices.SO.xml", package = "ddmore")

  # Load in SO
  SOObject = LoadSOObject(soXmlFilePath)

  # Test elements are populated correctly
	expect_true(isS4(SOObject), info = "Object is S4")

	mleStandardError <- SOObject@Estimation@PrecisionPopulationEstimates$MLE$StandardError
	expect_true(is.list(mleStandardError) && length(mleStandardError) == 2, 
		info="Estimation::PrecisionPopulationEstimates::MLE::StandardError should be a list of 2 elements")

  mleRelativeStandardError <- SOObject@Estimation@PrecisionPopulationEstimates$MLE$RelativeStandardError
  expect_true(is.list(mleRelativeStandardError) && length(mleRelativeStandardError) == 2, 
    info="Estimation::PrecisionPopulationEstimates::MLE::RelativeStandardError should be a list of 2 elements")

  expect_true(is.null(SOObject@Estimation@PrecisionPopulationEstimates$MLE$FIM), 
    info="Estimation::PrecisionPopulationEstimates::MLE::FIM should NOT be present")
	
  expect_true(is.null(SOObject@Estimation@PrecisionPopulationEstimates$MLE$CorrelationMatrix), 
    info="Estimation::PrecisionPopulationEstimates::MLE::CorrelationMatrix should NOT be present")
  
  expect_true(is.null(SOObject@Estimation@PrecisionPopulationEstimates$MLE$CovarianceMatrix), 
    info="Estimation::PrecisionPopulationEstimates::MLE::CovarianceMatrix should NOT be present")
  
})

