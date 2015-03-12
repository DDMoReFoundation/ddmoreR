library("DDMoRe.TEL")
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

context("Loading In SOObjects from PharmMLSO Version 0.1")


test_that("PharmML SO fills expected slots in Estimation", {

  # Clear workspace. 
  rm(list=ls())
  
  data.path = system.file("tests/data/PharmMLSO/HandCoded/warfarin_PK_ODE_SO_FULL-v0_1.xml",  
  		package = "DDMoRe.TEL")
        
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
    info = "All Message slots are not empty", )
  
  expect_true(
    length(names(SOObject@TaskInformation)[names(SOObject@TaskInformation) != "Messages"]) > 0, 
    info = "Messages is not the only element present")
  
  # Estimates
  slotnames = c("PopulationEstimates", "PrecisionPopulationEstimates",
    "IndividualEstimates", "PrecisionIndividualEstimates",
    "Residuals", "Predictions", "Likelihood")
  testSlotsNotEmpty(SOObject@Estimation, slotnames)

  # Simulation 
  slotnames = c("SimulationBlock")
  testSlotsNotEmpty(SOObject@Simulation, slotnames)
   
})

context("Loading an empty, i.e. no SOBlocks, PharmML SO Version 0.1. Checking an Error is raised")

# Clear workspace. 
rm(list=ls())

test_that("Error raised on passing an empty PharmML SO.", {

	soXmlFilePath = system.file("tests/data/PharmMLSO/HandCoded/emptySO_v0_1.xml", 
	package = "DDMoRe.TEL")

	# Single SO version
	expect_error(LoadSOObject(soXmlFilePath), 
		"Error in LoadSOObject\\(soXmlFilePath\\) : \\n  PharmML parsing aborted")

	# Multiple SO Version
	expect_error(LoadSOObjects(soXmlFilePath), 
		"Error in LoadSOObjects\\(soXmlFilePath\\) : \\n  PharmML parsing aborted")

})


context("Loading multiple SOBlocks contained within a PsN SSE PharmML SO returns all the SOObjects")

# Clear workspace. 
rm(list=ls())

soXmlFilePath = system.file("tests/data/PharmMLSO/MachineGenerated/PsN/Warfarin-ODE-latest-sse.SO.xml", package = "DDMoRe.TEL")

# Load in SO
SOObjects = LoadSOObjects(soXmlFilePath)		

test_that("Expected list of 20 objects to be returned from LoadSOObjects", {
		
	expect_true(is.list(SOObjects), info="LoadSOObjects should return a List")
	expect_equal(length(SOObjects), 20, info="Expected 20 objects in list returned from LoadSOObjects")
	
})

test_that("Checking some of the slots in one of the SOObjects", {
			
	expect_true(isS4(SOObjects[[1]]), info="SOObject is S4 class")
	
	expect_equal(length(SOObjects[[1]]@RawResults), 1, info="RawResults slot should not be empty")
	expect_equal(length(SOObjects[[1]]@RawResults@DataFiles), 2, info="RawResults@DataFiles slot should have two entries")

})

test_that("Checking that distinct SOObjects have been returned in the list", {

	df1a = SOObjects[[1]]@RawResults@DataFiles[[1]][["path"]]
	df1b = SOObjects[[1]]@RawResults@DataFiles[[2]][["path"]]
	df2a = SOObjects[[2]]@RawResults@DataFiles[[1]][["path"]]
	df2b = SOObjects[[2]]@RawResults@DataFiles[[2]][["path"]]
	
	expect_false(df1a == df2a, info="First data files of SOObjects 1 and 2 should be different")
	expect_false(df1b == df2b, info="Second data files of SOObjects 1 and 2 should be different")
	
})



context("Loading a PharmML SO with empty Matrix elements ignores these elements rather than trying to produce some kind of 0x1 dataframe")

# Clear workspace. 
rm(list=ls())

soXmlFilePath = system.file("tests/data/PharmMLSO/HandCoded/Warfarin-ODE-latest-Monolix-EmptyMatrices.SO.xml", package = "DDMoRe.TEL")

# Load in SO
SOObject = LoadSOObject(soXmlFilePath)

test_that("Expected Estimation::PrecisionPopulationEstimates::MLE::StandardError to be populated", {

	expect_true(isS4(SOObject), info = "Object is S4")
	mleStandardError <- SOObject@Estimation@PrecisionPopulationEstimates$MLE$StandardError
	
	expect_true(is.list(mleStandardError) && length(mleStandardError) == 2, info="Estimation::PrecisionPopulationEstimates::MLE::StandardError should be a list of 2 elements")
	
})

test_that("Expected Estimation::PrecisionPopulationEstimates::MLE::RelativeStandardError to be populated", {
			
	expect_true(isS4(SOObject), info = "Object is S4")
	mleRelativeStandardError <- SOObject@Estimation@PrecisionPopulationEstimates$MLE$RelativeStandardError
	
	expect_true(is.list(mleRelativeStandardError) && length(mleRelativeStandardError) == 2, info="Estimation::PrecisionPopulationEstimates::MLE::RelativeStandardError should be a list of 2 elements")
	
})

test_that("Expected Estimation::PrecisionPopulationEstimates::MLE::FIM to NOT be populated", {
			
	expect_true(isS4(SOObject), info = "Object is S4")
	
	expect_true(is.null(SOObject@Estimation@PrecisionPopulationEstimates$MLE$FIM), info="Estimation::PrecisionPopulationEstimates::MLE::FIM should NOT be present")
	
})

test_that("Expected Estimation::PrecisionPopulationEstimates::MLE::CorrelationMatrix to NOT be populated", {
			
	expect_true(isS4(SOObject), info = "Object is S4")
	
	expect_true(is.null(SOObject@Estimation@PrecisionPopulationEstimates$MLE$CorrelationMatrix), info="Estimation::PrecisionPopulationEstimates::MLE::CorrelationMatrix should NOT be present")
	
})

test_that("Expected Estimation::PrecisionPopulationEstimates::MLE::CovarianceMatrix to NOT be populated", {
			
	expect_true(isS4(SOObject), info = "Object is S4")
	
	expect_true(is.null(SOObject@Estimation@PrecisionPopulationEstimates$MLE$CovarianceMatrix), info="Estimation::PrecisionPopulationEstimates::MLE::CovarianceMatrix should NOT be present")
	
})


