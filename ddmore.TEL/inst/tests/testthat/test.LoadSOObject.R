
testSlotsNotEmpty <- function(S4class, slotnames) {

  for (slotname in slotnames) {

  SOslot = slot(S4class, slotname)
  
  expect_true(
    length(SOslot) > 0 , 
    info = paste("Slot", slotname, "should not be empty"), )
  }
  
}

# Helper function for verifying the content of a slot of type: DataSet S4 object
verifyDataSetSlot <- function(SOObject, objPath, nrowsExpect, ncolsExpect) {
	obj <- eval(parse(text = paste0("SOObject@", objPath)))
	expect_true(class(obj) == "DataSet", info = paste("Checking", objPath, "is a DataSet object"))
	
	df <- as.data.frame(obj)
	expect_true(nrow(df) == nrowsExpect, info = paste("Checking number of rows of", objPath, "dataframe"))
	expect_true(ncol(df) == ncolsExpect, info = paste("Checking number of columns of", objPath, "dataframe"))
	
}

# Helper function for verifying the content of a slot of type: data frame (Matrix)
verifyMatrixSlot <- function(SOObject, objPath, nrowsExpect, ncolsExpect) {
	obj <- eval(parse(text = paste0("SOObject@", objPath)))
	expect_true(class(obj) == "data.frame", info = paste("Checking", objPath, "is a data frame"))
	
	expect_true(nrow(obj) == nrowsExpect, info = paste("Checking number of rows of", objPath, "dataframe"))
	expect_true(ncol(obj) == ncolsExpect, info = paste("Checking number of columns of", objPath, "dataframe"))
}


context("Loading in SOObjects from Handcoded PharmML SO")

data.path <- system.file("tests/data/PharmMLSO/HandCoded/warfarin_PK_ODE_SO_FULL.xml", package = "ddmore")

# Load in SO
SOObject <- suppressMessages(
    suppressWarnings(LoadSOObject(data.path)))

test_that("LoadSOObject parses PharmML SO XML file", {
  
	# Test is S4
	expect_true(isS4(SOObject), info = "Object is S4")
	expect_true(class(SOObject) == "StandardOutputObject", info = "Object is StandardOutputObject class")
  
})
  
  # Test Slots have been populated #
  # ------------------------------ #

test_that("Checking ToolSettings slots", {

	expect_true(all(names(SOObject@ToolSettings) == c("ts1", "ts2", "ts3")), info = "Checking ToolSettings oids")
	expect_true(SOObject@ToolSettings$ts1 == "algorithms.xmlx", info = "Checking ToolSettings for oid=ts1")
	expect_true(SOObject@ToolSettings$ts2 == "solver.xmlx", info = "Checking ToolSettings for oid=ts2")

})

test_that("Checking RawResults slots", {
	
	testSlotsNotEmpty(SOObject@RawResults, c("DataFiles", "GraphicsFiles"))
			
})

test_that("Checking TaskInformation slots", {
	
	expect_true(length(SOObject@TaskInformation@ErrorMessages) == 1, info = "Checking TaskInformation::ErrorMessages")
	expect_true(all(names(SOObject@TaskInformation@ErrorMessages[[1]]) == c("Toolname", "Name", "Content", "Severity")), info = "Checking TaskInformation::ErrorMessages message 1 attribute names")
	expect_true(SOObject@TaskInformation@ErrorMessages[[1]]$Content == "Possibly long text...", info = "Checking TaskInformation::ErrorMessages message 1 content")
	
	expect_true(SOObject@TaskInformation@OutputFilePath == "/results/mainResultFile.txt", info = "Checking TaskInformation::OutputFilePath")
	expect_true(SOObject@TaskInformation@RunTime == 23232.5, info = "Checking TaskInformation::RunTime")
	expect_true(SOObject@TaskInformation@NumberChains == 3, info = "Checking TaskInformation::NumberChains")
	expect_true(SOObject@TaskInformation@NumberIterations == 1000000, info = "Checking TaskInformation::NumberIterations")
	
})

test_that("Checking Estimation::PopulationEstimates slots", {
			
	verifyDataSetSlot(SOObject, "Estimation@PopulationEstimates@MLE", 1, 13)
	verifyDataSetSlot(SOObject, "Estimation@PopulationEstimates@Bayesian@PosteriorMean", 1, 13)
	verifyDataSetSlot(SOObject, "Estimation@PopulationEstimates@Bayesian@PosteriorMedian", 1, 13)
	
	popEstsOtherMethod <- SOObject@Estimation@PopulationEstimates@OtherMethod
	expect_true(names(popEstsOtherMethod) == "Bootstrap", info = "Checking Estimation::PopulationEstimates::OtherMethod types")
	
	verifyDataSetSlot(SOObject, "Estimation@PopulationEstimates@OtherMethod[[\"Bootstrap\"]]@Mean", 1, 13)
	verifyDataSetSlot(SOObject, "Estimation@PopulationEstimates@OtherMethod[[\"Bootstrap\"]]@Median", 1, 13)
  
})

test_that("Checking Estimation::PrecisionPopulationEstimates::MLE slots", {
	
	verifyMatrixSlot(SOObject, "Estimation@PrecisionPopulationEstimates@MLE@FIM", 2, 12)
	verifyMatrixSlot(SOObject, "Estimation@PrecisionPopulationEstimates@MLE@CovarianceMatrix", 2, 12)
	verifyMatrixSlot(SOObject, "Estimation@PrecisionPopulationEstimates@MLE@CorrelationMatrix", 2, 12)
	verifyDataSetSlot(SOObject, "Estimation@PrecisionPopulationEstimates@MLE@StandardError", 4, 2)
	verifyDataSetSlot(SOObject, "Estimation@PrecisionPopulationEstimates@MLE@RelativeStandardError", 2, 2)
	verifyDataSetSlot(SOObject, "Estimation@PrecisionPopulationEstimates@MLE@AsymptoticCI", 2, 4)
	expect_true(SOObject@Estimation@PrecisionPopulationEstimates@MLE@ConditionNumber == 0.0001, info = "Checking Estimation::PrecisionPopulationEstimates::MLE::ConditionNumber")
	
})

test_that("Checking Estimation::PrecisionPopulationEstimates::Bayesian slots", {

	verifyDataSetSlot(SOObject, "Estimation@PrecisionPopulationEstimates@Bayesian@StandardDeviation", 2, 2)
			
	expect_true(class(SOObject@Estimation@PrecisionPopulationEstimates@Bayesian@PosteriorDistribution) == "DataSetDistribution", info = "Checking Estimation::PrecisionPopulationEstimates::Bayesian::PosteriorDistribution is a DataSetDistribution object")
	expect_true(SOObject@Estimation@PrecisionPopulationEstimates@Bayesian@PosteriorDistribution@distributionName == "RandomSample", info = "Checking Estimation::PrecisionPopulationEstimates::Bayesian::PosteriorDistribution distribution name")
	precisPopEstsBayesianPostDistrib <- as.data.frame(SOObject@Estimation@PrecisionPopulationEstimates@Bayesian@PosteriorDistribution)
	expect_true(nrow(precisPopEstsBayesianPostDistrib) == 10, info = "Checking number of rows of Estimation::PrecisionPopulationEstimates::Bayesian::PosteriorDistribution dataframe")
	expect_true(ncol(precisPopEstsBayesianPostDistrib) == 3, info = "Checking number of columns of Estimation::PrecisionPopulationEstimates::Bayesian::PosteriorDistribution dataframe")	

	verifyDataSetSlot(SOObject, "Estimation@PrecisionPopulationEstimates@Bayesian@PercentilesCI", 2, 3)
	
})

test_that("Checking Estimation::PrecisionPopulationEstimates::OtherMethod slots", {

	precisPopEstsOtherMethod <- SOObject@Estimation@PrecisionPopulationEstimates@OtherMethod
	expect_true(names(precisPopEstsOtherMethod) == "LLP", info = "Checking Estimation::PrecisionPopulationEstimates::OtherMethod types")

	verifyMatrixSlot(SOObject, "Estimation@PrecisionPopulationEstimates@OtherMethod[[\"LLP\"]]@CovarianceMatrix", 2, 12)
	verifyMatrixSlot(SOObject, "Estimation@PrecisionPopulationEstimates@OtherMethod[[\"LLP\"]]@CorrelationMatrix", 2, 12)
	verifyDataSetSlot(SOObject, "Estimation@PrecisionPopulationEstimates@OtherMethod[[\"LLP\"]]@StandardDeviation", 2, 2)
	verifyDataSetSlot(SOObject, "Estimation@PrecisionPopulationEstimates@OtherMethod[[\"LLP\"]]@StandardError", 2, 2)
	verifyDataSetSlot(SOObject, "Estimation@PrecisionPopulationEstimates@OtherMethod[[\"LLP\"]]@AsymptoticCI", 2, 4)
	
	precisPopEstsOtherMethodLLPPostDistrib <- SOObject@Estimation@PrecisionPopulationEstimates@OtherMethod[["LLP"]]@PosteriorDistribution
	expect_true(class(precisPopEstsOtherMethodLLPPostDistrib) == "DataSetDistribution", info = "Checking Estimation::PrecisionPopulationEstimates::OtherMethod[LLP]::PosteriorDistribution is a DataSetDistribution object")
	expect_true(precisPopEstsOtherMethodLLPPostDistrib@distributionName == "LogNormal1", info = "Checking Estimation::PrecisionPopulationEstimates::OtherMethod[LLP]::PosteriorDistribution distribution name")
	expect_true(all(names(precisPopEstsOtherMethodLLPPostDistrib@distributionParameters) == c("meanLog", "stdevLog")), info = "Checking Estimation::PrecisionPopulationEstimates::OtherMethod[LLP]::PosteriorDistribution distribution parameters' names")
	expect_true(precisPopEstsOtherMethodLLPPostDistrib@distributionParameters$meanLog == 10, info = "Checking Estimation::PrecisionPopulationEstimates::OtherMethod[LLP]::PosteriorDistribution distribution parameter \"meanLog\"")
	expect_true(precisPopEstsOtherMethodLLPPostDistrib@distributionParameters$stdevLog == 1.5, info = "Checking Estimation::PrecisionPopulationEstimates::OtherMethod[LLP]::PosteriorDistribution distribution parameter \"stdevLog\"")

	verifyDataSetSlot(SOObject, "Estimation@PrecisionPopulationEstimates@OtherMethod[[\"LLP\"]]@PercentilesCI", 2, 3)

})


# TODO: Estimation::IndividualEstimates

# TODO: Estimation::PrecisionIndividualEstimates

# TODO: Estimation::Residuals

# TODO: Estimation::Predictions


test_that("Checking Estimation::OFMeasures slots", {
	
	expect_true(SOObject@Estimation@OFMeasures@Likelihood == 1.23, info = "Checking Estimation::OFMeasures::Likelihood")
	expect_true(SOObject@Estimation@OFMeasures@LogLikelihood == 0.000223, info = "Checking Estimation::OFMeasures::LogLikelihood")
	expect_true(SOObject@Estimation@OFMeasures@Deviance == -3.23, info = "Checking Estimation::OFMeasures::Deviance")
	expect_true(SOObject@Estimation@OFMeasures@ToolObjFunction == 0.423, info = "Checking Estimation::OFMeasures::ToolObjFunction")
	verifyDataSetSlot(SOObject, "Estimation@OFMeasures@IndividualContribToLL", 2, 2)
	expect_true(SOObject@Estimation@OFMeasures@InformationCriteria$AIC == 3.456, info = "Checking Estimation::OFMeasures::InformationCriteria$AIC")
	expect_true(SOObject@Estimation@OFMeasures@InformationCriteria$BIC == 4.567, info = "Checking Estimation::OFMeasures::InformationCriteria$BIC")
	expect_true(SOObject@Estimation@OFMeasures@InformationCriteria$DIC == 5.678, info = "Checking Estimation::OFMeasures::InformationCriteria$DIC")
	
})


# TODO: ModelDiagnostic

# TODO: Simulation::simulationBlock

# TODO: OptimalDesign



# TODO: Read in a SO generated by a Bootstrap task and verify content



#-----------------------------------------------------------------------
context("Loading in SOObjects from Simulation task PharmMLSO Version 0.3")
#-----------------------------------------------------------------------

data.path <- system.file("tests/data/PharmMLSO/MachineGenerated/SOv0.3/run1.SO.xml", 
    package = "ddmore")
  
# Load in SO
SOObject <- suppressMessages(LoadSOObject(data.path))
  
test_that("LoadSOObject parses PharmML SO XML file", {
    
	# Test is S4
	expect_true(isS4(SOObject), info = "Object is S4")
	expect_true(class(SOObject) == "StandardOutputObject", info = "Object is StandardOutputObject class")
	
})


test_that("Checking Simulation slots", {
  
  expect_true(
    length(SOObject@Simulation) == 20, 
    info = "Simulation Block should contain 20 separate simulations")

# TODO More tests
  
})


#-------------------------------------------------------------------------------------------------
context("Loading an empty, i.e. no SOBlocks, PharmML SO. Checking an Error is raised")
#-------------------------------------------------------------------------------------------------

test_that("Error is raised on passing an empty PharmML SO.", {

	soXmlFilePath <- system.file("tests/data/PharmMLSO/HandCoded/emptySO_v0_1.xml", 
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

  soXmlFilePath <- system.file("tests/data/PharmMLSO/MachineGenerated/PsN/Warfarin-ODE-latest-sse.SO.xml", 
    package = "ddmore")

  # Load in SO
  SOObjects <- suppressMessages(LoadSOObjects(soXmlFilePath))
  
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

rm(testSlotsNotEmpty, verifyDataSetSlot, verifyMatrixSlot, data.path, SOObject)
