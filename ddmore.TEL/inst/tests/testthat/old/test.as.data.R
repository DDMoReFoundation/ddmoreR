#library("ddmore")
#library("XML")
#library("methods")

# Clear workspace. 
#rm(list=ls())

# Switch this if you want to invoke actual FIS service. Mind that it must already be running.
USE_MOCK <- TRUE

# Function setting up a FISService mock
setupMock <- function() {
    setClass(
        "MockFISServer",
        contains = "FISServer"
    )
    createMockFISServer <-
        function(url = "http://localhost:9010", operationalUrl = "http://localhost:9011",
                 startupScript = "MOCK",
                 jobStatusPollingDelay = 20, startupPollingMax = 60, startupPollingDelay = 1) {
            new(
                "MockFISServer",
                url = url,
                operationalUrl = operationalUrl,
                startupScript = startupScript,
                jobStatusPollingDelay = jobStatusPollingDelay,
                startupPollingMax = startupPollingMax,
                startupPollingDelay = startupPollingDelay
            )
        }
    
    setMethod("readMDL", signature = signature("MockFISServer"),
              function(fisServer, filePath) {
                  path <- sub(x=filePath, pattern="\\.mdl$", replacement=".json")
                  message("Trying to use ", path, " as conversion result.")
                  con <- file(path, "r")
                  json <- readLines(con)
                  close(con)
                  return(json)
              })
    
    mockServer<- createMockFISServer()
    DDMORE.setServer(mockServer)
}

if(USE_MOCK) {
    setupMock()
} else {
    DDMORE.setServer(createFISServer(startupScript = "MOCK"))
}

# context("Testing as.data conversion function.")

# test_that("as.data correctly merges raw input data with SO data", {

#   # Setup paths to files
#   csvFilePath = system.file("tests/data/warfarin_conc.csv", package = "ddmore")
#   data.path = system.file("tests/data/PharmMLSO/MachineGenerated/Warfarin-ODE-latest-Monolix.SO.xml",  
#     package = "ddmore")

#   # Load in SO
#   SOObject = LoadSOObject(data.path)

#   # Test for fetching Raw Data from an input file and combining with SO data 
#   MyDataFrame = as.data(SOObject, inputDataPath=csvFilePath)

#   # Find expected column names of resulting Data Frame 
#   prediction.names = setdiff(names(SOObject@Estimation@Predictions$data), c("ID", "TIME"))
#   residuals.names = setdiff(names(SOObject@Estimation@Residuals$ResidualTable$data), c("ID", "TIME"))
#   indivEst.estimates.names = setdiff(names(SOObject@Estimation@IndividualEstimates$Estimates$Mean$data), c("ID", "TIME"))
#   indivEst.randeffects.names = setdiff(names(SOObject@Estimation@IndividualEstimates$RandomEffects$EffectMean$data), c("ID", "TIME"))

#   all.names = c(toupper(names(read.csv(csvFilePath))), 
#               prediction.names, residuals.names, 
#               indivEst.estimates.names, indivEst.randeffects.names)

#   # Test no empty data frame is returned 
#   expect_true(
#     nrow(MyDataFrame) > 0 , 
#     info = "Data Frame should not be empty"
#   )
  
#   # Test column names are as expected
#   expect_true(
#     all(all.names == names(MyDataFrame)), 
#     info = "Data Frame should contain correct column names"
#   )

# })

context("Testing as.data can read machine generated SO and extract header information from associated mdl file.")

test_that("as.data correctly merges raw input data with SO data", {

  # Note: these tests require the local servers to be running in order to parse the mdl file.

  # Setup paths to files
  csvFilePath = system.file("tests/data/warfarin_infusion.csv", package = "ddmore")
  xml.data.path = system.file("tests/data/PharmMLSO/MachineGenerated/UseCase9.SO.xml",  
    package = "ddmore")
  expected.data.path = system.file("tests/data/PharmMLSO/MachineGenerated/UseCase9.SO.as.data.txt",  
    package = "ddmore")

  # Load in SO
  SOObject = LoadSOObject(xml.data.path)

  # Test for fetching Raw Data from an input file and combining with SO data 
  MyDataFrame = as.data(SOObject, inputDataPath=csvFilePath)

  # Test no empty data frame is returned 
  expect_true(
    nrow(MyDataFrame) > 0 , 
    info = "Data Frame should not be empty"
  )
  
  # Test data frame values are as expected
  expectedValues = dget(expected.data.path)
 
  expect_true(
    all.equal(expectedValues, MyDataFrame), 
    info = "Data Frame should contain correct values"
  )

})


test_that("as.data correctly merges raw input data with SO data when custom TIME col is specified in Nonmen Run", {

  # Note: these tests require the local servers to be running in order to parse the mdl file.

  # Setup paths to files
  csvFilePath = system.file("tests/data/warfarin_conc_TIMEchange.csv", package = "ddmore")
  xml.data.path = system.file("tests/data/PharmMLSO/MachineGenerated/UseCase2_TIMEchange_fixed.SO.xml",  
    package = "ddmore")
  expected.data.path = system.file("tests/data/PharmMLSO/MachineGenerated/UseCase2_TIMEchange_fixed.as.data.txt",  
    package = "ddmore")

  # Load in SO
  SOObject = LoadSOObject(xml.data.path)

  # Test for fetching Raw Data from an input file and combining with SO data 
  MyDataFrame = as.data(SOObject, inputDataPath=csvFilePath)

  # Test no empty data frame is returned 
  expect_true(
    nrow(MyDataFrame) > 0 , 
    info = "Data Frame should not be empty"
  )
  
  # Test data frame values are as expected
  expectedValues = dget(expected.data.path)
 
  expect_true(
    all.equal(expectedValues, MyDataFrame), 
    info = "Data Frame should contain correct values"
  )

})


# context("Testing as.data can deal with duplicate column names in SO slots.")

# test_that("as.data correctly merges raw input data with SO data", {

#   # Note: these tests require the local servers to be running in order to parse the mdl file.

#   # Setup paths to files
#   csvFilePath = system.file("tests/data/warfarin_conc.csv", package = "ddmore")
#   xml.data.path = system.file("tests/data/PharmMLSO/MachineGenerated/UseCase1_FOCEI.SO.xml",  
#     package = "ddmore")
#   expected.data.path = system.file("tests/data/PharmMLSO/MachineGenerated/UseCase1_FOCEI.SO.as.data.txt",  
#     package = "ddmore")

#   # Load in SO
#   SOObject = LoadSOObject(xml.data.path)

#   # Test for fetching Raw Data from an input file and combining with SO data 
#   MyDataFrame = as.data(SOObject, inputDataPath=csvFilePath)

#   # Test no empty data frame is returned 
#   expect_true(
#     nrow(MyDataFrame) > 0 , 
#     info = "Data Frame should not be empty"
#   )
  
#   # Test data frame values are as expected
#   expectedValues = dget(expected.data.path)
 
#   expect_true(
#     all.equal(expectedValues, MyDataFrame), 
#     info = "Data Frame should contain correct values"
#   )

# })
