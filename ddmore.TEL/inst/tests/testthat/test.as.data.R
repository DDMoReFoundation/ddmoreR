
# context("Testing as.data conversion function.")

# test_that("as.data correctly merges raw input data with SO data", {

#   # Setup paths to files
#   csvFilePath = system.file("tests/data/warfarin_conc.csv", package = "ddmore")
#   data.path = system.file("tests/data/PharmMLSO/MachineGenerated/Warfarin-ODE-latest-Monolix.SO.xml",  
#     package = "ddmore")

#   # Load in SO
#   SOObject = LoadSOObject(data.path)

#   # Test for fetching Raw Data from an input file and combining with SO data 
#   combData = as.data(SOObject, inputDataPath=csvFilePath)

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
#     nrow(combData) > 0 , 
#     info = "Data Frame should not be empty"
#   )
  
#   # Test column names are as expected
#   expect_true(
#     all(all.names == names(combData)), 
#     info = "Data Frame should contain correct column names"
#   )

# })

context("Testing as.data can read machine generated SO and extract header information from associated mdl file.")

test_that("as.data correctly merges raw input data with SO data", {

  # Setup paths to files
  csvFilePath <- system.file("tests", "data", "warfarin_conc.csv", package = "ddmore")
  xmlDataPath <- system.file("tests", "data", "PharmMLSO", "MachineGenerated", 
    "UseCase11.SO.xml",  
    package = "ddmore")

  # Load in SO
  SOObject <- suppressMessages(LoadSOObject(xmlDataPath))

  # Test for fetching Raw Data from an input file and combining with SO data 
  combData <- suppressWarnings(as.data(SOObject, inputDataPath = csvFilePath))

  # Test no empty data frame is returned 
  expect_equal(
    object = dim(combData), expected = c(288, 11), 
    info = "Data Frame should not be empty"
  )
  
  # Test data frame values are as expected
  expect_equal(
    object = sapply(X = combData, FUN = function(x) { mean(as.numeric(x), na.rm = TRUE) }),
    expected =  c(ID = 15.3055555555556, TIME = 45.2152777777778, WT = 68.7645833333333, 
        AMT = 105, DVID = 0.888888888888889, DV = 6.426, MDV = 0.131944444444444, 
        LOGTWT = -0.0370431077638889, BASECOUNT = 53.4895833333333, BETA = 1, ETA_PPV_EVENT = 49.0972222222222),
    info = "Data Frame should contain correct values"
  )

})

#
#test_that("as.data correctly merges raw input data with SO data when custom TIME col is specified in Nonmen Run", {
#
#  # Note: these tests require the local servers to be running in order to parse the mdl file.
#
#  # Setup paths to files
#  csvFilePath = system.file("tests/data/warfarin_conc_TIMEchange.csv", package = "ddmore")
#  xmlDataPath = system.file("tests/data/PharmMLSO/MachineGenerated/UseCase2_TIMEchange_fixed.SO.xml",  
#    package = "ddmore")
#  expected.data.path = system.file("tests/data/PharmMLSO/MachineGenerated/UseCase2_TIMEchange_fixed.as.data.txt",  
#    package = "ddmore")
#
#  # Load in SO
#  SOObject = LoadSOObject(xmlDataPath)
#
#  # Test for fetching Raw Data from an input file and combining with SO data 
#  combData = as.data(SOObject, inputDataPath=csvFilePath)
#
#  # Test no empty data frame is returned 
#  expect_true(
#    nrow(combData) > 0 , 
#    info = "Data Frame should not be empty"
#  )
#  
#  # Test data frame values are as expected
#  expectedValues = dget(expected.data.path)
# 
#  expect_true(
#    all.equal(expectedValues, combData), 
#    info = "Data Frame should contain correct values"
#  )
#
#})
#

# context("Testing as.data can deal with duplicate column names in SO slots.")

# test_that("as.data correctly merges raw input data with SO data", {

#   # Note: these tests require the local servers to be running in order to parse the mdl file.

#   # Setup paths to files
#   csvFilePath = system.file("tests/data/warfarin_conc.csv", package = "ddmore")
#   xmlDataPath = system.file("tests/data/PharmMLSO/MachineGenerated/UseCase1_FOCEI.SO.xml",  
#     package = "ddmore")
#   expected.data.path = system.file("tests/data/PharmMLSO/MachineGenerated/UseCase1_FOCEI.SO.as.data.txt",  
#     package = "ddmore")

#   # Load in SO
#   SOObject = LoadSOObject(xmlDataPath)

#   # Test for fetching Raw Data from an input file and combining with SO data 
#   combData = as.data(SOObject, inputDataPath=csvFilePath)

#   # Test no empty data frame is returned 
#   expect_true(
#     nrow(combData) > 0 , 
#     info = "Data Frame should not be empty"
#   )
  
#   # Test data frame values are as expected
#   expectedValues = dget(expected.data.path)
 
#   expect_true(
#     all.equal(expectedValues, combData), 
#     info = "Data Frame should contain correct values"
#   )

# })
