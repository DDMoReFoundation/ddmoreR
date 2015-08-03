library("DDMoRe.TEL")
library("XML")
require("methods")

context("Testing as.data conversion function.")

test_that("as.data correctly merges raw input data with SO data", {

  # Clear workspace. 
  rm(list=ls())
  
  # Setup paths to files
  csvFilePath = system.file("tests/data/warfarin_conc.csv", package = "DDMoRe.TEL")
  data.path = system.file("tests/data/PharmMLSO/MachineGenerated/Warfarin-ODE-latest-Monolix.SO.xml",  
    package = "DDMoRe.TEL")

  # Load in SO
  SOObject = LoadSOObject(data.path)

  # Test for fetching Raw Data from an input file and combining with SO data 
  MyDataFrame = as.data(SOObject, inputDataPath=csvFilePath)

  # Find expected column names of resulting Data Frame 
  prediction.names = setdiff(names(SOObject@Estimation@Predictions$data), c("ID", "TIME"))
  residuals.names = setdiff(names(SOObject@Estimation@Residuals$ResidualTable$data), c("ID", "TIME"))
  indivEst.estimates.names = setdiff(names(SOObject@Estimation@IndividualEstimates$Estimates$Mean$data), c("ID", "TIME"))
  indivEst.randeffects.names = setdiff(names(SOObject@Estimation@IndividualEstimates$RandomEffects$EffectMean$data), c("ID", "TIME"))

  all.names = c(toupper(names(read.csv(csvFilePath))), 
              prediction.names, residuals.names, 
              indivEst.estimates.names, indivEst.randeffects.names)

  # Test no empty data frame is returned 
  expect_true(
    nrow(MyDataFrame) > 0 , 
    info = "Data Frame should not be empty"
  )
  
  # Test column names are as expected
  expect_true(
    all(all.names == names(MyDataFrame)), 
    info = "Data Frame should contain correct column names"
  )

})

context("Testing as.data can read machine generated SO and extract header information from associated mdl file.")

test_that("as.data correctly merges raw input data with SO data", {

  # Note: these tests require the local servers to be running in order to parse the mdl file.

  # Clear workspace. 
  rm(list=ls())

  # Setup paths to files
  csvFilePath = system.file("tests/data/warfarin_infusion.csv", package = "DDMoRe.TEL")
  xml.data.path = system.file("tests/data/PharmMLSO/MachineGenerated/UseCase9.SO.xml",  
    package = "DDMoRe.TEL")
  expected.data.path = system.file("tests/data/PharmMLSO/MachineGenerated/UseCase9.SO.as.data.txt",  
    package = "DDMoRe.TEL")

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