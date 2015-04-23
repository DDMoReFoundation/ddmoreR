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