library("DDMoRe.TEL")
library("XML")
require("methods")

testSlotsNotEmpty <- function(S4class, slotnames) {

  for (slotname in slotnames){

  SOslot = slot(S4class, slotname)
  
  expect_true(
    length(SOslot) > 0 , 
    info = "Slot is not empty", )
  }
  
}

context("Loading SOObject from hand coded PharmML")

test_that("PharmML SO fills expected slots in Estimation", {

  # Clear workspace. 
  rm(list=ls())
  
  data.path = system.file("tests/data/PharmMLSO/HandCoded/warfarin_PK_ODE_SO_FULL.xml",  package = "DDMoRe.TEL")
        
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
  slotnames = c("Description", "OriginalDataset", "SimulationBlock")
  testSlotsNotEmpty(SOObject@Simulation, slotnames)
   
})
  