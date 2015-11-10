library("DDMoRe")
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

context("Loading SOObject from PsN Execute")

test_that("PharmML SO produced by execute in PsN fills expected slots in Estimation", {

  # Clear workspace. 
  rm(list=ls())
  
  data.path = system.file("tests/data/PharmMLSO/MachineGenerated/PsN/Warfarin-ODE-latest-execute.SO.xml",  
    package = "DDMoRe")

  # Load in SO
  SOObject = LoadSOObject(data.path)
  
  # Test is S4
  expect_true(isS4(SOObject), info = "Object is S4", )
  
  ## TODO: Commneted out/modified tests due to PSN SO not containing all slots. Should change once we have full data SO.  
  
  # Test Slots have been populated #
  # ------------------------------ #

  # # ToolSettings
  # # ------------
  # testSlotsNotEmpty(SOObject, "ToolSettings" 
  # )
  # expect_true(length(SOObject@ToolSettings) > 0 , info = "Object is not empty", )
  
  # # # RawResults
  # testSlotsNotEmpty(SOObject@RawResults, 
  #      c("DataFiles", "GraphicsFiles") 
  # )
  testSlotsNotEmpty(SOObject@RawResults, "DataFiles")

  # # TaskInformation
  # # ----------------
  expect_false(
    all(sapply(SOObject@TaskInformation$Messages, is.null)), 
    info = "All Message slots are not empty", )
  
  expect_true(
    length(names(SOObject@TaskInformation)[names(SOObject@TaskInformation) != "Messages"]) > 0, 
    info = "Messages is not the only element present")
  
  # Estimates
  # ---------
  # slotnames = c("PopulationEstimates", "IndividualEstimates", 
  #   "Residuals", "Predictions", "Likelihood")
  # testSlotsNotEmpty(SOObject@Estimation, slotnames)

  slotnames = c("PopulationEstimates", "IndividualEstimates", 
     "Predictions", "Likelihood")
  testSlotsNotEmpty(SOObject@Estimation, slotnames)

  # Simulation 
  # ----------
  # slotnames = c("Description", "OriginalDataset", "SimulationBlock")
  # testSlotsNotEmpty(SOObject@Simulation, slotnames)
   
})

context("Loading SOObject from PsN Bootstrap")

test_that("PharmML SO produced by Bootstrap in PsN fills expected slots in Estimation", {

  # Clear workspace. 
  rm(list=ls())
  
  data.path = system.file("tests/data/PharmMLSO/MachineGenerated/PsN/Warfarin-ODE-latest-bootstrap.SO.xml",  
    package = "DDMoRe")

  # Load in SO
  SOObject = LoadSOObject(data.path)
  
  # Test is S4
  expect_true(isS4(SOObject), info = "Object is S4", )
  
  ## TODO: Commneted out/modified tests due to PSN SO not containing all slots. Should change once we have full data SO. 

  # Test Slots have been populated #
  # ------------------------------ #
  
  # # ToolSettings
  # # ------------
  # testSlotsNotEmpty(SOObject, "ToolSettings" 
  # )
  # expect_true(length(SOObject@ToolSettings) > 0 , info = "Object is not empty", )
  
  # # RawResults
  # testSlotsNotEmpty(SOObject@RawResults, 
  #      c("DataFiles", "GraphicsFiles") 
  # )
    testSlotsNotEmpty(SOObject@RawResults, "DataFiles")  

  # # TaskInformation
  # # ----------------
  expect_false(
    all(sapply(SOObject@TaskInformation$Messages, is.null)), 
    info = "All Message slots are not empty", )
  
  expect_true(
    length(names(SOObject@TaskInformation)[names(SOObject@TaskInformation) != "Messages"]) > 0, 
    info = "Messages is not the only element present")
  
  # Estimates
  # ---------
  # slotnames = c("PopulationEstimates", "IndividualEstimates", 
  #   "Residuals", "Predictions", "Likelihood")
  testSlotsNotEmpty(SOObject@Estimation, 
    c("PopulationEstimates", "Likelihood", "PrecisionPopulationEstimates"))

  # Simulation 
  # ----------
  # slotnames = c("Description", "OriginalDataset", "SimulationBlock")
  # testSlotsNotEmpty(SOObject@Simulation, slotnames)
   
})

context("Loading SOObject from PsN VPC")

test_that("PharmML SO produced by VPC in PsN fills expected slots in Estimation", {

  # Clear workspace. 
  rm(list=ls())
  
  data.path = system.file("tests/data/PharmMLSO/MachineGenerated/PsN/Warfarin-ODE-latest-vpc.SO.xml",  
    package = "DDMoRe")

  # Load in SO
  SOObject = LoadSOObject(data.path)
  
  # Test is S4
  expect_true(isS4(SOObject), info = "Object is S4", )
  
  ## TODO: Commented out/modified tests due to PSN SO not containing all slots. Should change once we have full data SO.  

  # Test Slots have been populated #
  # ------------------------------ #
  
  # # ToolSettings
  # # ------------
  # testSlotsNotEmpty(SOObject, "ToolSettings" 
  # )
  # expect_true(length(SOObject@ToolSettings) > 0 , info = "Object is not empty", )
  
  # # RawResults
  # testSlotsNotEmpty(SOObject@RawResults, 
  #      c("DataFiles", "GraphicsFiles") 
  # )
  
  # # TaskInformation
  # # ----------------
  expect_false(
    all(sapply(SOObject@TaskInformation$Messages, is.null)), 
    info = "All Message slots are not empty", )
  
  expect_true(
    length(names(SOObject@TaskInformation)[names(SOObject@TaskInformation) != "Messages"]) > 0, 
    info = "Messages is not the only element present")
  
  # Estimates
  # ---------
  # slotnames = c("PopulationEstimates", "IndividualEstimates", 
  #   "Residuals", "Predictions", "Likelihood")
  # testSlotsNotEmpty(SOObject@Estimation, c("PopulationEstimates", "Likelihood"))

  # Simulation 
  # ----------
  # slotnames = c("Description", "OriginalDataset", "SimulationBlock")
  # testSlotsNotEmpty(SOObject@Simulation, slotnames)
   
})


### FIX ME: Test is currently failing because SSE has more than one SO block and 

# context("Loading SOObject from PsN SSE")

# test_that("PharmML SO produced by SSE in PsN fills expected slots in Estimation", {

#   # Clear workspace. 
#   rm(list=ls())
  
#   data.path = system.file("tests/data/PharmMLSO/MachineGenerated/PsN/Warfarin-ODE-latest-sse.SO.xml",  
#     package = "DDMoRe")

#   # Load in SO
#   SOObject = LoadSOObject(data.path)
  
#   # Test is S4
#   expect_true(isS4(SOObject), info = "Object is S4", )
  
#   # Test Slots have been populated #
#   # ------------------------------ #
  
#   # # ToolSettings
#   # # ------------
#   # testSlotsNotEmpty(SOObject, "ToolSettings" 
#   # )
#   # expect_true(length(SOObject@ToolSettings) > 0 , info = "Object is not empty", )
  
#   # # RawResults
#   # testSlotsNotEmpty(SOObject@RawResults, 
#   #      c("DataFiles", "GraphicsFiles") 
#   # )
  
#   # # TaskInformation
#   # # ----------------
#   expect_false(
#     all(sapply(SOObject@TaskInformation$Messages, is.null)), 
#     info = "All Message slots are not empty", )
  
#   expect_true(
#     length(names(SOObject@TaskInformation)[names(SOObject@TaskInformation) != "Messages"]) > 0, 
#     info = "Messages is not the only element present")
  
#   # Estimates
#   # ---------
#   # slotnames = c("PopulationEstimates", "IndividualEstimates", 
#   #   "Residuals", "Predictions", "Likelihood")
#   testSlotsNotEmpty(SOObject@Estimation, c("PopulationEstimates", "Likelihood"))

#   # Simulation 
#   # ----------
#   # slotnames = c("Description", "OriginalDataset", "SimulationBlock")
#   # testSlotsNotEmpty(SOObject@Simulation, slotnames)
   
# })



  