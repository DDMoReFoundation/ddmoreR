library("DDMoRe.TEL")
library("XML")
require("methods")

context("Loading SOObject from hand coded PharmML")

test_that("Loading SOObject from PharmML SO fills expected slots in Estimation", {

  # Clear workspace. 
  rm(list=ls())
  
  hand.coded.data.path = system.file("tests/data/PharmMLSO/HandCoded/warfarin_PK_ODE_SO_FULL.xml",  package = "DDMoRe.TEL")
        
  # Load in SO
  SOObject = LoadSOObject(hand.coded.data.path)
  
  # Test is S4
  expect_true(isS4(SOObject), info = "Object is S4", )
  
  # Test Slots have been populated #
  # ------------------------------ #
  
  # ToolSettings
  expect_true(length(SOObject@ToolSettings) > 0 , info = "Object is not empty", )
  
  # RawResults
  expect_true(
    length(SOObject@RawResults@DataFiles) > 0 , 
    info = "Slot is not empty", )
  
  expect_true(
    length(SOObject@RawResults@GraphicsFiles) > 0 , 
    info = "Slot is not empty", )
  
  #TaskInformation
  expect_false(
    all(sapply(SOObject@TaskInformation$Messages, is.null)), 
    info = "All Message slots are not empty", )
  
  expect_true(
    length(names(SOObject@TaskInformation)[names(SOObject@TaskInformation) != "Messages"]) > 0, 
    info = "Messages is not the only element present")
  
  # Estimates
  expect_true(
    length(SOObject@Estimation@PopulationEstimates) > 0 , 
    info = "Slot is not empty", )
  
  expect_true(
    length(SOObject@Estimation@PrecisionPopulationEstimates) > 0 , 
    info = "Slot is not empty", )
  
  expect_true(
    length(SOObject@Estimation@IndividualEstimates) > 0 , 
    info = "Slot is not empty", )
  
  expect_true(
    length(SOObject@Estimation@PrecisionIndividualEstimates) > 0 , 
    info = "Slot is not empty", )
  
  expect_true(
    length(SOObject@Estimation@Residuals) > 0 , 
    info = "Slot is not empty", )
  
  expect_true(
    length(SOObject@Estimation@Predictions) > 0 , 
    info = "Slot is not empty", )
  
  expect_true(
    length(SOObject@Estimation@Likelihood) > 0 , 
    info = "Slot is not empty", )
  
   
})
  
test_that("Loading SOObject from PharmML SO fills expected slots in Simulation", {

  # Clear workspace. 
  rm(list=ls())
  
  hand.coded.data.path = system.file("tests/data/PharmMLSO/HandCoded/warfarin_PK_ODE_SO_FULL.xml",  package = "DDMoRe.TEL")
        
  # Load in SO
  SOObject = LoadSOObject(hand.coded.data.path)
  
  # Test is S4
  expect_true(isS4(SOObject), info = "Object is S4", )
  
  # Test Slots have been populated #
  # ------------------------------ #
  
  # SimulatedProfiles
  expect_true(length(SOObject@Simulation@Description) > 0 , info = "Object is not empty", )
  
  # SimulatedProfiles
  expect_true(length(SOObject@Simulation@OriginalDataset) > 0 , info = "Object is not empty", )

  # SimulatedProfiles
  expect_true(length(SOObject@Simulation@SimulationBlock) > 0 , info = "Object is not empty", )
   
})



# 
# context("Loading SOObject from machine coded PharmML (Warfarin-ODE-latest.SO.xml)")
# 
# test_that("Loading SOObject from latest Machine Generated PharmML SO (Warfarin-ODE-latest.SO.xml)", {
#   
#   # Clear workspace. 
#   rm(list=ls())
#   
#   machine.coded.data.path = system.file("tests/data/PharmMLSO/MachineGenerated/Warfarin-ODE-latest.SO.xml",  package = "DDMoRe.TEL")
#   
#   # Load in SO
#   SOObject = LoadSOObject(machine.coded.data.path)
#   
#   # Test is S4
#   expect_true(isS4(SOObject), info = "Object is S4", )
#   
#   # Test Slots have been populated #
#   # ------------------------------ #
#   
#   # ToolSettings
#   expect_true(length(SOObject@ToolSettings) > 0 , info = "Object is not empty", )
#   
#   # RawResults
#   expect_true(
#     length(SOObject@RawResults@DataFiles) > 0 , 
#     info = "Slot is not empty", )
#   
#   expect_true(
#     length(SOObject@RawResults@GraphicsFiles) > 0 , 
#     info = "Slot is not empty", )
#   
#   #TaskInformation
#   expect_false(
#     all(sapply(SOObject@TaskInformation$Messages, is.null)), 
#     info = "All Message slots are not empty", )
#   
#   expect_true(
#     length(names(SOObject@TaskInformation)[names(SOObject@TaskInformation) != "Messages"]) > 0, 
#     info = "Messages is not the only element present")
#   
#   # Estimates
#   expect_true(
#     length(SOObject@Estimation@PopulationEstimates) > 0 , 
#     info = "Slot is not empty", )
#   
#   expect_true(
#     length(SOObject@Estimation@PrecisionPopulationEstimates) > 0 , 
#     info = "Slot is not empty", )
#   
#   expect_true(
#     length(SOObject@Estimation@IndividualEstimates) > 0 , 
#     info = "Slot is not empty", )
#   
#   expect_true(
#     length(SOObject@Estimation@PrecisionIndividualEstimates) > 0 , 
#     info = "Slot is not empty", )
#   
#   expect_true(
#     length(SOObject@Estimation@Residuals) > 0 , 
#     info = "Slot is not empty", )
#   
#   expect_true(
#     length(SOObject@Estimation@Predictions) > 0 , 
#     info = "Slot is not empty", )
#   
#   expect_true(
#     length(SOObject@Estimation@Likelihood) > 0 , 
#     info = "Slot is not empty", )
#   
# })
# 
# 
# context("Loading SOObject from machine coded PharmML (pheno.SO.xml)")
# 
# test_that("Loading SOObject from latest Machine Generated PharmML SO (pheno.SO.xml)", {
#   
#   # Clear workspace. 
#   rm(list=ls())
#   
#   machine.coded.data.path = system.file("tests/data/PharmMLSO/MachineGenerated/pheno.SO.xml",  package = "DDMoRe.TEL")
#   
#   # Load in SO
#   SOObject = LoadSOObject(machine.coded.data.path)
#   
#   # Test is S4
#   expect_true(isS4(SOObject), info = "Object is S4", )
#   
#   # Test Slots have been populated #
#   # ------------------------------ #
#   
#   # ToolSettings
#   expect_true(length(SOObject@ToolSettings) > 0 , info = "Object is not empty", )
#   
#   # RawResults
#   expect_true(
#     length(SOObject@RawResults@DataFiles) > 0 , 
#     info = "Slot is not empty", )
#   
#   expect_true(
#     length(SOObject@RawResults@GraphicsFiles) > 0 , 
#     info = "Slot is not empty", )
#   
#   #TaskInformation
#   expect_false(
#     all(sapply(SOObject@TaskInformation$Messages, is.null)), 
#     info = "All Message slots are not empty", )
#   
#   expect_true(
#     length(names(SOObject@TaskInformation)[names(SOObject@TaskInformation) != "Messages"]) > 0, 
#     info = "Messages is not the only element present")
#   
#   # Estimates
#   expect_true(
#     length(SOObject@Estimation@PopulationEstimates) > 0 , 
#     info = "Slot is not empty", )
#   
#   expect_true(
#     length(SOObject@Estimation@PrecisionPopulationEstimates) > 0 , 
#     info = "Slot is not empty", )
#   
#   expect_true(
#     length(SOObject@Estimation@IndividualEstimates) > 0 , 
#     info = "Slot is not empty", )
#   
#   expect_true(
#     length(SOObject@Estimation@PrecisionIndividualEstimates) > 0 , 
#     info = "Slot is not empty", )
#   
#   expect_true(
#     length(SOObject@Estimation@Residuals) > 0 , 
#     info = "Slot is not empty", )
#   
#   expect_true(
#     length(SOObject@Estimation@Predictions) > 0 , 
#     info = "Slot is not empty", )
#   
#   expect_true(
#     length(SOObject@Estimation@Likelihood) > 0 , 
#     info = "Slot is not empty", )
#   
# })
