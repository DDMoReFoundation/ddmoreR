library("DDMoRe.TEL")
library("XML")
require("methods")

#
# Tests currentl commented out due to abscence of machine generated data for pharmML SO v0.1 to use for test.
#


# context("Testing as.data conversion function.")

# test_that("as.data correctly merges raw input data with SO data", {

#   # Clear workspace. 
#   rm(list=ls())
  
#   # Setup paths to files
#   csvFilePath = system.file("tests/data/warfarin_conc.csv", package = "DDMoRe.TEL")
#   data.path = system.file("tests/data/PharmMLSO/MachineGenerated/Warfarin-ODE-latest.SO.xml",  
#     package = "DDMoRe.TEL")

#   # Load in SO
#   SOObject = LoadSOObject(data.path)

#   # Test for fetching Raw Data from an input file and combining with SO data 
#   MyDataFrame = as.data(SOObject, inputDataPath=csvFilePath)

#   # Find expected column names of resulting Data Frame 
#   prediction.names = setdiff(names(SOObject@Estimation@Predictions$data), c("ID", "TIME"))
#   residuals.names = setdiff(names(SOObject@Estimation@Residuals$data), c("ID", "TIME"))
#   indivEst.estimates.names = setdiff(names(SOObject@Estimation@IndividualEstimates$Estimates$Mean$data), c("ID", "TIME"))
#   indivEst.randeffects.names = setdiff(names(SOObject@Estimation@IndividualEstimates$RandomEffects$EffectMean$data), c("ID", "TIME"))

#   all.names = c(names(read.csv(csvFilePath)), 
#               prediction.names, residuals.names, 
#               indivEst.estimates.names, indivEst.randeffects.names)

  
#   expect_true(
#     nrow(MyDataFrame) > 0 , 
#     info = "Data Frame is not empty"
#   )
  
  # Convert to xpose data base. 
 #  myXpdb = as.xpdb(SOObject, inputDataPath=csvFilePath) 
  
  # Test Slots have been populated #
  # ------------------------------ #
  
  # # ToolSettings
  # # ------------
  # testSlotsNotEmpty(SOObject, "ToolSettings" 
  # )
  # expect_true(length(SOObject@ToolSettings) > 0 , info = "Object is not empty" )
  
  # # RawResults
  # testSlotsNotEmpty(SOObject@RawResults, 
  #      c("DataFiles", "GraphicsFiles") 
  # )
  
  # # TaskInformation
  # # ----------------
  # expect_false(
  #   all(sapply(SOObject@TaskInformation$Messages, is.null)), 
  #   info = "All Message slots are not empty" )
  
  # expect_true(
  #   length(names(SOObject@TaskInformation)[names(SOObject@TaskInformation) != "Messages"]) > 0, 
  #   info = "Messages is not the only element present")
  
  # Estimates
  # ---------
  # slotnames = c("PopulationEstimates", "IndividualEstimates", 
  #   "Residuals", "Predictions", "Likelihood")
  # testSlotsNotEmpty(SOObject@Estimation, slotnames)

  # Simulation 
  # ----------
  # slotnames = c("Description", "OriginalDataset", "SimulationBlock")
  # testSlotsNotEmpty(SOObject@Simulation, slotnames)
   
# })
