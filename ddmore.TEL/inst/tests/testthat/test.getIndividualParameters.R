
context("Test getIndividualParameters from MachineGenerated PharmMLSO Version 0.3")

test_that("Test getIndividualParameters", {
  
  data.path <- system.file("tests", "data", "PharmMLSO", "MachineGenerated", "SOv0.3", 
    "pheno.SO.xml",  
    package = "ddmore")
  
  # Load in SO
  SOObject <- suppressMessages(LoadSOObject(data.path))

  output <- getIndividualParameters(SOObject)
  
  expect_equal(sapply(X = output[-1], FUN = mean), 
    expected = c(CL = 0.00569101694915254, V = 1.4879293220339, 
        ETA_CL = -0.0176292728813559, 
        ETA_V = -0.0642248474576271))
  
  expect_error(getIndividualParameters(SOObject = SOObject, 
        what = "Invalid"), 
      regex = "Unrecognised value specified")
})


test_that("Test getIndividualParameters", {
  
  data.path <- system.file("tests", "data", "PharmMLSO", "MachineGenerated", 
    "UseCase11.SO.xml",  
    package = "ddmore")
  
  # Load in SO
  SOObject <- suppressMessages(LoadSOObject(data.path))

  output <- getIndividualParameters(SOObject)
  
  expect_equal(sapply(X = output[-1], FUN = mean), 
    expected = c(BASECOUNT = 10.245152, BETA = 0.50111, ETA_PPV_EVENT = 0.00103228))
  
})
