library("ddmore")
library("XML")
require("methods")

context("Test getEstimationInfo")

test_that("Test getEstimationInfo parses hand coded warfarin_PK_ODE_SO_FULL-v0_2.xml", {
  
  data.path <- system.file("tests//data//PharmMLSO/HandCoded//warfarin_PK_ODE_SO_FULL-v0_2.xml",  
                          package = "ddmore")
  
  # Load in SO
  SOObject <- LoadSOObject(data.path)

  output <- getEstimationInfo(SOObject)

  target <- structure(list(Likelihood = structure(list(LogLikelihood = 1.234, 
    Deviance = 2.345, IndividualContribToLL = structure(list(
        ID = structure(1:2, .Label = c("i1", "i2"), class = "factor"), 
        ICtoLL = structure(c(2L, 1L), .Label = c("7.670", "7.892"
        ), class = "factor")), .Names = c("ID", "ICtoLL"), row.names = c(NA, 
    -2L), class = "data.frame"), InformationCriteria = structure(list(
        AIC = 3.456, BIC = 4.567, DIC = 5.678), .Names = c("AIC", 
    "BIC", "DIC"))), .Names = c("LogLikelihood", "Deviance", 
"IndividualContribToLL", "InformationCriteria")), Messages = structure(list(
    Errors = structure(list(`nmtran error` = "Possibly long text..."), .Names = "nmtran error")), .Names = "Errors")), .Names = c("Likelihood", 
"Messages"))

  expect_equal(output, target)

})


test_that("Test getEstimationInfo parses machine generated bootstrap_UPDRS1.SO.xml", {
  
  data.path <- system.file("tests//data//PharmMLSO/MachineGenerated//bootstrap_UPDRS1.SO.xml",  
                          package = "ddmore")
  
  # Load in SO
  SOObject <- LoadSOObject(data.path)

  output <- getEstimationInfo(SOObject)

  target <- structure(list(Likelihood = structure(list(Deviance = 1151.01368878809), .Names = "Deviance"), 
    Messages = structure(list(Info = structure(list(minimization_successful = "1", 
        covariance_step_run = "1", covariance_step_successful = "1", 
        covariance_step_warnings = "0", rounding_errors = "0", 
        hessian_reset = "0", zero_gradients = "0", final_zero_gradients = "0", 
        estimate_near_boundary = "0", s_matrix_singular = "0", 
        significant_digits = "3.5", nmoutput2so_version = "This SOBlock was created with nmoutput2so version 4.4.4"), .Names = c("minimization_successful", 
    "covariance_step_run", "covariance_step_successful", "covariance_step_warnings", 
    "rounding_errors", "hessian_reset", "zero_gradients", "final_zero_gradients", 
    "estimate_near_boundary", "s_matrix_singular", "significant_digits", 
    "nmoutput2so_version"))), .Names = "Info")), .Names = c("Likelihood", 
"Messages"))

  expect_equal(output, target)

})

