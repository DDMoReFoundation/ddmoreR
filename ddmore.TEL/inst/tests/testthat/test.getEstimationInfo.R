################################################################################
# Copyright (C) 2016 Mango Business Solutions Ltd, http://www.mango-solutions.com
#
# This program is free software: you can redistribute it and/or modify it under
# the terms of the GNU Affero General Public License as published by the
# Free Software Foundation, version 3.
#
# This program is distributed in the hope that it will be useful,
# but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY
# or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License
# for more details.
#
# You should have received a copy of the GNU Affero General Public License along
# with this program. If not, see <http://www.gnu.org/licenses/agpl-3.0.html>.
################################################################################


context("Test getEstimationInfo from MachineGenerated PharmMLSO Version 0.3")

test_that("Test getEstimationInfo", {
  
  data.path <- system.file("tests", "data", "PharmMLSO", "MachineGenerated", "SOv0.3", 
    "pheno.SO.xml",  
    package = "ddmore")
  
  # Load in SO
  SOObject <- suppressMessages(LoadSOObject(data.path))

  output <- getEstimationInfo(SOObject)

  target <- structure(
    list(
        OFMeasures = structure(
            list(
                Deviance = list(758.663331069924)), 
            .Names = "Deviance"),
        Messages = structure(
            list(Info = structure(
                list(
                    minimization_successful = "1", 
                    covariance_step_run = "1", 
                    covariance_step_successful = "1", 
                    covariance_step_warnings = "0", 
                    rounding_errors = "0", 
                    hessian_reset = "0", 
                    zero_gradients = "0", 
                    final_zero_gradients = "0", 
                    estimate_near_boundary = "0", 
                    s_matrix_singular = "0", 
                    significant_digits = "4.1", 
                    nmoutput2so_version = "This SOBlock was created with nmoutput2so version 4.5.13"), 
                .Names = c("minimization_successful", 
                    "covariance_step_run", "covariance_step_successful", 
                    "covariance_step_warnings", 
                    "rounding_errors", "hessian_reset", "zero_gradients", 
                    "final_zero_gradients", 
                    "estimate_near_boundary", "s_matrix_singular", 
                    "significant_digits", 
                    "nmoutput2so_version"))), 
            .Names = "Info")), 
        .Names = c("OFMeasures", "Messages"))


  expect_equal(output, target)

})


test_that("Test getEstimationInfo no OFMeasures.", {
  
  data.path <- system.file("tests", "data", "PharmMLSO", "MachineGenerated", "SOv0.3", 
    "UseCase2-bootstrap.SO.xml",  
    package = "ddmore")
  
  # Load in SO
  SOObject <- suppressMessages(LoadSOObject(data.path))

  output <- suppressMessages(getEstimationInfo(SOObject))
  
  expect_equal(ceiling(output$OFMeasures$Deviance[[1]]), 
      expected = -284)
  
  expect_equal(names(output$Messages$Info), 
      c("estimation_successful", "covariance_step_run", "rounding_errors", 
          "estimate_near_boundary", "s_matrix_singular", "nmoutput2so_version"))

})


test_that("Test getEstimationInfo returns correct statistics by default for MLE estimates.", {
  
  data.path <- system.file("tests", "data", "PharmMLSO", "MachineGenerated", 
    "UseCase11.SO.xml",  
    package = "ddmore")
  
  # Load in SO
  SOObject <- suppressMessages(LoadSOObject(data.path))
  
  output <- suppressWarnings(getEstimationInfo(SOObject))
  
  expect_equal(ceiling(output$OFMeasures$Deviance[[1]]), 
      expected = 3560)
})
