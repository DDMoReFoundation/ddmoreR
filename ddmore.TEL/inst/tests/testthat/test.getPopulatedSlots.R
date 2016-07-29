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



context("Test getPopulatedSlots from MachineGenerated PharmMLSO Version 0.3")

test_that("Test getPopulatedSlots returns a character vector of slot names.", {
  
  data.path <- system.file("tests", "data", "PharmMLSO", "MachineGenerated", "SOv0.3", 
    "pheno.SO.xml",  
    package = "ddmore")
  
  # Load in SO
  SOObject <- suppressMessages(LoadSOObject(data.path))
  
  # By default will return estimates,measures for precisions and confidence intervals; 
  # for each of: MLE Baysian and Bootstrap
  output <- ddmore:::getPopulatedSlots(SOObject)
    
  expect_equal(output, 
    expected = c("RawResults", 
        "Estimation::PopulationEstimates::MLE", 
        "Estimation::PrecisionPopulationEstimates::MLE", 
        "Estimation::IndividualEstimates::Estimates", 
        "Estimation::IndividualEstimates::RandomEffects", 
        "Estimation::Residuals::ResidualTable", 
        "Estimation::Predictions", 
        "Estimation::OFMeasures::Deviance"))
   
 })


test_that("Test getPopulatedSlots returns a character vector of slot names.", {
  
  data.path <- system.file("tests", "data", "PharmMLSO", "MachineGenerated", "SOv0.3", 
    "UseCase2-bootstrap.SO.xml",  
    package = "ddmore")
  
  # Load in SO
  SOObject <- suppressMessages(LoadSOObject(data.path))
  
  # By default will return estimates,measures for precisions and confidence intervals; 
  # for each of: MLE Baysian and Bootstrap
  output <- ddmore:::getPopulatedSlots(SOObject)
  
  # TODO check why "PopulationEstimates::OtherMethod[1]"
  expect_equal(output, 
    expected = c("RawResults", 
        "Estimation::PopulationEstimates::MLE", 
        "Estimation::PopulationEstimates::OtherMethodBootstrap", 
        "Estimation::PrecisionPopulationEstimates::OtherMethodBootstrap", 
        "Estimation::IndividualEstimates::Estimates", 
        "Estimation::IndividualEstimates::RandomEffects", 
        "Estimation::Residuals::ResidualTable", 
        "Estimation::Predictions", 
        "Estimation::OFMeasures::Deviance"))
    
 })


test_that("Test getPopulatedSlots returns NULL for simulation output.", {
  
  data.path <- system.file("tests", "data", "PharmMLSO", "MachineGenerated", "SOv0.3", 
    "run1.SO.xml",  
    package = "ddmore")
  
  # Load in SO
  SOObject <- suppressMessages(LoadSOObject(data.path))
  
  output <- ddmore:::getPopulatedSlots(SOObject@Estimation)
  
  expect_true(is.null(output))

})


test_that("Test getPopulatedSlots returns a character vector of slot names for Use Case 11 output.", {
  
  data.path <- system.file("tests", "data", "PharmMLSO", "MachineGenerated", 
    "UseCase11.SO.xml",  
    package = "ddmore")
  
  # Load in SO
  SOObject <- suppressMessages(LoadSOObject(data.path))
  
  output <- ddmore:::getPopulatedSlots(SOObject@Estimation)
  
  expect_equal(output, 
    expected = c("PopulationEstimates::MLE", "IndividualEstimates::Estimates", 
        "IndividualEstimates::RandomEffects", "OFMeasures::Deviance"))

})
