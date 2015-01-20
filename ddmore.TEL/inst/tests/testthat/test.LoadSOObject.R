library("DDMoRe.TEL")
library("XML")
context("Loading SOObject from PharmML")

test_that("Loading SOObject from latest hand coded PharmML SO", {

  # Clear workspace. 
  rm(list=ls())
  
  hand.coded.data.path = system.file("tests/data/PharmMLSO/HandCoded/warfarin_PK_ODE_SO_FULL.xml",  package = "DDMoRe.TEL")
        
  # Load in SO
  SOObject = LoadSOObject(hand.coded.data.path)
  
  expect_that( isS4(SOObject), TRUE, label = "Object is S4")
   
})
  
