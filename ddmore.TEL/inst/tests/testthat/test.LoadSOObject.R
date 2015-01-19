library("DDMoRe.TEL")
library("XML")
context("Loading SOObject from PharmML")

test_that("Loading SOObject from latest hand coded PharmML SO", {

  # Clear workspace. 
  rm(list=ls())
  
  hand.coded.data.path = system.file("data/tests/PharmMLSO/HandCoded/warfarin_PK_ODE_SO_FULL.xml",  package = "DDMoRe.TEL")
        
  # Load in SO
  SOObject = LoadSOObject(hand.coded.data.path)
  
  
  expect_equal(str_length("a"), 1)
  expect_equal(str_length("ab"), 2)
  expect_equal(str_length("abc"), 3)
   
})
  
