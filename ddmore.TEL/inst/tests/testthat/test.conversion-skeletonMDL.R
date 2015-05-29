#' Tests for MDL file with just the requisite top-level objects present but no sub-blocks

library("DDMoRe.TEL")
require("methods")

context("Loading in MDL into R objects, for skeleton MDL file")

# Clear workspace 
rm(list=ls())

skeletonJsonFile <- system.file("tests/data/json/skeleton.json", package = "DDMoRe.TEL")

test_that("Checking the existence of the test data file containing the JSON-format text representing the MDL", {
    expect_true(file.exists(skeletonJsonFile), info="Checking that test data file exists")
})

skeletonJsonAsNestedLists <- rjson:::fromJSON(file=skeletonJsonFile)

test_that("Checking that JSON-format text representing the MDL could be parsed", {
    expect_true(is.list(skeletonJsonAsNestedLists), info="JSON-as-nested-lists should be a list")
    expect_true(is.list(skeletonJsonAsNestedLists[[1]]), info="JSON-as-nested-lists should be a list of lists")
    expect_equal(length(skeletonJsonAsNestedLists[[1]]), 5, info="Should be 5 main elements in the JSON-as-nested-lists")
})

skeletonJsonAsNestedLists <- skeletonJsonAsNestedLists[[1]]

test_that("Expected dataObj to have been created from the JSON-format text representing the MDL", {
	
	mySkeletonDataObj <<- DDMoRe.TEL:::.extractTypeObject(skeletonJsonAsNestedLists, "dataobj")[[1]]
	
	expect_true(isS4(mySkeletonDataObj), info="dataObj should be an S4 class")
})

test_that("Expected parObj to have been created from the JSON-format text representing the MDL", {
	
	mySkeletonParObj <<- DDMoRe.TEL:::.extractTypeObject(skeletonJsonAsNestedLists, "parobj")[[1]]
	
	expect_true(isS4(mySkeletonParObj), info="parObj should be an S4 class")
})

test_that("Expected mdlObj to have been created from the JSON-format text representing the MDL", {
			
	mySkeletonMdlObj <<- DDMoRe.TEL:::.extractTypeObject(skeletonJsonAsNestedLists, "mdlobj")[[1]]
	
	expect_true(isS4(mySkeletonMdlObj), info="mdlObj should be an S4 class")
})

test_that("Expected taskObj to have been created from the JSON-format text representing the MDL", {
			
	mySkeletonTaskObj <<- DDMoRe.TEL:::.extractTypeObject(skeletonJsonAsNestedLists, "taskobj")[[1]]
	
	expect_true(isS4(mySkeletonTaskObj), info="taskObj should be an S4 class")
})

test_that("Expected output Mog to have been created", {

	mySkeletonOutputMog <<- createMogObj(mySkeletonDataObj, mySkeletonParObj, mySkeletonMdlObj, mySkeletonTaskObj, "skeleton_mog")
	
	expect_true(isS4(mySkeletonOutputMog), info="Output MOG object should be an S4 class")
	expect_true(class(mySkeletonOutputMog) == "mogObj", info="Checking the class of the Output MOG object")
	expect_identical(mySkeletonOutputMog@dataObj, mySkeletonDataObj, info="dataObj slot in the Output MOG object should have been populated")
	expect_identical(mySkeletonOutputMog@parObj, mySkeletonParObj, info="parObj slot in the Output MOG object should have been populated")
	expect_identical(mySkeletonOutputMog@mdlObj, mySkeletonMdlObj, info="mdlObj slot in the Output MOG object should have been populated")
	expect_identical(mySkeletonOutputMog@taskObj, mySkeletonTaskObj, info="taskObj slot in the Output MOG object should have been populated")
	expect_equal(mySkeletonOutputMog@name, "skeleton_mog", info="Checking the name of the Output MOG object")
	
})

test_that("Expected output Mog to have been written out as JSON", {
			
	skeletonJsonFileOutput <- file.path(tempdir(), "skeleton.output.json")
	
	write(mySkeletonOutputMog, skeletonJsonFileOutput)
	
	expect_true(file.exists(skeletonJsonFileOutput), info="Output JSON file should exist")
	
	skeletonJsonAsNestedListsOutput <<- rjson:::fromJSON(file=skeletonJsonFileOutput)
	
})

test_that("Checking that output JSON-as-nested-lists could be parsed", {
	expect_true(is.list(skeletonJsonAsNestedListsOutput), info="JSON-as-nested-lists should be a list")
	expect_true(is.list(skeletonJsonAsNestedListsOutput[[1]]), info="JSON-as-nested-lists should be a list of lists")
	expect_equal(length(skeletonJsonAsNestedListsOutput[[1]]), 5, info="Should be 5 main elements in the JSON-as-nested-lists")
})

skeletonJsonAsNestedListsOutput <- skeletonJsonAsNestedListsOutput[[1]]

test_that("Expected nested list representing the dataObj in the output Mog to be empty apart from its identifier", {
	expect_equal(skeletonJsonAsNestedListsOutput$skeleton_data, list(identifier="dataobj"))
})

test_that("Expected nested list representing the parObj in the output Mog to be empty apart from its identifier", {
	expect_equal(skeletonJsonAsNestedListsOutput$skeleton_par, list(identifier="parobj"))
})

test_that("Expected nested list representing the mdlObj in the output Mog to be empty apart from its identifier", {
	expect_equal(skeletonJsonAsNestedListsOutput$skeleton_mdl, list(identifier="mdlobj"))
})

test_that("Expected nested list representing the taskObj in the output Mog to be empty apart from its identifier", {
	expect_equal(skeletonJsonAsNestedListsOutput$skeleton_task, list(identifier="taskobj"))
})

