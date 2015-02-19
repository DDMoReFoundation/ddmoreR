library("DDMoRe.TEL")
require("methods")

context("Loading in MDL into R objects")

# Clear workspace. 
rm(list=ls())

jsonFile <- system.file("tests/data/json/Warfarin-ODE-latest.json", package = "DDMoRe.TEL")

test_that("Checking the existence of the test data file containing the JSON-format text representing the MDL", {
	expect_true(file.exists(jsonFile), info="Checking that test data file exists")
})

jsonAsNestedLists <- rjson:::fromJSON(file=jsonFile)

test_that("Checking that JSON-format text representing the MDL could be parsed", {
	expect_true(is.list(jsonAsNestedLists), info="JSON-as-nested-lists should be a list")
	expect_true(is.list(jsonAsNestedLists[[1]]), info="JSON-as-nested-lists should be a list of lists")
	expect_equal(length(jsonAsNestedLists[[1]]), 5, info="Should be 5 main elements in the JSON-as-nested-lists")
})

jsonAsNestedLists <- jsonAsNestedLists[[1]]

test_that("Expected dataObj to have been created from the JSON-format text representing the MDL", {

	myDataObj <- DDMoRe.TEL:::.extractTypeObject(jsonAsNestedLists, "dataobj")
	
	myDataObj <- myDataObj[[1]]
	
	expect_true(isS4(myDataObj), info="dataObj should be an S4 class")
	
	expect_false(is.null(myDataObj@DATA_INPUT_VARIABLES), info="DATA_INPUT_VARIABLES slot should be populated")
	expect_false(is.null(myDataObj@SOURCE), info="SOURCE slot should be populated")

})

test_that("Expected parObj to have been created from the JSON-format text representing the MDL", {

	myParObj <- DDMoRe.TEL:::.extractTypeObject(jsonAsNestedLists, "parobj")[[1]]
	
	expect_true(isS4(myParObj), info="parObj should be an S4 class")
	
	expect_false(is.null(myParObj@STRUCTURAL), info="STRUCTURAL slot should be populated")
	expect_false(is.null(myParObj@VARIABILITY), info="VARIABILITY slot should be populated")
	
})

test_that("Expected mdlObj to have been created from the JSON-format text representing the MDL", {

	myMdlObj <- DDMoRe.TEL:::.extractTypeObject(jsonAsNestedLists, "mdlobj")[[1]]
	
	expect_true(isS4(myMdlObj), info="mdlObj should be an S4 class")
	
	expect_false(is.null(myMdlObj@MODEL_INPUT_VARIABLES), info="MODEL_INPUT_VARIABLES slot should be populated")
	expect_false(is.null(myMdlObj@STRUCTURAL_PARAMETERS), info="STRUCTURAL_PARAMETERS slot should be populated")
	expect_false(is.null(myMdlObj@VARIABILITY_PARAMETERS), info="VARIABILITY_PARAMETERS slot should be populated")
	expect_false(is.null(myMdlObj@RANDOM_VARIABLE_DEFINITION), info="RANDOM_VARIABLE_DEFINITION slot should be populated")
	expect_false(is.null(myMdlObj@INDIVIDUAL_VARIABLES), info="INDIVIDUAL_VARIABLES slot should be populated")
	expect_false(is.null(myMdlObj@MODEL_PREDICTION), info="MODEL_PREDICTION slot should be populated")
	expect_false(is.null(myMdlObj@MODEL_PREDICTION@ODE), info="MODEL_PREDICTION::ODE slot should be populated")
	expect_false(is.null(myMdlObj@OBSERVATION), info="OBSERVATION slot should be populated")
	expect_false(is.null(myMdlObj@MODEL_OUTPUT_VARIABLES), info="MODEL_OUTPUT_VARIABLES slot should be populated")
	
})

test_that("Expected taskObj to have been created from the JSON-format text representing the MDL", {

	myTaskObj <- DDMoRe.TEL:::.extractTypeObject(jsonAsNestedLists, "taskobj")[[1]]
	
	expect_true(isS4(myTaskObj), info="taskObj should be an S4 class")

	expect_false(is.null(myTaskObj@ESTIMATE), info="ESTIMATE slot should be populated")
	
})


