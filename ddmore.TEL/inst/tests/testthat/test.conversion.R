library("DDMoRe.TEL")
require("methods")

context("Loading in MDL into R objects")

# Clear workspace. 
rm(list=ls())

jsonFile <- system.file("tests/data/json/FullyPopulated.json", package = "DDMoRe.TEL")

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

	myDataObj <<- DDMoRe.TEL:::.extractTypeObject(jsonAsNestedLists, "dataobj")[[1]]
	
	expect_true(isS4(myDataObj), info="dataObj should be an S4 class")
	
	expect_false(is.null(myDataObj@DECLARED_VARIABLES), info="DECLARED_VARIABLES slot should be populated")
	expect_false(is.null(myDataObj@DATA_INPUT_VARIABLES), info="DATA_INPUT_VARIABLES slot should be populated")
	expect_false(is.null(myDataObj@SOURCE), info="SOURCE slot should be populated")

})

test_that("Expected parObj to have been created from the JSON-format text representing the MDL", {

	myParObj <<- DDMoRe.TEL:::.extractTypeObject(jsonAsNestedLists, "parobj")[[1]]
	
	expect_true(isS4(myParObj), info="parObj should be an S4 class")
	
	expect_false(is.null(myParObj@DECLARED_VARIABLES), info="DECLARED_VARIABLES slot should be populated")
	expect_false(is.null(myParObj@STRUCTURAL), info="STRUCTURAL slot should be populated")
	expect_false(is.null(myParObj@VARIABILITY), info="VARIABILITY slot should be populated")
	
})

test_that("Expected mdlObj to have been created from the JSON-format text representing the MDL", {

	myMdlObj <<- DDMoRe.TEL:::.extractTypeObject(jsonAsNestedLists, "mdlobj")[[1]]
	
	expect_true(isS4(myMdlObj), info="mdlObj should be an S4 class")
	
	expect_false(is.null(myMdlObj@IDV), info="IDV slot should be populated")
	expect_false(is.null(myMdlObj@COVARIATES), info="COVARIATES slot should be populated")
	expect_false(is.null(myMdlObj@VARIABILITY_LEVELS), info="VARIABILITY_LEVELS slot should be populated")
	expect_false(is.null(myMdlObj@STRUCTURAL_PARAMETERS), info="STRUCTURAL_PARAMETERS slot should be populated")
	expect_false(is.null(myMdlObj@VARIABILITY_PARAMETERS), info="VARIABILITY_PARAMETERS slot should be populated")
	expect_false(is.null(myMdlObj@RANDOM_VARIABLE_DEFINITION), info="RANDOM_VARIABLE_DEFINITION slot should be populated")
	expect_false(is.null(myMdlObj@INDIVIDUAL_VARIABLES), info="INDIVIDUAL_VARIABLES slot should be populated")
	expect_false(is.null(myMdlObj@MODEL_PREDICTION), info="MODEL_PREDICTION slot should be populated")
	expect_false(is.null(myMdlObj@MODEL_PREDICTION$.DEQ), info="MODEL_PREDICTION::DEQ slot should be populated")
	expect_false(is.null(myMdlObj@MODEL_PREDICTION$.PKMACRO), info="MODEL_PREDICTION::PKMACRO slot should be populated")
	expect_false(is.null(myMdlObj@MODEL_PREDICTION$.COMPARTMENT), info="MODEL_PREDICTION::COMPARTMENT slot should be populated")
	expect_false(is.null(myMdlObj@OBSERVATION), info="OBSERVATION slot should be populated")
	expect_false(is.null(myMdlObj@MODEL_OUTPUT_VARIABLES), info="MODEL_OUTPUT_VARIABLES slot should be populated")
	
})

test_that("Expected taskObj to have been created from the JSON-format text representing the MDL", {

	myTaskObj <<- DDMoRe.TEL:::.extractTypeObject(jsonAsNestedLists, "taskobj")[[1]]
	
	expect_true(isS4(myTaskObj), info="taskObj should be an S4 class")

	expect_false(is.null(myTaskObj@ESTIMATE), info="ESTIMATE slot should be populated")
	
})

test_that("Expected output Mog to have been created", {

	myOutputMog <<- createMogObj(myDataObj, myParObj, myMdlObj, myTaskObj, "warfarin_PK_ODE_mog")
	
	expect_true(isS4(myOutputMog), info="Output MOG object should be an S4 class")
	expect_true(class(myOutputMog) == "mogObj", info="Checking the class of the Output MOG object")
	expect_identical(myOutputMog@dataObj, myDataObj, info="dataObj slot in the Output MOG object should have been populated")
	expect_identical(myOutputMog@parObj, myParObj, info="parObj slot in the Output MOG object should have been populated")
	expect_identical(myOutputMog@mdlObj, myMdlObj, info="mdlObj slot in the Output MOG object should have been populated")
	expect_identical(myOutputMog@taskObj, myTaskObj, info="taskObj slot in the Output MOG object should have been populated")
	expect_equal(myOutputMog@name, "warfarin_PK_ODE_mog", info="Checking the name of the Output MOG object")

})

test_that("Expected output Mog to have been written out as JSON", {

	jsonFileOutput <- file.path(tempdir(), "FullyPopulated.output.json")
	
	write(myOutputMog, jsonFileOutput)
	
	expect_true(file.exists(jsonFileOutput), info="Output JSON file should exist")
	
	jsonAsNestedListsOutput <<- rjson:::fromJSON(file=jsonFileOutput)

})

test_that("Checking that output JSON-as-nested-lists could be parsed", {
	expect_true(is.list(jsonAsNestedListsOutput), info="JSON-as-nested-lists should be a list")
	expect_true(is.list(jsonAsNestedListsOutput[[1]]), info="JSON-as-nested-lists should be a list of lists")
	expect_equal(length(jsonAsNestedListsOutput[[1]]), 5, info="Should be 5 main elements in the JSON-as-nested-lists")
})

jsonAsNestedListsOutput <- jsonAsNestedListsOutput[[1]]

# Compares the attributes of an item within a block
compareAttributesOfElementOfBlock <- function(objName, blockName, elemNo, inputAttributes, outputAttributes) {
	#cat(paste("Comparing attributes for:", objName, blockName, elemNo, "\n"))
	
	test_that(paste("Comparing the names of the attributes of item", elemNo, "in the input and output nested-list representations of block:", blockName), {
		expect_equal(length(outputAttributes), length(inputAttributes))
		expect_equal(length(names(inputAttributes)), length(inputAttributes)) # Ensure the attributes are named
		expect_equal(length(names(outputAttributes)), length(outputAttributes)) # Ensure the attributes are named
		expect_equal(sort(names(outputAttributes)), sort(names(inputAttributes)))
	})

	attrNames <- names(inputAttributes)

	lapply(attrNames, function(attrName) {

		if ( (blockName == 'MODEL_PREDICTION') && (attrName %in% c(".DEQ", ".PKMACRO", ".COMPARTMENT")) ) {
			# Nested sub-blocks have to be treated as blocks in their own right for the purposes of comparison of their content
			compareNestedListsRepresentationOfBlock(objName, paste0('MODEL_PREDICTION::', attrName), inputAttributes[[attrName]], outputAttributes[[attrName]]) 
		} else {
			test_that(paste("Comparing the", attrName, "attribute of item", elemNo, "in the input and output nested-list representations of block:", blockName), {
				expect_equal(outputAttributes[[attrName]], inputAttributes[[attrName]])
			})
		}
	})

}

# Compares the blocks within dataObj, parObj, mdlObj or taskObj, in nested-list representation
compareNestedListsRepresentationOfBlock <- function(objName, blockName, inputBlockAsNestedList, outputBlockAsNestedList) {
	
	cat(paste0("Comparing block name ", objName, "::", blockName, "...\n"))
	
	test_that(paste("Same number of items expected for input and output nested-list representations of block:", blockName), {
		expect_equal(length(outputBlockAsNestedList), length(inputBlockAsNestedList))
	})

	lapply(seq(1:length(inputBlockAsNestedList)), function(i) {
		compareAttributesOfElementOfBlock(objName, blockName, i, inputBlockAsNestedList[[i]], outputBlockAsNestedList[[i]])
	})

}

# Compares dataObj, parObj, mdlObj or taskObj, in nested-list representation
compareNestedListsRepresentationOfTopLevelObject <- function(objName) {
	
	inputObjAsNestedLists <- jsonAsNestedLists[[objName]]
	outputObjAsNestedLists <- jsonAsNestedListsOutput[[objName]]

	test_that(paste("Comparing the blocks for the input and output nested-list representations of object:", objName), {

		expect_true(
			is.list(inputObjAsNestedLists) && length(inputObjAsNestedLists) > 0,
			info=paste("Input JSON-as-nested-lists should contain list of blocks for object:", objName)
		)
		expect_true(
			is.list(outputObjAsNestedLists) && length(outputObjAsNestedLists) > 0,
			info=paste("Output JSON-as-nested-lists should contain list of blocks for object:", objName)
		)
		
		inputObjBlockNames <<- names(inputObjAsNestedLists)
		outputObjBlockNames <<- names(outputObjAsNestedLists)
		
		expect_equal(length(outputObjBlockNames), length(inputObjBlockNames),
			info=paste("Same number of blocks expected for input and output nested-list representations of object:", objName))
		
		expect_equal(sort(outputObjBlockNames), sort(inputObjBlockNames),
			info=paste("Same list of blocks expected for input and output nested-list representations of object:", objName))
		
	})

	lapply(inputObjBlockNames, function(blockName) {
		
		if (blockName == 'identifier') {
			test_that(paste('Comparing the identifiers for the input and output nested-list representations of object:', objName), {
				expect_equal(outputObjAsNestedLists[['identifier']], inputObjAsNestedLists[['identifier']])
			})
		} else if (blockName == 'SOURCE') {
			# Treat the SOURCE block specially since it is a set of attributes itself rather than being a list of sets of attributes
			compareAttributesOfElementOfBlock(objName, "SOURCE", NA, inputObjAsNestedLists[[blockName]], outputObjAsNestedLists[[blockName]])
		} else if (blockName == 'ESTIMATE') {
			# Treat the ESTIMATE block (of a task object; ideally we'd check this too) specially since it is a plain character vector
			test_that(paste('Comparing the text of the ESTIMATE block for the input and output nested-list representations of object:', objName), {
				expect_equal(outputObjAsNestedLists[['ESTIMATE']], inputObjAsNestedLists[['ESTIMATE']])
			})
		} else {
			compareNestedListsRepresentationOfBlock(objName, blockName, inputObjAsNestedLists[[blockName]], outputObjAsNestedLists[[blockName]])
		}
	})
	
}


compareNestedListsRepresentationOfTopLevelObject(myDataObj@name)
compareNestedListsRepresentationOfTopLevelObject(myParObj@name)
compareNestedListsRepresentationOfTopLevelObject(myMdlObj@name)
compareNestedListsRepresentationOfTopLevelObject(myTaskObj@name)
compareNestedListsRepresentationOfTopLevelObject("warfarin_PK_ODE_mog")

