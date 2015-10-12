#' Tests for MDL file with as many blocks and sub-blocks populated as possible, and with a variety of types/formats of variables and attributes

library("DDMoRe.TEL")
require("methods")

context("Loading in MDL into R objects, for fully populated MDL file")

# Clear workspace 
rm(list=ls())

jsonFile <- system.file("tests/data/json/FullyPopulated.json", package = "DDMoRe.TEL")

test_that("Checking the existence of the test data file containing the JSON-format text representing the MDL", {
	expect_true(file.exists(jsonFile), info="Checking that test data file exists")
})

jsonAsNestedLists <- rjson:::fromJSON(file=jsonFile)

test_that("Checking that JSON-format text representing the MDL could be parsed", {
	expect_true(is.list(jsonAsNestedLists), info="JSON-as-nested-lists should be a list")
	expect_true(length(jsonAsNestedLists) == 5, info="JSON-as-nested-lists should be a list containing 5 elements")
})

test_that("Expected dataObj to have been created from the JSON-format text representing the MDL", {

	myDataObj <<- DDMoRe.TEL:::.extractTypeObjects(jsonAsNestedLists, "dataObj")[[1]]
	
	expect_true(isS4(myDataObj), info="dataObj should be an S4 class")
	
	expect_false(is.null(myDataObj@SOURCE), info="SOURCE slot should be populated")
	expect_false(is.null(myDataObj@DECLARED_VARIABLES), info="DECLARED_VARIABLES slot should be populated")
	expect_false(is.null(myDataObj@DATA_INPUT_VARIABLES), info="DATA_INPUT_VARIABLES slot should be populated")
	expect_false(is.null(myDataObj@DATA_DERIVED_VARIABLES), info="DATA_DERIVED_VARIABLES slot should be populated")

})

test_that("Expected parObj to have been created from the JSON-format text representing the MDL", {

	myParObj <<- DDMoRe.TEL:::.extractTypeObjects(jsonAsNestedLists, "parObj")[[1]]
	
	expect_true(isS4(myParObj), info="parObj should be an S4 class")
	
	expect_false(is.null(myParObj@DECLARED_VARIABLES), info="DECLARED_VARIABLES slot should be populated")
	expect_false(is.null(myParObj@STRUCTURAL), info="STRUCTURAL slot should be populated")
	expect_false(is.null(myParObj@VARIABILITY), info="VARIABILITY slot should be populated")
	
})

test_that("Expected mdlObj to have been created from the JSON-format text representing the MDL", {

	myMdlObj <<- DDMoRe.TEL:::.extractTypeObjects(jsonAsNestedLists, "mdlObj")[[1]]
	
	expect_true(isS4(myMdlObj), info="mdlObj should be an S4 class")
	
	expect_false(is.null(myMdlObj@IDV), info="IDV slot should be populated")
	expect_false(is.null(myMdlObj@COVARIATES), info="COVARIATES slot should be populated")
	expect_false(is.null(myMdlObj@VARIABILITY_LEVELS), info="VARIABILITY_LEVELS slot should be populated")
	expect_false(is.null(myMdlObj@STRUCTURAL_PARAMETERS), info="STRUCTURAL_PARAMETERS slot should be populated")
	expect_false(is.null(myMdlObj@VARIABILITY_PARAMETERS), info="VARIABILITY_PARAMETERS slot should be populated")
	expect_false(is.null(myMdlObj@RANDOM_VARIABLE_DEFINITION), info="RANDOM_VARIABLE_DEFINITION slot should be populated")
	expect_false(is.null(myMdlObj@INDIVIDUAL_VARIABLES), info="INDIVIDUAL_VARIABLES slot should be populated")
	expect_false(is.null(myMdlObj@MODEL_PREDICTION), info="MODEL_PREDICTION slot should be populated")
	# TODO fix this check
	#expect_false(is.null(myMdlObj@MODEL_PREDICTION$DEQ), info="MODEL_PREDICTION::DEQ slot should be populated")
	# TODO fix this check
	#expect_false(is.null(myMdlObj@MODEL_PREDICTION$COMPARTMENT), info="MODEL_PREDICTION::COMPARTMENT slot should be populated")
	expect_false(is.null(myMdlObj@OBSERVATION), info="OBSERVATION slot should be populated")
	expect_false(is.null(myMdlObj@GROUP_VARIABLES), info="GROUP_VARIABLES slot should be populated")
	
})

test_that("Expected taskObj to have been created from the JSON-format text representing the MDL", {

	myTaskObj <<- DDMoRe.TEL:::.extractTypeObjects(jsonAsNestedLists, "taskObj")[[1]]
	
	expect_true(isS4(myTaskObj), info="taskObj should be an S4 class")

	expect_false(is.null(myTaskObj@ESTIMATE), info="ESTIMATE slot should be populated")
	
})

test_that("Expected output Mog to have been created", {

	myOutputMog <<- createMogObj(myDataObj, myParObj, myMdlObj, myTaskObj, "fully_populated_mog")
	
	expect_true(isS4(myOutputMog), info="Output MOG object should be an S4 class")
	expect_true(class(myOutputMog) == "mogObj", info="Checking the class of the Output MOG object")
	expect_identical(myOutputMog@dataObj, myDataObj, info="dataObj slot in the Output MOG object should have been populated")
	expect_identical(myOutputMog@parObj, myParObj, info="parObj slot in the Output MOG object should have been populated")
	expect_identical(myOutputMog@mdlObj, myMdlObj, info="mdlObj slot in the Output MOG object should have been populated")
	expect_identical(myOutputMog@taskObj, myTaskObj, info="taskObj slot in the Output MOG object should have been populated")
	expect_equal(myOutputMog@name, "fully_populated_mog", info="Checking the name of the Output MOG object")

})

test_that("Expected output Mog to have been written out as JSON", {

	jsonFileOutput <<- file.path(tempdir(), "FullyPopulated.output.json")
	
	writeLines(.generateJSON(myOutputMog), jsonFileOutput)
	
	expect_true(file.exists(jsonFileOutput), info="Output JSON file should exist")
	
	jsonAsNestedListsOutput <<- rjson:::fromJSON(file=jsonFileOutput)

})

stopifnot(file.exists(jsonFileOutput))

test_that("Checking that output JSON-as-nested-lists could be parsed", {
	expect_true(is.list(jsonAsNestedListsOutput), info="JSON-as-nested-lists should be a list")
	expect_true(length(jsonAsNestedListsOutput) == 5, info="JSON-as-nested-lists should be a list containing 5 elements")
})

# Compares a variable/item within a block
compareBlockItems <- function(objName, blockName, itemContext, inputBlockItems, outputBlockItems) {
    if (is.null(inputBlockItems) || is.null(outputBlockItems)) {
        message(paste("Warning one of the item lists of",blockName,"is null.\n"))
        test_that(paste("Both blocks", blockName, " should be null, since one of them is"), {
            expect_true(is.null(inputBlockItems)&&is.null(outputBlockItems))
        })
        return(NULL) # return at this point to remove warnings from output
    }
	
	test_that(paste("Comparing the names of the attributes of item", itemContext, "in the input and output nested-list representations of block:", blockName), {
		expect_equal(length(outputBlockItems), length(inputBlockItems))
		expect_equal(length(names(inputBlockItems)), length(inputBlockItems)) # Ensure the attributes are named
		expect_equal(length(names(outputBlockItems)), length(outputBlockItems)) # Ensure the attributes are named
		expect_equal(sort(names(outputBlockItems)), sort(names(inputBlockItems)))
	})

	attrNames <- names(inputBlockItems)

	lapply(attrNames, function(attrName) {
				
		message("Comparing ", objName, "::", blockName, "::", itemContext, "::", attrName, "\n")
		
		test_that(paste("Same class type expected for the input and output nested-list representations of block: ", blockName, ", item: ", attrName), {
			expect_equal(class(outputBlockItems[[attrName]]), class(inputBlockItems[[attrName]]))
		})
		
		if (attrName%in%c("DEQ", "COMPARTMENT")) {
			compareNestedListsRepresentationOfBlock(objName, attrName, inputBlockItems[[attrName]], outputBlockItems[[attrName]])
		} else if (is.list(inputBlockItems[[attrName]])) { # Extra layer of nesting
			compareBlockItems(objName, blockName, paste0(itemContext,"::",attrName), inputBlockItems[[attrName]], outputBlockItems[[attrName]])
		} else {
			test_that(paste("Comparing the", attrName, "attribute of item", itemContext, "in the input and output nested-list representations of block:", blockName), {
				expect_equal(outputBlockItems[[attrName]], inputBlockItems[[attrName]])
			})
		}
		
	})

}

# Compares the blocks within dataObj, parObj, mdlObj or taskObj, in vector representation
compareVectorRepresentationOfBlock <- function(objName, blockName, inputBlockAsVector, outputBlockAsVector) {
	
	message(paste0("\nComparing block name ", objName, "::", blockName, "...\n"))

	if (is.null(inputBlockAsVector) || is.null(outputBlockAsVector)) {
		message(paste("Warning one of the blocks",blockName,"is null.\n"))
		test_that(paste("Both blocks", blockName, " should be null, since one of them is"), {
			expect_true(is.null(inputBlockAsVector)&&is.null(outputBlockAsVector))
		})
		return(NULL) # return at this point to remove warnings from output
	}
	test_that(paste("Identical vectors of items expected for input and output vector representations of block:", blockName), {
		expect_equal(outputBlockAsVector, inputBlockAsVector)
	})
	
}

# Compares the blocks within dataObj, parObj, mdlObj or taskObj, in nested-list representation
compareNestedListsRepresentationOfBlock <- function(objName, blockName, inputBlockAsNestedList, outputBlockAsNestedList) {
	
	message(paste0("\nComparing block name ", objName, "::", blockName, "...\n"))
    
    if (is.null(inputBlockAsNestedList) || is.null(outputBlockAsNestedList)) {
        message(paste("Warning one of the blocks",blockName,"is null.\n"))
        test_that(paste("Both blocks", blockName, " should be null, since one of them is"), {
            expect_true(is.null(inputBlockAsNestedList)&&is.null(outputBlockAsNestedList))
        })
        return(NULL) # return at this point to remove warnings from output
    }
	test_that(paste("Same number of items expected for input and output nested-list representations of block:", blockName), {
		expect_equal(length(outputBlockAsNestedList), length(inputBlockAsNestedList))
	})

	lapply(seq(1:length(inputBlockAsNestedList)), function(i) {
		
		compareBlockItems(objName, blockName, paste0("#",i), inputBlockAsNestedList[[i]], outputBlockAsNestedList[[i]])
		
	})

}

# Compares dataObj, parObj, mdlObj or taskObj, in nested-list representation
compareNestedListsRepresentationOfTopLevelObject <- function(inputData, outputData) {
	
	test_that(paste("Comparing the object names for the input and output nested-list representations of object:", inputData$name, "(", inputData$type, ")"), {
		expect_equal(outputData$name, inputData$name)
	})
	
	test_that(paste("Comparing the object types for the input and output nested-list representations of object:", inputData$name, "(", inputData$type, ")"), {
		expect_equal(outputData$type, inputData$type)
	})

	test_that(paste("Comparing the blocks for the input and output nested-list representations of object:", inputData$name, "(", inputData$type, ")"), {

		expect_true(
			is.list(inputData$blocks) && length(inputData$blocks) > 0,
			info=paste("Input JSON-as-nested-lists should contain list of blocks for object:", inputData$name, "(", inputData$type, ")")
		)
		expect_true(
			is.list(outputData$blocks) && length(outputData$blocks) > 0,
			info=paste("Output JSON-as-nested-lists should contain list of blocks for object:", outputData$name, "(", inputData$type, ")")
		)
		
		inputObjBlockNames <<- names(inputData$blocks)
		outputObjBlockNames <<- names(outputData$blocks)
		
		expect_equal(length(outputData$blocks), length(inputData$blocks),
			info=paste("Same number of blocks expected for input and output nested-list representations of object:", inputData$name, "(", inputData$type, ")"))
		
		expect_equal(sort(outputObjBlockNames), sort(inputObjBlockNames),
			info=paste("Same list of blocks expected for input and output nested-list representations of object:", inputData$name, "(", inputData$type, ")"))
		
	})

	lapply(inputObjBlockNames, function(blockName) {
		if (blockName%in%c("IDV", "STRUCTURAL_PARAMETERS", "VARIABILITY_PARAMETERS")) {
			# Compare vectors of variable names
			compareVectorRepresentationOfBlock(inputData$name, blockName, inputData$blocks[[blockName]], outputData$blocks[[blockName]])
		} else {
			compareNestedListsRepresentationOfBlock(inputData$name, blockName, inputData$blocks[[blockName]], outputData$blocks[[blockName]])
		}
	})
	
}


compareNestedListsRepresentationOfTopLevelObject(
	jsonAsNestedLists[sapply(jsonAsNestedLists, function(it) { it$type=="dataObj"})][[1]],
	jsonAsNestedListsOutput[sapply(jsonAsNestedListsOutput, function(it) { it$type=="dataObj"})][[1]]
)
compareNestedListsRepresentationOfTopLevelObject(
	jsonAsNestedLists[sapply(jsonAsNestedLists, function(it) { it$type=="parObj"})][[1]],
	jsonAsNestedListsOutput[sapply(jsonAsNestedListsOutput, function(it) { it$type=="parObj"})][[1]]
)
compareNestedListsRepresentationOfTopLevelObject(
	jsonAsNestedLists[sapply(jsonAsNestedLists, function(it) { it$type=="mdlObj"})][[1]],
	jsonAsNestedListsOutput[sapply(jsonAsNestedListsOutput, function(it) { it$type=="mdlObj"})][[1]]
)
compareNestedListsRepresentationOfTopLevelObject(
	jsonAsNestedLists[sapply(jsonAsNestedLists, function(it) { it$type=="taskObj"})][[1]],
	jsonAsNestedListsOutput[sapply(jsonAsNestedListsOutput, function(it) { it$type=="taskObj"})][[1]]
)
# TODO: compareNestedListsRepresentationOfTopLevelObject() for MOG obj

