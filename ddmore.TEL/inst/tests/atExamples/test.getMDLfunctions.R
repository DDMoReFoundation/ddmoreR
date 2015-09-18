# PURPOSE: Test TEL MDL reading functions
# DESCRIPTION: Reads MDL file and checks that objects are of correct type and structure
# TODO: 
# 
# Author: smith_mk
# Date: 03 Aug 2015
# Revisions: 
###############################################################################

setwd("./Product4.1/models/")
library(testthat)
case<-"UseCase1"
context(case)
mdlfile <- paste(case,".mdl",sep="")

## getMDLObjects
test_getMDLObjects(paste("Get MDL objects for",case), {
			myMDLObj <- try(getMDLObjects(mdlfile))
			## Doesn't crash with errors
			expect_false(class(myMDLObj) == "try-error")
			## Should be a list
			expect_is(myMDLObj, "list")
			## Should be 4 items
			expect_equal(length(myMDLObj),4)
			## Should have dataObj, parObj, mdlObj and taskObj classes
			expectedMDLObjects <- c("dataObj", "parObj", "mdlObj", "taskObj") 
			readMDLObjects <- unlist(lapply(myMDLObj,class))
			names(readMDLObjects) <- NULL
			expect_identical(readMDLObjects, expectedMDLObjects) 
		)
}

## getDataObject
test_getDataObject(paste("Get Data Object for",case), {
			myDataObj <- try(getDataObjects(mdlfile))
			names(myDataObj) <- NULL
			## Doesn't crash with errors
			expect_false(class(myDataObj) == "try-error")
			## Should be a list
			expect_is(myDataObj, "list")
			## Should be 1 item
			expect_equal(length(myDataObj), 1)

			## Extract first object
			myDataObj <- myDataObj[[1]]
			## Should be of class dataObj
			expect_is(myDataObj, "dataObj")

			## Expected slots are present
			expectedDataBlocks <- c("SOURCE", "DECLARED_VARIABLES", "DATA_INPUT_VARIABLES", "DATA_DERIVED_VARIABLES", "TARGET_CODE")
			expectedDataBlocks <- c(expectedDataBlocks, "name")
			readDataBlocks <- slotNames(myDataObj)
			expect_equal(readDataBlocks, expectedDataBlocks)
				
			## Read data from file
			myData <- read(myDataObj)
			expect_is(myData, "data.frame")
			## Expect some content
			expect_false(is.null(myData))
			## Check column names
			expectedDataColumns <- names(myDataObj@DATA_INPUT_VARIABLES)
			readDataColumns <- names(myData)
			expect_equal(readDataColumns, expectedDataColumns)
		)
}

## getParameterObject
test_getParObject(paste("Get Parameter Object for",case), {
			myParObj <- try(getParameterObjects(mdlfile))
			names(myParObj) <- NULL
			## Doesn't crash with errors
			expect_false(class(myParObj) == "try-error")
			## Should be a list
			expect_is(myParObj, "list")
			## Should be 1 item
			expect_equal(length(myParObj), 1)
			
			## Extract first object
			myParObj <- myParObj[[1]]
			## Should be of class dataObj
			expect_is(myParObj, "parObj")
			
			## Expected slots are present
			expectedParBlocks <- c("DECLARED_VARIABLES", "STRUCTURAL", "VARIABILITY", "PRIOR_PARAMETERS", "TARGET_CODE")
			expectedParBlocks <- c(expectedParBlocks, "name")
			readParBlocks <- slotNames(myParObj)
			expect_equal(readParBlocks, expectedParBlocks)
		)
}

## getModelObject
test_getMDLObject(paste("Get Model Object for",case), {
			myModelObj <- try(getModelObjects(mdlfile))
			names(myModelObj) <- NULL
			## Doesn't crash with errors
			expect_false(class(myModelObj) == "try-error")
			## Should be a list
			expect_is(myModelObj, "list")
			## Should be 1 item
			expect_equal(length(myModelObj), 1)
			
			## Extract first object
			myModelObj <- myModelObj[[1]]
			## Should be of class dataObj
			expect_is(myModelObj, "mdlObj")

			expectedModelBlocks <- c("IDV",                       "COVARIATES",               
									 "VARIABILITY_LEVELS",        "STRUCTURAL_PARAMETERS",    
		 						 	 "VARIABILITY_PARAMETERS",    "RANDOM_VARIABLE_DEFINITION",
		 						 	 "INDIVIDUAL_VARIABLES",      "MODEL_PREDICTION",         
		 						 	 "OBSERVATION",               "GROUP_VARIABLES",          
		 						 	 "MODEL_OUTPUT_VARIABLES",    "ESTIMATION",               
								 	 "SIMULATION",                "TARGET_CODE" )
			expectedModelBlocks <- c(expectedModelBlocks, "name")
			readModelBlocks <- slotNames(myModelObj)
			expect_equal(readModelBlocks, expectedModelBlocks)
		)
}

## getModelObject
test_getTaskObject(paste("Get Task Properties Object for",case), {
			myTaskObj <- try(getTaskPropertiesObjects(mdlfile))
			names(myTaskObj) <- NULL
			## Doesn't crash with errors
			expect_false(class(myTaskObj) == "try-error")
			## Should be a list
			expect_is(myTaskObj, "list")
			## Should be 1 item
			expect_equal(length(myTaskObj), 1)
			
			## Extract first object
			myTaskObj <- myTaskObj[[1]]
			## Should be of class dataObj
			expect_is(myTaskObj, "taskObj")
			
			## Expected slots are present
			expectedTaskBlocks <- c("ESTIMATE", "SIMULATE", "EVALUATE", "OPTIMISE", "DATA", "MODEL", "TARGET_CODE")
			expectedTaskBlocks <- c(expectedTaskBlocks, "name")
			readTaskBlocks <- slotNames(myTaskObj)
			expect_equal(readTaskBlocks, expectedTaskBlocks)
		)
}
