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
			expect_is(myMDLObj, list)
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
			## Doesn't crash with errors
			expect_false(class(myDataObj) == "try-error")
			## Should be a list
			expect_is(myMDLObj, list)
			## Should be 4 items
			expect_equal(length(myMDLObj),4)
		)
}


## 
			expect_false(is.null(list.files("./Monolix",pattern="\\.SO.xml$")))
			# No errors
			expect_true(is.null(mlx@TaskInformation$Messages$Errors)) 
			# MLE values are populated
			expect_false(is.null(mlx@Estimation@PopulationEstimates$MLE$data))
			# Log-Likelihood value is populated
			expect_false(is.null(mlx@Estimation@Likelihood$LogLikelihood))
			# Names of parameters match Parameter Object
			myParObj <- getParameterObjects(mdlfile)[[1]]
			myParObjNames <- c(names(myParObj@STRUCTURAL),
					names(myParObj@VARIABILITY) )
			mySONames <- names(getPopulationParameters(nm)$MLE)
			expect_true(setequal(myParObjNames, mySONames))
		}
	)

	

	
#' Examine the structure of myDataObj
	str(myDataObj)
	
#' Let's look at the MCL data object. What items does it contain?
	slotNames(myDataObj)
	
#' Where is the data (ASCII file) stored?
	myDataObj@SOURCE
#' What are the column names of the data set?
	names(myDataObj@DATA_INPUT_VARIABLES)
#' and what type is each?
	myDataObj@DATA_INPUT_VARIABLES
	
#' Note that this information can equally be extracted using the entire Model Object:
#' Extract named item from the myMDLObj object:
	myMDLObj$warfarin_PK_ODE_dat
	
#' We can also use getDataObjects with a named item
	myDataObj <- getDataObjects(mdlfile,name="warfarin_PK_ODE_dat")
	
#' Use TEL function getParameterObjects() to retrieve parameter object(s) from an existing .mdl file
	myParObj <- getParameterObjects(mdlfile)[[1]]
	
#' Let's look at the MCL parameter object
	myParObj
#' What are the structural parameter initial values and bounds?
	myParObj@STRUCTURAL
#' and the VARIABILITY parameters?
	myParObj@VARIABILITY
	
#' Use TEL function getModelObjects() to retrieve model object(s) from an existing .mdl file.
#' We don't expect users to alter or modify the model from within TEL, however the model blocks are 
#' parsed similarly to the other MCL objects.
	myModObj <- getModelObjects(mdlfile)[[1]]
	
#' Let's look at the MCL model object
	myModObj
	
#' Use TEL function getTaskPropertiesObjects() to retrieve task properties object(s) from an existing .mdl file
	myTaskObj <- getTaskPropertiesObjects(mdlfile)[[1]]
	
#' Let's look at the MCL task properties object
	myTaskObj
	