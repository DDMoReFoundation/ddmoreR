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

################################################################################
#' readDataObj
#'
#' A "readDataObj" method for Data Objects and MOG objects  (S4 objects of class 
#' \code{dataObj} and \code{mogObj}. This uses information within the dataObj
#' object to define where and how to retrieve the data, attributes of dataset
#' columns (naming, type, units where applicable) and, if specified, how to
#' derive additional dataset columns.
#'
#' @param object Object of class \code{dataObj} or \code{mogObj}.
#' @param sourceDir If provided, the directory in which the data file(s) can be found.
#'        Defaults to the current directory.
#' @param deriveVariables (Boolean) Whether to apply any code specified within the
#'        DATA_DERIVED_VARIABLES block. Defaults to TRUE. Please note that the code
#'        provided in the block must be valid R syntax. It must also be written in
#'        a way that allows the code to be applied to each row of a data frame in
#'        turn. For example, if a row "WEIGHT" exists in the data frame,
#'        \code{WEIGHT>5} would be valid, whereas \code{data$WEIGHT>5} would not.
#' @param categoricalAsFactor (Boolean) Whether to convert any dataset variables
#'        defined as categorical in DATA_INPUT_VARIABLES, to factor. Defaults to TRUE.
#' @param recode (Boolean) Whether to apply any recode attributes defined within the
#'        DATA_INPUT_VARIABLES block. Defaults to TRUE.
#' @param asRaw (Boolean) If TRUE, equivalent to setting deriveVariables,
#'        categoricalAsFactor and recode all to FALSE.
#' @param ... Other named arguments to pass on to \link{read.csv}
#'
#' @return Returns a data frame or list of data frames containing the dataset(s)
#'         as described by the Data Object SOURCE, DATA_INPUT_VARIABLES and
#'         DATA_DERIVED_VARIABLES information.
#' @examples 
#'  myDataObj <- getDataObjects("UseCase2.mdl")[[1]]
#'  myData <- readDataObj(myDataObj)
#'  head(myData)
#' @export
#' @docType methods
#' @rdname readDataObj-methods
#'
#' @examples
#' mydata <- readDataObj(slot(warfMOG, name = "dataObj"), sourceDir='C:\\SEE\\MDL_IDE\\workspace\\Product1\\models')
#' head(myData)
#'
#' @include Classes.R
#' @include utils.R

setGeneric("readDataObj", function(object, sourceDir=getwd(), deriveVariables=TRUE, categoricalAsFactor=TRUE, recode=TRUE, asRaw=FALSE, ...) { 
  standardGeneric("readDataObj")
})


#' @rdname readDataObj-methods
#' @aliases readDataObj,dataObj,dataObj-method
setMethod("readDataObj", "dataObj", function(object, sourceDir=getwd(), deriveVariables=TRUE, categoricalAsFactor=TRUE, recode=TRUE, asRaw=FALSE, ...) {
  # if asRaw=TRUE, set the following arguments as FALSE
  if (asRaw) {
    deriveVariables <- FALSE
    categoricalAsFactor <- FALSE
    recode <- FALSE
  }
  
  res <- .importData(object, sourceDir=sourceDir, ...)
  
  # Overwrite the names from the csv with names from DATA_INPUT_VARIABLES
  if (!is.null(names(object@DATA_INPUT_VARIABLES))) {
    names(res) <- names(object@DATA_INPUT_VARIABLES)
  }
  
  # Apply code from DATA_DERIVED_VARIABLES:
  if (deriveVariables) {
    env1 <- new.env()
    codeString <- object@DATA_DERIVED_VARIABLES
    
    # Test the codeString on one row of the data to see if it is valid R code.
    # Only continue to apply to the rest of the data if no error is returned.
    test <- try(.rowEvaluate(res[1,], codeString), silent=TRUE)
    if (inherits(test, "try-error")) {
      warning("Unable to apply code from DATA_DERIVED_VARIABLES to the data. The code
        may not be valid R syntax, or it may not be written to function on a row-by-row
        basis. Please see ?readDataObj for further information. The data has been imported without
        applying the code.")
    } else {
    
      if (is.data.frame(res)) {
		# return directly if there is only one data file
		res <- .rowEvaluate(res, codeString)
      } else { # create a list of the results
		res <- lapply(res, .rowEvaluate, codeString)
      }
    }
  }
  
  # Apply code from DATA_INPUT_VARIABLES
  if (recode) {
    input <- object@DATA_INPUT_VARIABLES
    vars <- names(input)
    
    for (v in vars) {
      useStr <- input[[v]]$use
      
      if (!is.null(useStr)) {
        # Try to convert columns if type is specified in MDL
        if (str_detect(useStr, "catCov") && categoricalAsFactor) {
			
			# TODO: Extract the levels and labels directly from the parsed MDL once the MDL->JSON->R
			# conversion actually parses the "... catCov withCategories ..." string
			allCategoriesStr <- str_match(useStr, "withCategories\\s*\\{\\s*(.*)\\s*\\}")[[2]]
			categoriesStrs <- strsplit(allCategoriesStr, "\\s*,\\s*")[[1]]
			lvls <- sapply(str_split(categoriesStrs, "\\s+"), function(v) { v[[3]] })
			lbls <- sapply(str_split(categoriesStrs, "\\s+"), function(v) { v[[1]] })
			
			try(res[, v] <- factor(x=res[, v], levels=lvls, labels=lbls))
			# TODO: reinstate this once type information is available with new grammar
			# } else if (type=="continuous") {
			#   try(res[, v] <- as.numeric(res[, v]))
        }
        # TODO: add more options here
      }  

    }
  }
  return(res)
})


#' @rdname readDataObj-methods
#' @aliases readDataObj,mogObj,mogObj-method
setMethod("readDataObj", "mogObj", function(object, sourceDir=getwd(), deriveVariables=TRUE, categoricalAsFactor=TRUE, recode=TRUE, asRaw=FALSE, ...) {
  # extract dataObj
  ob <- object@dataObj
  # pass to method for dataObj
  readDataObj(ob,  deriveVariables=deriveVariables, categoricalAsFactor=categoricalAsFactor, recode=recode, asRaw=asRaw, ... )
})


################################################################################
#' @name importData
#' @aliases .importData
#' @title Import Data from Data Object
#'
#' Imports the data file from a data object (class dataObj) and returns a data frame.
#
#' @param dataObj object of class "dataObj"
#' @param sourceDir if specified, the directory in which the data file is held,
#'                  in case they are not in the current directory
#' @return data.frame
.importData <- function(dataObj, sourceDir=getwd(), ...) {
	
	if (is.null(dataObj@SOURCE[[1]]$file)) {
		stop("No data file referenced in the data object")
	}

	# Prepend the source directory (the current directory if not specified) to the data files
	dataFile <- file.path(sourceDir, dataObj@SOURCE[[1]]$file)
  
	read.NONMEMDataSet(dataFile, ...) # return this as the result

}


################################################################################
#' read.NONMEMDataSet
#'
#' Use this function to read in the (NONMEM-type data set) data file associated
#' with a model.
#' 
#' It is a wrapper for read.csv() that finds the first row of actual data (by
#' assuming that this data starts with a numeric value), attempts to parse the
#' names of the columns from the line immediately preceding this first data row,
#' discards any header or comment rows before the data rows, and finally calls
#' \code{read.csv()} using appropriate parameters as determined from this pre-processing.
#
#' @param filePath - path to the CSV file to be read in
#' @param colNames - Column names to use in returned rawData if given (default=NULL).  
#' @return A data frame (data.frame) containing a representation of the data in the file
#' 
#' @export
#' @seealso R base function \code{read.csv}
#' @examples
#' warf <- read.NONMEMDataSet(filePath = system.file("tests", "data", 
#'     "warfarin_conc.csv", package = "ddmore"))
#' head(warf)
read.NONMEMDataSet <- function(filePath, colNames=NULL) {
	
	if (!file.exists(filePath)) {
		stop("Specified data file ", filePath, " does not exist.")
	}
	# Maximum arbitrary number of potential header rows to skip past
	potentialHeaderRows <- readLines(filePath, n=5) 

	# The first row of actual data starts with a numeric value
	firstDataRow = grep(pattern = '^\\d.*', x = potentialHeaderRows)[[1]]

	# Assume that the line immediately preceding the first row of data is the
	# header row, and skip past any comment characters or whitespace (i.e.,
	# skip past any non-alphabetical characters)
	headerRow <- sub(pattern = '[^A-Za-z]*', 
        replacement = '', 
        x = potentialHeaderRows[[firstDataRow - 1]])
	
  if (is.null(colNames)) {
  	# Read the comma-delimited column names from the row that has been assumed to be the header row
  	colNames <- strsplit(gsub(pattern="\\s*", replacement="", x=headerRow), split=",", fixed=TRUE)[[1]]    
  }
	
	# Now actually read in the CSV file, using appropriate parameters as determined above
	read.csv(filePath, header=FALSE, col.names=colNames, na.strings=".", stringsAsFactors=FALSE, skip=firstDataRow-1) # handle comment.char, if we know what it is?
	
}

