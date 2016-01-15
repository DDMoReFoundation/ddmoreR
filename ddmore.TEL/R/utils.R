################################################################################
# Package-local variables
ddmore.utils <- new.env()
# is debug log level enabled
ddmore.utils$debug <- FALSE

#' Utility function to test whether a slot in the SO is empty or not.
#'
#' The default at current for an empyt slot in the SO is a list of length 0, though 
#' settting to NULL could be a future option. 
#'
is.empty <- function(slot) {
  return(is.null(slot) || length(slot) == 0)
}

################################################################################
#' Override of the standard message() function.
#' 
#' Prints to stdout not stderr in order that benign messages are not printed in
#' red in the R console.
#' Also added optional parameter \code{file}, which defaults to \code{stdout()},
#' as per \link{cat}.
#' 
#' Taken from answer on:
#' http://stackoverflow.com/questions/25306819/send-r-diagnostic-messages-to-stdout-instead-stderr
#' 
#' @param ... Zero or more objects which can be coerced to character (and which are pasted together with
#'            no separator) or a single condition object.
#' @param appendLF Logical: should messages given as a character string have a newline appended?
#' @param file A connection, or a character string naming the file to print to. If not specified then
#'             prints to the standard output connection.
#' 
#' @export
message <- function (..., domain = NULL, appendLF = TRUE, file=stdout())
{
	args <- list(...)
	cond <- if (length(args) == 1L && inherits(args[[1L]], "condition")) {
				if (nargs() > 1L) 
					warning("additional arguments ignored in message()")
				args[[1L]]
			}
			else {
				msg <- .makeMessage(..., domain = domain, appendLF = appendLF)
				call <- sys.call()
				simpleMessage(msg, call)
			}
	defaultHandler <- function(c) {
		cat(conditionMessage(c), file = file, sep = "")
	}
	withRestarts({
				signalCondition(cond)
				defaultHandler(cond)
			}, muffleMessage = function() NULL)
	invisible()
}


# -------------------- #
# Custom Print Methods #
# -------------------- #

#' Pretty print a Definition-Table element combination in the SO
#'
#' Can take a single element that contains a single <Definition> and <Table> elements as its children.
#' Or can take a list of elements, who each have <Definition> and <Table> elements as their children.
#'
#' @param listObject either a single or multiple SO elements that have the necessary Definition-Table element combination.
#' @param title A character vector to be printed at the start of the output
#' @param headings A list of names to print alongside the elements given, must be one for each element in listObject.
#'
pprintDefTable <- function(listObject, title=NULL, headings=NULL) {

  # Fetch data DataFrame
  if ("data" %in% names(listObject)) {
    dataList = list(listObject$data)
  } else {
    # Check for one level down
    listNames = names(listObject)
    have.data = sapply(listNames, FUN = function(x) {"data" %in% names(listObject[[x]])} )
    if (!all(have.data)) {
      warning("Not all objects in the list have a 'data' component at the same level.")
    }
    dataList = lapply(listNames, FUN = function(x) {listObject[[x]][["data"]]} ) 
    names(dataList) <- listNames
  }

  # Assign class name and attributes to be used by print.dataList
  class(dataList) <- "dataList"
  attr(dataList, "title") <- title
  attr(dataList, "headings") <- headings
  
  dataList
}

#' Custom S3 print method for data tables made of Description-Table tag elements 
#'
#' @export
print.dataList <- function(dataList) {

  # If title is present, print it
  if (!is.null(attr(dataList, "title"))) {    
    message(paste0("\n", attr(dataList, "title"), ":\n"))
  }

  # Check headings are valid and use them as names if so
  if (!is.null(attr(dataList, "headings")) & length(dataList) != length(attr(dataList, "headings"))) {
    warning("Number of headings given does not match number of elements")
  } else if (!is.null(attr(dataList, "headings"))) {
    names(dataList) <- attr(dataList, "headings")
  } 
  
  if (length(dataList) == 1) {
    attributes(dataList) <- NULL 
    x <- dataList[[1]]
    if (class(x) == 'data.frame'){
        print.data.frame(x)
      } else {
        print.default(x)
      }
  
  } else if (length(dataList) > 1) {
    for (i in 1:length(dataList)) {

      message("\n--- ", names(dataList)[[i]], " ---\n")
      # Call base printing functions 
      x <- dataList[[i]]
      if (length(x) == 0){
        message("(empty)\n")
      } else if (class(x) == 'data.frame'){
        print.data.frame(x)
      } else {
        print.default(x)
      }

    }
  } else {
    message("(empty)\n")
  }
}

#' Pretty print a listObject
#'
pprintList <- function(listObject, title=NULL) {

  attr(listObject, "title") <- title
  class(listObject) <- "listObject"
  listObject
}

#' Custom S3 print method for listObject 
#'
#' @export
print.listObject <- function(listObject){

  if (!is.null(attr(listObject, "title"))) {    
    message("\n", attr(listObject, "title"), ":\n", sep="")
  }

	if (length(listObject) > 0) {
		for (i in 1:length(listObject)) {
	  	message("\n--- ", names(listObject)[[i]], " ---\n")
      # Call base printing functions
      x <- listObject[[i]]
      if (length(x) == 0){
        message("(empty)\n")
      } else if (class(x) == 'data.frame'){
        print.data.frame(x)
      } else {
        print.default(x)
      }
		}
	} else {
		message("(empty)\n")
	}
}

#' ToFactor
#'
#' Takes in a data frame or a column of a data frame and converts the contents
#' to a factor then returns it
#'
#' @param object a data frame or a column of a data frame. 
#'
ToFactor <- function(object) {

    if (class(object) == "data.frame"){    
      output = lapply(object, ToFactor)
      object = data.frame(output)

    } else if (class(object) == "numeric") {
      object = as.factor(object)

    } else if (class(object) == "integer") {
      object = as.factor(object)

    } else if (class(object) == "character") {
      object = as.factor(object)
    } else if (class(object) == "factor") {
      object = object
    } else {
      warning("Object class not recognised. Skipping conversion ...")
    }

    return(object)
}

#' ToNumeric
#'
#' Takes in a data frame or a column of a data frame and converts the contents
#' to numeric then returns it
#'
#' @param object a data frame or a column of a data frame. 
#'
ToNumeric <- function(object) {

    if (class(object) == "data.frame"){
      output = lapply(object, ToNumeric)
      object = data.frame(output)

    } else if (class(object) == "factor") {
      object = as.numeric(as.character(object))

    } else if (class(object) == "integer") {
      object = as.numeric(object)

    } else if (class(object) == "character") {
      object = as.numeric(object)
    } else if (class(object) == "numeric") {
      object = object
    } else {
      warning("Object class not recognised. Skipping conversion ...")
    }
    return(object)    
}



##############################################################
#' URLencode
#'
#' As per the standard utility function utils::URLencode but encoding
#' the plus character in the URL string; this being a bug in URLencode.
#'
#' @param x URL or GET/POST string to be URL-encoded
#' @param the resulting URL-encoded string
URLencode <- function(x, ...) {
    gsub('[+]', '%2B', utils::URLencode(x, ...))
}

##############################################################
#' strip_quotes
#'
#' Remove any enclosing double quotes around a string if present.
#'
#' @param x the input string
#' @param the output string
strip_quotes <- function(x) {
	if (!is.null(x)) {
    	gsub("^\"(.*)\"$", "\\1", x)
	}
}

##############################################################
#' add_quotes
#'
#' Add enclosing double quotes round a string if none are already present.
#'
#' @param x the input string
#' @param the output string
add_quotes <- function(x) {
	if (!is.null(x)) {
		gsub("^(.*)$", "\"\\1\"", strip_quotes(x))
	}
}


##############################################################
#' parent.folder
#'
#' Derive the absolute path to a file (or folder), takes its parent,
#' and returns the path to this parent folder.
#'
#' Note that the file/folder must exist.
#'
#' @param f file/folder for which to find its parent
#' @param the absolute path to the parent folder of the input file/folder
parent.folder <- function(f) {
    dirname(file_path_as_absolute(f))
}


##############################################################
#' .assignFun
#'
#' Assigns column x of a data frame into an environment
.assignFun <- function(x, dat, env){
  assign(x, dat[x], envir=env)
}


##############################################################
#' .rowEvaluate
#'
#' Evaluates a string containing R code on each row of a data frame and returns
#' an updated data frame
.rowEvaluate <- function(dat, codeString){
  
  # Create empty environment in which to evaluate codeString
  env1 <- new.env()
  
  # Loop along each row of the data frame
  for(ii in seq_along(dat[,1])){
    
    # Extract desired row and col names
    temp <- dat[ii,]
    
    nam <- names(temp)

    # Assign the values to the environment
    sapply(nam, .assignFun, dat=temp, env=env1)
      # Evaluate the code in the new environment
      eval(parse(text=codeString), envir=env1)
      # Bring back the updated values and put back into temp
      for(nn in objects(env1)){
        temp[nn] <- eval(parse(text=paste0("env1$", nn)))
      }
     
    # Overwrite the original row with the updated values 
    dat[ii,] <- temp
  }

  # Remove the environment
  rm(env1)
  
  return(dat)
}


#' Utilities to extract parts of the mdl file that shares the same name as the current SO.xml 
#' 
.getMdlInfoFromSO <- function(SOObject, what="mdl", fisServer = DDMORE.getServer()) {
	
  # Look up MDL file, which is for now assumed to be in the same folder and shares the same name.
  mdlFile <- sub(x=SOObject@.pathToSourceXML, pattern="\\.SO\\.xml$", replacement=".mdl")
  if (!file.exists(mdlFile)) {
    stop("as.xpdb() and as.data() expected an MDL file at ", mdlFile, ", perhaps it has been moved or deleted")
  }
 
  if (tolower(what) == "parameter") {
    objs <- getParameterObjects(mdlFile, fisServer=fisServer)[[1]]
  } else if (tolower(what) == "mdl") {
    objs <- getMDLObjects(mdlFile, fisServer=fisServer)[[1]]
  } else if (tolower(what) == "data") {
    objs <- getDataObjects(mdlFile, fisServer=fisServer)[[1]]  
  } else if (tolower(what) == "model") {
    objs <- getModelObjects(mdlFile, fisServer=fisServer)[[1]]  
  } else {
    stop("Value for what not recognised, must be one of ('mdl', 'parameter', 'model', 'data')")
  }

  if (length(objs) > 1) {
    stop("More than one object found in MDL file ", mdlFile)
  }

  return(objs)
}

.deriveStructuralParametersFromAssociatedMDL <- function(SOObject, fisServer = DDMORE.getServer()) {
  names(.getMdlInfoFromSO(SOObject, what="parameter", fisServer=fisServer)@STRUCTURAL)
}

.deriveVariabilityParametersFromAssociatedMDL <- function(SOObject, fisServer = DDMORE.getServer()) {
  names(.getMdlInfoFromSO(SOObject, what="parameter", fisServer=fisServer)@VARIABILITY)
}

.convertObjectToNamedList <- function(obj) {
    tmp <- lapply(slotNames(obj), function(slotName) { slot(obj, slotName) } )
    names(tmp) <- slotNames(obj)
    tmp
}


#' Utility function to check function arguments
.precondition.checkArgument <- function(condition, argument, message) {
    if(!condition) {
        stop(sprintf("Illegal Argument %s. %s", argument, message))
    }
}

#' logs debug message to output stream
log.debug <- function(message) {
    if(ddmore.utils$debug) {
        message(sprintf("DEBUG: %s", message))
    }
}

#' sets debug mode
.setDebugMode <- function(debug = FALSE) {
    ddmore.utils$debug <- debug
}

#'
#' Performs HTTP Post request.
#' 
#' It is a wrapper function for RCurl:::curlPerform
#'
#' This function stops only if the RCurl fails.
#'
#'@param url - where the request should be sent to
#'@param body - content of the request
#'@param headers - headers of the request
#'
#'@return a named list representing http response:
#'        \itemize{
#'          \item{\code{header}} - Response's HTTP headers.
#'          \item{\code{body}} - Response's body.
#'        }
#'
.httpPost <- function(url, body = "", headers= c()) {
    .precondition.checkArgument(length(url)>0, "url", "Should be non empty string.")
    respContentCollector <- basicTextGatherer()
    respHeaderCollector <- basicHeaderGatherer()
    log.debug(sprintf("Performing POST request to [%s]. Headers [%s] body [%s]", url, paste(headers, collapse=','), body))
    curlRet <-
        RCurl:::curlPerform(
            url = url, postfields = body, httpheader = headers, writefunction =
                respContentCollector$update, headerFunction = respHeaderCollector$update
        )
    if(curlRet!=0) {
        #RCurl should always return zero (as per documentation) and non-zero should never happen (RCurl should already throw an exception)
        stop("Internal Error. RCurl returned non-zero value.")
    }
    response <- list()
    response$header <- respHeaderCollector$value()
    response$body <- respContentCollector$value()
    log.debug(sprintf("Received from FIS Server response: header [%s], body [%s]", paste(response$header, collapse=','), response$body))
    return(response)
}