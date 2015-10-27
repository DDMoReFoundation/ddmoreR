#' Pretty print a Definition-Table element combination in the SO
#'
#' Can take a single element that contains a single <Definition> and <Table> elements as its children.
#' Or can take a list of elements, who each have <Definition> and <Table> elements as their children.
#'
#' @param listObject either a single or multiple SO elements that have the necesarry Definition-Table element combination.
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
    cat(paste0("\n", attr(dataList, "title"), ":\n"))
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
      
      cat("\n--- ", names(dataList)[[i]], " ---\n")
      # Call base printing functions 
      x <- dataList[[i]]
      if (length(x) == 0){
        cat("(empty)\n")
      } else if (class(x) == 'data.frame'){
        print.data.frame(x)
      } else {
        print.default(x)
      }
      
    }
  } else {
    cat("(empty)\n")
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
    cat("\n", attr(listObject, "title"), ":\n", sep="")
  }
  
  if (length(listObject) > 0) {
    for (i in 1:length(listObject)) {
      cat("\n--- ", names(listObject)[[i]], " ---\n")
      # Call base printing functions
      x <- listObject[[i]]
      if (length(x) == 0){
        cat("(empty)\n")
      } else if (class(x) == 'data.frame'){
        print.data.frame(x)
      } else {
        print.default(x)
      }
    }
  } else {
    cat("(empty)\n")
  }
}

PopulationEstimates <- SOObject@Estimation@PopulationEstimates

# pprintDefTable(P.est[["Bayesian"]], 'Bayesian Population Estimates')
# bays = pprintDefTable(P.est[["Bayesian"]], 'Bayesian Population Estimates')
# 
# pprintDefTable(P.est[["MLE"]], 'MLE Population Estimates')
# mle = pprintDefTable(P.est[["MLE"]], 'MLE Population Estimates')
L = list()

if ("MLE" %in% names(PopulationEstimates)) {
  L[["MLE"]] <- PopulationEstimates[["MLE"]]
} 
if ("Bayesian" %in% names(PopulationEstimates)) {
  B <- PopulationEstimates[["Bayesian"]]
  names(B) <- paste0('Bayes:', names(B))
  L <- c(L, B)
}


# Pretty print a list of data table elements 
pprintDefTable(L, title="Population Estimates")
x = pprintDefTable(L, title="Population Estimates")


