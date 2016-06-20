################################################################################
#' @title sample from a dataObj
#'
#' @description A sample method for objects of class \code{dataObj}. Returns an 
#' object of class \code{dataObj} with number of records / individuals defined by 
#' arguments. Sampling may be with or without replacement. Default sampling scheme 
#' is with replacement. Default is to assume the same number of records / 
#' individuals as in the original dataset i.e. bootstrap the dataset. The user 
#' may optionally specify to preserve the relative proportions (from the original data) 
#' of certain subgroups when sampling from the dataset by defining strata. The 
#' default behaviour is to sample by ID variables and retain all data for each 
#' sampled ID.
#'
#' @usage sample(object, size, replace=FALSE, prob=NULL, by= "ID", fileout = NULL) 
#'
#' @param object  an object of class \code{dataObj}
#' @param size a non-negative integer giving 
#' the total number of samples. The proportions of samples from each strata 
#' (as selected by the "by" argument) will be preserved. For example, if the population
#' has 40 females and 60 males, but selecting a size=10, 4 females and 6 males will be returned.
#' @param replace should sampling be with replacement?
#' @param prob a vector of probabilities for obtaining the elements of the 
#'  observations being sampled. Must sum to 1.
#' @param by a stratification variable defined within object. Default is to sample by ID.
#' @param fileout single character or NULL. If not NULL, a CSV file containing the sampled data 
#' will be created with name <fileout><timestamp>.csv (default "sample")
#' @param ... additional arguments to be passed to the DDMORE \code{\link{readDataObj}} method
#'
#' @seealso R base function \code{sample}, \code{\link{readDataObj}}
#' @return A copy of the dataObj with the sampled data set substituted for the original
#'
#' @examples 
#' ## Create myData based on ThamDataObject
#' ThamDataObject <- getDataObjects("Tham2008.mdl", name="tumour_size_dat")
#' ## Create bootstrap sample i.e. same size as original data
#' myNewData <- sample(ThamDataObject)
#' 
#' warfData <- 	getDataObjects("warfarin_PK_CONC.mdl",name="warf_PK_CONC_dat")
#' ## Retain the proportion of male and female in the sampled data.
#' newWarfData <- sample(warfData, by="SEX")
#'
#' @include Classes.R
#' @export
#' @docType methods
#' @rdname sample-methods
setGeneric("sample", 
  function(object, size, replace=FALSE, prob=NULL, by="ID", 
    fileout = "sample", ...){
      standardGeneric("sample")
})
#' @rdname sample-methods
#' @aliases sample,mogObj,mogObj-method
setMethod("sample", signature=signature(object="mogObj"), 
  function(object, size, replace=FALSE, prob=NULL, by="ID", 
    fileout = "sample", ...){
  # Extract out dataObj:
  obj <- object@dataObj
  
  
  # Then call the dataObj method
  sample(obj, size, replace, prob, by, fileout, ... )

})
#' @rdname sample-methods
#' @aliases sample,dataObj,dataObj-method
setMethod("sample", signature = signature(object="dataObj"), 
  function(object, size, replace = FALSE, prob = NULL, by = "ID", 
    fileout = "sample", ...){
    
    if (!(length(by) == 1L && is.character(by))) {
        stop("by must be a single character naming the column by which to sample")
    }
    # Check "by" column exists in the data
    if(!(by %in% names(object@DATA_INPUT_VARIABLES))){
      stop("'by' name is not a column of the data, as detailed in the DATA_INPUT_VARIABLES slot")
    }
    if (!is.null(fileout) && !(length(fileout) == 1L && is.character(fileout))) {
        stop("fileout must be a single character stating the data output path or NULL")
    }
    # do not write if fileout is missing
    if (!is.null(fileout)) {
        if (is.na(fileout)) { fileout <- NULL }
    }
    # read in the data:
    dat <- readDataObj(object, ...)
    
    # If not specified, the sample is assumed to be the same size as the original dataset
    if (missing(size)){
      size <- dim(dat)[1]
    }
    
    # if prob exists, append to data frame
    if(missing(prob)){prob<-NULL}
    
    if(!is.null(prob)){
      
      if(length(prob)!=dim(dat)[[1]]){stop("Length of 'prob' vector is not equal to the number of 
        rows in the data frame")}
      
      if(sum(prob)!=1){stop("Sum of 'prob' vector is not equal to 1. If you are specifying weights,
        please rescale to convert to probabilities")}
      
      # Add prob values to data frame
      dat$prob <- prob
    }
    
    # If "by" variable is specified, split the data into strata. 
      spF <- dat[,by]
      splitDat <- split(dat, f=spF)
      if(!is.null(prob)){
      
        # Rescale the probabilities
        # within each strata. This is based on:
        # prob(being chosen) = prob(being in strata) * prob(being selected from strata), we
        # therefore scale by dividing by the probability of being in the strata (the
        # number of rows in the strata divided by the number of rows in the data set)
        splitDat <- lapply(splitDat,stratProb, data=dat) 
      }
      finalDat <- splitDat
       
    # Now look at each strata, and sample the required number of values
    
    stratOut <- lapply(finalDat, strataSampler, data=dat, size=size, replace=replace)
   
    sampledList <- lapply(stratOut, function(x){x[[1]]})
    res <- do.call("rbind", sampledList)
    row.names(res) <- NULL
    res$prob <- NULL
    
    # Extract out logicals indicating whether the sample was floored and a warning
    # is required:
    logi <- sapply(stratOut, function(x){x[[2]]})
    
    if(any(logi)){warning(paste("Due to stratified sampling, the number of samples from
        at least one strata is/are rounded to the nearest whole number. Therefore, the number of samples returned
        may not match the number requested."))}
    
    if (!is.null(fileout)) {
        # Write the data out as a csv:
        fileName <- paste0(fileout, format(Sys.time(), "%Y%m%d_%H%M%S"), ".csv")
        
        write.csv(res, fileName, row.names=FALSE)
    } else {
        fileName <- ""
    }
    # Create a dataObj with a pointer to the data
    mogOut <- object
    mogOut@SOURCE$file <- fileName
    
    return(mogOut)
})



#' strataSampler
#'
#' Function that returns stratified samples
#'
strataSampler <- function(strata, data, size, replace, ...){
      # Calculate the size of the strata relative to the size of the data, then 
      # multiply by size to obtain the number of samples to take from this strata.
      # We floor the value to obtain an integer, and to ensure we don't have too
      # many samples returned.
      len <- dim(strata)[1]
  
      rs <- len/dim(data)[1] * size

      relSize <- round(rs)
      if(rs != as.integer(rs)){
        warn=TRUE
      } else{
        warn=FALSE
      }

      # Sample the correct number of values from the strata.
      if("prob" %in% names(strata)){
        ind <- base:::sample(rownames(strata), size=relSize, prob=strata$prob, replace=replace, ...)
        samp <- strata[ind,]
      } else{
        ind <- base:::sample(rownames(strata), size=relSize, prob=NULL, replace=replace, ...)
        samp <- strata[ind,]
      }
      
      return(list(samp, warn))

    }

#' stratProb
#' Function to rescale probabilities of being selected for a stratified sample. 
#'Rescale the probabilities
#' within each strata. This is based on:
#' prob(being chosen) = prob(being in strata) * prob(being selected from strata), we
#' therefore scale by dividing by the probability of being in the strata (the
#' number of rows in the strata divided by the number of rows in the data set) 
#'
stratProb <- function(strata, data){
  strataProb <- dim(strata)[1]/dim(data)[[1]]
  strata[,"prob"] <- strata[,"prob"]/strataProb
  return(strata)
}

