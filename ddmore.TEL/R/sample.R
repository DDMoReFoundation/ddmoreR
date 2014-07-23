#' sample
#'
#' A sample method for objects of class \code{dataObj}. Returns an object of class 
#' \code{dataObj} with number of records / individuals defined by 
#' arguments. Sampling may be with or without replacement. Default sampling scheme 
#' is with replacement. Default is to assume the same number of records / 
#' individuals as in the original dataset i.e. bootstrap the dataset. The user 
#' may optionally specify to preserve the relative proportions (from the original data) 
#' of certain subgroups when sampling from the dataset by defining strata. The 
#' default behaviour is to sample by ID variables and retain all data for each 
#' sampled ID.
#'
#' @usage sample(dataObject, size, replace=FALSE, prob=NULL, by=dataObject@Data$ID,...) 
#'
#' @param dataObject  an object of class \code{dataObj}
#' @param size a non-negative integer giving 
#' the total number of samples. The proportions of samples from each strata 
#' (as selected by the "by" argument) will be preserved. For example, if the population
#' has 40 females and 60 males, but selecting a size=10, 4 females and 6 males will be returned.
#' @param replace should sampling be with replacement?
#' @param prob a vector of probabilities for obtaining the elements of the 
#'  observations being sampled. Must sum to 1.
#' @param by a stratification variable defined within dataObject. Default is to sample by ID.
#' @param... additional arguments to be passed to the TEL read method
#'
#' @seealso R base function \code{sample}
#' @return A data frame with sampled data
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
#' @include telClasses.R
#' @export
#' @docType methods
#' @rdname sample-methods
setGeneric("sample", 
  function(dataObject, size, replace=FALSE, prob=NULL, by="ID",...){
      standardGeneric("sample")
})
#' @rdname sample-methods
#' @aliases sample,dataObj,dataObj-method

setMethod("sample", signature=signature(dataObject="dataObj"), 
  function(dataObject, size, replace=FALSE, prob=NULL, by="ID",...){
    # Check "by" column exists in the data
    if(!("ID" %in% names(dataObject@DATA_INPUT_VARIABLES))){
      stop("'by' name is not a column of the data, as detailed in the DATA_INPUT_VARIABLES slot")
    }
    # read in the data and split by "by" column:
    dat <- read(dataObject, ...)
    
    # If not specified, the sample is assumed to be the same size as the original dataset
    if(missing(size)){
      size <- dim(dat)[1]
    }
    
    # if prob exists, append to data frame
    if(!missing(prob)){
      
      if(length(prob)!=dim(dat)[[1]]){stop("Length of 'prob' vector is not equal to the number of 
        rows in the data frame")}
      
      if(sum(prob)!=1){stop("Sum of 'prob' vector is not equal to 1. If you are specifying weights,
        please rescale to convert to probabilities")}
      
      # Add prob values to data frame
      dat$prob <- prob
    }
    
    # If "by" variable is specified, split the data into strata. 
    if(!missing(by)){
      spF <- dat[,by]
      splitDat <- split(dat, f=spF)
      if(!missing(prob)){
      
      # Rescale the probabilities
      # within each strata. This is based on:
      # prob(being chosen) = prob(being in strata) * prob(being selected from strata), we
      # therefore scale by dividing by the probability of being in the strata (the
      # number of rows in the strata divided by the number of rows in the data set)
      splitDat <- lapply(splitDat,stratProb, data=dat) 
      }
      finalDat <- splitDat
    } else{
      finalDat <- list(dat)
    }
    
    # Now look at each strata, and sample the required number of values
    
    sampledList <- lapply(finalDat, strataSampler, data=dat, size=size, by=by, replace=replace) 
    res <- do.call("rbind", sampledList)
    row.names(res) <- NULL
    res$prob <- NULL
    return(res)
})



#' strataSampler
#'
#' Function that returns stratified samples
#'
strataSampler <- function(strata, data, size, by, replace, ...){
      # Calculate the size of the strata relative to the size of the data, then 
      # multiply by size to obtain the number of samples to take from this strata.
      # We floor the value to obtain an integer, and to ensure we don't have too
      # many samples returned.
      len <- dim(strata)[1]
      print(len)
      rs <- len/dim(data)[1] * size
      print(rs)
      if(!rs == as.integer(rs)){warning(paste("Due to stratified sampling, the number of samples from
        the", by, "=", strata[1,by], "strata is rounded to the nearest whole number. Therefore, the number of samples returned
        may not match the number requested. To prevent this, consider removing the 'by' argument
        to no longer take a stratified sample"))}
      relSize <- round(rs)
      
      # Sample the correct number of values from the strata.
      if("prob" %in% names(strata)){
        ind <- base:::sample(rownames(strata), size=relSize, prob=strata$prob, replace=replace, ...)
        samp <- strata[ind,]
      } else{
        ind <- base:::sample(rownames(strata), size=relSize, prob=NULL, replace=replace, ...)
        samp <- strata[ind,]
      }
      
      return(samp)

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

