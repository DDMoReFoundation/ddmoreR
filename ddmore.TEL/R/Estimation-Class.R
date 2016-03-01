


#' The DataSet Object Class (S4) 
#'
#' An object to house all data associated with the data set
#' 
#' @slot description header information for dataset
#' @slot data matrix information with one column for each column in description
#' 
#' @name DataSet-class
#' @rdname DataSet-class
#' @exportClass DataSet
#' @aliases DataSet
#' @examples
#' dat <- new(Class = "DataSet", 
#'     description = data.frame(A = c("1", "unknown", "real"), B = c("2", "unknown", "real"), 
#'         row.names = c("columnNum", "columnType", "valueType"), stringsAsFactors = FALSE), 
#'     data = matrix(1:10, ncol = 2))
#' print(dat)
#' validObject(dat)

setClass(Class = "DataSet",
    slots = c("description", "data"), 
    prototype = list(description = data.frame(Col1 = c("1", "unknown", "real")), 
        data = matrix(data = NA_real_, ncol = 1, nrow = 1)), 
    validity = function(object) { 
        stopifnot(ncol(object@description) == ncol(object@data))
        stopifnot(all(rownames(object@description) == c("columnNum", "columnType", "valueType")))
        return(TRUE) })

#' Instantiate a new object of class DataSet
#' 
#' Calls \code{new} trivially
#' @param dots arguments to \code{new}
#' @return object of class DataSet
#' @export

DataSet <- function(...) {
    new(Class = "DataSet", ...)
}


#' Coerce DataSet object to data.frame
#' 
#' Make Data useable in R session
#' @param x DataSet object
#' @param dots
#' @return data.frame
#' @examples
#' dat <- new(Class = "DataSet", 
#'     description = data.frame(A = c("1", "unknown", "real"), B = c("2", "unknown", "real"), 
#'         row.names = c("columnNum", "columnType", "valueType"), stringsAsFactors = FALSE), 
#'     data = matrix(1:10, ncol = 2))
#' as.data.frame(x = dat)

setGeneric("as.data.frame")

setMethod(f = "as.data.frame", 
    signature = "DataSet", 
    definition = function(x, ...) {
    validObject(x)
    out <- as.data.frame(x@data, stringsAsFactors = FALSE)
    
    # update order
    index <- as.numeric(unlist(x@description["columnNum", , drop = FALSE]))
    out <- out[, index, drop = FALSE]
    colnames(out) <- colnames(x@description)
    for (i in seq_along(x@description)) {
        # manually parse valueType
        asType <- switch(x@description["valueType", i],
            "real" = as.numeric, 
            "Real" = as.numeric, 
            "integer" = as.integer, 
            "Int" = as.integer, 
            "Char" = as.character, 
            "String" = as.character, 
            "Text" = as.character, {
                warning("type not recognized in DataSet, treating as Real")
                as.numeric
            })
        out[[i]] <- asType(out[[i]])
    }
    return(out)
})



#' The PopulationEstimates Object Class (S4) 
#'
#' An object to house all data associated with population stimates
#' 
#' @slot MLE object DataSet
#' @slot Bayesian object data.frame
#' @slot OtherMethod object list
#' 
#' @name PopulationEstimates-class
#' @rdname PopulationEstimates-class
#' @exportClass PopulationEstimates
#' @aliases PopulationEstimates
#' @examples
#' est <- new(Class = "PopulationEstimates")
#' print(est)
#' validObject(est)
#estx <- "<?xml version='1.0' encoding='utf-8'?>
#  <SO xmlns='http://www.pharmml.org/so/0.3/StandardisedOutput' 
#    xmlns:xsi='http://www.w3.org/2001/XMLSchema-instance' 
#    xmlns:ds='http://www.pharmml.org/pharmml/0.8/Dataset' 
#    xmlns:ct='http://www.pharmml.org/pharmml/0.8/CommonTypes' 
#    xsi:schemaLocation='http://www.pharmml.org/so/0.3/StandardisedOutput' 
#    implementedBy='MJS' 
#    writtenVersion='0.3' 
#    id='i1'>
#  <Estimation>
#    <PopulationEstimates>
#      <MLE>
#        <ds:Definition>
#          <ds:Column columnId='TVCL' columnType='structParameter' valueType='real' columnNum='1'/>
#          <ds:Column columnId='TVV' columnType='structParameter' valueType='real' columnNum='2'/>
#          <ds:Column columnId='IVCL' columnType='varParameter_stdev' valueType='real' columnNum='3'/>
#          <ds:Column columnId='IVV' columnType='varParameter_var' valueType='real' columnNum='4'/>
#        </ds:Definition>
#        <ds:Table>
#          <ds:Row>
#            <ct:Real>0.0056746</ct:Real>
#            <ct:Real>1.47827</ct:Real>
#            <ct:Real>0.455023</ct:Real>
#            <ct:Real>0.182876</ct:Real>
#          </ds:Row>
#        </ds:Table>
#      </MLE>
#    </PopulationEstimates>
#  </Estimation>
#</SO>"
#estxml <- xmlTreeParse(file = estx, 
#    asText = TRUE, asTree = TRUE)
#estroot <- xmlRoot(x = estxml)
#est <- .getChildNodes(parentNode = estroot)$Estimation
#popEst <- .getChildNodes(parentNode = est)$PopulationEstimates
#popEstMLE <- .getChildNodes(parentNode = popEst)
#ddmore:::ParseElement(node = popEstMLE[["MLE"]])

setClass(Class = "PopulationEstimates",
    slots = c("MLE", "Bayesian", "OtherMethod"), 
    prototype = list(
        # Placeholder for MLE population estimates
        MLE = DataSet(), 
        # Placeholder for Bayesian population estimates.
        Bayesian = data.frame(
            PosteriorMean = numeric(0), 
            PosteriorMedian = numeric(0),
            PosteriorMode = numeric(0)
            ), 
        # Placeholder for Bootstrap population estimates.
        OtherMethod = list()), 
    validity = function(object) {
        stopifnot(is.data.frame(object@MLE))
        stopifnot(is.data.frame(object@Bayesian))
        stopifnot(is.list(object@OtherMethod))
        return(TRUE)
    })


PopulationEstimates <- function(...) {
    new(Class = "PopulationEstimates", ...)
}


#' The PrecisionPopulationEstimates Object Class (S4) 
#'
#' An object to house all data associated with precision of the population 
#' estimates
#' 
#' @slot MLE object list
#' @slot Bayesian object list
#' @slot OtherMethod object list
#' 
#' @name PrecisionPopulationEstimates-class
#' @rdname PrecisionPopulationEstimates-class
#' @exportClass PrecisionPopulationEstimates
#' @aliases PrecisionPopulationEstimates
#' @examples
#' est <- new(Class = "PrecisionPopulationEstimates")
#' print(est)
#' validObject(est)

setClass(Class = "PrecisionPopulationEstimates",
    slots = c("MLE", "Bayesian", "OtherMethod"), 
    prototype = list(
        # FIM, CovarianceMatrix, CorrelationMatrix, StandardError, 
        # RelativeStandardError, AsymptoticCI, ConditionNumber
        MLE = list(), 
        # StandardDeviation, PosteriorDistribution, PercentilesCI
        Bayesian = list(), 
        OtherMethod = list()), 
    validity = function(object) {
        if (length(object@MLE) > 0L) {
            stopifnot(all(names(object@MLE) %in% c(
                "FIM", "CovarianceMatrix", "CorrelationMatrix", "StandardError", 
                "RelativeStandardError", "AsymptoticCI", "ConditionNumber")))
        }
        if (length(object@Bayesian) > 0L) {
            stopifnot(all(names(object@MLE) %in% c(
                "StandardDeviation", "PosteriorDistribution", "PercentilesCI")))
        }
        return(TRUE) })


PrecisionPopulationEstimates <- function(...) {
    new(Class = "PrecisionPopulationEstimates", ...)
}


#' The IndividualEstimates Object Class (S4) 
#'
#' An object to house all data associated with the individual
#' estimates
#' 
#' @slot Estimates object list
#' @slot Bayesian object list
#' @slot OtherMethod object list
#' 
#' @name IndividualEstimates-class
#' @rdname IndividualEstimates-class
#' @exportClass IndividualEstimates
#' @aliases IndividualEstimates
#' @examples
#' est <- new(Class = "IndividualEstimates")
#' print(est)
#' validObject(est)

setClass(Class = "IndividualEstimates",
    slots = c("Estimates", "RandomEffects", "EtaShrinkage"), 
    prototype = list(Estimates = list(
            Mean = DataSet(), 
            Median = DataSet(), 
            Mode = DataSet()), 
        RandomEffects = list(
            EffectMean = DataSet(), 
            EffectMedian = DataSet(), 
            EffectMode = DataSet()), 
        EtaShrinkage = DataSet()),
    validity = function(object) { return(TRUE) })


IndividualEstimates <- function(...) {
    new(Class = "IndividualEstimates", ...)
}



#' The PrecisionIndividualEstimates Object Class (S4) 
#'
#' An object to house all data associated with the individual
#' estimates
#' 
#' @slot Estimates object list
#' @slot Bayesian object list
#' @slot OtherMethod object list
#' 
#' @name PrecisionIndividualEstimates-class
#' @rdname PrecisionIndividualEstimates-class
#' @exportClass PrecisionIndividualEstimates
#' @aliases PrecisionIndividualEstimates
#' @examples
#' est <- new(Class = "PrecisionIndividualEstimates")
#' print(est)
#' validObject(est)


setClass(Class = "PrecisionIndividualEstimates",
    slots = c("StandardDeviation", "EstimatesDistribution", "PercentilesCI"), 
    prototype = list(StandardDeviation = DataSet(), 
        # TODO make SOTableDistrib
        EstimatesDistribution = list(),
        PercentilesCI = DataSet()), 
    validity = function(object) { 
        # TODO
        return(TRUE) })


PrecisionIndividualEstimates <- function(...) {
    new(Class = "PrecisionIndividualEstimates", ...)
}


#' The Residuals Object Class (S4) 
#'
#' An object to house all data associated with the residuals
#' 
#' @slot Estimates object list
#' @slot Bayesian object list
#' @slot OtherMethod object list
#' 
#' @name Residuals-class
#' @rdname Residuals-class
#' @exportClass Residuals
#' @aliases Residuals
#' @examples
#' res <- new(Class = "Residuals")
#' print(res)
#' validObject(res)

setClass(Class = "Residuals",
    slots = c("ResidualTable", "EpsShrinkage"), 
    prototype = list(
        ResidualTable = DataSet(), EpsShrinkage = DataSet()), 
    validity = function(object) { return(TRUE) })


Residuals <- function(...) {
    new(Class = "Residuals", ...)
}


# The Predictions Object Class (S4) 
#
# An object to house all data associated with the Predictions
# 
# @slot Estimates object list
# @slot Bayesian object list
# @slot OtherMethod object list
# 
# @name Predictions-class
# @rdname Predictions-class
# @exportClass Predictions
# @aliases Predictions
# @examples
# pred <- new(Class = "Predictions")
# print(pred)
# validObject(pred)

# TODO
#setClass(Class = "Predictions",
#    slots = c(), 
#    prototype = c(), 
#    validity = function(object) { return(TRUE) })
    


#' The OFMeasures Object Class (S4) 
#'
#' An object to house all data associated with the objective function measures
#' 
#' @slot Estimates object list
#' @slot Bayesian object list
#' @slot OtherMethod object list
#' 
#' @name OFMeasures-class
#' @rdname OFMeasures-class
#' @exportClass OFMeasures
#' @aliases OFMeasures
#' @examples
#' meas <- new(Class = "OFMeasures")
#' print(meas)
#' validObject(meas)

setClass(Class = "OFMeasures",
    slots = c("Likelihood", "LogLikelihood", "Deviance", 
        "ToolObjFunction", "IndividualContribToLL", "InformationCriteria"), 
    prototype = list(Likelihood = numeric(0), 
        LogLikelihood = numeric(0), 
        Deviance = numeric(0), 
        # TODO update to ToolObjFunction
        ToolObjFunction = numeric(0), 
        IndividualContribToLL = DataSet(), 
        # TODO update to InformationCriteria
        InformationCriteria = numeric(0)), 
    validity = function(object) { 
        # TODO implement checking
        return(TRUE) })


OFMeasures <- function(...) {
    new(Class = "OFMeasures", ...)
}




# TODO
#setClass(Class = "TargetToolMessages",
#    slots = c(), 
#    prototype = c(), 
#    validity = function(object) { return(TRUE) })
    
    

##############################################################################
#' The Estimation Object Class (S4) 
#'
#' An object to house all data associated with population and individual estimates, 
#' precision, rediduals, predictions, liklihoods and output messages (and error
#' messages) from individual modeling software. 
#'
#' As the input data is not well defined at current, the slots for most of this class
#' are currently defined as lists for flexibility
#' 
#' @slot PopulationEstimates object of class PopulationEstimates
#' @slot PrecisionPopulationEstimates object of class PrecisionPopulationEstimates
#' @slot IndividualEstimates object of class IndividualEstimates
#' @slot PrecisionIndividualEstimates object of class PrecisionIndividualEstimates
#' @slot Residuals object of class Residuals
#' @slot Predictions object of class Predictions
#' @slot OFMeasures object of class OFMeasures
#' @slot TargetToolMessages object of class TargetToolMessages
#' 
#' @name Estimation-class
#' @rdname Estimation-class
#' @exportClass Estimation
#' @aliases Estimation
#' @examples
#' est <- new(Class = "Estimation")
#' print(est)
#' validObject(est)

setClass("Estimation", 
    # Define the slots
    slots = c(
        PopulationEstimates = "PopulationEstimates",
        PrecisionPopulationEstimates = "PrecisionPopulationEstimates",
        IndividualEstimates = "IndividualEstimates", 
        PrecisionIndividualEstimates = "PrecisionIndividualEstimates", 
        Residuals = "Residuals",
        Predictions = "list",
        OFMeasures = "OFMeasures",
        TargetToolMessages = "list"),
    # Set Default Values to blank lists with names in place
    prototype = list(
        PopulationEstimates = PopulationEstimates(),
        PrecisionPopulationEstimates = PrecisionPopulationEstimates(),
        IndividualEstimates = IndividualEstimates(),
        PrecisionIndividualEstimates = PrecisionIndividualEstimates(),
        Residuals = Residuals(),
        # TODO
        Predictions = list(),
        OFMeasures = OFMeasures(), 
        TargetToolMessages = list()),
    # Validity Checking Function 
    validity = function(object) {
        stopifnot(validObject(object@PopulationEstimates))
        stopifnot(validObject(object@PrecisionPopulationEstimates))
        stopifnot(validObject(object@IndividualEstimates)) 
        stopifnot(validObject(object@PrecisionIndividualEstimates)) 
        stopifnot(validObject(object@Residuals))
        #stopifnot(validObject(object@Predictions))
        stopifnot(validObject(object@OFMeasures))
        return(TRUE)
    }
)


Estimation <- function(...) {
    new(Class = "Estimation", ...)
}
