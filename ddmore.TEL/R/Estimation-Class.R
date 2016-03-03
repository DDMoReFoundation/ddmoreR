


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
    slots = c("MLE", "BayesianPosteriorMean", "BayesianPosteriorMedian", "BayesianPosteriorMode", "OtherMethod"), 
    prototype = list(
        # Placeholder for MLE population estimates
        MLE = DataSet(), 
        # Placeholder for Bayesian population estimates.
        #Bayesian = data.frame(
        #    PosteriorMean = numeric(0), 
        #    PosteriorMedian = numeric(0),
        #    PosteriorMode = numeric(0)
        #    ),
		BayesianPosteriorMean = DataSet(),
		BayesianPosteriorMedian = DataSet(),
		BayesianPosteriorMode = DataSet(),
        # Placeholder for Bootstrap population estimates.
        OtherMethod = list()), # List of DataSet
    validity = function(object) {
        #stopifnot(is.data.frame(object@MLE))
        #stopifnot(is.data.frame(object@Bayesian))
        #stopifnot(is.list(object@OtherMethod))
        return(TRUE)
    })


PopulationEstimates <- function(xmlNodePopulationEstimates = NULL, ...) {
    newObj <- new(Class = "PopulationEstimates", ...)
	
	if (!is.null(xmlNodePopulationEstimates)) {
		for (child in .getChildNodes(xmlNodePopulationEstimates)) {
			childName <- xmlName(child)
			switch(childName,
    			"MLE" = {
					L <- ParseElement(child)
					# TODO: Do this on DataSet() constructor instead
			        #newObj@MLE <- DataSet()
					newObj@MLE@description <- L$description
					newObj@MLE@data <- L$data
				},
				"Bayesian" = {
					# Fetch sub-children of Node
			        bayesianChildren <- .getChildNodes(child)
			        # Parse XML DataSet Structure and update SO
					for (bayesianChildName in names(bayesianChildren)) {
			            if (bayesianChildName %in% c("PosteriorMean", "PosteriorMedian", "PosteriorMode")) {
			                L <- ParseElement(bayesianChildren[[bayesianChildName]])
							# Dynamically update the appropriate slot
							slot(newObj, paste0("Bayesian", bayesianChildName))@description <- L$description
							slot(newObj, paste0("Bayesian", bayesianChildName))@data <- L$data
			            }
						else {
							warning(paste("Unexpected child node of PopulationEstimates::Bayesian node encountered: ", bayesianChildName))
						}
			        }
				},
				"OtherMethod" = {
					method <- xmlAttrs(child)[["method"]]
					if (is.null(method)) {
                		warning("Attribute \"method\" expected on PopulationEstimates::OtherMethod sub-block (since v0.3)")
					}
					otherMethodChildNodes <- .getChildNodes(child)
                	# Parse XML DataSet Structure and update SO
					newObj@OtherMethod[[method]] <- list()
					for (otherMethodChildName in names(otherMethodChildNodes)) {
						if (otherMethodChildName %in% c("Mean", "Median")) {
                    		L <- ParseElement(otherMethodChildNodes[[otherMethodChildName]])
							newObj@OtherMethod[[method]][[otherMethodChildName]] <- DataSet()
							# TODO: Do this on DataSet() constructor instead
							newObj@OtherMethod[[method]][[otherMethodChildName]]@description <- L$description
							newObj@OtherMethod[[method]][[otherMethodChildName]]@data <- L$data
							
						}
						else {
							warning(paste("Unexpected child node of PopulationEstimates::OtherMethod node encountered: ", otherMethodChildName))
						}
					}
				},
				warning(paste("Unexpected child node of PopulationEstimates node encountered: ", childName))
			)
		} # end for
	}

	newObj
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
    	# CovarianceMatrix, CorrelationMatrix, StandardDeviation, StandardError,
		# AsymptoticCI, PosteriorDistribution, PercentilesCI
        OtherMethod = list()), # list of lists since can have multiple OtherMethod blks
    validity = function(object) {
        if (length(object@MLE) > 0L) {
            stopifnot(all(names(object@MLE) %in% c(
                "FIM", "CovarianceMatrix", "CorrelationMatrix", "StandardError", 
                "RelativeStandardError", "AsymptoticCI", "ConditionNumber")))
        }
        if (length(object@Bayesian) > 0L) {
            stopifnot(all(names(object@Bayesian) %in% c(
                "StandardDeviation", "PosteriorDistribution", "PercentilesCI")))
        }
		if (length(object@OtherMethod) > 0L) {
			stopifnot(all(names(object@OtherMethod) %in% c(
				"CovarianceMatrix", "CorrelationMatrix", "StandardDeviation", "StandardError",
				"AsymptoticCI", "PosteriorDistribution", "PercentilesCI")))
		}
        return(TRUE)
	}
)


PrecisionPopulationEstimates <- function(xmlNodePrecisionPopulationEstimates = NULL, ...) {
    newObj <- new(Class = "PrecisionPopulationEstimates", ...)
	
	if (!is.null(xmlNodePrecisionPopulationEstimates)) {
		for (child in .getChildNodes(xmlNodePrecisionPopulationEstimates)) {
			childName <- xmlName(child)
			switch(childName,
				"MLE" = {
	        		for (mleChild in .getChildNodes(child)) {
						mleChildName <- xmlName(mleChild)
						if (mleChildName %in% c("FIM", "CovarianceMatrix", "CorrelationMatrix")) {
							L <- ParseElement(mleChild)
							# Matrix expected - TODO call specific function
							newObj@MLE[[mleChildName]] <- DataSet()
							# TODO: Do this on DataSet() constructor instead
							newObj@MLE[[mleChildName]]@description <- L$description
							newObj@MLE[[mleChildName]]@data <- L$data
						}
						else if (mleChildName %in% c("StandardError", "RelativeStandardError", "AsymptoticCI")) {
							L <- ParseElement(mleChild)
							# Table expected - TODO call specific function
							newObj@MLE[[mleChildName]] <- DataSet()
							# TODO: Do this on DataSet() constructor instead
							newObj@MLE[[mleChildName]]@description <- L$description
							newObj@MLE[[mleChildName]]@data <- L$data
						}
						else if (mleChildName %in% c("ConditionNumber")) {
							newObj@MLE[[mleChildName]] <- xmlValue(mleChild)
						}
						else {
							warning(paste("Unexpected child node of PrecisionPopulationEstimates::MLE node encountered: ", mleChildName))
						}
					}
				},
				"Bayesian" = {
					for (bayesianChild in .getChildNodes(child)) {
						bayesianChildName <- xmlName(bayesianChild)
						if (bayesianChildName %in% c("StandardDeviation", "PercentilesCI")) {
							L <- ParseElement(bayesianChild)
							# Table expected - TODO call specific function
							newObj@Bayesian[[bayesianChildName]] <- DataSet()
							# TODO: Do this on DataSet() constructor instead
							newObj@Bayesian[[bayesianChildName]]@description <- L$description
							newObj@Bayesian[[bayesianChildName]]@data <- L$data
						}
						else if (bayesianChildName %in% c("PosteriorDistribution")) {
							# Table _or Sample_ expected - TODO cater for Sample too
							warning("TODO: Parse Distribution block")
							#newObj@Bayesian[[bayesianChildName]] <- ParseElement(bayesianChild)
						}
						else {
							warning(paste("Unexpected child node of PrecisionPopulationEstimates::Bayesian node encountered: ", bayesianChildName))
						}
					}
				},
				"OtherMethod" = {
					method <- xmlAttrs(child)[["method"]]
					if (is.null(method)) {
                		warning("Attribute \"method\" expected on PrecisionPopulationEstimates::OtherMethod sub-block (since v0.3)")
					}
					otherMethodChildNodes <- .getChildNodes(child)
                	# Parse XML DataSet Structure and update SO
					newObj@OtherMethod[[method]] <- list()
					for (otherMethodChildName in names(otherMethodChildNodes)) {
						if (otherMethodChildName %in% c("CovarianceMatrix", "CorrelationMatrix")) {
							# Matrix expected - TODO call specific function
                			L <- ParseElement(otherMethodChildNodes[[otherMethodChildName]])
							newObj@OtherMethod[[method]][[otherMethodChildName]] <- DataSet()
							# TODO: Do this on DataSet() constructor instead
							newObj@OtherMethod[[method]][[otherMethodChildName]]@description <- L$description
							newObj@OtherMethod[[method]][[otherMethodChildName]]@data <- L$data
						}
						else if (otherMethodChildName %in% c("StandardDeviation", "StandardError", "AsymptoticCI", "PercentilesCI")) {
							# Table expected - TODO call specific function
                			L <- ParseElement(otherMethodChildNodes[[otherMethodChildName]])
							newObj@OtherMethod[[method]][[otherMethodChildName]] <- DataSet()
							# TODO: Do this on DataSet() constructor instead
							newObj@OtherMethod[[method]][[otherMethodChildName]]@description <- L$description
							newObj@OtherMethod[[method]][[otherMethodChildName]]@data <- L$data
						}
						else if (otherMethodChildName %in% c("PosteriorDistribution")) {
							# Table _or Sample_ expected - TODO cater for Sample too
							warning("TODO: Parse Distribution block")
						}
						else {
							warning(paste("Unexpected child node of PrecisionPopulationEstimates::OtherMethod node encountered: ", otherMethodChildName))
						}
					}
				},
				warning(paste("Unexpected child node of PrecisionPopulationEstimates node encountered: ", childName))
			)
		} # end for
	}

	newObj
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


IndividualEstimates <- function(xmlNodeIndividualEstimates = NULL, ...) {
	newObj <- new(Class = "IndividualEstimates", ...)
	
	if (!is.null(xmlNodeIndividualEstimates)) {
		for (child in .getChildNodes(xmlNodeIndividualEstimates)) {
			childName <- xmlName(child)
			switch(childName,
				"Estimates" = {
					for (randomEffectsChild in .getChildNodes(child)) {
						randomEffectsChildName <- xmlName(randomEffectsChild)
						if (randomEffectsChildName %in% c("Mean", "Median", "Mode")) {
							L <- ParseElement(randomEffectsChild)
							# Table expected - TODO call specific function
							newObj@Estimates[[randomEffectsChildName]] <- DataSet()
							# TODO: Do this on DataSet() constructor instead
							newObj@Estimates[[randomEffectsChildName]]@description <- L$description
							newObj@Estimates[[randomEffectsChildName]]@data <- L$data
						} else {
							warning(paste("Unexpected child node of IndividualEstimates::Estimates node encountered: ", randomEffectsChildName))
						}
					}
				},
				"RandomEffects" = {
					for (randomEffectsChild in .getChildNodes(child)) {
						randomEffectsChildName <- xmlName(randomEffectsChild)
						if (randomEffectsChildName %in% c("EffectMean", "EffectMedian", "EffectMode")) {
							L <- ParseElement(randomEffectsChild)
							# Table expected - TODO call specific function
							newObj@RandomEffects[[randomEffectsChildName]] <- DataSet()
							# TODO: Do this on DataSet() constructor instead
							newObj@RandomEffects[[randomEffectsChildName]]@description <- L$description
							newObj@RandomEffects[[randomEffectsChildName]]@data <- L$data
						} else {
							warning(paste("Unexpected child node of IndividualEstimates::Estimates node encountered: ", randomEffectsChildName))
						}
					}
				},
				"EtaShrinkage" = {
					L <- ParseElement(child)
					# Table expected - TODO call specific function
					newObj@EtaShrinkage <- DataSet()
					# TODO: Do this on DataSet() constructor instead
					newObj@EtaShrinkage@description <- L$description
					newObj@EtaShrinkage@data <- L$data
				},
				warning(paste("Unexpected child node of PrecisionPopulationEstimates node encountered: ", childName))
			)
		} # end for
	}

	newObj
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


PrecisionIndividualEstimates <- function(xmlNodeIndividualEstimates = NULL, ...) {
    newObj <- new(Class = "PrecisionIndividualEstimates", ...)
	
	if (!is.null(xmlNodeIndividualEstimates)) {
		for (child in .getChildNodes(xmlNodeIndividualEstimates)) {
			childName <- xmlName(child)
			switch(childName,
				"StandardDeviation" = {
					L <- ParseElement(child)
					# Table expected - TODO call specific function
					newObj@StandardDeviation <- DataSet()
					# TODO: Do this on DataSet() constructor instead
					newObj@StandardDeviation@description <- L$description
					newObj@StandardDeviation@data <- L$data
				},
				"EstimatesDistribution" = {
					# Table _or Sample_ expected - TODO cater for Sample too
					warning("TODO: Parse Distribution block")
				},
				"PercentilesCI" = {
					L <- ParseElement(child)
					# Table expected - TODO call specific function
					newObj@PercentilesCI <- DataSet()
					# TODO: Do this on DataSet() constructor instead
					newObj@PercentilesCI@description <- L$description
					newObj@PercentilesCI@data <- L$data
				},
				warning(paste("Unexpected child node of PrecisionIndividualEstimates node encountered: ", childName))
			)
		}
	}
	
	newObj
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


Residuals <- function(xmlNodeResiduals = NULL, ...) {
    newObj <- new(Class = "Residuals", ...)

	if (!is.null(xmlNodeResiduals)) {
		for (child in .getChildNodes(xmlNodeResiduals)) {
			childName <- xmlName(child)
			if (childName %in% c("ResidualTable", "EpsShrinkage")) {
				L <- ParseElement(child)
				# Table expected - TODO call specific function
				# TODO: Do this on DataSet() constructor instead
				slot(newObj, childName)@description <- L$description
				slot(newObj, childName)@data <- L$data
			} else {
				warning(paste("Unexpected child node of Residuals node encountered: ", childName))
			}
		}
	}

	newObj
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
