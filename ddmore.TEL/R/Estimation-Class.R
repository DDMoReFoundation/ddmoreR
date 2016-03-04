
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
	# TODO BayesianXXX to be split out into nested class
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
	# TODO implement checking
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
					newObj@MLE <- ParseElement(child)
				},
				"Bayesian" = {
					# Fetch sub-children of Node
			        bayesianChildren <- .getChildNodes(child)
			        # Parse XML DataSet Structure and update SO
					for (bayesianChildName in names(bayesianChildren)) {
			            if (bayesianChildName %in% c("PosteriorMean", "PosteriorMedian", "PosteriorMode")) {
							# Dynamically update the appropriate slot
							slot(newObj, paste0("Bayesian", bayesianChildName)) <- ParseElement(bayesianChildren[[bayesianChildName]])
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
							newObj@OtherMethod[[method]][[otherMethodChildName]] <- ParseElement(otherMethodChildNodes[[otherMethodChildName]])
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
    # TODO implement nested classes
    prototype = list(
        # FIM, CovarianceMatrix, CorrelationMatrix, StandardError, 
        # RelativeStandardError, AsymptoticCI, ConditionNumber
        MLE = list(), 
        # StandardDeviation, PosteriorDistribution, PercentilesCI
        Bayesian = list(),
    	# CovarianceMatrix, CorrelationMatrix, StandardDeviation, StandardError,
		# AsymptoticCI, PosteriorDistribution, PercentilesCI
        OtherMethod = list()), # list of lists since can have multiple OtherMethod blks
	# TODO implement checking
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
		# TODO OtherMethod is a list of lists, rework this appropriately
#		if (length(object@OtherMethod) > 0L) {
#			stopifnot(all(names(object@OtherMethod) %in% c(
#				"CovarianceMatrix", "CorrelationMatrix", "StandardDeviation", "StandardError",
#				"AsymptoticCI", "PosteriorDistribution", "PercentilesCI")))
#		}
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
							# Matrix expected - TODO call specific function ?
							newObj@MLE[[mleChildName]] <- ParseElement(mleChild)
						}
						else if (mleChildName %in% c("StandardError", "RelativeStandardError", "AsymptoticCI")) {
							# Table expected - TODO call specific function ?
							newObj@MLE[[mleChildName]] <- ParseElement(mleChild)
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
							# Table expected - TODO call specific function ?
							newObj@Bayesian[[bayesianChildName]] <- ParseElement(bayesianChild)
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
							# Matrix expected - TODO call specific function ?
							newObj@OtherMethod[[method]][[otherMethodChildName]] <- ParseElement(otherMethodChildNodes[[otherMethodChildName]])
						}
						else if (otherMethodChildName %in% c("StandardDeviation", "StandardError", "AsymptoticCI", "PercentilesCI")) {
							# Table expected - TODO call specific function ?
							newObj@OtherMethod[[method]][[otherMethodChildName]] <- L <- ParseElement(otherMethodChildNodes[[otherMethodChildName]])
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
	# TODO implement checking
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
							# Table expected - TODO call specific function ?
							newObj@Estimates[[randomEffectsChildName]] <- ParseElement(randomEffectsChild)
						} else {
							warning(paste("Unexpected child node of IndividualEstimates::Estimates node encountered: ", randomEffectsChildName))
						}
					}
				},
				"RandomEffects" = {
					for (randomEffectsChild in .getChildNodes(child)) {
						randomEffectsChildName <- xmlName(randomEffectsChild)
						if (randomEffectsChildName %in% c("EffectMean", "EffectMedian", "EffectMode")) {
							# Table expected - TODO call specific function ?
							newObj@RandomEffects[[randomEffectsChildName]] <- ParseElement(randomEffectsChild)
						} else {
							warning(paste("Unexpected child node of IndividualEstimates::Estimates node encountered: ", randomEffectsChildName))
						}
					}
				},
				"EtaShrinkage" = {
					# Table expected - TODO call specific function ?
					newObj@EtaShrinkage <- ParseElement(child)
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
        # TODO implement checking
        return(TRUE) })


PrecisionIndividualEstimates <- function(xmlNodeIndividualEstimates = NULL, ...) {
    newObj <- new(Class = "PrecisionIndividualEstimates", ...)
	
	if (!is.null(xmlNodeIndividualEstimates)) {
		for (child in .getChildNodes(xmlNodeIndividualEstimates)) {
			childName <- xmlName(child)
			switch(childName,
				"StandardDeviation" = {
					# Table expected - TODO call specific function ?
					newObj@StandardDeviation <- ParseElement(child)
				},
				"EstimatesDistribution" = {
					# Table _or Sample_ expected - TODO cater for Sample too
					warning("TODO: Parse Distribution block")
				},
				"PercentilesCI" = {
					# Table expected - TODO call specific function ?
					newObj@PercentilesCI <- ParseElement(child)
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
#' @slot ResidualTable DataSet object
#' @slot EpsShrinkage DataSet object
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
	# TODO implement checking
    validity = function(object) { return(TRUE) })


Residuals <- function(xmlNodeResiduals = NULL, ...) {
    newObj <- new(Class = "Residuals", ...)

	if (!is.null(xmlNodeResiduals)) {
		for (child in .getChildNodes(xmlNodeResiduals)) {
			childName <- xmlName(child)
			if (childName %in% c("ResidualTable", "EpsShrinkage")) {
				# Table expected - TODO call specific function ?
				slot(newObj, childName) <- ParseElement(child)
			} else {
				warning(paste("Unexpected child node of Residuals node encountered: ", childName))
			}
		}
	}

	newObj
}


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
    prototype = list(
		Likelihood = numeric(0), 
        LogLikelihood = numeric(0), 
        Deviance = numeric(0), 
        ToolObjFunction = numeric(0), 
        IndividualContribToLL = DataSet(), 
        # TODO update to InformationCriteria
        InformationCriteria = list(AIC = numeric(0), BIC = numeric(0), DIC = numeric(0))),
    validity = function(object) { 
        # TODO implement checking
        return(TRUE) })


OFMeasures <- function(xmlNodeOFMeasures = NULL, ...) {
	newObj <- new(Class = "OFMeasures", ...)
	
	if (!is.null(xmlNodeOFMeasures)) {
		for (child in .getChildNodes(xmlNodeOFMeasures)) {
			childName <- xmlName(child)
			if (childName %in% c("Likelihood", "LogLikelihood", "Deviance", "ToolObjFunction")) {
				slot(newObj, childName) <- xmlValue(child)
			}
			else if (childName %in% c("IndividualContribToLL")) {
				# Table expected - TODO call specific function ?
				slot(newObj, childName) <- ParseElement(child)
			}
			else if (childName %in% c("InformationCriteria")) {
				for (icChild in .getChildNodes(child)) {
					icChildName <- xmlName(icChild)
					if (icChildName %in% c("AIC", "BIC", "DIC")) {
						slot(newObj, childName)[[icChildName]] <- xmlValue(icChild)
					} else {
						warning(paste("Unexpected child node of OFMeasures::InformationCriteria node encountered: ", icChildName))
					}
				}
			}
			else {
				warning(paste("Unexpected child node of OFMeasures node encountered: ", childName))
			}
		} # end for
	}

	newObj
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
        Predictions = "DataSet",
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
        Predictions = DataSet(),
        OFMeasures = OFMeasures(), 
        TargetToolMessages = list()),
    # Validity Checking Function 
    validity = function(object) {
        stopifnot(validObject(object@PopulationEstimates))
        stopifnot(validObject(object@PrecisionPopulationEstimates))
        stopifnot(validObject(object@IndividualEstimates)) 
        stopifnot(validObject(object@PrecisionIndividualEstimates)) 
        stopifnot(validObject(object@Residuals))
        stopifnot(validObject(object@Predictions))
        stopifnot(validObject(object@OFMeasures))
        return(TRUE)
    }
)


Estimation <- function(...) {
    new(Class = "Estimation", ...)
}
