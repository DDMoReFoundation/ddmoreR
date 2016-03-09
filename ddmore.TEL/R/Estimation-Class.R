
#' The PopulationEstimatesBayesian Object Class (S4) 
#'
#' An object to house all data associated with the Bayesian-related data of the population estimates.
#' 
#' @slot PosteriorMean DataSet object
#' @slot PosteriorMedian DataSet object
#' @slot PosteriorMode DataSet object
#' 
#' @name PopulationEstimatesBayesian-class
#' @rdname PopulationEstimatesBayesian-class
#' @exportClass PopulationEstimatesBayesian
#' @aliases PopulationEstimatesBayesian
#' @examples
#' bayes <- new(Class = "PopulationEstimatesBayesian")
#' print(bayes)
#' validObject(bayes)
#' 
#' @include StandardOutputObjectXmlParsers.R

setClass(Class = "PopulationEstimatesBayesian",
	slots = c(
		PosteriorMean = "DataSet",
		PosteriorMedian = "DataSet",
		PosteriorMode = "DataSet"
	),
	validity = function(object) {
		# TODO implement checking
		return(TRUE)
	}
)

#' Initialisation function / Constructor for PopulationEstimatesBayesian S4 class
#' @param .Object new instance of the class
#' @param xmlNodePopulationEstimatesBayesian XML Node representation of the block
#' @include StandardOutputObjectXmlParsers.R
setMethod("initialize", "PopulationEstimatesBayesian", function(.Object, xmlNodePopulationEstimatesBayesian = NULL) {
	.genericParseElements(.Object, xmlNodePopulationEstimatesBayesian)
})

#' The PopulationEstimatesOtherMethod Object Class (S4) 
#'
#' An object to house all data associated with a set of OtherMethod-related data of the population estimates.
#' 
#' @slot Mean DataSet object
#' @slot Median DataSet object
#' 
#' @name PopulationEstimatesOtherMethod-class
#' @rdname PopulationEstimatesOtherMethod-class
#' @exportClass PopulationEstimatesOtherMethod
#' @aliases PopulationEstimatesOtherMethod
#' @examples
#' om <- new(Class = "PopulationEstimatesOtherMethod")
#' print(om)
#' validObject(om)
#' 
#' @include StandardOutputObjectXmlParsers.R

setClass(Class = "PopulationEstimatesOtherMethod",
	slots = c(
		Mean = "DataSet",
		Median = "DataSet"
	),
	validity = function(object) {
		# TODO implement checking
		return(TRUE)
	}
)

#' Initialisation function / Constructor for PopulationEstimatesOtherMethod S4 class
#' @param .Object new instance of the class
#' @param xmlNodePopulationEstimatesOtherMethod XML Node representation of the block
#' @include StandardOutputObjectXmlParsers.R
setMethod("initialize", "PopulationEstimatesOtherMethod", function(.Object, xmlNodePopulationEstimatesOtherMethod = NULL) {
	.genericParseElements(.Object, xmlNodePopulationEstimatesOtherMethod)
})

#' The PopulationEstimates Object Class (S4) 
#'
#' An object to house all data associated with population estimates.
#' 
#' @slot MLE DataSet object
#' @slot Bayesian \linkS4class{PopulationEstimatesBayesian} object
#' @slot OtherMethod list of \linkS4class{PopulationEstimatesBayesian} objects
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
#ddmore:::parseSODataElement(node = popEstMLE[["MLE"]])

setClass(Class = "PopulationEstimates",
    slots = c(
		MLE = "DataSet",
		Bayesian = "PopulationEstimatesBayesian",
		OtherMethod = "list" # of PopulationEstimatesOtherMethod objects
	),
    validity = function(object) {
		# TODO implement checking
        #stopifnot(is.list(object@OtherMethod))
        return(TRUE)
	}
)

#' Initialisation function / Constructor for PopulationEstimates S4 class
#' @param .Object new instance of the class
#' @param xmlNodePopulationEstimates XML Node representation of the block
#' @include StandardOutputObjectXmlParsers.R
setMethod("initialize", "PopulationEstimates", function(.Object, xmlNodePopulationEstimates = NULL) {
	
	if (!is.null(xmlNodePopulationEstimates)) {
		for (child in .getChildNodes(xmlNodePopulationEstimates)) {
			childName <- xmlName(child)
			switch(childName,
    			"MLE" = {
					.Object@MLE <- parseSODataElement(child)
				},
				"Bayesian" = {
					.Object@Bayesian <- new (Class = "PopulationEstimatesBayesian", child)
				},
				"OtherMethod" = {
					method <- xmlAttrs(child)[["method"]]
					if (is.null(method)) {
                		stop("Attribute \"method\" required on PopulationEstimates::OtherMethod sub-block (since v0.3)")
					}
					.Object@OtherMethod[[method]] <- new (Class = "PopulationEstimatesOtherMethod", child)
				},
				warning(paste("Unexpected child node of PopulationEstimates node encountered: ", childName))
			)
		} # end for
	}

	.Object
})

#' The PrecisionPopulationEstimatesMLE Object Class (S4) 
#'
#' An object to house all data associated with the MLE-related data of the
#' precision of the population estimates.
#' 
#' @slot FIM data.frame (Matrix)
#' @slot CovarianceMatrix data.frame (Matrix)
#' @slot CorrelationMatrix data.frame (Matrix)
#' @slot StandardError DataSet object
#' @slot RelativeStandardError DataSet object
#' @slot AsymptoticCI DataSet object
#' @slot ConditionNumber real
#' 
#' @name PrecisionPopulationEstimatesMLE-class
#' @rdname PrecisionPopulationEstimatesMLE-class
#' @exportClass PrecisionPopulationEstimatesMLE
#' @aliases PrecisionPopulationEstimatesMLE
#' @examples
#' mle <- new(Class = "PrecisionPopulationEstimatesMLE")
#' print(mle)
#' validObject(mle)
#' 
#' @include StandardOutputObjectXmlParsers.R

setClass(Class = "PrecisionPopulationEstimatesMLE",
	slots = c(
		FIM = "data.frame",
		CovarianceMatrix = "data.frame",
		CorrelationMatrix = "data.frame",
		StandardError = "DataSet",
		RelativeStandardError = "DataSet",
		AsymptoticCI = "DataSet",
		ConditionNumber = "numeric"
	),
	validity = function(object) {
		# TODO implement checking
		return(TRUE)
	}
)

#' Initialisation function / Constructor for PrecisionPopulationEstimatesMLE S4 class
#' @param .Object new instance of the class
#' @param xmlNodePrecisionPopEstsMLE XML Node representation of the block
#' @include StandardOutputObjectXmlParsers.R
setMethod("initialize", "PrecisionPopulationEstimatesMLE", function(.Object, xmlNodePrecisionPopEstsMLE = NULL) {
	if (!is.null(xmlNodePrecisionPopEstsMLE)) {
		.Object <- .genericParseElements(.Object, xmlNodePrecisionPopEstsMLE, customParseChildNodeNames = c("ConditionNumber"))
		xmlNodeCondNum <- .getChildNode(.getChildNodes(xmlNodePrecisionPopEstsMLE), "ConditionNumber")
		if (!is.null(xmlNodeCondNum)) {
			.Object@ConditionNumber <- as.numeric(xmlValue(xmlNodeCondNum))
		}
	}
	.Object
})

#' The PrecisionPopulationEstimatesBayesian Object Class (S4) 
#'
#' An object to house all data associated with the Bayesian-related data of the
#' precision of the population estimates.
#' 
#' @slot StandardDeviation DataSet
#' @slot PosteriorDistribution DataSet or DataSetDistribution object
#' @slot PercentilesCI DataSet
#' 
#' @name PrecisionPopulationEstimatesBayesian-class
#' @rdname PrecisionPopulationEstimatesBayesian-class
#' @exportClass PrecisionPopulationEstimatesBayesian
#' @aliases PrecisionPopulationEstimatesBayesian
#' @examples
#' bayes <- new(Class = "PrecisionPopulationEstimatesBayesian")
#' print(bayes)
#' validObject(bayes)
#' 
#' @include StandardOutputObjectXmlParsers.R

setClass(Class = "PrecisionPopulationEstimatesBayesian",
	slots = c(
		StandardDeviation = "DataSet",
		PosteriorDistribution = "DataSet", # or DataSetDistribution
		PercentilesCI = "DataSet"
	),
	validity = function(object) {
		# TODO implement checking
		return(TRUE)
	}
)

#' Initialisation function / Constructor for PrecisionPopulationEstimatesBayesian S4 class
#' @param .Object new instance of the class
#' @param xmlNodePrecisionPopEstsBayesian XML Node representation of the block
#' @include StandardOutputObjectXmlParsers.R
setMethod("initialize", "PrecisionPopulationEstimatesBayesian", function(.Object, xmlNodePrecisionPopEstsBayesian = NULL) {
	.genericParseElements(.Object, xmlNodePrecisionPopEstsBayesian)
})

#' The PrecisionPopulationEstimatesOtherMethod Object Class (S4) 
#'
#' An object to house all data associated with one of the OtherMethod-related
#' sets of data of the precision of the population estimates.
#' 
#' @slot CovarianceMatrix data.frame (Matrix)
#' @slot CorrelationMatrix data.frame (Matrix)
#' @slot StandardDeviation DataSet object
#' @slot StandardError DataSet object
#' @slot AsymptoticCI DataSet object
#' @slot PosteriorDistribution DataSet or DataSetDistribution object
#' @slot PercentilesCI DataSet object
#' 
#' @name PrecisionPopulationEstimatesOtherMethod-class
#' @rdname PrecisionPopulationEstimatesOtherMethod-class
#' @exportClass PrecisionPopulationEstimatesOtherMethod
#' @aliases PrecisionPopulationEstimatesOtherMethod
#' @examples
#' om <- new(Class = "PrecisionPopulationEstimatesOtherMethod")
#' print(om)
#' validObject(om)
#' 
#' @include StandardOutputObjectXmlParsers.R

setClass(Class = "PrecisionPopulationEstimatesOtherMethod",
	slots = c(
		CovarianceMatrix = "data.frame",
		CorrelationMatrix = "data.frame",
		StandardDeviation = "DataSet",
		StandardError = "DataSet",
		AsymptoticCI = "DataSet",
		PosteriorDistribution = "DataSet", # or DataSetDistribution
		PercentilesCI = "DataSet"
	),
	validity = function(object) {
		# TODO implement checking
		return(TRUE)
	}
)

#' Initialisation function / Constructor for PrecisionPopulationEstimatesOtherMethod S4 class
#' @param .Object new instance of the class
#' @param xmlNodePrecisionPopEstsOtherMethod XML Node representation of the block
#' @include StandardOutputObjectXmlParsers.R
setMethod("initialize", "PrecisionPopulationEstimatesOtherMethod", function(.Object, xmlNodePrecisionPopEstsOtherMethod = NULL) {
	.genericParseElements(.Object, xmlNodePrecisionPopEstsOtherMethod)
})

#' The PrecisionPopulationEstimates Object Class (S4) 
#'
#' An object to house all data associated with the precision of the population estimates.
#' 
#' @slot MLE instance of \linkS4class{PrecisionPopulationEstimatesMLE} S4 class
#' @slot Bayesian instance of \linkS4class{PrecisionPopulationEstimatesBayesian} class
#' @slot OtherMethod list of instances of \linkS4class{PrecisionPopulationEstimatesOtherMethod}
#'					 class (since can have multiple such blocks)
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
    slots = c(
		MLE = "PrecisionPopulationEstimatesMLE",
		Bayesian = "PrecisionPopulationEstimatesBayesian",
		OtherMethod = "list" # of OtherMethod objects
	),
    validity = function(object) {
		# TODO implement this checking
        return(TRUE)
	}
)

#' Initialisation function / Constructor for PrecisionPopulationEstimates S4 class
#' @param .Object new instance of the class
#' @param xmlNodePrecisionPopulationEstimates XML Node representation of the block
#' @include StandardOutputObjectXmlParsers.R
setMethod("initialize", "PrecisionPopulationEstimates", function(.Object, xmlNodePrecisionPopulationEstimates = NULL) {
	
	if (!is.null(xmlNodePrecisionPopulationEstimates)) {
		for (child in .getChildNodes(xmlNodePrecisionPopulationEstimates)) {
			childName <- xmlName(child)
			switch(childName,
				"MLE" = {
					.Object@MLE <- new (Class = "PrecisionPopulationEstimatesMLE", child)
				},
				"Bayesian" = {
					.Object@Bayesian <- new (Class = "PrecisionPopulationEstimatesBayesian", child)
				},
				"OtherMethod" = {
					method <- xmlAttrs(child)[["method"]]
					if (is.null(method)) {
                		stop("Attribute \"method\" required on PrecisionPopulationEstimates::OtherMethod sub-block (since v0.3)")
					}
					.Object@OtherMethod[[method]] <- new (Class = "PrecisionPopulationEstimatesOtherMethod", child)
				},
				warning(paste("Unexpected child node of PrecisionPopulationEstimates node encountered: ", childName))
			)
		} # end for
	}

	.Object
})


#' The IndividualEstimatesParamEstimates Object Class (S4) 
#'
#' An object to house all data associated with the parameter estimates related data
#' of the individual estimates.
#' 
#' @slot Mean DataSet object
#' @slot Median DataSet object
#' @slot Mode DataSet object
#' 
#' @name IndividualEstimatesParamEstimates-class
#' @rdname IndividualEstimatesParamEstimates-class
#' @exportClass IndividualEstimatesParamEstimates
#' @aliases IndividualEstimatesParamEstimates
#' @examples
#' paramEsts <- new(Class = "IndividualEstimatesParamEstimates")
#' print(paramEsts)
#' validObject(paramEsts)
#' 
#' @include StandardOutputObjectXmlParsers.R

setClass(Class = "IndividualEstimatesParamEstimates",
	slots = c(
		Mean = "DataSet",
		Median = "DataSet",
		Mode = "DataSet"
	),
	validity = function(object) {
		# TODO implement checking
		return(TRUE)
	}
)

#' Initialisation function / Constructor for IndividualEstimatesParamEstimates S4 class
#' @param .Object new instance of the class
#' @param xmlNodeIndividualEstimatesParamEstimates XML Node representation of the block
#' @include StandardOutputObjectXmlParsers.R
setMethod("initialize", "IndividualEstimatesParamEstimates", function(.Object, xmlNodeIndividualEstimatesParamEstimates = NULL) {
	.genericParseElements(.Object, xmlNodeIndividualEstimatesParamEstimates)
})

#' The IndividualEstimatesRandomEffects Object Class (S4) 
#'
#' An object to house all data associated with the random effects related data
#' of the individual estimates.
#' 
#' @slot EffectMean DataSet object
#' @slot EffectMedian DataSet object
#' @slot EffectMode DataSet object
#' 
#' @name IndividualEstimatesRandomEffects-class
#' @rdname IndividualEstimatesRandomEffects-class
#' @exportClass IndividualEstimatesRandomEffects
#' @aliases IndividualEstimatesRandomEffects
#' @examples
#' ranEffs <- new(Class = "IndividualEstimatesRandomEffects")
#' print(ranEffs)
#' validObject(ranEffs)
#' 
#' @include StandardOutputObjectXmlParsers.R

setClass(Class = "IndividualEstimatesRandomEffects",
	slots = c(
		EffectMean = "DataSet",
		EffectMedian = "DataSet",
		EffectMode = "DataSet"
	),
	validity = function(object) {
		# TODO implement checking
		return(TRUE)
	}
)

#' Initialisation function / Constructor for IndividualEstimatesRandomEffects S4 class
#' @param .Object new instance of the class
#' @param xmlNodeIndividualEstimatesRandomEffects XML Node representation of the block
#' @include StandardOutputObjectXmlParsers.R
setMethod("initialize", "IndividualEstimatesRandomEffects", function(.Object, xmlNodeIndividualEstimatesRandomEffects = NULL) {
	.genericParseElements(.Object, xmlNodeIndividualEstimatesRandomEffects)
})

#' The IndividualEstimates Object Class (S4) 
#'
#' An object to house all data associated with the individual estimates.
#' 
#' @slot Estimates instance of \linkS4class{IndividualEstimatesParamEstimates} S4 class
#' @slot RandomEffects instance of \linkS4class{IndividualEstimatesRandomEffects} S4 class
#' @slot EtaShrinkage DataSet object
#' 
#' @name IndividualEstimates-class
#' @rdname IndividualEstimates-class
#' @exportClass IndividualEstimates
#' @aliases IndividualEstimates
#' @examples
#' indivEst <- new(Class = "IndividualEstimates")
#' print(indivEst)
#' validObject(indivEst)

setClass(Class = "IndividualEstimates",
    slots = c(
		Estimates = "IndividualEstimatesParamEstimates",
		RandomEffects = "IndividualEstimatesRandomEffects",
		EtaShrinkage = "DataSet"
	),
    validity = function(object) {
		# TODO implement checking
		return(TRUE)
	}
)

#' Initialisation function / Constructor for IndividualEstimates S4 class
#' @param .Object new instance of the class
#' @param xmlNodeIndividualEstimates XML Node representation of the block
#' @include StandardOutputObjectXmlParsers.R
setMethod("initialize", "IndividualEstimates", function(.Object, xmlNodeIndividualEstimates = NULL) {
	
	if (!is.null(xmlNodeIndividualEstimates)) {
		for (child in .getChildNodes(xmlNodeIndividualEstimates)) {
			childName <- xmlName(child)
			switch(childName,
				"Estimates" = {
					.Object@Estimates <- new (Class = "IndividualEstimatesParamEstimates", child)
    			},
				"RandomEffects" = {
					.Object@RandomEffects <- new (Class = "IndividualEstimatesRandomEffects", child)
				},
				"EtaShrinkage" = {
					# Table expected - TODO call specific function ?
					.Object@EtaShrinkage <- parseSODataElement(child)
				},
				warning(paste("Unexpected child node of PrecisionPopulationEstimates node encountered: ", childName))
			)
		} # end for
	}

	.Object
})


#' The PrecisionIndividualEstimates Object Class (S4) 
#'
#' An object to house all data associated with the precision of the individual estimates.
#' 
#' @slot StandardDeviation DataSet object
#' @slot EstimatesDistribution DataSet or DataSetDistribution object
#' @slot PercentilesCI DataSet object
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
    slots = c(
		StandardDeviation = "DataSet",
		EstimatesDistribution = "DataSet", # or DataSetDistribution
		PercentilesCI = "DataSet"
	),
    validity = function(object) { 
        # TODO implement checking
        return(TRUE)
	}
)

#' Initialisation function / Constructor for PrecisionIndividualEstimates S4 class
#' @param .Object new instance of the class
#' @param xmlNodePrecisionIndividualEstimates XML Node representation of the block
#' @include StandardOutputObjectXmlParsers.R
setMethod("initialize", "PrecisionIndividualEstimates", function(.Object, xmlNodePrecisionIndividualEstimates = NULL) {
	.genericParseElements(.Object, xmlNodePrecisionIndividualEstimates)
})


#' The Residuals Object Class (S4) 
#'
#' An object to house all data associated with the residuals.
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
    slots = c(
		ResidualTable = "DataSet",
		EpsShrinkage = "DataSet"
	), 
	# TODO implement checking
    validity = function(object) {
		return(TRUE)
	}
)

#' Initialisation function / Constructor for Residuals S4 class
#' @param .Object new instance of the class
#' @param xmlNodeResiduals XML Node representation of the block
#' @include StandardOutputObjectXmlParsers.R
setMethod("initialize", "Residuals", function(.Object, xmlNodeResiduals = NULL) {
	.genericParseElements(.Object, xmlNodeResiduals)
})


#' The OFMeasures Object Class (S4) 
#'
#' An object to house all data associated with the objective function measures.
#' 
#' @slot Likelihood real value
#' @slot LogLikelihood real value
#' @slot Deviance real value
#' @slot ToolObjFunction real value
#' @slot IndividualContribToLL DataSet object
#' @slot InformationCriteria named list of AIC, BIC, DIC real values
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
    slots = c(
		Likelihood = "numeric",
		LogLikelihood = "numeric",
		Deviance = "numeric",
		ToolObjFunction = "numeric",
		IndividualContribToLL = "DataSet",
		InformationCriteria = "list"
	), 
    prototype = list(
        InformationCriteria = list(AIC = numeric(0), BIC = numeric(0), DIC = numeric(0))
	),
    validity = function(object) { 
        # TODO implement checking
        return(TRUE)
	}
)

#' Initialisation function / Constructor for OFMeasures S4 class
#' @param .Object new instance of the class
#' @param xmlNodeOFMeasures XML Node representation of the block
#' @include StandardOutputObjectXmlParsers.R
setMethod("initialize", "OFMeasures", function(.Object, xmlNodeOFMeasures = NULL) {
	
	if (!is.null(xmlNodeOFMeasures)) {
		for (child in .getChildNodes(xmlNodeOFMeasures)) {
			childName <- xmlName(child)
			if (childName %in% c("Likelihood", "LogLikelihood", "Deviance", "ToolObjFunction")) {
				slot(.Object, childName) <- as.numeric(xmlValue(child))
			}
			else if (childName %in% c("IndividualContribToLL")) {
				# Table expected - TODO call specific function ?
				slot(.Object, childName) <- parseSODataElement(child)
			}
			else if (childName %in% c("InformationCriteria")) {
				for (icChild in .getChildNodes(child)) {
					icChildName <- xmlName(icChild)
					if (icChildName %in% c("AIC", "BIC", "DIC")) {
						slot(.Object, childName)[[icChildName]] <- xmlValue(icChild)
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

	.Object
})


##############################################################################
#' The Estimation Object Class (S4) 
#'
#' An object to house all data associated with population and individual estimates, 
#' precision, rediduals, predictions, liklihoods and output messages (and error
#' messages) from individual modeling software. 
#' 
#' @slot PopulationEstimates object of class PopulationEstimates
#' @slot PrecisionPopulationEstimates object of class PrecisionPopulationEstimates
#' @slot IndividualEstimates object of class IndividualEstimates
#' @slot PrecisionIndividualEstimates object of class PrecisionIndividualEstimates
#' @slot Residuals object of class Residuals
#' @slot Predictions object of class Predictions
#' @slot OFMeasures object of class OFMeasures
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
    slots = c(
        PopulationEstimates = "PopulationEstimates",
        PrecisionPopulationEstimates = "PrecisionPopulationEstimates",
        IndividualEstimates = "IndividualEstimates", 
        PrecisionIndividualEstimates = "PrecisionIndividualEstimates", 
        Residuals = "Residuals",
        Predictions = "DataSet",
        OFMeasures = "OFMeasures"
	),
    validity = function(object) {
    	# Validity Checking Function, TODO: enhance this
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

#' Initialisation function / Constructor for Estimation S4 class
#' @param .Object new instance of the class
#' @param xmlNodeEstimation XML Node representation of the block
#' @include StandardOutputObjectXmlParsers.R
setMethod("initialize", "Estimation", function(.Object, xmlNodeEstimation = NULL) {
	
	if (!is.null(xmlNodeEstimation)) {
		for (child in .getChildNodes(xmlNodeEstimation)) {
			childName <- xmlName(child)
			if (childName %in% c("Predictions")) {
				slot(.Object, childName) <- parseSODataElement(child)
			}
			else if (childName %in% slotNames(.Object)) {
				slot(.Object, childName) <- new (Class = childName, child)
			}
			else {
				warning(paste("Unexpected child node of Estimation node encountered:", childName))
			}
		}
	}

	.Object
})

