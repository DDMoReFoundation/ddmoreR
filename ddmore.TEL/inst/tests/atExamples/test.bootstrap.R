# PURPOSE: Test TEL bootstrap
# DESCRIPTION: Runs bootstrap with PsN
#   and provides an automated test that each case runs successfully. 
#   NB: DOES NOT test that the answer is *correct*, just that there are no errors
# TODO: 
# 
# Author: smith_mk
# Date: 14 August 2015
###############################################################################

setwd("./Product4/models/")
library(testthat)
case<-"UseCase1_FOCEI"
context(case)
mdlfile <- paste(case,".mdl",sep="")

#bootstrapResults <- LoadSOObject("./bootstrap_warfarin/Warfarin-ODE-latest-FOCEI.SO.xml")
#bootstrap.PsN(mdlfile.FOCEI, samples=10, seed=876543,
#		bootstrapOptions=" -no-skip_minimization_terminated -threads=2",
#		subfolder="bootstrap_warfarin", plot=TRUE)

test_that(paste("bootstrap",case), {
			bootstrap <- try(bootstrap.PsN(mdlfile, samples=20, seed=876543,
							bootstrapOptions=" -no-skip_minimization_terminated -threads=2",
							subfolder="bootstrap_testthat") )
			# Doesn't crash
			expect_false(class(bootstrap),"try-error")		
			# Bootstrap result should be an S4 class StandardOutputObject
			expect_is(bootstrap[[1]],"StandardOutputObject")
			# bootstrapResults <- LoadSOObject("UC1/bootstrap/UseCase1_FOCEI.SO.xml")

			# bootstrapResults should have the correct slots populated
			expect_false(is.null(bootstrapResults@Estimation@PopulationEstimates$Bootstrap$Mean$data))
			expect_false(is.null(bootstrapResults@Estimation@PopulationEstimates$Bootstrap$Median$data))
			expect_false(is.null(bootstrapResults@Estimation@PrecisionPopulationEstimates$Bootstrap$Percentiles$data))

			# ADD TEST FOR ASYMPTOTIC RESULTS AND CIs
			# expect_false(is.null(bootstrapResults@Estimation@PrecisionPopulationEstimates$Bootstrap$Percentiles$data))

			# getPopulationParameters returns appropriate values
			expect_false(is.null(getPopulationParameters(bootstrapResults)$Bootstrap)

			# All parameters should have a bootstrap result
			bootstrapNames <- getPopulationParameters(bootstrapResults)$Bootstrap$Parameter
			parameterNames <- c(names(getParameterObjects(mdlfile)[[1]]@STRUCTURAL), names(getParameterObjects(mdlfile)[[1]]@VARIABILITY))
			expect_identical(bootstrapNames, parameterNames)
		}
)
		