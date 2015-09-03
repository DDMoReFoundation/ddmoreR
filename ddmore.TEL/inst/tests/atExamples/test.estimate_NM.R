# PURPOSE: Test TEL estimate
# DESCRIPTION: Runs estimation with Monolix / NONMEM
#   and provides an automated test that each case runs successfully. 
#   NB: DOES NOT test that the answer is *correct*, just that there are no errors
# TODO: 
#   In testing as.xpdb need to find a good way to check expected rows in xpdb vs rawData
# 
# Author: smith_mk
# Date: 13 May 2015
###############################################################################

setwd("./Product4/models/")
library(testthat)
case<-"UseCase1_FOCEI"
context(case)
mdlfile <- paste(case,".mdl",sep="")

## ESTIMATION WITH NONMEM
test_that(paste("Estimating",case,"with NONMEM"), {
			nm <- try(estimate(mdlfile, target="NONMEM", subfolder="NONMEM"))
			## Doesn't crash with errors
			expect_false(class(nm),"try-error")
			## SOME content in subfolder
			expect_false(is.null(list.files("./NONMEM"))) 
			# SO exists
			expect_true(is.null(list.files("./NONMEM",pattern="\\.SO.xml$")))
			# No errors
			expect_true(is.null(nm@TaskInformation$Messages$Errors)) 
			# MLE values are populated
			expect_false(is.null(nm@Estimation@PopulationEstimates$MLE$data))
			# Log-Likelihood value is populated
			expect_false(is.null(nm@Estimation@Likelihood$Deviance))
			# Names of parameters match Parameter Object
			myParObj <- getParameterObjects(mdlfile)[[1]]
			myParObjNames <- c(names(myParObj@STRUCTURAL),
					names(myParObj@VARIABILITY) )
			mySONames <- names(getPopulationParameters(nm)$MLE)
			expect_true(setequal(myParObjNames, mySONames))
		}
)

## Conversion of SO output to xpdb
test_that(paste("Converting NONMEM output for",case,"to XPDB"), {
			nm.xpdb <- try(as.xpdb(nm,getDataObjects(mdlfile)[[1]]@SOURCE$file))
			## Doesn't crash with errors
			expect_false(class(nm.xpdb),"try-error")
			# Is an Xpose database object
			expect_is(nm.xpdb,"xpose.data") 
			# Some data in the merged dataset
			expect_false(is.null(nm.xpdb@Data))
			expect_true(nrow(nm.xpdb@Data)>0)
			# Number of rows in xpdb@Data = nrows in dataset where MDV != 0 
			#rawData <- read(getDataObjects(mdlfile)[[1]])
			#expect_equal(nrow(nm.xpdb@Data), nrow(subset.data.frame(rawData, (MDV==0)))) 
		}
)

