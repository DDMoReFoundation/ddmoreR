# PURPOSE: Test TEL MDL reading functions
# DESCRIPTION: Reads MDL file and checks that objects are of correct type and structure
# TODO: 
# 
# Author: smith_mk
# Date: 03 Aug 2015
# Revisions: 
###############################################################################

setwd("./Product4.1/models/")
library(testthat)
case<-"UseCase1"
context(case)
mdlfile <- paste(case,".mdl",sep="")

test_that(paste("Estimating",case,"with Monolix"), {
			mlx <- try(estimate(mdlfile, target="Monolix", subfolder="Monolix"))
			## Doesn't crash with errors
			expect_false(class(mlx),"try-error")
			## SOME content in subfolder
			expect_false(is.null(list.files("./Monolix"))) 
			# SO exists
			expect_false(is.null(list.files("./Monolix",pattern="\\.SO.xml$")))
			# No errors
			expect_true(is.null(mlx@TaskInformation$Messages$Errors)) 
			# MLE values are populated
			expect_false(is.null(mlx@Estimation@PopulationEstimates$MLE$data))
			# Log-Likelihood value is populated
			expect_false(is.null(mlx@Estimation@Likelihood$LogLikelihood))
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
				mlx.xpdb <- try(as.xpdb(mlx,getDataObjects(mdlfile)[[1]]@SOURCE$file))
				## Doesn't crash with errors
				expect_false(class(mlx.xpdb),"try-error")
				# Is an Xpose database object
				expect_is(mlx.xpdb,"xpose.data") 
				# Some data in the merged dataset
				expect_false(is.null(mlx.xpdb@Data))
				expect_true(nrow(mlx.xpdb@Data)>0)
				# Number of rows in xpdb@Data = nrows in dataset where MDV != 0 
				#rawData <- read(getDataObjects(mdlfile)[[1]])
				#expect_equal(nrow(mlx.xpdb@Data), nrow(subset.data.frame(rawData, (MDV==0)))) 
			}
	)
	