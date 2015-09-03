# PURPOSE: Test TEL estimate
# DESCRIPTION: Runs estimation with Monolix / NONMEM
#   and provides an automated test that each case runs successfully. 
#   NB: DOES NOT test that the answer is *correct*, just that there are no errors
# TODO: 
#   In testing as.xpdb need to find a good way to check expected rows in xpdb vs rawData
# 
# Author: smith_mk
# Date: 13 May 2015
# Revision: 02 Sept 2015
###############################################################################

setwd("./Product4.1/models/")
library(testthat)
case<-"UseCase1"
context(case)
mdlfile <- paste(case,".mdl",sep="")
# SO.Objects <- list.files(pattern="\\.SO.xml$",recursive=T)
mlx <- LoadSOObject("UC1/Monolix/UseCase1.SO.xml")
nm <- LoadSOObject("UC1/NONMEM_FOCEI/UseCase1_FOCEI.SO.xml")
		
## Conversion of Monolix SO output to xpdb
test_that(paste("Converting NONMEM output for",case,"to XPDB"), {
			mlx.xpdb <- try(as.xpdb(mlx,getDataObjects(mdlfile)[[1]]@SOURCE$file))
			## Doesn't crash with errors
			expect_false(class(mlx.xpdb),"try-error")
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

## Conversion of Monolix SO output to xpdb
test_that(paste("Converting NONMEM output for",case,"to XPDB"), {
			nm.xpdb <- try(as.xpdb(nm,getDataObjects(mdlfile)[[1]]@SOURCE$file))
			## Doesn't crash with errors
			expect_false(class(mlx),"try-error")
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

