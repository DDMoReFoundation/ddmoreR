# PURPOSE: Test TEL VPC
# DESCRIPTION: Runs VPC with PsN
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

SOObjects <- list.files(pattern=".SO.xml",recursive=T)
SOOutput <- grep(case,SOObjects,value=T)
nm <- try(LoadSOObject(SOOutput))

test_that(paste("VPC",case), {
			vpcFiles <- try(VPC.PsN(mdlfile, 
							samples=100, seed=123456, 
							subfolder="vpc_testthat", plot=FALSE)) 
			# Doesn't crash
			expect_false(class(vpcFiles),"try-error")			
			# Produces a plot
			myVPCPlot <- xpose.VPC(vpc.info=vpcFiles$vpc.info,vpctab=vpcFiles$vpctab,main="VPC warfarin")
			expect_is(myVPCPlot,"trellis")
		}
)
		