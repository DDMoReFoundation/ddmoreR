#' Installation Qualification - DDMoRe SEE Public Beta release
#' ===============================================================
#' This example script is intended to test and qualify 
#' the installation of the DDMoRe Standalone Execution Environment (SEE). 
#' It uses UseCase2.mdl - analytic model of warfarin
#' population pharmacokinetics.  
#'   
#' The following steps check installation by: 
#' * Using the "ddmore" R package to read and parse MDL files.
#' * Parameter estimation with Monolix
#' * Parameter estimation of the same model with NONMEM
#' * Updating parameter estimates in the MDL Parameter Object 
#'   using MLE values from NONMEM
#' * Performing a Visual Predictive Check (VPC) using Perl speaks NONMEM (PsN)
#' * Using MLE values from Monolix to simulate new observed values using the simulx function
#'   in the mlxR package.
#'   
#' If all of the above steps complete successfully then the SEE has been installed and setup correctly.
#' An HTML file containing the commands in this file and associated output has been provided to allow the user
#' to verify that their own results are as expected. 

#' Initialisation
#' ===============
#' Clear workspace
rm(list=ls(all=F))

setwd(file.path(.MDLIDE_WORKSPACE_PATH,"/InstallationTest/models/"))

#' Set name of .mdl file
mdlfile <- "UseCase2.mdl"
datafile <- "warfarin_conc.csv"

#' Test "ddmore" R package 
#' =================
#' The ddmore R package is loaded by default when starting the MDL-IDE.
#'   
#' Use ddmore functions getModelObjects() to retrieve model object(s) 
#' from an existing .mdl file. This function reads the MDL in an .mdl file
#' and parses the MDL code for each MDL Object into objects of appropriate types
#' with names corresponding to the MDL Object names given in the file.
myMDLObj <- getMDLObjects(mdlfile)
length(myMDLObj)
names(myMDLObj)

#' Test MONOLIX installation
#' ===========================

#' Estimate model parameters using Monolix
#' ----------------------------------------
#' The ddmore "estimate" function translates the contents of the .mdl file to 
#' MLXTRAN and then estimates parameters using Monolix. After estimation, the output 
#' from Monolix is converted to a Standardised Output object which is saved in a .SO.xml file.
#'   
#' Translated files and Monolix output will be returned in the ./Monolix subfolder.
#' The Standardised Output object (.SO.xml) is read and parsed into an R object called "mlx" 
#' of (S4) class "StandardOutputObject".
#+ Monolix, cache=TRUE
mlx <- estimate(mdlfile, target="MONOLIX", subfolder="Monolix")
class(mlx)

#' Retrieve estimated parameters
#' ------------------------------
#' The ddmore function getPopulationParameters extracts the Population Parameter values from 
#' an R object of (S4) class "StandardOutputObject" and returns the MLE estimates. 
#' See documentation for getPopulationParameters to see other arguments and settings for this function.  
getPopulationParameters(mlx, what="estimates")

#' Perform basic model diagnostics 
#' --------------------------------
#' Use ddmore function as.xpdb() to create an Xpose database object from an R object of (S4) class "StandardOutputObject".
#' We can then call Xpose functions referencing this mlx.xpdb object as the input.
mlx.xpdb<-as.xpdb(mlx, datafile)

#' basic.gof is an Xpose function for displaying basic Goodness of Fit (gof) plots following estimation.
basic.gof(mlx.xpdb, main="MONOLIX")


#' Test NONMEM installation
#' -------------------------
#' The ddmore "estimate" function translates the contents of the .mdl file to 
#' NMTRAN and then estimates parameters using NONMEM. After estimation, the output 
#' from NONMEM is converted to a Standardised Output object which is saved in a .SO.xml file.
#'   
#' Translated files and NONMEM output will be returned in the ./NONMEM subfolder.
#' The Standardised Output object (.SO.xml) is read and parsed into an R object called "nm" 
#' of (S4) class "StandardOutputObject".
#+ NONMEM, cache=TRUE
nm <- estimate(mdlfile, target="NONMEM", subfolder="NONMEM")
class(nm)

#' Retrieve estimated parameters
#' ------------------------------
#' Similar to above - extract the Population Parameter estimates from the "nm" object.
getPopulationParameters(nm, what="estimates") 

#' Perform basic model diagnostics 
#' --------------------------------
#' As above, use ddmore function as.xpdb() to create an Xpose database object the "nm" object. 
nm.xpdb<-as.xpdb(nm, datafile)

#' basic.gof is an Xpose function for displaying basic Goodness of Fit (gof) plots following estimation.
basic.gof(nm.xpdb, main="NONMEM")

#' Test PsN installation 
#' ======================
#' Before running the VPC with PsN we must update the (initial) values in the MDL Parameter Object
#' to the MLE estimates from the NONMEM estimation.
#'   
#' Extract MLE values from the "nm" object 
parValues <- getPopulationParameters(nm, what="estimates")$MLE
#' Get the names of the returned parameters
parNames <- names(parValues)

#' The ddmore function "getParameterObjects" reads and parses the Parameter Object of an MDL file.
#' Since the .mdl file may have more than one such Parameter Object, the function returns a list.
#' We know there is only one Parameter Object in the supplied file so we extract the first element of the list
#' using the R syntax for subsetting lists: "[[1]]"
myParObj <- getParameterObjects(mdlfile)[[1]]
#' The Parameter Object blocks are supplied as slots in the (S4) object of class "parObj".  
#' Extract the parameter names from the STRUCTURAL block in the Parameter Object.
structuralNames <- names(myParObj@STRUCTURAL)
#' Extract the parameter names from the VARIABILITY block in the Parameter Object.
variabilityNames <- names(myParObj@VARIABILITY)

#' In the current version of the SO standard, we need to manually update parameter names for correlation and
#' covariance parameters to match the SO with the MDL. This will not be needed in future releases.
#' The SO object returned from NONMEM has parameter PPV_CL_PPV_V. 
#' This needs to be renamed to conform to model Correlation name OMEGA
parNames[grep("PPV_CL_PPV_V", parNames)] <- grep("OMEGA",variabilityNames,value=T) 

#' Update the parameter object using the ddmore "updateParObj" function.
#' This function updates an R object of (S4) class "parObj".
#' The user chooses which block to update, what items within that block,
#' and what to replace those items with. 
#' NOTE: that updateParObj can only update attributes which ALREADY EXIST in the MDL Parameter Object for that item.
#' This ensures that valid MDL is preserved.   
myParObjUpdated <- updateParObj(myParObj,block="STRUCTURAL",item=parNames[parNames%in%structuralNames],
		                  with=list(value=parValues[parNames%in%structuralNames]))
myParObjUpdated <- updateParObj(myParObjUpdated,block="VARIABILITY",
		                  item=parNames[parNames%in%variabilityNames],
						  with=list(value=parValues[parNames%in%variabilityNames]))

#' A bug in the writeMogObj function means that for now, we must manually add the square bracket around the OMEGA value
#' to signify that this is a vector (of length 1).
myParObjUpdated@VARIABILITY$OMEGA$value<-paste0("[",myParObjUpdated@VARIABILITY$OMEGA$value,"]")

#' Assemble the new Modelling Obejct Group (MOG). 
#' Note that we reuse the Data, Model and Task Properties Objects from the previous run.  
myVPCMOG <- createMogObj(dataObj = getDataObjects(mdlfile)[[1]], 
		parObj = myParObjUpdated, 
		mdlObj = getModelObjects(mdlfile)[[1]], 
		taskObj = getTaskPropertiesObjects(mdlfile)[[1]])

#' Write the MOG back out to an .mdl file.
mdlfile.VPC <- "UseCase2_VPC.mdl"
writeMogObj(myVPCMOG,mdlfile.VPC)


#' Now run the VPC using this new mdlfile.
#+ VPC, cache=TRUE
vpcFiles <- VPC.PsN(mdlfile.VPC,samples=50, seed=12345,
		vpcOptions ="-n_simulation=10 -auto_bin=10",
		subfolder="VPC", plot=TRUE) 

#' To replay the visualisation using information from the VPC SO file
# xpose.VPC(vpc.info= file.path("./VPC",vpcFiles@RawResults@DataFiles$PsN_VPC_results$path),
#		  	vpctab= file.path("./VPC",vpcFiles@RawResults@DataFiles$PsN_VPC_vpctab$path),
#			main="VPC warfarin")


#' Test Simulx
#' =============
#' The mlxR package has been developed to visualize and explore models using MLXTRAN to
#' encode models.
library(mlxR)

#' The ddmore function as.PharmML translates an MDL file (extension .mdl) to its PharmML
#' representation. The output file (extension .xml) is saved in the working directory.
myPharmML <- as.PharmML(mdlfile)

#' Get parameter values from the Monolix estimation above.
parValues <-getPopulationParameters(mlx, what="estimates")$MLE


#' Simulate for the typical weight of 70. 
#' Recall that logtWT = log(WT/70). 
p <- c(parValues,WT=70)

#' Define values to be simulated. Here we simulate the outcome variable Y (with residual unexplained variability)
#' at nominated times post dose.
y   <- list( name = c('Y'), time = c(0, 0.5, 1, 2, 3, 4, 6, 8, 12, 24, 36, 48, 72, 96, 120))

#' Define dosing regimen: a dose of 100mg given at time 0 into the GUT (oral administration)
adm <- list(time = 0, amount = 100)

#' Define group characteristics for simulation: 12 subjects.
#' We will simulate parameters at the individual level for treatments defined in the adm object.
g <- list( size = 12, level = 'individual', treatment=adm)

#' Call simulx.
#' simulx can take PharmML as an input - so the model does NOT need to be translated to MLXTRAN.
res  <- simulx(model = myPharmML,
		       parameter = p,
			   group = g,
			   output = list(y),
			   settings=list(seed=12345))

#' Plot simulated results. mlxR automatically loads the ggplot2 package.
plot(ggplot() + 
				geom_line(data=res$Y, aes(x=time, y=Y, colour=id)) +
				xlab("time (h)") + ylab("concentration") )

