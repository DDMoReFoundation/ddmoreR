#' Product 3 - Expanding TEL task functionality with the warfarin ODE model
#' =========================================================================================
#' Using TEL functions, Monolix, NONMEM, Xpose, PsN, and simulx in a workflow with an initial
#' estimation step in Monolix, followed by an SAEM estimation step in NONMEM (using the
#' final estimates from Monolix is initial estimates in NONMEM), an FOCEI estimation step
#' in NONMEM (using the final estimates from the Monolix step as initial estimates for the
#' FOCEI step), VPC and bootstrap steps in PsN (based on the NONMEM FOCEI step), and a
#' simulation step in simulx (based on the NONMEM FOCEI model).


#' Initialisation
#' =========================
#+ cache=T
#' Clear workspace
rm(list=ls(all=T))

#' Set working directory
setwd("PUT THE PATH TO YOUR MDL-IDE WORKSPACE HERE")  #' eg C:/SEE/MDL_IDE/workspace
setwd("Product3_EXAMPLE/models/Warfarin-ODE-latest-Monolix")

#' List files available in working directory
list.files()

#' Introduction to DDMoRe.TEL
#' =========================

#' We can see the functions available in the TEL package. These are the "high level" functions which we would
#' expect the user to use..
objects("package:DDMoRe.TEL")

#' Set name of .mdl file
mdlfile <- "Warfarin-ODE-latest-Monolix.mdl" 

#' View objects within the .mdl file using TEL
#' ---------------------------
#' Use TEL function getModelObjects() to retrieve model object(s) from an existing .mdl file
myMDLObj <- getMDLObjects(mdlfile)

length(myMDLObj)
names(myMDLObj)

#' Let's look at the MDL Objects
myMDLObj

#' Use TEL function getDataObjects() to retrieve data object(s) from an existing .mdl file.
#' getDataObjects retrieves the MCL Data Object from the file and allows the user to make changes to 
#' elements of this object programmatically. This may be useful, for example, if the user wants to change
#' location (altering file path) or naming of the input data set.   
#' getDataObjects retrieves ALL objects of class dataObj from the .mdl file and returns a list. 
#' Here we ony want the first of those objects (there *is* only one in the .mdl file).

myDataObj <- getDataObjects(mdlfile)[[1]]

#' Examine the structure of myDataObj
str(myDataObj)

#' Let's look at the MCL data object. What items does it contain?
slotNames(myDataObj)

#' Alternatively:
# myDataObj <- getDataObjects(file=mdlfile, name="warfarin_PK_ODE_dat"))

#' Where is the data (ASCII file) stored?
myDataObj@SOURCE
#' What are the column names of the data set?
names(myDataObj@DATA_INPUT_VARIABLES)
#' and what type is each?
myDataObj@DATA_INPUT_VARIABLES

#' Note that this information can equally be extracted using the entire Model Object:
#' Extract named item from the myMDLObj object:
myMDLObj$warfarin_PK_ODE_dat

#' Use TEL function getParameterObjects() to retrieve parameter object(s) from an existing .mdl file
myParObj <- getParameterObjects(mdlfile)[[1]]

#' Let's look at the MCL parameter object
myParObj
#' What are the structural parameter initial values and bounds?
myParObj@STRUCTURAL
#' and the VARIABILITY parameters?
myParObj@VARIABILITY

#' Use TEL function getModelObjects() to retrieve model object(s) from an existing .mdl file.
#' We don't expect users to alter or modify the model from within TEL, however the model blocks are 
#' parsed similarly to the other MCL objects.
myModObj <- getModelObjects(mdlfile)[[1]]

#' Let's look at the MCL model object
myModObj

#' Use TEL function getTaskPropertiesObjects() to retrieve task properties object(s) from an existing .mdl file
myTaskObj <- getTaskPropertiesObjects(mdlfile)[[1]]

#' Let's look at the MCL task properties object
myTaskObj

#' Exploratory Data Analysis
#' =========================

#' Recall that getDataObjects only reads the MCL code from the .mdl file.
#' Use TEL function read() to create an R object from the MCL data object.
myData <- read(myDataObj)

#' Let's look at the first 6 lines of the data set
head(myData)

#' Extract only observation records
myEDAData<-myData[myData$MDV==0,]

#' Now plot the data using xyplot from the lattice library (graphs are exported to PDF)
xyplot(DV~TIME,groups=ID,data=myEDAData,type="b",ylab="Conc. (mg/L)",xlab="Time (h)")
xyplot(DV~TIME|ID,data=myEDAData,type="b",layout=c(3,4),ylab="Conc. (mg/L)",xlab="Time (h)",scales=list(relation="free"))

#' Model Development
#' =========================

#' ESTIMATE model parameters using Monolix
#' -------------------------
mlx <- estimate(mdlfile, target="MONOLIX", subfolder="Monolix")

#' Use TEL functions getEstimationInfo() and getParameterEstimates() to retrieve results via the standardised output object.  
#' get<<...>> functions are still being refined so that they return useful information to users. For now, they return all 
#' relevant output.  

#' Log-likelihood and individual contribution to log-likelihood
getEstimationInfo(mlx)
#' Parameter estimates & precision. 
#' MLE, FIM, Correlation matrix, Standard Errors, Relative Standard Errors
getParameterEstimates(mlx)

#' Examine the structure of the Parameter Estimates
str(getParameterEstimates(mlx))

#' Assign the get<<...>> output to an object to find out the elements contained within.
#' Names are taken from the naming in the Standard Output object XML tags / sections.
parameterEstimates <- getParameterEstimates(mlx)
names(parameterEstimates)
names(parameterEstimates$PrecisionPopulationEstimates$MLE)

#' Maximum likelihood estimates of population parameters.
parameterEstimates$PopulationEstimates$MLE$data

#' Lower-level get<<...>> functions are also available.
#' It is not intended for the user to use these functions directly.
#' They are provided and wrapped up into the "higher level" functions.
#DDMoRe.TEL:::getLikelihood(mlx)
#DDMoRe.TEL:::getPopulationEstimates(mlx)
#DDMoRe.TEL:::getPrecisionPopulationEstimates(mlx)
#DDMoRe.TEL:::getIndividualEstimates(mlx)
#DDMoRe.TEL:::getPrecisionIndividualEstimates(mlx)
#DDMoRe.TEL:::getResiduals(mlx)
#DDMoRe.TEL:::getPredictions(mlx)
#DDMoRe.TEL:::getRawResults(mlx)
#DDMoRe.TEL:::getToolSettings(mlx)
#DDMoRe.TEL:::getSoftwareMessages(mlx)

#' Perform model diagnostics for the base model using Xpose functions (graphs are exported to PDF)
#' -------------------------

#' Use TEL function as.xpdb() to create an Xpose database object from the standardised output object, regardless of 
#' target software used for estimation. Users can then call xpose functions directly.  
  
mlx.xpdb<-as.xpdb(mlx,"warfarin_conc.csv")

#' The following functions are Xpose functions.
#' For more information see ?xpose4  

#' Basic diagnostics for Monolix fit.
basic.gof(mlx.xpdb)

#' Individual plots of DV, PRED and IPRED
ind.plots(mlx.xpdb)

#' SAEM Estimation with NONMEM
#' -------------------------
NM <- estimate(mdlfile, target="NONMEM", subfolder="NONMEM")

#' Results from Monolix should be comparable with results from NONMEM.
getParameterEstimates(mlx)$PopulationEstimates$MLE$data
#' Note that the estimate of RUV_ADD from the NONMEM SAEM fit is ~0.
getParameterEstimates(NM)$PopulationEstimates$MLE$data

#' Xpose diagnostics using NONMEM output
#' -------------------------
nm.xpdb<-as.xpdb(NM,"warfarin_conc.csv")

#' Basic diagnostics for NONMEM fit.
basic.gof(nm.xpdb)

#' Comparison of Monolix and NONMEM fits
xplot1 <- dv.vs.pred(mlx.xpdb, main = "Monolix", pass.plot.list = TRUE)
xplot2 <- dv.vs.pred(nm.xpdb, main = "NONMEM", pass.plot.list = TRUE)
xplot3 <- dv.vs.ipred(mlx.xpdb, main = "Monolix", pass.plot.list = TRUE)
xplot4 <- dv.vs.ipred(nm.xpdb, main = "NONMEM", pass.plot.list = TRUE)

plotList <- list(xplot1, xplot2, xplot3, xplot4)
default.plot.title <- "Comparison of Monolix and NONMEM estimation"
xpose.multiple.plot(plotList, default.plot.title)

#' VPC of model
#' --------------------------------
#' VPC.PsN will in future allow the user to specify the -lst option 
#' and will copy the appropriate .lst NONMEM output file into the temporary run directory.
#' For now (and also when basing VPC on estimation from a target software other than NONMEM)
#' we must update the parameter values.

#' We need to update the initial values of the parameters with the final estimates from the SAEM estimation in Monolix
#' using getPopulationEstimates(...) function and a bit of extra code.  
#' Note that getPopulationEstimates is not intended as a function that users would use directly -
#' it is part of the getParameterEstimates(...) function. 

temp <- DDMoRe.TEL:::getPopulationEstimates(mlx)
temp <- temp$MLE$data
parValues <- as.numeric(temp[1,])
parNames <- names(as.data.frame(temp))

#' Ensure that parameter names are consistent with the model parameters.
#' Parameter names should come from the PharmML - due to issues with naming in the Standard Output Object
#' we need to provide these here for now.
structuralNames <- c("POP_CL","POP_V","POP_KA","POP_TLAG")
variabilityNames <- c("PPV_CL","PPV_V","PPV_KA","PPV_TLAG","RUV_PROP","RUV_ADD")

#' We can then update the parameter object using the "update" function.  
#' In future, we will alter the update(...) function to take a vector of parameters from the estimation
#' to update ALL initial values.

myParObj <- getParameterObjects(mdlfile)[[1]]
myParObjUpdated <- update(myParObj,block="STRUCTURAL",item=parNames[parNames%in%structuralNames],with=list(value=parValues[parNames%in%structuralNames]))
myParObjUpdated <- update(myParObjUpdated,block="VARIABILITY",item=parNames[parNames%in%variabilityNames],with=list(value=parValues[parNames%in%variabilityNames]))

#' The name of the correlation parameter 'CORR_PPV_CL_V' is different in the SO ('r_V_CL'), and needs to be handled differently 
myParObjUpdated <- update(myParObjUpdated,block="VARIABILITY",item="CORR_PPV_CL_V",with=list(value=parValues[parNames%in%"CORR_PPV_CL_V"]))

#' Let's now look at the updated MCL parameter object.
myParObjUpdated

#' Create a new MOG object combining the original data, model and task information with the updated Parameter object.
myNewMOG <- createMogObj(dataObj = getDataObjects(mdlfile)[[1]], 
		parObj = myParObjUpdated, 
		mdlObj = getModelObjects(mdlfile)[[1]], 
		taskObj = getTaskPropertiesObjects(mdlfile)[[1]])

#' We can then write the MOG back out to an .mdl file.
mdlfile.updated <- "Warfarin-ODE-latest-VPC.mdl"
write(myNewMOG,mdlfile.updated)

#' Now run the VPC using this new mdlfile.
vpcFiles <- VPC.PsN(mdlfile.updated, 
					samples=100, seed=123456, 
					subfolder="vpc_warfarin", plot=FALSE) 

#' By default VPC.PsN will also create the graph, but we can opt to do so separately 
#' if we want to make changes to the visualisation later without re-running the VPC.
xpose.VPC(vpc.info=vpcFiles$vpc.info,vpctab=vpcFiles$vpctab,main="VPC warfarin")

#' Change estimation methodology
#' ---------------------------------
#' Change estimation method to FOCE for bootstrap (for speed). 
#' Keeping the same task properties in MCL will result in bootstrap with SAEM estimation which is slow.
myTaskProperties <- getTaskPropertiesObjects(mdlfile)[[1]]
myNewTaskProperties <- myTaskProperties
#' Note that we insert the R "\n" new line separator and also an additional "\" to escape the quotes around FOCEI.
myNewTaskProperties@ESTIMATE <- "target=NMTRAN_CODE \n cov=false \n algo=[\"FOCEI\"]"

#' Assembling the new MOG. Note that we reuse the data and model from the previous run.  
#' Parameter initial values are based on the final estimates from the SAEM estimation.  
#' Task properties are created above - changing from SAEM to FOCE with interaction (FOCEI).  

myNewerMOG <- createMogObj(dataObj = getDataObjects(mdlfile)[[1]], 
		parObj = getParameterObjects(mdlfile)[[1]], 
		mdlObj = getModelObjects(mdlfile)[[1]], 
		taskObj = myNewTaskProperties)

#' We can then write the MOG back out to an .mdl file.
mdlfile.FOCEI <- "Warfarin-ODE-latest-FOCEI.mdl"
write(myNewerMOG,mdlfile.FOCEI)

#' Test estimation using this new MOG - check that estimation works prior to running the bootstrap.
NM.FOCEI <- estimate(mdlfile.FOCEI, target="NONMEM", subfolder="NONMEM_FOCEI")

#' Run the bootstrap using PsN
#' ------------------------------------
#' Note that we need to use the additional option -threads=2 on the desktop to limit the number of processors used
#' in the bootstrap. The default (as set in the PsN configuration file "C:\SEE\perl\site\lib\PsN_4_3_1\psn.conf") is 5.
bootstrapResults <- bootstrap.PsN(mdlfile.FOCEI, samples=100, seed=876543,
		bootstrapOptions=" -no-skip_minimization_terminated -threads=2",
		subfolder="bootstrap_warfarin", plot=TRUE)

#' Extract results from the bootstrap results files.
#' Since bootstrap results are not yet fully incorporated into the Standard Output Object (SO) we must use
#' a helper function to read in the bootstrap results file and parse into R objects.
source("workflowFunctions.R")

bootstrap.summary <- bs.summary("./bootstrap_warfarin/bootstrap_results.csv")

names(bootstrap.summary)

#' Diagnostics for the bootstrap run.
#' Note that since we are bootstrapping individuals from the warfarin data where only a fraction of subjects have
#' oral dosing, so some bootstrapped data may have limited numbers of subjects with oral administration.
#' As a consequence, PPV_TLAG and PPV_KA may not be well estimated in bootstrap runs (and models may not converge).
bootstrap.summary$diagnostics

#' Bootstrap medians, bias nad percentile interval estimates. Ultimately, these should be stored in the SO.
bootstrap.summary$medians
bootstrap.summary$bias
bootstrap.summary$perc_CI

#' Simulation using simulx
#' --------------------------------
myPharmML <- as.PharmML("Warfarin-ODE-latest.mdl")

#' Use parameter values from the FOCEI estimation
temp <- DDMoRe.TEL:::getPopulationEstimates(NM.FOCEI)$MLE$data
parValues <- as.numeric(temp[1,])
parNames <- names(as.data.frame(temp))
names(parValues) <- parNames

#' Simulate for the typical weight of 70. 
#' Recall that logtWT = log(WT/70). 
p <- c(parValues,logtWT=0)

#' Parameter values used in simulation
p 

#' Simulate for a dose of 100mg given at time 0 into the GUT (oral administration)
adm <- list(target='GUT', time = 0, amount = 100)

#' Simulate PK parameters for individuals
ind <- list(name = c('TLAG','KA','CL','V'))

#' Simulate predicted (CC) and observed concentration values (Y)
f   <- list( name = c('CC'), time = seq(0,to=50,by=1))
y   <- list( name = c('Y'), time = c(0, 0.5, 1, 2, 3, 4, 6, 8, 12, 24, 36, 48))

#' Simulate 12 subjects
g <- list( size = 12, level = 'individual',  treatment = adm)

#' Call simulx
res  <- simulx(model = myPharmML, parameter = p, group = g, output = list(ind,f,y))

#' Simulated parameter values for each individual
print(res$parameter)

#' Plot simulated results
plot(ggplot() + 
				geom_line(data=res$CC, aes(x=time, y=CC, colour=id)) +
				geom_point(data=res$Y, aes(x=time, y=Y, colour=id)) +
				xlab("time (h)") + ylab("concentration") )

#' Just the predictions (without residual error)
plot(ggplot() + 
				geom_line(data=res$CC, aes(x=time, y=CC, colour=id)) +
				xlab("time (h)") + ylab("concentration") )
