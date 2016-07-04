# TODO: Add comment
# 
# Author: smith_mk
###############################################################################

# Additional functions to perform conversion

vectorMDLtoR <- function(MDLvec){
	unlist(strsplit(gsub("\\[|\\]", "", MDLvec),split=","))
}

#################
getwd()
#' Some useful functions for working with the SEE and workspaces in the MDL-IDE
#' A system variable has been set which retrieves the directory of the workspaces
Sys.getenv("MDLIDE_WORKSPACE_HOME")
#' By typing "~" you can get to this directory
mydir <- file.path("~/PAGEdemo/models")
setwd(mydir)
#' Another function from the ddmore R package retrieves the directory that the SEE is installed in.
#' You can then navigate relative to these directories.
ddmore:::DDMORE.checkConfiguration()

#' Set name of .mdl file and dataset 
datafile <- "Simulated_PAGE_estimation.csv"
mdlfile <- "Simeoni_PAGE_Estimation.mdl"
model <- "Simeoni_PAGE_Estimation"

#' Read the different MDL objects needed for upcoming tasks.
#' An MDL file may contain more than one object of any type 
#' (though typically only one model!) so these functions return
#' a list of all objects of that type found in the target MDL file.
#' To pick out the first, use the double square bracket notation in R
#' to retrieve the first item of the list. 
myModelObj <- getModelObjects(mdlfile)[[1]] 
myDataObj <- getDataObjects(mdlfile)[[1]]
myParameterObj <- getParameterObjects(mdlfile)[[1]]
myPriorObj <- getPriorObjects(mdlfile)[[1]]
myDesignObj <- getDesignObjects(mdlfile)[[1]]
myTaskPropertiesObj_NM <- getTaskPropertiesObjects(mdlfile)[["simeoni2004_NONMEM_task"]]
myTaskPropertiesObj_MLX <- getTaskPropertiesObjects(mdlfile)[["simeoni2004_Monolix_task"]]
myTaskPropertiesObj_BUGS <- getTaskPropertiesObjects(mdlfile)[["simeoni2004_BUGS_task"]]
myTaskPropertiesObj_PFIM <- getTaskPropertiesObjects(mdlfile)[["simeoni2004_Evaltask"]]

simulx.MOG <- createMogObj(designObj = myDesignObj, 
		parObj = myParameterObj, 
		mdlObj = myModelObj, 
		taskObj = myTaskPropertiesObj_PFIM)

#' We can then write the MOG back out to an .mdl file as before.
mdlfile.simulx <- paste0(model,"_simulx.mdl")
writeMogObj(simulx.MOG,mdlfile.simulx)

mdlfile <- mdlfile.simulx

as.simulx <- function(mdlfile){
	
	model <- as.PharmML(mdlfile)
	
	myParameterObj <- getParameterObjects(mdlfile)[[1]]
	
	STRUCTURAL <- sapply(myParameterObj@STRUCTURAL,function(x)as.numeric(x[["value"]]))
	VARIABILITY <- sapply(myParameterObj@VARIABILITY,function(x)as.numeric(x[["value"]]))
	
	parameter <- c(STRUCTURAL, VARIABILITY)
	
	myDesignObj <- getDesignObjects(mdlfile)[[1]]
	
	adm1 <- sapply(myDesignObj@INTERVENTION, function(x)lapply(x,unlist))
	
	adm<- lapply(adm1, function(x)
			{ 
				list(amount=as.numeric(x["amount"]),
						time=as.numeric(x["doseTime"]),
						target=x["input"])})		
	
	sampling1 <- sapply(myDesignObj@SAMPLING, function(x)lapply(x,unlist))
	
	sampling <- lapply(sampling1, function(x)
			{ list(time=as.vector(as.numeric(vectorMDLtoR(x["sampleTime"]))),
						name=x["outcome"])})		
	
	design1 <- sapply(myDesignObj@STUDY_DESIGN, function(x)lapply(x,unlist))
	
	design <- lapply(design1, function(x)
			{list(size=as.numeric(x["armSize"]), level="longitudinal",
						treatment=adm[[x["interventionSequence.admin"]]],
						output=sampling[[x["samplingSequence.sample"]]]
				)
			}
	)
	return(list(model=model, parameter=parameter, adm=adm, sampling=sampling, design=design))
}

simulx.input <- as.simulx(mdlfile)

library(mlxR)

simulx(model = simulx.input$model,
		parameter=simulx.input$parameter,
		group=simulx.input$design,
		output=simulx.input$sampling)