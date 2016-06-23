#' @title Execute PFIM
#' 
#' This function executes PFIM against specifiied MDL or PharmML encoded model file. 
#'
#' @param mdlfile MDL encoded model file (mutually exclusive with pharmmlfile parameter)
#' @param pharmmlfile PharmML encoded model file (mutually exclusive with mdlfile parameter) 
#' @param subfolder of the current working directory where PFIM is going to be exeecuted
#' @param jarLocation location where PFIM converter JAR resides (if NULL the current working directory is assumed)
#' @param run flag indicating if PFIM execution should be performed as well or just conversion should be performed
#' @param PFIMlocation location of PFIM R scripts directory
#' 
#' @return current working directory
#' 
#' @examples 
#' runPFIM("simeoni-eval.xml")
#'
#' @author Mike K. Smith, Mateusz Rogalski
#' @export
runPFIM <- function (mdlfile=NULL, pharmmlfile=NULL, subfolder="PFIM", 
					jarLocation=file.path(Sys.getenv("PFIM_CONVERTER_HOME")), run=T,
					PFIMlocation=file.path(Sys.getenv("PFIM_HOME"),"Program")) {
	
	pharmmlfile <- ifelse(!is.null(mdlfile) & is.null(pharmmlfile),as.PharmML(mdlfile),pharmmlfile)
	outFolder <- file.path(getwd(),subfolder)
	dir.create(outFolder, showWarnings = FALSE)
	jarFile <- ifelse(is.null(jarLocation),"pfim.jar",file.path(jarLocation,"pfim.jar"))
	
	command <- paste("java -jar", jarFile, "-p", PFIMlocation, "-i", pharmmlfile, "-o", outFolder)
	# Example command line:
	#	command <- "java -jar pfim.jar -p C:/SEE/PFIM4.0/program -i simeoni-eval.xml -o PFIM"
	print(command)
	shell(command, intern=T)
	oldWD <- getwd()
	if(run){
		setwd(outFolder)
		shell(file.path(outFolder,"run.bat"))
		setwd(oldWD)
	}
}