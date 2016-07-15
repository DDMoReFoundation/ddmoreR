################################################################################
#' runPFIM
#'
#' Converts an MDL file into appropriate inputs for use with PFIM and executes PFIM.
#'
#' @param mdlfile       An MDL file
#' @param pharmmlfile   A PharmML file
#' @param subfolder     Subfolder for converted files and associated scripts.
#' @param jarLocation   Location for the Java script for PFIM conversion
#' @param pfimLocation  Location for the Java script for PFIM conversion
#' @param run           Whether to run PFIM after conversion or stop
#' @return MDL file converted to inputs ready for PFIM, Batch script for running PFIM
#' 
#' @author Mike K Smith

runPFIM <- function (mdlfile=NULL, pharmMLfile=NULL, subfolder="PFIM", 
                     jarLocation=NULL, run=T,
                     pfimLocation=file.path(ddmore:::DDMORE.checkConfiguration(),"PFIM","PFIM4.0","program")) {
  
  pharmmlfile <- ifelse(!is.null(mdlfile) & is.null(pharmmlfile),as.PharmML(mdlfile),pharmmlfile)
  outFolder <- file.path(getwd(),subfolder)
  dir.create(outFolder, showWarnings = FALSE)
  jarFile <- ifelse(is.null(jarLocation),"pfim.jar",file.path(jarLocation,"pfim.jar"))
  
  command <- paste("java -jar", jarFile, "-p", pfimLocation, "-i", pharmmlfile, "-o", outFolder)
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