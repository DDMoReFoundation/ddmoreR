TEL.getInputs <- function(modelfile=NULL) {
  
  ## Parse the data object from the model file

  ## Get the input files
  if (file_ext(modelfile)=="ctl") {
    parsedCtl <- importNmMod(modelfile)
    datafile <- parsedCtl[[4]][[1]]$Data[1]
    strip_quotes(datafile)
    #c("warfpk.csv") 
  }
  else {
    # TODO: This is a temporary fix in order to get the data files copied.
    # The correct fix would be to actually parse the MDL or PharmML model file in order to extract the data file references.
    # Also, the data files cannot currently be under a subfolder. The recursive=FALSE can be switched to
    # recursive=TRUE once the specific data file relative path(s) has been identified.
    dir(pattern=".*\\.csv", recursive=FALSE)
  }
  
}