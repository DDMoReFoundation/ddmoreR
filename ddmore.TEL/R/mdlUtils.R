TEL.getInputs <- function(modelfile=NULL) {
  
  ## Parse the data object from the model file

  ## Get the input files
  if (file_ext(modelfile)=="ctl") {
    parsedCtl <- importNmMod(modelfile)
    parsedCtl[[4]][[1]]$Data[1]
    #c("warfpk.csv") 
  }
  else {
    # TODO: This is a temporary fix in order to get the data files copied.
    # The correct fix would be to actually parse the MDL or PharmML model file in order to extract the data file references. 
    dir(recursive=1)
  }
  
}