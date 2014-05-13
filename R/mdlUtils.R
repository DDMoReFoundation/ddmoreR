TEL.getInputs <- function(modelfile=NULL){
  
  ## Parse the data object from the model file

  ## Get the input files
  if(file_ext(modelfile)=="ctl") {
    c("warfpk.csv") 
  }
  else {
    #c("warfarin_conc_pca.csv")
    c()
  }
}