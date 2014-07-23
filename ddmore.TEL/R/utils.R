
##############################################################
#' URLencode
#'
#' As per the standard utility function utils::URLencode but encoding
#' the plus character in the URL string; this being a bug in URLencode.
#'
#' @param x URL or GET/POST string to be URL-encoded
#' @param the resulting URL-encoded string

URLencode <- function(x, ...) {
    gsub('[+]', '%2B', utils::URLencode(x, ...))
}

##############################################################
#' strip_quotes
#'
#' Remove any enclosing double quotes around a string if present.
#'
#' @param x the input string
#' @param the output string
strip_quotes <- function(x) {
    gsub("^\"(.*)\"$", "\\1", x)
}

##############################################################
#' add_quotes
#'
#' Add enclosing double quotes round a string if none are already present.
#'
#' @param x the input string
#' @param the output string
add_quotes <- function(x) {
    gsub("^(.*)$", "\"\\1\"", strip_quotes(x))
}


##############################################################
#' parent.folder
#'
#' Derive the absolute path to a file (or folder), takes its parent,
#' and returns the path to this parent folder.
#'
#' Note that the file/folder must exist.
#'
#' @param f file/folder for which to find its parent
#' @param the absolute path to the parent folder of the input file/folder
parent.folder <- function(f) {
    file_path_as_absolute(file.path(file_path_as_absolute(f), ".."))
}


##############################################################
#' .assignFun
#'
#' Assigns column x of a data frame into an environment
.assignFun <- function(x, dat, env){
  assign(x, dat[x], envir=env)
}


##############################################################
#' .rowEvaluate
#'
#' Evaluates a string containing R code on each row of a data frame and returns
#' an updated data frame
.rowEvaluate <- function(dat, codeString){
  
  # Create empty environment in which to evaluate codeString
  env1 <- new.env()
  
  # Loop along each row of the data frame
  for(ii in seq_along(dat[,1])){
    
    # Extract desired row and col names
    temp <- dat[ii,]
    
    nam <- names(temp)

    # Assign the values to the environment
    sapply(nam, .assignFun, dat=temp, env=env1)
      # Evaluate the code in the new environment
      eval(parse(text=codeString), envir=env1)
      # Bring back the updated values and put back into temp
      for(nn in objects(env1)){
        temp[nn] <- eval(parse(text=paste0("env1$", nn)))
      }
     
    # Overwrite the original row with the updated values 
    dat[ii,] <- temp
  }

  # Remove the environment
  rm(env1)
  
  return(dat)
}
