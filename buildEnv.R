##############################
# Author: khanley
# Last modified: 02/06/2014
##############################
## Bring back the R_LIBS location
localLib <- Sys.getenv("R_LIBS")
.libPaths(localLib)


cat("Checking for and installing dependencies\n")
if (!require(bitops, lib.loc=localLib)){
  install.packages("./dependencies/bitops_1.0-6.tar.gz", repos=NULL, 
  lib=localLib, type="source", INSTALL_opts = "--no-lock")
}

if (!require(RCurl, lib.loc=localLib)){
  install.packages("./dependencies/RCurl_0.3-0.tar.gz", repos=NULL, 
  lib=localLib, type="source", INSTALL_opts = "--no-lock")
}

if (!require(XML, lib.loc=localLib)){
  install.packages("XML", repos="http://cran.rstudio.com/", lib=localLib, INSTALL_opts = "--no-lock")
}
if (!require(XML, lib.loc=localLib)){
  stop("XML does not seem to be installed")
}

if (!require(rjson, lib.loc=localLib)){
  install.packages("./dependencies/rjson_0.2.13.tar.gz", repos=NULL, 
  lib=localLib, type="source", INSTALL_opts = "--no-lock")
}
if (!require(rjson, lib.loc=localLib)){
  stop("rjson does not seem to be installed")
}
if (!require(digest, lib.loc=localLib)){
  install.packages("./dependencies/digest_0.6.4.tar.gz", repos=NULL, 
  lib=localLib, type="source", INSTALL_opts = "--no-lock")
}
if (!require(digest, lib.loc=localLib)){
  stop("digest does not seem to be installed")
}
if (!require(roxygen2, lib.loc=localLib)){
  install.packages("./dependencies/brew_1.0-6.tar.gz", repos=NULL, 
  lib=localLib, type="source", INSTALL_opts = "--no-lock")
  
  install.packages("./dependencies/Rcpp_0.11.1.tar.gz", repos=NULL, 
  lib=localLib, type="source", INSTALL_opts = "--no-lock")  
  
  install.packages("./dependencies/stringr_0.6.2.tar.gz", repos=NULL, 
  lib=localLib, type="source", INSTALL_opts = "--no-lock")
  
  install.packages("./dependencies/roxygen2_3.1.0.tar.gz", repos=NULL, 
  lib=localLib, type="source", INSTALL_opts = "--no-lock")
}
if (!require(roxygen2, lib.loc=localLib)){
  stop("roxygen2 does not seem to be installed")
}

cat("done\n")