##############################
# Author: khanley
# Last modified: 02/06/2014
##############################
## Bring back the R_LIBS location
localLib <- Sys.getenv("R_LIBS")
.libPaths(localLib)


cat("Checking for and installing dependencies\n")
if (!require(bitops, lib.loc=localLib)){
  install.packages("./dependencies/bitops_1.0-6.zip", repos=NULL, 
  lib=localLib, INSTALL_opts = "--no-lock")
}
if (!require(bitops, lib.loc=localLib)){
  stop("bitops does not seem to be installed")
}
if (!require(RCurl, lib.loc=localLib)){
  install.packages("./dependencies/RCurl_1.95-4.1.zip", repos=NULL, 
  lib=localLib, INSTALL_opts = "--no-lock")
}
if (!require(RCurl, lib.loc=localLib)){
  stop("RCurl does not seem to be installed")
}
if (!require(XML, lib.loc=localLib)){
  install.packages("./dependencies/XML_3.98-1.1.zip", repos=NULL, 
  lib=localLib, INSTALL_opts = "--no-lock")
}
if (!require(XML, lib.loc=localLib)){
  stop("XML does not seem to be installed")
}

if (!require(rjson, lib.loc=localLib)){
  install.packages("./dependencies/rjson_0.2.13.zip", repos=NULL, 
  lib=localLib, INSTALL_opts = "--no-lock")
}
if (!require(rjson, lib.loc=localLib)){
  stop("rjson does not seem to be installed")
}
if (!require(digest, lib.loc=localLib)){
  install.packages("./dependencies/digest_0.6.4.zip", repos=NULL, 
  lib=localLib, INSTALL_opts = "--no-lock")
}
if (!require(digest, lib.loc=localLib)){
  stop("digest does not seem to be installed")
}
if (!require(roxygen2, lib.loc=localLib)){
  install.packages("./dependencies/brew_1.0-6.zip", repos=NULL, 
  lib=localLib, INSTALL_opts = "--no-lock")
  
  install.packages("./dependencies/Rcpp_0.11.1.zip", repos=NULL, 
  lib=localLib, INSTALL_opts = "--no-lock")  
  
  install.packages("./dependencies/stringr_0.6.2.zip", repos=NULL, 
  lib=localLib, INSTALL_opts = "--no-lock")
  
  install.packages("./dependencies/roxygen2_3.1.0.zip", repos=NULL, 
  lib=localLib, INSTALL_opts = "--no-lock")
}
if (!require(RNMImport, lib.loc=localLib)){
   install.packages("./dependencies/RNMImport_3.9.zip", repos=NULL, 
  lib=localLib, INSTALL_opts = "--no-lock")
}

if (!require(roxygen2, lib.loc=localLib)){
  stop("roxygen2 does not seem to be installed")
}
if (!require(Rcpp, lib.loc=localLib)){
  stop("Rcpp does not seem to be installed")
}
if (!require(stringr, lib.loc=localLib)){
  stop("stringr does not seem to be installed")
}
if (!require(brew, lib.loc=localLib)){
  stop("brew does not seem to be installed")
}
if (!require(RNMImport, lib.loc=localLib)){
  stop("RNMImport does not seem to be installed")
}
if (!require(testthat, lib.loc=localLib)){
  install.packages("./dependencies/testthat_0.7.2.99.tar.gz", repos=NULL, 
  lib=localLib, INSTALL_opts = "--no-lock", type="source")
}
if (!require(testthat, lib.loc=localLib)){
  stop("testthat does not seem to be installed")
}

cat("done\n")