##############################
# Author: khanley
# Last modified: 02/06/2014
##############################
## Bring back the R_LIBS location
localLib <- Sys.getenv("R_LIBS")
.libPaths(localLib)

message("Checking for and installing dependencies...")

if (!suppressWarnings(require(bitops, lib.loc=localLib))) {
  install.packages("./dependencies/bitops_1.0-6.zip", repos=NULL, 
  lib=localLib, INSTALL_opts = "--no-lock")
}
library(bitops, lib.loc=localLib)

if (!suppressWarnings(require(RCurl, lib.loc=localLib))) {
  install.packages("./dependencies/RCurl_1.95-4.1.zip", repos=NULL, 
  lib=localLib, INSTALL_opts = "--no-lock")
}
library(RCurl, lib.loc=localLib)

if (!suppressWarnings(require(XML, lib.loc=localLib))) {
  install.packages("./dependencies/XML_3.98-1.1.zip", repos=NULL, 
  lib=localLib, INSTALL_opts = "--no-lock")
}
library(XML, lib.loc=localLib)

if (!suppressWarnings(require(rjson, lib.loc=localLib))) {
  install.packages("./dependencies/rjson_0.2.13.zip", repos=NULL, 
  lib=localLib, INSTALL_opts = "--no-lock")
}
library(rjson, lib.loc=localLib)

# documentation tools
if (!suppressWarnings(require(digest, lib.loc=localLib))) {
  install.packages("./dependencies/digest_0.6.4.zip", repos=NULL, 
  lib=localLib, INSTALL_opts = "--no-lock")
}
library(digest, lib.loc=localLib)

if (!suppressWarnings(require(roxygen2, lib.loc=localLib))) {
  install.packages("./dependencies/brew_1.0-6.zip", repos=NULL, 
  lib=localLib, INSTALL_opts = "--no-lock")
  
  install.packages("./dependencies/Rcpp_0.11.1.zip", repos=NULL, 
  lib=localLib, INSTALL_opts = "--no-lock")  
  
  install.packages("./dependencies/stringr_0.6.2.zip", repos=NULL, 
  lib=localLib, INSTALL_opts = "--no-lock")
  
  install.packages("./dependencies/roxygen2_3.1.0.zip", repos=NULL, 
  lib=localLib, INSTALL_opts = "--no-lock")
}

library(Rcpp, lib.loc=localLib)

library(stringr, lib.loc=localLib)

library(brew, lib.loc=localLib)

library(roxygen2, lib.loc=localLib)

# test framework
if (!suppressWarnings(require(testthat, lib.loc=localLib))) {
  install.packages("./dependencies/testthat_0.7.2.99.tar.gz", repos=NULL, 
  lib=localLib, INSTALL_opts = "--no-lock", type="source")
}
library(testthat, lib.loc=localLib)

# RNMImport
if (!suppressWarnings(require(RNMImport, lib.loc=localLib))) {
   install.packages("./dependencies/RNMImport_3.9.zip", repos=NULL, 
  lib=localLib, INSTALL_opts = "--no-lock")
}
library(RNMImport, lib.loc=localLib)


message("installation of R package dependencies: done")
