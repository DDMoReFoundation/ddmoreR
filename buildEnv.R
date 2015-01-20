##############################
# Author: cmusselle
##############################

## Fetch the R_LIBS location
localLib <- Sys.getenv("R_LIBS")
.libPaths(localLib)

# Global options
repo = "http://cran.rstudio.com/"
dest.dir = "dependencies"

dependencies = c("bitops", "brew", "digest", "Rcpp", "RCurl", "rjson", "RNMImport", 
                 "roxygen2", "stringr",  "XML", "devtools")

message("Checking for and installing dependencies...")

installFromLocalSourceIfPossible <- function(pkg.name, localLib, dest.dir) {

  if (!require(pkg.name, lib.loc=localLib, character.only = TRUE)){
    
    # Check if already downloaded
    matching.files = list.files(dest.dir, pattern = paste0(pkg.name, "*"))
    
    if (length(matching.files) == 0) {
      # Download latest from CRAN
      install.packages(eval(pkg.name), lib=localLib, 
          INSTALL_opts = "--no-lock", repos = repo, destdir = dest.dir, type="source")
    
    } else {
      
      if (length(matching.files) > 1) {
        warning("Multiple versions of local source file detected")
      }
      
      #  Install from local source
      install.path = paste(dest.dir, matching.files[[1]], sep="/")
      
      install.packages(install.path, repos=NULL, 
                      lib=localLib, INSTALL_opts = "--no-lock", 
                      type="source")
    }   
    
    if (!require(pkg.name, lib.loc=localLib, character.only = TRUE)){
      stop(paste0(pkg.name, "failed to be installed"))
    }
  }
} 


for (pkg.name in dependencies) {

  installFromLocalSourceIfPossible(pkg.name, localLib, dest.dir)

}

message("done")
