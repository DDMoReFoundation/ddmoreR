## Bring back the R_LIBS location
localLib <- Sys.getenv("R_LIBS")
.libPaths(localLib)
if (interactive()){
  cmd <- c("output")
}else{
  cmd <- commandArgs(TRUE)
}

parkageSource <- cmd[1]

install.packages(parkageSource, repos=NULL, lib=localLib, INSTALL_opts = "--no-lock", type="source")
require(DDMoRe.TEL, lib.loc = localLib)
require(testthat, lib.loc = localLib) 
require(XML, lib.loc = localLib) 
## Parse command line arg
outputFolder <- cmd[2]
packageName <- "DDMoRe.TEL"

## Load Dependencies
require(tools)
dpds <- pkgDepends(packageName)$Depends
lapply(strsplit(dpds, " "), FUN = function(e){require(e[1], character.only = TRUE)})
## Run tests
try( test_package(packageName,
  reporter = JUnitReporter$new(file =file.path(getwd(),
  paste("testReport", packageName, ".xml", sep = "")    )))
)




