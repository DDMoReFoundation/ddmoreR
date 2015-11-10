## Bring back the R_LIBS location
localLib <- Sys.getenv("R_LIBS")
.libPaths(localLib)
if (interactive()){
  cmd <- c("output")
}else{
  cmd <- commandArgs(TRUE)
}

packageSource <- cmd[1]

install.packages(packageSource, repos=NULL, lib=localLib, INSTALL_opts = "--no-lock", type="source")
require(DDMoRe, lib.loc = localLib)
require(testthat, lib.loc = localLib) 
require(XML, lib.loc = localLib) 
## Parse command line arg
outputFolder <- cmd[2]
packageName <- "DDMoRe"

## Load Dependencies
require(tools)
dpds <- pkgDepends(packageName)$Depends
lapply(strsplit(dpds, " "), FUN = function(e){require(e[1], character.only = TRUE)})
## Run tests
try( test_package(packageName,
  reporter = JUnitReporter$new(file =file.path(getwd(),
  paste("testReport", packageName, ".xml", sep = "")    )))
)

message("Saving test report to ", getwd())
message("Saving warnings to ", getwd())

warnings(file = file.path(getwd(),"testWarnings.log"), append=TRUE)

