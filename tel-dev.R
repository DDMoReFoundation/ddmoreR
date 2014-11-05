##
## Load in the R files so that the workflow functions can be modified on-the-fly,
## rather than having to re-build the DDMoRE.TEL package every time a change is made.
##

scripts.dir <- paste0(dirname(sys.frame(1)$ofile), "/ddmore.TEL/R/")

script.files = c(
    "telClasses", "createMogObj",
    "utils", "execute", "fileUtils", "mdlUtils", "conversion", "read", "update",
    "getDataObjects", "getParameterObjects", "getModelObjects", "getTaskPropertiesObjects", "getMDLObjects",
    "server","psnWrappers", "StandardOutputMethods", "StandardOutputSubClasses", "StandardOutputObject"
)


sapply(script.files, function(s) {
    script.file <- paste0(scripts.dir, s);
    cat(c("Loading in", script.file, "...\n"))
    source(script.file)
})

# Load required libraries
library(rjson)
library(RCurl)
library(RNMImport)
