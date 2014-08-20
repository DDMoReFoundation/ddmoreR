##
## Load in the R files so that the workflow functions can be modified on-the-fly,
## rather than having to re-build the DDMoRE.TEL package every time a change is made.
##

scripts.dir <- paste0(dirname(sys.frame(1)$ofile), "/ddmore.TEL/R/")
#scripts.dir <- "C:\\Users\\khanley\\Documents\\Projects\\ddmore\\TEL\\TEL.R\\ddmore.TEL\\R"

# script.files = c(
    # "telClasses", "createMogObj",
    # "utils", "execute", "fileUtils", "mdlUtils", "conversion", "read", "update", "outputObject",
    # "getDataObjects", "getParameterObjects", "getModelObjects", "getTaskPropertiesObjects", "getMDLObjects",
    # "server"
# )

# list files in directory:
script.files <- list.files(scripts.dir, full.names = TRUE)

sapply(script.files, function(s) {
    cat(c("Loading in", s, "...\n"))
    source(s)
})

# Load required libraries
library(rjson)
library(RCurl)
library(RNMImport)



# data:
setwd("C:\\Users\\khanley\\Documents\\Projects\\ddmore\\TEL\\TEL.R\\ddmore.TEL\\inst\\data\\training")
dat <- getMDLObjects("tumour_size_25June2014_OAM.mdl")


# data object:
data <- dat[[1]]
myData <- read(data)

# change names to test plot:
names(myData) <- c("ID", "IDV", "AMT", "DV")

by="AMT"
xyplot(formula("DV ~ IDV | AMT"), group=ID, data=myData)
plot(dat[[1]], by="AMT", group="ID", IDVVar="TIME")
test(dat[[1]],  sourceDir=getwd(), by="AMT", group="ID")
class(dat[[1]])
myMog <- as.mogObj(dat)
plot(myMog, by="AMT", group="ID", IDVVar="TIME")


setwd("C:\\Users\\khanley\\Documents\\Projects\\ddmore\\TEL\\TEL.R\\ddmore.TEL\\inst\\data\\training")
dat <- getMDLObjects("tumour_size_25June2014_OAM.mdl")
myMog <- as.mogObj(dat)
myData <- read(myMog@dataObj)

# Subset:
subset(myMog@dataObj, by=TIME==0)
subset(myMog, by=TIME==0)




