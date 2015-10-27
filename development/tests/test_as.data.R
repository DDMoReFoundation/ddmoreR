# As.data test 

# Clear workspace. 
rm(list=ls())

# Paths setup. Set this to TEL repo location
root = "C:\\Users\\cmusselle\\Projects\\DDmore\\TEL-R"
setwd(root)

TEL.zip.path = paste(root, ".__artefacts/DDMoRe.TEL_0.0.3.tar.gz", sep="\\")

# Detach, install and reload Tell package 
if ("DDMoRe.TEL" %in% .packages()) {
  detach("package:DDMoRe.TEL", unload=TRUE)
}

install.packages(TEL.zip.path, repos = NULL, type = "source")
require("DDMoRe.TEL")

require("testthat")


source( paste(root, "ddmore.TEL\\R\\StandardOutputObject.R", sep="\\"))

#Monolix.SO.path = paste(root, "development data\\MONOLIX_SO", sep="\\")
test.SO.path = paste(root, "dev scripts\\Product3_EXAMPLE\\models\\Warfarin-ODE-latest-Monolix\\Warfarin-ODE-latest-Monolix.SO.xml", sep="\\")
csv.file.path = paste(root, "dev scripts\\Product3_EXAMPLE\\models\\Warfarin-ODE-latest-Monolix\\warfarin_conc.csv", sep="\\")

# Load in SO
SOObject = LoadSOObject(test.SO.path)

# Test for fetching Raw Data from a file 
MyDataFrame = as.data(SOObject, inputDataPath=csv.file.path)

# Convert to xpose data base. 
myXpdb = as.xpdb(SOObject, inputDataPath=csv.file.path) 

# # -------------------
# # MonoLix Test 
# # -------------------
# for (i in c(Monolix.SO.path, Nonmem.SO.path)){
#   
#   data.path = i
#   
#   # Load in SO
#   SOObject = LoadSOObject(paste(data.path, "Warfarin-ODE-latest.SO.xml", sep="\\"))
#   
#   # Test for fetching Raw Data from a file 
#   MyDataFrame = as.data(SOObject, inputDataPath=paste(data.path, "warfarin_conc.csv",  sep="\\"))
#   
#   # Convert to xpose data base. 
#   myXpdb = as.xpdb(SOObject, inputDataPath=paste(data.path, "warfarin_conc.csv",  sep="\\")) 
#   
#   
# }  