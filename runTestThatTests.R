
# Clear Workspace
rm(list=ls())

# Paths setup. Set this to TEL repo location
root = "C:\\Users\\cmusselle\\Projects\\DDmore\\TEL-R"

TEL.zip.path = paste(root, ".__artefacts/ddmore.TEL_0.0.4.tar.gz", sep="\\")

# Detach, install and reload Tell package 
if ("ddmore" %in% .packages()) {
  detach("package:ddmore", unload=TRUE)
} 

install.packages(TEL.zip.path, repos = NULL, type = "source")

library(testthat)
test_package("ddmore")


