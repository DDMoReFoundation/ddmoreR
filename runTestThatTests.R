
# Clear Workspace
rm(list=ls())

# Paths setup. Set this to TEL repo location
root = "C:\\Users\\cmusselle\\Projects\\DDmore\\TEL-R"

TEL.zip.path = paste(root, ".__artefacts/DDMoRe.TEL_0.0.3.tar.gz", sep="\\")

# Detach, install and reload Tell package 
if ("DDMoRe.TEL" %in% .packages()) {
  detach("package:DDMoRe.TEL", unload=TRUE)
} 

install.packages(TEL.zip.path, repos = NULL, type = "source")

library(testthat)
test_package("DDMoRe.TEL")


