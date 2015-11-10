
full_install = TRUE
root = "C:\\Users\\cmusselle\\Projects\\DDmore\\TEL-R"

if ("DDMoRe" %in% .packages()) {
  detach("package:DDMoRe", unload=TRUE)
} 

setwd(root)
if (full_install) {
  install.packages("C:\\Users\\cmusselle\\Projects\\DDmore\\TEL-R\\.__artefacts/DDMoRe_0.0.4.tar.gz", repos=NULL, type="source")
} else {
  ddmore = as.package("DDMoRe")
  # document(ddmore)
  load_all(ddmore)
}


library(testthat)
require(devtools)
require(DDMoRe)

test_dir("ddmore.TEL/inst/tests/testthat/")
