
if ("ddmore" %in% .packages()) {
  detach("package:ddmore", unload=TRUE)
} 

library(testthat)
require(devtools)
root = "C:\\Users\\cmusselle\\Projects\\DDmore\\TEL-R"
setwd(root)

ddmore = as.package("DDMoRe.TEL")

load_all(ddmore)

require(ddmore)

test_dir("ddmore.TEL/inst/tests/testthat/")


