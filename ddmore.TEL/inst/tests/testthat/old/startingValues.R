context("Tests for the startingValues methods")

# Set working directory to temporary location
oldwd <- getwd()
tmp <- tempdir()
setwd(tmp)


# Import data
source(system.file("data", "training", "mogObjTumourSize.r", package="DDMoRe"))
stopifnot(DDMoRe:::is.mogObj(myMog))

test_that("startingValues method is able to specify distributions for parameters", {

## before finishing this, need to update the function to take starting values by default from the 
## object, defaulting to 0, 1 only if required


})




# Re-set to old working directory:
setwd(oldwd)





