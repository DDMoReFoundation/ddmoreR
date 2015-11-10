context("Tests for the subset methods")

# Set working directory to temporary location
oldwd <- getwd()
tmp <- tempdir()
setwd(tmp)

# Copy csv file to temp directory:
csv <- system.file("data", "training", "tumour_exposure.csv", package="DDMoRe")
logi <- file.copy(csv, ".")

# Stop if not copied successfully:
stopifnot(logi)

# Import data
source(system.file("data", "training", "mogObjTumourSize.r", package="DDMoRe"))
stopifnot(DDMoRe:::is.mogObj(myMog))

set.seed(123)

test_that("Warning is returned when stratified sampling causes number of sampled rows to
  not match the number requested", {

  expect_warning(sample(myMog@dataObj, size=20, replace=FALSE, by="TIME"), 
    reg="Due to stratified sampling, the number of samples from")
  
})


test_that("Error is returned when requesting a sample without replacement when
  sample size is larger than number of data rows", {

  expect_error(sample(myMog@dataObj, size=20000, replace=FALSE, by="TIME"), 
    reg="cannot take a sample larger than the population when 'replace = FALSE'")
  
})

  
res <- read(sample(myMog@dataObj, size=1000, replace=TRUE))
test_that("Samples returns results with all columns and correct number of rows", {

  expect_equal(dput(dim(res)), c(1005L, 4L))
  expect_equal(names(res), c( "ID", "TIME", "AMT", "DV"))


})


test_that("Expected errors are produced when incorrect input is provided", {
  
  expect_error(sample(myMog@dataObj, by="hello"), 
    reg = "'by' name is not a column of the data, as detailed in the DATA_INPUT_VARIABLES slot")

})


test_that("Expected errors are produced when length of prob vector is incorrect", {
  
  expect_error(sample(myMog@dataObj, prob="0.1"), 
    reg = "Length of 'prob' vector")

})


test_that("Expected errors are produced when sum of prob vector is greater than 1", {
  
  expect_error(sample(myMog@dataObj, size=10, prob=1:699), 
    reg = "Sum of 'prob' vector is not equal to 1")

})

  
res2 <- read(sample(myMog, size=1000, replace=TRUE))
test_that("When called on a mogObj, the function correctly extracts the dataObj and 
  passes to the correct sample method", {
  

  expect_equal(dput(dim(res2)), c(1005L, 4L))
  expect_equal(names(res2), c( "ID", "TIME", "AMT", "DV"))

})

# Re-set to old working directory:
setwd(oldwd)





