context("Tests for the subset methods")

# Set working directory to temporary location
oldwd <- getwd()
tmp <- tempdir()
setwd(tmp)

# Copy csv file to temp directory:
csv <- system.file("data", "training", "tumour_exposure.csv", package="DDMoRe.TEL")
logi <- file.copy(csv, ".")

# Stop if not copied successfully:
stopifnot(logi)

# Import data
source(system.file("data", "training", "mogObjTumourSize.r", package="DDMoRe.TEL"))
stopifnot(DDMoRe.TEL:::is.mogObj(myMog))


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


test_that("Samples are reproducible after setting a seed", {
  
  set.seed(123)
  res <- head(sample(myMog@dataObj, size=1000, replace=TRUE, by="TIME"))
  res$ID <- as.numeric(res$ID)
  
  comp <- structure(list(ID = c(4, 11, 6, 12, 13, 1), TIME = c(1, 1, 1, 
    1, 1, 1), AMT = c(168, 2016, 504, 2184, 2352, 0), DV = c(0, 2794.7, 
    0, 0, 0, 5448.7), `NA` = c(14.288, 0, 9.664, 6.6009, 11.702, 
    0)), .Names = c("ID", "TIME", "AMT", "DV", NA), row.names = c(NA, 
    6L), class = "data.frame")
  
  expect_equal(res, comp)

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


test_that("When called on a mogObj, the function correctly extracts the dataObj and 
  passes to the correct sample method", {
  
  set.seed(123)
  res <- head(sample(myMog, size=1000, replace=TRUE, by="TIME"))
  res$ID <- as.numeric(res$ID)
  
  comp <- structure(list(ID = c(4, 11, 6, 12, 13, 1), TIME = c(1, 1, 1, 
    1, 1, 1), AMT = c(168, 2016, 504, 2184, 2352, 0), DV = c(0, 2794.7, 
    0, 0, 0, 5448.7), `NA` = c(14.288, 0, 9.664, 6.6009, 11.702, 
    0)), .Names = c("ID", "TIME", "AMT", "DV", NA), row.names = c(NA, 
    6L), class = "data.frame")
  
  expect_equal(res, comp)

})





# Re-set to old working directory:
setwd(oldwd)





