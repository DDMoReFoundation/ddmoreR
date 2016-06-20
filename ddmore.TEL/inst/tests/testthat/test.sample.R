context("Tests for the sample methods")

csvpath <- system.file("training", "data", package="ddmore")

# Import data
source(system.file("training", "data", "mogData.R", package="ddmore"))

stopifnot(ddmore:::is.mogObj(mogData))

set.seed(123)

test_that("Warning is returned when stratified sampling causes number of sampled rows to
  not match the number requested", {

  expect_warning(sample(object = mogData@dataObj, 
      size=20, replace = FALSE, by = "TIME", sourceDir = csvpath, fileout = NULL), 
    reg = "Due to stratified sampling, the number of samples from")
  
})


test_that("Error is returned when requesting a sample without replacement when
  sample size is larger than number of data rows", {

  expect_error(sample(object = mogData@dataObj, 
      size = 20000, replace = FALSE, by = "TIME", sourceDir = csvpath, fileout = NULL), 
    reg = "cannot take a sample larger than the population when 'replace = FALSE'")
  
})

  
res <- suppressWarnings(readDataObj(
    sample(object = mogData@dataObj, 
        size = 1000, replace = TRUE, sourceDir = csvpath, fileout = NULL), 
    sourceDir = csvpath))

test_that("Samples returns results with all columns and correct number of rows", {
  
  expect_true(nrow(res) > 250)
  expect_equal(object = names(res), 
    expected = c("ID", "TIME", "WT", "AMT", "DVID", "DV", "MDV", "logtWT"))

})


test_that("Expected errors are produced when incorrect input is provided", {
  
  expect_error(sample(mogData@dataObj, by = "hello", sourceDir = csvpath), 
    reg = "'by' name is not a column of the data, as detailed in the DATA_INPUT_VARIABLES slot")

})


test_that("Expected errors are produced when length of prob vector is incorrect", {
  
  expect_error(sample(mogData@dataObj, prob = "0.1", sourceDir = csvpath), 
    reg = "Length of 'prob' vector")

})


test_that("Expected errors are produced when sum of prob vector is greater than 1", {
  
  expect_error(sample(mogData@dataObj, size = 10, prob = 1:288, sourceDir = csvpath), 
    reg = "Sum of 'prob' vector is not equal to 1")

})

  
res2 <- suppressWarnings(readDataObj(
    sample(mogData, size = 1000, replace = TRUE, sourceDir = csvpath, fileout = NULL), 
  sourceDir = csvpath))

test_that("When called on a mogObj, the function correctly extracts the dataObj and 
  passes to the correct sample method", {
  
  expect_equal(dim(res2), dim(res))
  expect_equal(colnames(res2), colnames(res))
})

