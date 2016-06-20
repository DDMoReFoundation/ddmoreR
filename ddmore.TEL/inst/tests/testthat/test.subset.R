context("Tests for the subset methods")

csvpath <- system.file("training", "data", package="ddmore")

# Import data
source(system.file("training", "data", "mogData.R", package="ddmore"))

stopifnot(ddmore:::is.mogObj(mogData))

test_that("When passed a dataObj, subset returns the correct names and correct values of TIME", {
  # Generate test data:
  t1 <- subset(x = mogData@dataObj, subset = TIME==0, sourceDir = csvpath)
  
  expect_identical(object = names(t1), 
    expected = c("ID", "TIME", "WT", "AMT", "DVID", "DV", "MDV", "logtWT"))
  expect_identical(object = t1$TIME, expected = rep(0, times = 32))
  
  t1a <- subset(x = mogData@dataObj, 
    subset = rep(c(TRUE, FALSE, FALSE, FALSE), times = 72), 
    sourceDir = csvpath)
  
  expect_identical(object = dim(t1a), expected = c(72L, 8L))
})


test_that("Able to pass other arguments to the base::subset function using ellipses", {

  # Test "select" argument:
  t2 <- subset(x = mogData@dataObj, subset = TIME==0, sourceDir = csvpath, 
    select="ID")
  
  expect_identical(object = names(t2), expected = "ID")

  # Test "drop" argument:
  t3 <- subset(x = mogData@dataObj, subset = TIME==0, sourceDir = csvpath, 
    select = "AMT", drop = TRUE)
  
  expect_true(is.vector(t3))  
})

