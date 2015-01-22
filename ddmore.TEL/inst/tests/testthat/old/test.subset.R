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


test_that("When passed a dataObj, subset returns the correct values for the first 10 rows, 
  and the correct number of dimensions", {

# Generate test data:
  t1 <- subset(myMog@dataObj, by=TIME==0)
  
  comp <- structure(list(ID = structure(c(1L, 1L, 2L, 2L, 3L, 3L, 4L, 4L, 
    5L, 5L), .Label = c("1", "2", "3", "4", "5", "6", "7", "8", "9", 
    "10", "11", "12", "13", "14", "15", "16", "17", "18", "19", "20", 
    "21", "22", "23", "24", "25", "26", "27", "28", "29", "30", "31", 
    "32", "33", "34", "35", "36", "37", "38", "39", "40", "41", "42", 
    "43", "44", "45", "46", "47", "48", "49", "50", "51", "52", "53", 
    "54", "55", "56"), class = "factor"), TIME = c(0, 0, 0, 0, 0, 
    0, 0, 0, 0, 0), AMT = c(5448.7, 0, 5589.4, 0, 3802.3, 0, 5779.5, 
    0, 4562.7, 0), DV = c(0, 10.524, 0, 8.3373, 0, 15.208, 0, 4.2693, 
    0, 18.786)), .Names = c("ID", "TIME", "AMT", "DV"), row.names = c(1L, 
    2L, 14L, 15L, 20L, 21L, 31L, 32L, 40L, 41L), class = "data.frame"
  )
  gen <- dput(head(t1, n=10))
  expect_identical(gen, comp)

  comp <- c(112L, 4L)
  gen <- dput(dim(t1))
  expect_identical(gen, comp)
  
})


test_that("Able to pass other arguments to the base:::subset function using ellipses", {

  # Test "select" argument:
  t2 <- subset(myMog@dataObj, by=TIME==0, select="ID")
  comp <- structure(list(ID = structure(c(1L, 1L, 2L, 2L, 3L, 3L), .Label = c("1", 
    "2", "3", "4", "5", "6", "7", "8", "9", "10", "11", "12", "13", 
    "14", "15", "16", "17", "18", "19", "20", "21", "22", "23", "24", 
    "25", "26", "27", "28", "29", "30", "31", "32", "33", "34", "35", 
    "36", "37", "38", "39", "40", "41", "42", "43", "44", "45", "46", 
    "47", "48", "49", "50", "51", "52", "53", "54", "55", "56"), class = "factor")), .Names = "ID", row.names = c(1L, 
    2L, 14L, 15L, 20L, 21L), class = "data.frame")
  gen <- dput(head(t2))
  expect_identical(gen, comp)

  # Test "drop" argument:
  t3 <- subset(myMog@dataObj, by=TIME==0, select="AMT", drop=TRUE)
  expect_true(is.vector(t3))
    
})


# Re-set to old working directory:
setwd(oldwd)





