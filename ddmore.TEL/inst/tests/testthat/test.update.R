context("Tests for the subset methods")

# Set working directory to temporary location
oldwd <- getwd()
tmp <- tempdir()
setwd(tmp)


# Import data
source(system.file("data", "training", "mogObjTumourSize.r", package="DDMoRe.TEL"))
stopifnot(DDMoRe.TEL:::is.mogObj(myMog))

test_that("Update method for parObj objects is able to update parameter objects with
  single values", { 

  comp <- update(myMog@parObj, block="STRUCTURAL", type="POP_SIZE0", list(value=-10000))
  expect_equivalent(class(myMog@parObj), "parObj")
  expect_equal(comp@STRUCTURAL$POP_SIZE0$value, -10000)
  
})

test_that("Update method for parObj objects is able to update parameter objects with
  multiple values", { 

  comp <- update(myMog@parObj, block="STRUCTURAL", type="POP_TOVER", list(value=55555, lo="66666"))
  expect_equivalent(class(myMog@parObj), "parObj")
  expect_equal(comp@STRUCTURAL$POP_TOVER$value, 55555)
  expect_equal(comp@STRUCTURAL$POP_TOVER$lo, 66666)
  
})


# Re-set to old working directory:
setwd(oldwd)





