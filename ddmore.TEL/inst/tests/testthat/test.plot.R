context("Tests for the plot methods")

oldwd <- getwd()
tmp <- tempdir()
setwd(tmp)

# Copy MDL and related csv file to temp directory:
mdl <- system.file("data", "training", "tumour_size_25June2014_OAM.mdl", package="DDMoRe.TEL")
csv <- system.file("data", "training", "tumour_exposure.csv", package="DDMoRe.TEL")

logi <- file.copy(c(mdl, csv), ".")

# Stop if not copied successfully:
stopifnot(all(logi))

# Import data from mdl:
dat <- getMDLObjects("tumour_size_25June2014_OAM.mdl")
myMog <- as.mogObj(dat)
stopifnot(DDMoRe.TEL:::is.mogObj(myMog))


test_that("Error messages are returned for incorrect inputs for dataObj object", {

  expect_error(plot(myMog@dataObj, by="AMT", group="ID"), reg="Column specified by IDVVar argument is not present in the data set. Unable to produce plot.")
  
  expect_error(plot(myMog@dataObj, by="fakeColName", group="ID", IDVVar="TIME"), reg="Column specified by 'by' argument is not present in the data set. Unable to produce plot.")

  expect_error(plot(myMog@dataObj, by="AMT", group="fakeColName", IDVVar="TIME"), reg="Column specified by 'group' argument is not present in the data set. Unable to produce plot.")
  
})



test_that("Plots are produced correctly", {

  # Use md5 sums to check plot equality
  bmp("out.bmp")
  plot(myMog, by="AMT", group="ID", IDVVar="TIME")
  dev.off()
  
  md5 <- tools::md5sum("out.bmp")
  
  if (.Platform$OS.type == "windows") {
    expect_equivalent(md5, "fbe0488f5eb47d6fb728b3addb967ecd")
  } else if (.Platform$OS.type == "unix") {
    expect_equivalent(md5, "!! Unix MD5 Here !!")
  } else {
    stop("unsupported OS type: ", .Platform$OS.type)
}
      
})


# Re-set to old working directory:
setwd(oldwd)