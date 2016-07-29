################################################################################
# Copyright (C) 2016 Mango Business Solutions Ltd, http://www.mango-solutions.com
#
# This program is free software: you can redistribute it and/or modify it under
# the terms of the GNU Affero General Public License as published by the
# Free Software Foundation, version 3.
#
# This program is distributed in the hope that it will be useful,
# but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY
# or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License
# for more details.
#
# You should have received a copy of the GNU Affero General Public License along
# with this program. If not, see <http://www.gnu.org/licenses/agpl-3.0.html>.
################################################################################

context("Tests for the plot methods")

csvpath <- system.file("training", "data", package="ddmore")

# Import data
source(system.file("training", "data", "mogData.R", package="ddmore"))

stopifnot(ddmore:::is.mogObj(mogData))

test_that("Error messages are returned for incorrect inputs for dataObj object", {

  expect_error(plot(mogData@dataObj, by="AMT", group="ID", sourceDir = csvpath), 
    reg="Column specified by IDVVar argument is not present in the data set")
  
  expect_error(plot(mogData@dataObj, by="fakeColName", group="ID", 
        IDVVar="TIME", sourceDir = csvpath), 
    reg="Column specified by 'by' argument is not present in the data set")

  expect_error(plot(mogData@dataObj, by="AMT", group="fakeColName", 
        IDVVar="TIME", sourceDir = csvpath), 
    reg="Column specified by 'group' argument is not present in the data set")
  
})

test_that("Plots are produced correctly", {
  
  fname <- "mogObj-delete-me.png"
  # Use md5 sums to check plot equality
  png(file = fname)
  print(plot(mogData, by = "DVID", group = "ID", IDVVar = "TIME", 
        sourceDir = csvpath))
  dev.off()
  
  moImgSize <- file.info(fname)[1, "size"]
  expect_true(object = moImgSize > 5000)
  
  momd5 <- unname(tools::md5sum(fname))
  
  cfmd5 <- "330f486a6fabf5900196684a9e47cc22"
  
  if (.Platform$OS.type == "windows") {
    
    if (momd5 != cfmd5) {
        warning("different md5sum on this machine:", momd5)
    } else {
        expect_equal(object = momd5, 
            expected = cfmd5)
    }
  } else {
    warning("unsupported OS type when testing plot output: ", .Platform$OS.type)
  }
  unlink(fname)
  
  fname <- "dataObj-delete-me.png"
  # Use md5 sums to check plot equality
  png(file = fname)
  print(plot(mogData@dataObj, by = "DVID", group = "ID", IDVVar = "TIME", 
        sourceDir = csvpath))
  dev.off()
  
  expect_true(object = file.info(fname)[1, "size"] == moImgSize)
  
  domd5 <- unname(tools::md5sum(fname))
  
  expect_equal(object = domd5, 
      expected = momd5)
  unlink(fname)
  
})
