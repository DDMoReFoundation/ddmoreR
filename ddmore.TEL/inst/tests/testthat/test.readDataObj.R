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


context("Reading in data from data file referenced by data object")

myDataObjFile <- system.file("tests/data/UseCase5DataObj.Rdata", package = "ddmore")

load(file=myDataObjFile)

myData <- readDataObj(UseCase5DataObj, sourceDir = ddmore:::parent.folder(myDataObjFile), categoricalAsFactor=TRUE)

test_that("Checking that a dataframe is returned", {
	expect_true(is.data.frame(myData))
})

test_that("Checking the names of the variables in the dataframe", {
	expect_equal(names(myData), c("ID","TIME","WT","AGE","SEX","AMT","DVID","DV","MDV"))
})

test_that("Checking the size of the dataframe (by checking the number of items for the ID column variable)", {
	expect_equal(length(myData$ID), 288)
})

test_that("Checking that the SEX column variable is parsed as a factor", {
	expect_true(is.factor(myData$SEX))
	expect_equal(class(myData$SEX[[1]]), "factor")
})

test_that("Checking the factor levels of the SEX column variable", {
	expect_equal(levels(myData$SEX), c("female", "male"))
})

test_that("Checking the correct factor values have been assigned to elements in the SEX column variable", {
	# expect_equal won't work directly here since we would be trying to compare a factor with a character
	expect_true(myData$SEX[[169]] == "female")
	expect_true(myData$SEX[[170]] == "male")
})

