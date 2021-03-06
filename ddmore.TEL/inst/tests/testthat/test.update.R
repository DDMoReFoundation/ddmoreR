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

library("ddmore")
require("methods")

context("Testing the updating of attribute values of variables within a parameter object")

# Clear workspace. 
rm(list=ls())


jsonFile <- system.file("tests/data/json/UseCase1.json", package = "ddmore")

test_that("Checking the existence of the test data file containing the JSON-format text representing the MDL", {
  expect_true(file.exists(jsonFile), info="Checking that test data file exists")
})

jsonAsNestedLists <- rjson:::fromJSON(file=jsonFile)

myParObj<-ddmore:::.extractTypeObjects(jsonAsNestedLists, "parObj")[[1]]
stopifnot(ddmore:::is.parObj(myParObj))

test_that("A single update of an attribute value of a variable", {
	
	updMyParObj <- updateParObj(myParObj, block="STRUCTURAL", item="POP_V", with=list(value=12.34))
	expect_equivalent(class(updMyParObj), "parObj")
	expect_equal(updMyParObj@STRUCTURAL$POP_V$value, "12.34", info="Checking that the \"value\" attr of the appropriate variable was updated")
	expect_equal(updMyParObj@STRUCTURAL$POP_V$lo, "0.001", info="Checking that the \"lo\" attr of the appropriate variable was unchanged")
	expect_equal(updMyParObj@STRUCTURAL$POP_CL$value, "0.1", info="Checking that \"value\" attrs of other variables were unchanged")
	
})

test_that("An update of multiple attribute values of a variable", {
	
	updMyParObj <- updateParObj(myParObj, block="VARIABILITY", item="PPV_TLAG", with=list(value=0.5))
	expect_equivalent(class(updMyParObj), "parObj")
	expect_equal(updMyParObj@VARIABILITY$PPV_TLAG$value, "0.5", info="Checking that the \"value\" attr of the appropriate variable was updated")
	expect_equal(updMyParObj@VARIABILITY$PPV_TLAG$fix, "true", info="Checking that the \"fix\" attr of the appropriate variable was unchanged")
})

test_that("An update of values of the same attribute across multiple variables", {

	structVarNames <- c("POP_CL","POP_KA","POP_TLAG")
	varValues <- c(0.2345,0.3456,0.4567)
	
	updMyParObj <- updateParObj(myParObj, block="STRUCTURAL", item=structVarNames, with=list(value=varValues))

	expect_equivalent(class(updMyParObj), "parObj")
	expect_equal(updMyParObj@STRUCTURAL$POP_CL$value, "0.2345", info="Checking that the \"value\" attr of variable POP_CL was updated")
	expect_equal(updMyParObj@STRUCTURAL$POP_KA$value, "0.3456", info="Checking that the \"value\" attr of variable POP_KA was updated")
	expect_equal(updMyParObj@STRUCTURAL$POP_TLAG$value, "0.4567", info="Checking that the \"value\" attr of variable POP_TLAG was updated")
	expect_equal(updMyParObj@STRUCTURAL$POP_V$value, "8", info="Checking that the \"value\" attr of variable POP_V was unchanged")
	expect_equal(updMyParObj@STRUCTURAL$BETA_CL_WT$value, "0.75", info="Checking that the \"value\" attr of variable BETA_CL_WT was unchanged")
	expect_equal(updMyParObj@STRUCTURAL$BETA_V_WT$value, "1", info="Checking that the \"value\" attr of variable BETA_V_WT was unchanged")
	
})

test_that("An update of values of multiple attribute across multiple variables", {
	
	structVarNames <- c("POP_CL","POP_KA","POP_TLAG")
	varLos <- c(0.1,0.2,0.3)
	varValues <- c(0.51,0.52,0.53)
	
	updMyParObj <- updateParObj(myParObj, block="STRUCTURAL", item=structVarNames, with=list(lo=varLos, value=varValues))
	
	expect_equivalent(class(updMyParObj), "parObj")
	expect_equal(updMyParObj@STRUCTURAL$POP_CL$lo, "0.1", info="Checking that the \"lo\" attr of variable POP_CL was updated")
	expect_equal(updMyParObj@STRUCTURAL$POP_CL$value, "0.51", info="Checking that the \"value\" attr of variable POP_CL was updated")
	expect_equal(updMyParObj@STRUCTURAL$POP_KA$lo, "0.2", info="Checking that the \"lo\" attr of variable POP_KA was updated")
	expect_equal(updMyParObj@STRUCTURAL$POP_KA$value, "0.52", info="Checking that the \"value\" attr of variable POP_KA was updated")
	expect_equal(updMyParObj@STRUCTURAL$POP_TLAG$lo, "0.3", info="Checking that the \"lo\" attr of variable POP_TLAG was updated")
	expect_equal(updMyParObj@STRUCTURAL$POP_TLAG$value, "0.53", info="Checking that the \"value\" attr of variable POP_TLAG was updated")
	
})

test_that("An update of values of a \"fix\" boolean attribute of a variable", {

	updMyParObj <- updateParObj(myParObj, block="STRUCTURAL", item="BETA_V_WT", with=list(fix="false"))
	
	expect_equivalent(class(updMyParObj), "parObj")
	expect_equal(updMyParObj@STRUCTURAL$BETA_V_WT$fix, "false", info="Checking that the \"fix\" attr of variable BETA_V_WT was updated")
	
})

test_that("Specifying a item to update that is not an item of the specified block, raises an error", {
	
	expect_error(updateParObj(myParObj, block="VARIABILITY", item=c("PPV_TLAG","POP_TLAG"), with=list(value=c(1.5, 2.5))),
		regexp="Item \\(e\\.g\\. variable\\) provided does not exist in given block \"VARIABILITY\"")
	
})

test_that("Specifying an attribute to update that is not an attribute of all the specified items, raises an error", {
	
	expect_error(updateParObj(myParObj, block="VARIABILITY", item=c("OMEGA","PPV_TLAG"), with=list(parameter="[blah]")),
	             regexp="Item \\(e\\.g\\. variable\\) provided does not exist in given block \"VARIABILITY\"")
	
})
