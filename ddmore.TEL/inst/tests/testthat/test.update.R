#library("DDMoRe.TEL")
require("methods")

context("Testing the updating of attribute values of variables within a parameter object")

# Clear workspace. 
rm(list=ls())

warfParObjFile = system.file("tests/data/warfParObj.Rdata", package = "DDMoRe.TEL")
#warfParObjFile <- 'C:\\GitRepositories\\TEL.R.2\\ddmore.TEL\\inst\\tests\\data\\warfParObj.Rdata'
stopifnot(file.exists(warfParObjFile))
load(file=warfParObjFile)
stopifnot(DDMoRe.TEL:::is.parObj(warfParObj))

test_that("A single update of an attribute value of a variable", {
	
	updWarfParObj <- update(warfParObj, block="STRUCTURAL", item="POP_V", with=list(value=12.34))
	expect_equivalent(class(updWarfParObj), "parObj")
	expect_equal(updWarfParObj@STRUCTURAL$POP_V$value, "12.34", info="Checking that the \"value\" attr of the appropriate variable was updated")
	expect_equal(updWarfParObj@STRUCTURAL$POP_V$lo, "0.001", info="Checking that the \"lo\" attr of the appropriate variable was unchanged")
	expect_equal(updWarfParObj@STRUCTURAL$POP_CL$value, "0.13302", info="Checking that \"value\" attrs of other variables were unchanged")
	
})

test_that("An update of multiple attribute values of a variable", {
	
	updWarfParObj <- update(warfParObj, block="VARIABILITY", item="CORR_PPV_CL_V", with=list(lo=-0.5, hi=+0.5))
	expect_equivalent(class(updWarfParObj), "parObj")
	expect_equal(updWarfParObj@VARIABILITY$CORR_PPV_CL_V$lo, "-0.5", info="Checking that the \"lo\" attr of the appropriate variable was updated")
	expect_equal(updWarfParObj@VARIABILITY$CORR_PPV_CL_V$hi, "0.5", info="Checking that the \"hi\" attr of the appropriate variable was updated")
	expect_equal(updWarfParObj@VARIABILITY$CORR_PPV_CL_V$value, "0.21308", info="Checking that the \"value\" attr of the appropriate variable was unchanged")
	
})

test_that("An update of values of the same attribute across multiple variables", {

	structVarNames <- c("POP_CL","POP_KA","POP_TLAG")
	varValues <- c(0.2345,0.3456,0.4567)
	
	updWarfParObj <- update(warfParObj, block="STRUCTURAL", item=structVarNames, with=list(value=varValues))

	expect_equivalent(class(updWarfParObj), "parObj")
	expect_equal(updWarfParObj@STRUCTURAL$POP_CL$value, "0.2345", info="Checking that the \"value\" attr of variable POP_CL was updated")
	expect_equal(updWarfParObj@STRUCTURAL$POP_KA$value, "0.3456", info="Checking that the \"value\" attr of variable POP_KA was updated")
	expect_equal(updWarfParObj@STRUCTURAL$POP_TLAG$value, "0.4567", info="Checking that the \"value\" attr of variable POP_TLAG was updated")
	expect_equal(updWarfParObj@STRUCTURAL$POP_V$value, "8.08959", info="Checking that the \"value\" attr of variable POP_V was unchanged")
	expect_equal(updWarfParObj@STRUCTURAL$BETA_CL_WT$value, "0.75", info="Checking that the \"value\" attr of variable BETA_CL_WT was unchanged")
	expect_equal(updWarfParObj@STRUCTURAL$BETA_V_WT$value, "1", info="Checking that the \"value\" attr of variable BETA_V_WT was unchanged")
	
})

test_that("An update of values of multiple attribute across multiple variables", {
	
	structVarNames <- c("POP_CL","POP_KA","POP_TLAG")
	varLos <- c(0.1,0.2,0.3)
	varValues <- c(0.51,0.52,0.53)
	
	updWarfParObj <- update(warfParObj, block="STRUCTURAL", item=structVarNames, with=list(lo=varLos, value=varValues))
	
	expect_equivalent(class(updWarfParObj), "parObj")
	expect_equal(updWarfParObj@STRUCTURAL$POP_CL$lo, "0.1", info="Checking that the \"lo\" attr of variable POP_CL was updated")
	expect_equal(updWarfParObj@STRUCTURAL$POP_CL$value, "0.51", info="Checking that the \"value\" attr of variable POP_CL was updated")
	expect_equal(updWarfParObj@STRUCTURAL$POP_KA$lo, "0.2", info="Checking that the \"lo\" attr of variable POP_KA was updated")
	expect_equal(updWarfParObj@STRUCTURAL$POP_KA$value, "0.52", info="Checking that the \"value\" attr of variable POP_KA was updated")
	expect_equal(updWarfParObj@STRUCTURAL$POP_TLAG$lo, "0.3", info="Checking that the \"lo\" attr of variable POP_TLAG was updated")
	expect_equal(updWarfParObj@STRUCTURAL$POP_TLAG$value, "0.53", info="Checking that the \"value\" attr of variable POP_TLAG was updated")
	
})

test_that("An update of values of a \"fix\" boolean attribute of a variable", {

	updWarfParObj <- update(warfParObj, block="STRUCTURAL", item="BETA_V_WT", with=list(fix="false"))
	
	expect_equivalent(class(updWarfParObj), "parObj")
	expect_equal(updWarfParObj@STRUCTURAL$BETA_V_WT$fix, "false", info="Checking that the \"fix\" attr of variable BETA_V_WT was updated")
	
})

test_that("Specifying a item to update that is not an item of the specified block, raises an error", {
	
	expect_error(update(warfParObj, block="VARIABILITY", item=c("PPV_TLAG","POP_TLAG"), with=list(value=c(1.5, 2.5))),
		regexp="Item \\(e\\.g\\. variable\\) provided does not exist in given block \"VARIABILITY\"")
	
})

test_that("Specifying an attribute to update that is not an attribute of all the specified items, raises an error", {
	
	expect_error(update(warfParObj, block="VARIABILITY", item=c("PPV_TLAG","CORR_PPV_CL_V"), with=list(hi=2.0)),
		regexp="Names given do not exist in given block \"VARIABILITY\"and item \"PPV_TLAG\"")
	
})


