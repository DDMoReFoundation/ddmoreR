#' Tests for MDL file with just the requisite top-level objects present but no sub-blocks


context("Loading in MDL into R objects, for skeleton MDL file")


skeletonJsonFile <- system.file("tests/data/json/skeleton.json", package = "ddmore")

test_that("Checking the existence of the test data file containing the JSON-format text representing the MDL", {
    expect_true(file.exists(skeletonJsonFile), info="Checking that test data file exists")
})

skeletonJsonAsNestedLists <- rjson:::fromJSON(file=skeletonJsonFile)

test_that("Checking that JSON-format text representing the MDL could be parsed", {
    expect_true(is.list(skeletonJsonAsNestedLists), info="JSON-as-nested-lists should be a list")
	expect_true(length(skeletonJsonAsNestedLists) == 5, info="JSON-as-nested-lists should be a list containing 5 elements")
})

test_that("Expected dataObj to have been created from the JSON-format text representing the MDL", {
	
	mySkeletonDataObj <<- ddmore:::.extractTypeObjects(skeletonJsonAsNestedLists, "dataObj")[[1]]
	
	expect_true(isS4(mySkeletonDataObj), info="dataObj should be an S4 class")
	expect_true(class(mySkeletonDataObj) == "dataObj", info="dataObj should be the correct class type")
})

test_that("Expected parObj to have been created from the JSON-format text representing the MDL", {
	
	mySkeletonParObj <<- ddmore:::.extractTypeObjects(skeletonJsonAsNestedLists, "parObj")[[1]]
	
	expect_true(isS4(mySkeletonParObj), info="parObj should be an S4 class")
	expect_true(class(mySkeletonParObj) == "parObj", info="parObj should be the correct class type")
})

test_that("Expected mdlObj to have been created from the JSON-format text representing the MDL", {
			
	mySkeletonMdlObj <<- ddmore:::.extractTypeObjects(skeletonJsonAsNestedLists, "mdlObj")[[1]]
	
	expect_true(isS4(mySkeletonMdlObj), info="mdlObj should be an S4 class")
	expect_true(class(mySkeletonMdlObj) == "mdlObj", info="mdlObj should be the correct class type")
})

test_that("Expected taskObj to have been created from the JSON-format text representing the MDL", {
			
	mySkeletonTaskObj <<- ddmore:::.extractTypeObjects(skeletonJsonAsNestedLists, "taskObj")[[1]]
	
	expect_true(isS4(mySkeletonTaskObj), info="taskObj should be an S4 class")
	expect_true(class(mySkeletonTaskObj) == "taskObj", info="taskObj should be the correct class type")
})

test_that("Expected output Mog to have been created", {

	mySkeletonOutputMog <<- createMogObj(dataObj = mySkeletonDataObj, parObj = mySkeletonParObj, mdlObj = mySkeletonMdlObj, taskObj = mySkeletonTaskObj, mogName = "skeleton_mog")
	
	expect_true(isS4(mySkeletonOutputMog), info="Output MOG object should be an S4 class")
	expect_true(class(mySkeletonOutputMog) == "mogObj", info="Checking the class of the Output MOG object")
	expect_identical(mySkeletonOutputMog@dataObj, mySkeletonDataObj, info="dataObj slot in the Output MOG object should have been populated")
	expect_identical(mySkeletonOutputMog@parObj, mySkeletonParObj, info="parObj slot in the Output MOG object should have been populated")
	expect_identical(mySkeletonOutputMog@mdlObj, mySkeletonMdlObj, info="mdlObj slot in the Output MOG object should have been populated")
	expect_identical(mySkeletonOutputMog@taskObj, mySkeletonTaskObj, info="taskObj slot in the Output MOG object should have been populated")
	expect_equal(mySkeletonOutputMog@name, "skeleton_mog", info="Checking the name of the Output MOG object")
	
})

test_that("Expected output Mog to have been written out as JSON", {
			
	skeletonJsonFileOutput <<- file.path(tempdir(), "skeleton.output.json")
	
	writeLines(ddmore:::.generateJSON(mySkeletonOutputMog), skeletonJsonFileOutput)
	
	expect_true(file.exists(skeletonJsonFileOutput), info="Output JSON file should exist")
	
	skeletonJsonAsNestedListsOutput <<- rjson:::fromJSON(file=skeletonJsonFileOutput)
	
})

stopifnot(file.exists(skeletonJsonFileOutput))
    
test_that("Checking that output JSON-as-nested-lists could be parsed", {
	expect_true(is.list(skeletonJsonAsNestedListsOutput), info="JSON-as-nested-lists should be a list")
	expect_true(length(skeletonJsonAsNestedListsOutput) == 5, info="JSON-as-nested-lists should be a list containing 5 elements")
})

test_that("Checking the content of the nested list representing the dataObj in the output Mog", {
	expect_equal(skeletonJsonAsNestedListsOutput[[1]], list(name="skeleton_data", type="dataObj", blocks=list()))
})

test_that("Checking the content of the nested list representing the parObj in the output Mog", {
	expect_equal(skeletonJsonAsNestedListsOutput[[2]], list(name="skeleton_par", type="parObj", blocks=list()))
})

test_that("Checking the content of the nested list representing the mdlObj in the output Mog", {
	expect_equal(skeletonJsonAsNestedListsOutput[[3]], list(name="skeleton_mdl", type="mdlObj", blocks=list()))
})

test_that("Checking the content of the nested list representing the taskObj in the output Mog", {
	expect_equal(skeletonJsonAsNestedListsOutput[[4]], list(name="skeleton_task", type="taskObj", blocks=list()))
})

# TODO: Implement this test
#test_that("Checking the content of the nested list representing the mogObj in the output Mog", {
#	expect_equal(skeletonJsonAsNestedListsOutput[[5]], list(name="skeleton_mog", blocks=list(), type="mogObj"))
#})

