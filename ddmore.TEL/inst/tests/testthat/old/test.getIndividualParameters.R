library("ddmore")
require("methods")

context("Test getIndividualParameters")

test_that("getIndividualParameters fails for invalid 'what' parameter", {
    expect_error(getIndividualParameters(what="Invalid"), ".*parameter was not recognised.*Invalid.*")
    }
)

# NB: Need more tests here using mocked SO object