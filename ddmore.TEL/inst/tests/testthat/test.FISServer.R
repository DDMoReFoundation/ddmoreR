library("DDMoRe.TEL")
library("XML")
require("methods")
clearTestVariables <- function() {
    rm(list=ls())
}

context("FIS Server's createFISServer.")

test_that("createFISServer creates a valid object with defaults", {
    
    clearTestVariables()
    
    instance <- createFISServer(startupScript = "MOCK")
    
    expect_true(is.FISServer(instance), 
        info = "Instance was not of type FISServer"
    )
    
    
    expect_true(instance@url == "http://localhost:9010", info = "URL was incorrect.")
    expect_true(instance@operationalUrl == "http://localhost:9011", info = "operationalUrl was incorrect")
    expect_true(instance@startupScript == "MOCK", info = "startupScript was incorrect")
    expect_true(instance@jobStatusPollingDelay == 20, info = "jobStatusPollingDelay was incorrect")
    expect_true(instance@startupPollingMax == 120, info = "startupPollingMax was incorrect")
    expect_true(instance@startupPollingDelay == 1, info = "startupPollingDelay was incorrect")
    
})

test_that("createFISServer creates a valid object from a properties file", {
    
    clearTestVariables()
    
    
    instance <- createFISServerFromProperties(propertiesFile=system.file("tests/data/json/FISServerProperties.json", package = "DDMoRe.TEL"))
    
    expect_true(is.FISServer(instance), 
                info = "Instance was not of type FISServer"
    )
    
    expect_true(instance@url == "mock-url", info = "URL was incorrect.")
    expect_true(instance@operationalUrl == "mock-operationalUrl", info = "operationalUrl was incorrect")
    expect_true(instance@startupScript == "mock-startupScript", info = "startupScript was incorrect")
    expect_true(instance@jobStatusPollingDelay == 1, info = "jobStatusPollingDelay was incorrect")
    expect_true(instance@startupPollingMax == 60, info = "startupPollingMax was incorrect")
    expect_true(instance@startupPollingDelay == 1, info = "startupPollingDelay was incorrect")
    
})
