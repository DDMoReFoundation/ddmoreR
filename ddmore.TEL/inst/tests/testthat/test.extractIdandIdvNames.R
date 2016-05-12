
context("Check Extraction from SOO")

test_that("Extraction from SOO Slots Use Case 2", {
    
    soXmlFilePath <- system.file("tests", "data", "PharmMLSO", "MachineGenerated", 
        "UseCase2_TIMEchange_fixed.SO.xml", 
        package = "ddmore")
    
    # note this model was created under version 0.2, 
    # so ignore message about Likelihood block 
    # (this was renamed in version 0.3)
    SOObject <- suppressMessages(LoadSOObject(soXmlFilePath))
    
    p1 <- ddmore:::extractIdandIdvNames(SOObject = SOObject, 
        PredictionsSlotName = "Estimation::Predictions")
    
    cf1 <- structure(
        list(
            ID.index = structure(c(TRUE, FALSE, FALSE, FALSE, FALSE), 
                .Dim = c(1L, 5L), 
                .Dimnames = list("columnType", c("ID", "TIME_H", "PRED", "IPRED", "DVID"))), 
            ID.colName = "ID", 
            TIME.index = structure(c(FALSE, TRUE, FALSE, FALSE, FALSE), 
                .Dim = c(1L, 5L), .Dimnames = list("columnType", 
                    c("ID", "TIME_H", "PRED", "IPRED", "DVID"))), 
            TIME.colName = "TIME_H"), 
        .Names = c("ID.index", "ID.colName", "TIME.index", "TIME.colName"))

    expect_equal(object = p1, expected = cf1)
    
    p2 <- ddmore:::extractIdandIdvNames(SOObject = SOObject, 
            ResidualsSlotName = "Estimation::Residuals::ResidualTable")

    cf2 <- structure(
        list(
            ID.index = structure(c(TRUE, FALSE, FALSE, FALSE, FALSE, FALSE, FALSE), 
                .Dim = c(1L, 7L), 
                .Dimnames = list("columnType", c("ID", "TIME_H", "RES", "IRES", "WRES", "IWRES", "DVID"))), 
            ID.colName = "ID", 
            TIME.index = structure(c(FALSE, TRUE, FALSE, FALSE, FALSE, FALSE, FALSE), 
                .Dim = c(1L, 7L), .Dimnames = list("columnType", 
                    c("ID", "TIME_H", "RES", "IRES", "WRES", "IWRES", "DVID"))), 
            TIME.colName = "TIME_H"), 
        .Names = c("ID.index", "ID.colName", "TIME.index", "TIME.colName"))
    
    expect_equal(object = p2, expected = cf2)
    
    p3 <- ddmore:::extractIdandIdvNames(SOObject = SOObject, 
            PredictionsSlotName = "Estimation::Predictions", 
            ResidualsSlotName = "Estimation::Residuals::ResidualTable")
    
    expect_equal(object = p3, expected = cf1)
    
    # named block not present
    p4 <- ddmore:::extractIdandIdvNames(SOObject = SOObject, 
            PredictionsSlotName = "XXX", 
            ResidualsSlotName = "Estimation::Residuals::ResidualTable")
    
    expect_equal(object = p4, expected = cf2)
    
    p5 <- suppressWarnings(ddmore:::extractIdandIdvNames(SOObject = SOObject, 
            PredictionsSlotName = "XXX", 
            ResidualsSlotName = "YYY"))
    
    cf0 <- list(
      ID.index = matrix(data = NA, nrow = 1, ncol = 0, 
        dimnames = list("columnType", character(0))), 
      ID.colName = character(0), 
      TIME.index = matrix(data = NA, nrow = 1, ncol = 0, 
        dimnames = list("columnType", character(0))), 
      TIME.colName = character(0))
    
    expect_equal(object = p5, expected = cf0)
})


test_that("Extraction from SOO Slots Use Case 11 (blank predictions and residuals)", {
    
    soXmlFilePath <- system.file("tests", "data", "PharmMLSO", "MachineGenerated", 
        "UseCase11.SO.xml", 
        package = "ddmore")
    
    SOObject <- suppressMessages(LoadSOObject(soXmlFilePath))
    
    p1 <- suppressWarnings(ddmore:::extractIdandIdvNames(SOObject = SOObject, 
        PredictionsSlotName = "Estimation::Predictions"))
    
    cf0 <- list(
      ID.index = matrix(data = NA, nrow = 1, ncol = 0, 
        dimnames = list("columnType", character(0))), 
      ID.colName = character(0), 
      TIME.index = matrix(data = NA, nrow = 1, ncol = 0, 
        dimnames = list("columnType", character(0))), 
      TIME.colName = character(0))
    
    expect_equal(object = p1, expected = cf0)
    
    p2 <- suppressWarnings(ddmore:::extractIdandIdvNames(SOObject = SOObject, 
            ResidualsSlotName = "Estimation::Residuals::ResidualTable"))
    
    expect_equal(object = p2, expected = cf0)
    
    # incorrect block named
    expect_error(ddmore:::extractIdandIdvNames(SOObject = SOObject, 
            ResidualsSlotName = "Estimation::IndividualEstimates::RandomEffects"), 
        regex = "no slot of name")
})


