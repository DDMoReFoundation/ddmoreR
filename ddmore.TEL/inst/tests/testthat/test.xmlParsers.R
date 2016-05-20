

context("Test XML parsers")

test_that("Test Parse Element low level tool", {
    
    cf1 <- new("DataSet", 
        description = structure(
            list(
                POP_BASECOUNT = list("1", "structParameter", "real"), 
                POP_BETA = list("2", "structParameter", "real"), 
                PPV_EVENT = list("3", "varParameter_var", "real")), 
            .Names = c("POP_BASECOUNT", "POP_BETA", "PPV_EVENT"), 
            row.names = c("columnNum", "columnType", "valueType"), 
            class = "data.frame"), 
        data = structure(
            c("10.0443", "0.50111", "0.0395245"), 
            .Dim = c(1L, 3L), 
            .Dimnames = list(
                NULL, 
                c("POP_BASECOUNT", "POP_BETA", "PPV_EVENT")))
    )
    
    dataPath <- system.file("tests", "data", "PharmMLSO", "MachineGenerated", 
        "UseCase11.SO.xml",  
        package = "ddmore")
    # read file
    root <- ddmore:::validateAndLoadXMLSOFile(file = dataPath)
    # SO Block
    soBlocks <- root[names(root) == "SOBlock"]
    SOChildren <- xmlChildren(x = soBlocks[[1]])
    # MLE
    children <- xmlChildren(x = SOChildren[["Estimation"]][["PopulationEstimates"]])
    
    # previously named ParseElement
    expect_equal(object = ddmore:::parseSODataElement(xmlNode = children[["MLE"]]), 
        expected = cf1)
    
})
