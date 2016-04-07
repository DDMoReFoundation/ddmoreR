

context("Test XML parsers")

test_that("Test Parse Element low level tool", {
    
    cf1 <- structure(
        list(
            description = structure(
                list(POP_CL = list("1", "popParameter", "real"), 
                    POP_V = list("2", "popParameter", "real"), 
                    POP_KA = list("3", "popParameter", "real"), 
                    POP_TLAG = list("4", "popParameter", "real"), 
                    BETA_CL_WT = list("5", "popParameter", "real"), 
                    BETA_V_WT = list("6", "popParameter", "real"), 
                    PPV_CL = list("7", "varParameter_var", "real"), 
                    PPV_V = list("8", "varParameter_var", "real"), 
                    PPV_KA = list("9", "varParameter_var", "real"), 
                    PPV_TLAG = list("10", "varParameter_var", "real"), 
                    RUV_PROP = list("11", "popParameter", "real"), 
                    RUV_ADD = list("12", "popParameter", "real"), 
                    CORR_PPV_CL_V = list("13", "varParameter_corr", "real")), 
                .Names = c("POP_CL", "POP_V", "POP_KA", "POP_TLAG", "BETA_CL_WT", 
                    "BETA_V_WT", "PPV_CL", "PPV_V", "PPV_KA", "PPV_TLAG", 
                    "RUV_PROP", "RUV_ADD", "CORR_PPV_CL_V"), 
                row.names = c("columnNum", "columnType", "valueType"), 
                class = "data.frame"), 
            data = structure(
                c("0.93882", "1.63219", "8.10703", "1", "0.13426", "0.75", 
                    "0.54806", "0.76833", "0.13334", "0.26875", "0.16612", 
                    "0.32349", "0.07736"), 
                .Dim = c(1L, 13L), 
                .Dimnames = list(NULL, 
                    c("POP_CL", "POP_V", "POP_KA", "POP_TLAG", "BETA_CL_WT", 
                        "BETA_V_WT", "PPV_CL", "PPV_V", "PPV_KA", "PPV_TLAG", 
                        "RUV_PROP", "RUV_ADD", "CORR_PPV_CL_V")))), 
        .Names = c("description", "data"))
    
    dataPath <- system.file("tests", "data", "PharmMLSO", "HandCoded", 
        "warfarin_PK_ODE_SO_FULL.xml",  
        package = "ddmore")
    # read file
    root <- ddmore:::validateAndLoadXMLSOFile(file = dataPath)
    # SO Block
    soBlocks <- root[names(root) == "SOBlock"]
    SOChildren <- xmlChildren(x = soBlocks[[1]])
    # MLE
    children <- xmlChildren(x = SOChildren[["Estimation"]][["PopulationEstimates"]])
    
    expect_equal(object = ddmore:::ParseElement(Node = children[["MLE"]]), 
        expected = cf1)
    
    children <- xmlChildren(x = SOChildren[["Simulation"]][["SimulationBlock"]])
    
    expect_equal(object = ddmore:::ParseElement(Node = children[["SimulatedProfiles"]]), 
        expected = cf1)
    
    
})
