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
