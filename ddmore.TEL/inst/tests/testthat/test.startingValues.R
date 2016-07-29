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


context("Tests for the startingValues methods")

if (is(try(DDMORE.getServer(), silent = TRUE), class2 = "try-error")) {
    mockServer <- ddmore:::createMockFISServer(jobStatusPollingDelay = 1)
    suppressWarnings(DDMORE.setServer(mockServer))
}

test_that("startingValues method is able to specify distributions for parameters", {
    
    ## Read in example data (found in the R package under data/training)
    fpath <- system.file(package = "ddmore", "training", "data", 
        "UseCase2.mdl")
    dat <- suppressMessages(
        suppressWarnings(getMDLObjects(x = fpath)))
    
    ucMog <- as.mogObj(dat)

    # Create a list of distributions and variable names:
    set.seed(446242)
    res <- startingValues(object = ucMog, distList = list())
    
    expect_equal(object = names(res$STRUCTURAL), 
        expected = c("POP_CL", "POP_V", "POP_KA", "POP_TLAG", "BETA_CL_WT", "BETA_V_WT"))
    
    expect_equal(object = names(res$VARIABILITY), 
        expected = c("PPV_CL", "PPV_V", "PPV_KA", "PPV_TLAG", "CORR_CL_V", 
            "RUV_PROP", "RUV_ADD"))
    
    expect_equal(object = sum(unlist(res$STRUCTURAL)), 
        expected = 3.77756942599081)
    
    expect_equal(object = sum(unlist(res$VARIABILITY)), 
        expected = 2.50900792772882)
    
    distList1 <- ddmore:::.createListTemplate(ucMog@parObj)
    
    distList1$STRUCTURAL$POP_CL$dist <- "rnorm"
    
    distList1$STRUCTURAL$POP_CL$args <- list(mean = 100, sd = 2)
    
    set.seed(65252)
    
    res <- startingValues(object = ucMog, distList = distList1)
    
    expect_equal(object = names(res$STRUCTURAL), 
        expected = c("POP_CL", "POP_V", "POP_KA", "POP_TLAG", "BETA_CL_WT", "BETA_V_WT"))
    
    expect_equal(object = names(res$VARIABILITY), 
        expected = c("PPV_CL", "PPV_V", "PPV_KA", "PPV_TLAG", "CORR_CL_V", 
            "RUV_PROP", "RUV_ADD"))
    
    expect_equal(object = sum(unlist(res$STRUCTURAL)), 
        expected = 102.891235726589)
    
    expect_equal(object = sum(unlist(res$VARIABILITY)), 
        expected = 3.9977150533814)
})
