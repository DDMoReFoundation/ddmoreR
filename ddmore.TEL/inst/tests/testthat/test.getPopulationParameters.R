library("DDMoRe.TEL")
library("XML")
require("methods")

context("Test getPopulationParameters from Handcoded PharmMLSO Version 0.2")

test_that("Test getPopulationParameters returns a list of dataframes, one per estimate type.", {
  
  data.path = system.file("tests//data//PharmMLSO/HandCoded//warfarin_PK_ODE_SO_FULL-v0_2.xml",  
                          package = "DDMoRe.TEL")
  
  # Load in SO
  SOObject = LoadSOObject(data.path)
  
  # By default will return estimates,measures for precision and confidence intervals; for each of: MLE Baysian and Bootstrap
  output = getPopulationParameters(SOObject)
    
  expect_true(class(output) == "list", info = "Returned object should be a list")

  expect_true(length(output) == 3, info = "Returned list should be of length 3")

  expect_true(all(names(output) == c("MLE", "Bayesian", "Bootstrap")), info = "Names of elements should match SO slots")
  
  for (df in output) {
  	expect_true(class(df) == "data.frame", info = "Elements should be data frames")
  	expect_true(length(df) > 0, info = "Elements should not be empty")
  }
   
 })


test_that("Test getPopulationParameters returns correct statistics by default for MLE, Bayesian and Bootstrap estimates.", {
  
  data.path = system.file("tests//data//PharmMLSO/HandCoded//warfarin_PK_ODE_SO_FULL-v0_2.xml",  
                          package = "DDMoRe.TEL")
  
  # Load in SO
  SOObject = LoadSOObject(data.path)
  
  # By default will return estimates, measures for precision and confidence intervals; for each of: MLE Baysian and Bootstrap
  output = getPopulationParameters(SOObject)
  
  PARAMETERS = c("BETA_CL_WT", "BETA_V_WT", "CORR_PPV_CL_V", "POP_CL", "POP_KA", "POP_TLAG", 
                  "POP_V", "PPV_CL", "PPV_KA", "PPV_TLAG", "PPV_V", "RUV_ADD", "RUV_PROP")

  # MLE
  # --------------

  MLE = output[["MLE"]]
  expect_true(all(colnames(MLE) == c("Parameter", "MLE", "SE", "RSE", "CI", "LowerBound", "UpperBound")),
  				 info = "MLE should have correct column names.")

  expect_true(all(MLE[["Parameter"]] == PARAMETERS),
  			   info = "MLE should contain statistics of correct parameter names.")

  # Check all number columns are of type numeric 
  coltypes = sapply(MLE[, setdiff(colnames(MLE), "Parameter")], class)
  expect_true(all(coltypes == "numeric"), info = "Column types for values should be numeric.")

  # Bayesian
  # ---------------

  Bayesian = output[["Bayesian"]]
  expect_true(all(colnames(Bayesian) == c("Parameter", "Mean", "Median", "Mode", "SDP", 
                    "Perc_0.05", "Perc_0.25", "Perc_0.75", "Perc_0.95")),
           info = "Bayesian should have correct column names.")

  expect_true(all(Bayesian[["Parameter"]] == PARAMETERS),
           info = "Bayesian should contain statistics of correct parameter names.")

  # Check all number columns are of type numeric 
  coltypes = sapply(Bayesian[, setdiff(colnames(Bayesian), "Parameter")], class)
  expect_true(all(coltypes == "numeric"), info = "Column types for values should be numeric.")

  # Bootstrap
  # --------------

  Bootstrap = output[["Bootstrap"]]
  expect_true(all(colnames(Bootstrap) == c("Parameter", "Mean", "Median", "StandardDeviation", 
                                      "StandardError", "LowerCI", "UpperCI", "Alpha", 
                                      "Perc_0.05", "Perc_0.25", "Perc_0.75", "Perc_0.95")),
           info = "Bootstrap should have correct column names.")

  expect_true(all(Bootstrap[["Parameter"]] == PARAMETERS),
           info = "Bootstrap should contain statistics of correct parameter names.")

  # Check all number columns are of type numeric 
  coltypes = sapply(Bootstrap[, setdiff(colnames(Bootstrap), "Parameter")], class)
  expect_true(all(coltypes == "numeric"), info = "Column types for values should be numeric.")
  
 })


test_that("Test getPopulationParameters returns correct statistics using 'estimates' option for MLE, Bayesian and Bootstrap estimates.", {
  
  data.path = system.file("tests//data//PharmMLSO/HandCoded//warfarin_PK_ODE_SO_FULL-v0_2.xml",  
                          package = "DDMoRe.TEL")
  
  # Load in SO
  SOObject = LoadSOObject(data.path)
  
  # By specifying just estimates, and a keep.only for Bayesian and Bootstrap, only a named list is returned
  output = getPopulationParameters(SOObject, what="estimates", keep.only="mean")
  
  PARAMETERS = c("BETA_CL_WT", "BETA_V_WT", "CORR_PPV_CL_V", "POP_CL", "POP_KA", "POP_TLAG", 
                  "POP_V", "PPV_CL", "PPV_KA", "PPV_TLAG", "PPV_V", "RUV_ADD", "RUV_PROP")

  # MLE
  # ----------
  MLE = output[["MLE"]]
  expect_true(all(sort(names(MLE)) == sort(PARAMETERS)),
           info = "MLE estimates vector should contain correct parameter names.")
  # Check all number columns are of type numeric 
  coltypes = sapply(MLE, class)
  expect_true(all(coltypes == "numeric"), info = "Column types for values should be numeric.")

  # Bayesian
  # ----------
  Bayesian = output[["Bayesian"]]
  expect_true(all(sort(names(Bayesian)) == sort(PARAMETERS)),
           info = "Bayesian estimates vector should contain correct parameter names.")
  # Check all number columns are of type numeric 
  coltypes = sapply(Bayesian, class)
  expect_true(all(coltypes == "numeric"), info = "Column types for values should be numeric.")

  # Bootstrap
  # ----------
  Bootstrap = output[["Bootstrap"]]
  expect_true(all(sort(names(Bootstrap)) == sort(PARAMETERS)),
           info = "Bootstrap estimates vector should contain correct parameter names.")

  # Check all number columns are of type numeric 
  coltypes = sapply(Bootstrap, class)
  expect_true(all(coltypes == "numeric"), info = "Column types for values should be numeric.")
  
 })


test_that("Test getPopulationParameters returns correct statistics using 'precision' option for MLE, Bayesian and Bootstrap estimates.", {
  
  data.path = system.file("tests//data//PharmMLSO/HandCoded//warfarin_PK_ODE_SO_FULL-v0_2.xml",  
                          package = "DDMoRe.TEL")
  
  # Load in SO
  SOObject = LoadSOObject(data.path)
  
  # By default will return estimates, measures for precision and confidence intervals; for each of: MLE Bayesian and Bootstrap
  output = getPopulationParameters(SOObject, what="precision", keep.only="mean")

  PARAMETERS = c("BETA_CL_WT", "BETA_V_WT", "CORR_PPV_CL_V", "POP_CL", "POP_KA", "POP_TLAG", 
                  "POP_V", "PPV_CL", "PPV_KA", "PPV_TLAG", "PPV_V", "RUV_ADD", "RUV_PROP")
    
  # MLE
  # --------------
  MLE = output[["MLE"]]
  expect_true(all(colnames(MLE) == c("Parameter", "MLE", "SE", "RSE")),
           info = "MLE should have correct column names.")

  expect_true(all(MLE[["Parameter"]] == PARAMETERS),
           info = "MLE should contain statistics of correct parameter names.")
  # Check all number columns are of type numeric 
  coltypes = sapply(MLE[, setdiff(colnames(MLE), "Parameter")], class)
  expect_true(all(coltypes == "numeric"), info = "Column types for values should be numeric.")

  # Bayesian
  # ---------------
  Bayesian = output[["Bayesian"]]
  expect_true(all(colnames(Bayesian) == c("Parameter", "Mean", "SDP")),
           info = "Bayesian should have correct column names.")

  expect_true(all(Bayesian[["Parameter"]] == PARAMETERS),
           info = "Bayesian should contain statistics of correct parameter names.")
  # Check all number columns are of type numeric 
  coltypes = sapply(Bayesian[, setdiff(colnames(Bayesian), "Parameter")], class)
  expect_true(all(coltypes == "numeric"), info = "Column types for values should be numeric.")

  # Bootstrap
  # --------------
  Bootstrap = output[["Bootstrap"]]
  expect_true(all(colnames(Bootstrap) == c("Parameter", "Mean", "StandardDeviation", 
                                      "StandardError", "LowerCI", "UpperCI", "Alpha")),
           info = "Bootstrap should have correct column names.")

  expect_true(all(Bootstrap[["Parameter"]] == PARAMETERS),
           info = "Bootstrap should contain statistics of correct parameter names.")
  # Check all number columns are of type numeric 
  coltypes = sapply(Bootstrap[, setdiff(colnames(Bootstrap), "Parameter")], class)
  expect_true(all(coltypes == "numeric"), info = "Column types for values should be numeric.")
  
 })

test_that("Test getPopulationParameters returns correct statistics using 'intervals' option for MLE, Bayesian and Bootstrap estimates.", {
  
  data.path = system.file("tests//data//PharmMLSO/HandCoded//warfarin_PK_ODE_SO_FULL-v0_2.xml",  
                          package = "DDMoRe.TEL")
  
  # Load in SO
  SOObject = LoadSOObject(data.path)
  
  # By default will return estimates, measures for precision and confidence intervals; for each of: MLE Baysian and Bootstrap
  output = getPopulationParameters(SOObject, what="intervals", keep.only="mean")
    
  # MLE
  # --------------

  PARAMETERS = c("BETA_CL_WT", "BETA_V_WT", "CORR_PPV_CL_V", "POP_CL", "POP_KA", "POP_TLAG", 
                  "POP_V", "PPV_CL", "PPV_KA", "PPV_TLAG", "PPV_V", "RUV_ADD", "RUV_PROP")

  MLE = output[["MLE"]]
  expect_true(all(colnames(MLE) == c("Parameter", "MLE", "CI", "LowerBound", "UpperBound")),
           info = "MLE should have correct column names.")

  expect_true(all(MLE[["Parameter"]] == PARAMETERS),
           info = "MLE should contain statistics of correct parameter names.")

  # Check all number columns are of type numeric 
  coltypes = sapply(MLE[, setdiff(colnames(MLE), "Parameter")], class)
  expect_true(all(coltypes == "numeric"), info = "Column types for values should be numeric.")

  # Bayesian
  # ---------------

  Bayesian = output[["Bayesian"]]
  expect_true(all(colnames(Bayesian) == c("Parameter", "Mean",
                  "Perc_0.05", "Perc_0.25", "Perc_0.75", "Perc_0.95")),
           info = "Bayesian should have correct column names.")

  expect_true(all(Bayesian[["Parameter"]] == PARAMETERS),
           info = "Bayesian should contain statistics of correct parameter names.")

  # Check all number columns are of type numeric 
  coltypes = sapply(Bayesian[, setdiff(colnames(Bayesian), "Parameter")], class)
  expect_true(all(coltypes == "numeric"), info = "Column types for values should be numeric.")

  # Bootstrap
  # --------------

  Bootstrap = output[["Bootstrap"]]
  expect_true(all(colnames(Bootstrap) == c("Parameter", "Mean", 
                                      "Perc_0.05", "Perc_0.25", "Perc_0.75", "Perc_0.95")),
           info = "Bootstrap should have correct column names.")

  expect_true(all(Bootstrap[["Parameter"]] == PARAMETERS),
           info = "Bootstrap should contain statistics of correct parameter names.")

  # Check all number columns are of type numeric 
  coltypes = sapply(Bootstrap[, setdiff(colnames(Bootstrap), "Parameter")], class)
  expect_true(all(coltypes == "numeric"), info = "Column types for values should be numeric.")
  
 })

