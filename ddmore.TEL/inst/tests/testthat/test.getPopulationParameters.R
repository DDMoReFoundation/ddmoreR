library("DDMoRe")
library("XML")
require("methods")

context("Test getPopulationParameters from Handcoded PharmMLSO Version 0.2")

test_that("Test getPopulationParameters returns a list of dataframes, one per estimate type.", {
  
  data.path = system.file("tests//data//PharmMLSO/HandCoded//warfarin_PK_ODE_SO_FULL-v0_2.xml",  
                          package = "DDMoRe")
  
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
                          package = "DDMoRe")
  
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
                          package = "DDMoRe")
  
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
                          package = "DDMoRe")
  
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
                          package = "DDMoRe")
  
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


context("Test getPopulationParameters from Machine Generated PharmMLSO Version 0.2")


test_that("Test getPopulationParameters returns correct statistics using bootstrap_UPDRS1.SO.xml sample data with what='all'.", {
  
  data.path = system.file("tests//data//PharmMLSO/MachineGenerated//bootstrap_UPDRS1.SO.xml",  
                          package = "DDMoRe")
  
  # Load in SO
  SOObject = LoadSOObject(data.path)
  
  # By specifying just estimates, and a keep.only for Bayesian and Bootstrap, only a named list is returned
  output = getPopulationParameters(SOObject, what="all", keep.only="mean")
  
  PARAMETERS = c("ETA_PPV_BASE_ETA_PPV_KE0", "POP_C50", "POP_E0INT", "POP_E0SLP", 
                 "POP_EMAX", "POP_GAM", "POP_KE0", "PPV_BASE", "PPV_C50", "PPV_GAM", 
                 "PPV_KE0", "RUV_ADD", "SIGMA")

  # MLE
  # ---------------

  MLE = output[["MLE"]]
  expect_true(all(colnames(MLE) == c("Parameter", "MLE", "SE", "RSE")),
           info = "MLE should have correct column names.")

  expect_true(all(MLE[["Parameter"]] == PARAMETERS),
           info = "MLE should contain statistics of correct parameter names.")

  # Check all number columns are of type numeric 
  coltypes = sapply(MLE[, setdiff(colnames(MLE), "Parameter")], class)
  expect_true(all(coltypes == "numeric"), info = "Column types for values should be numeric.")

  # Check the values 
  expectedValues = structure(list(MLE = c(0.00772375, 966.974, 57.5868, 0.0726516, 
    0.510841, 3.27451, 2.10992, 0.00867582, 0.0798188, 0.118566, 
    0.0261995, 5.11201, 1), SE = c(0.00633907, 61.6584, 1.29848, 
    0.00434249, 0.0231504, 0.586038, 0.223964, 0.002042, 0.0411279, 
    0.0917208, 0.0724179, 0.266654, NA), RSE = c(82.0724389059718, 
    6.37642790809267, 2.25482228566269, 5.97714296725743, 4.5318210558667, 
    17.8969677906007, 10.6148100401911, 23.5366801063185, 51.5265827098378, 
    77.3584332776681, 276.409473463234, 5.21622610284409, NA)), .Names = c("MLE", 
    "SE", "RSE"), row.names = c(NA, -13L), class = "data.frame")
  expect_true(all.equal(expectedValues, MLE[2:4]), info = "Values in dataframe should be as expected.")

  # Bootstrap
  # --------------

  Bootstrap = output[["Bootstrap"]]
  expect_true(all(colnames(Bootstrap) == c("Parameter", "Mean", "Perc_2.5", "Perc_5", "Perc_95", "Perc_97.5")),
           info = "Bootstrap should have correct column names.")

  expect_true(all(Bootstrap[["Parameter"]] == PARAMETERS),
           info = "Bootstrap should contain statistics of correct parameter names.")

  # Check all number columns are of type numeric 
  coltypes = sapply(Bootstrap[, setdiff(colnames(Bootstrap), "Parameter")], class)
  expect_true(all(coltypes == "numeric"), info = "Column types for values should be numeric.")

  # Check the values 
  expectedValues = structure(list(Mean = c(0.006758791, 958.9215, 57.62008, 0.07155076, 
    0.5069021, 3.402315, 2.115561, 0.008008466, 0.07631372, 0.1176205, 
    0.03163205, 5.046938, 1), Perc_2.5 = c(-0.0054543865, 832.9295, 
    54.46873, 0.05912859, 0.4619634, 2.361053, 1.707477, 0.003173246, 
    5e-06, 0.00017, 3.91641825e-05, 4.509949, 1), Perc_5 = c(-0.00253213875, 
    846.5632, 54.86693, 0.06304201, 0.4674843, 2.457718, 1.766692, 
    0.003553421, 5e-06, 0.00017, 6.3191005e-05, 4.673608, 1)), .Names = c("Mean", 
    "Perc_2.5", "Perc_5"), row.names = c(NA, -13L), class = "data.frame")
  expect_true(all.equal(expectedValues, Bootstrap[2:4]), info = "Values in dataframe should be as expected.")

 })

test_that("Test getPopulationParameters returns correct statistics using bootstrap_UPDRS1.SO.xml sample data with what='precision'.", {
  
  data.path = system.file("tests//data//PharmMLSO/MachineGenerated//bootstrap_UPDRS1.SO.xml",  
                          package = "DDMoRe")
  
  # Load in SO
  SOObject = LoadSOObject(data.path)
  
  # By specifying just estimates, and a keep.only for Bayesian and Bootstrap, only a named list is returned
  output = getPopulationParameters(SOObject, what="precision", keep.only="mean")
  
  PARAMETERS = c("ETA_PPV_BASE_ETA_PPV_KE0", "POP_C50", "POP_E0INT", "POP_E0SLP", 
                 "POP_EMAX", "POP_GAM", "POP_KE0", "PPV_BASE", "PPV_C50", "PPV_GAM", 
                 "PPV_KE0", "RUV_ADD", "SIGMA")

  # MLE
  # ---------------
  MLE = output[["MLE"]]
  expect_true(all(colnames(MLE) == c("Parameter", "MLE", "SE", "RSE")),
           info = "MLE should have correct column names.")

  expect_true(all(MLE[["Parameter"]] == PARAMETERS),
           info = "MLE should contain statistics of correct parameter names.")

  # Check all number columns are of type numeric 
  coltypes = sapply(MLE[, setdiff(colnames(MLE), "Parameter")], class)
  expect_true(all(coltypes == "numeric"), info = "Column types for values should be numeric.")

  # Check the values 
  expectedValues = structure(list(MLE = c(0.00772375, 966.974, 57.5868, 0.0726516, 
    0.510841, 3.27451, 2.10992, 0.00867582, 0.0798188, 0.118566, 
    0.0261995, 5.11201, 1), SE = c(0.00633907, 61.6584, 1.29848, 
    0.00434249, 0.0231504, 0.586038, 0.223964, 0.002042, 0.0411279, 
    0.0917208, 0.0724179, 0.266654, NA), RSE = c(82.0724389059718, 
    6.37642790809267, 2.25482228566269, 5.97714296725743, 4.5318210558667, 
    17.8969677906007, 10.6148100401911, 23.5366801063185, 51.5265827098378, 
    77.3584332776681, 276.409473463234, 5.21622610284409, NA)), .Names = c("MLE", 
    "SE", "RSE"), row.names = c(NA, -13L), class = "data.frame")
  expect_true(all.equal(expectedValues, MLE[2:4]), info = "Values in dataframe should be as expected.")

  # Bootstrap
  # --------------

  Bootstrap = output[["Bootstrap"]]
  expect_true(all(colnames(Bootstrap) == c("Parameter", "Mean")),
           info = "Bootstrap should have correct column names.")

  expect_true(all(Bootstrap[["Parameter"]] == PARAMETERS),
           info = "Bootstrap should contain statistics of correct parameter names.")

  # Check all number columns are of type numeric 
  coltypes = sapply(Bootstrap[, setdiff(colnames(Bootstrap), "Parameter")], class)
  expect_true(all(coltypes == "numeric"), info = "Column types for values should be numeric.")

  # Check the values 
  expectedValues = structure(list(Mean = c(0.006758791, 958.9215, 57.62008, 0.07155076, 
    0.5069021, 3.402315, 2.115561, 0.008008466, 0.07631372, 0.1176205, 
    0.03163205, 5.046938, 1)), .Names = c("Mean"), row.names = c(NA, -13L), class = "data.frame")
  expect_true(all.equal(expectedValues, Bootstrap[2]), info = "Values in dataframe should be as expected.")

 })

test_that("Test getPopulationParameters returns correct statistics using bootstrap_UPDRS1.SO.xml sample data with what='intervals'.", {
  
  data.path = system.file("tests//data//PharmMLSO/MachineGenerated//bootstrap_UPDRS1.SO.xml",  
                          package = "DDMoRe")
  
  # Load in SO
  SOObject = LoadSOObject(data.path)
  
  # By specifying just estimates, and a keep.only for Bayesian and Bootstrap, only a named list is returned
  output = getPopulationParameters(SOObject, what="intervals", keep.only="mean")
  
  PARAMETERS = c("ETA_PPV_BASE_ETA_PPV_KE0", "POP_C50", "POP_E0INT", "POP_E0SLP", 
                 "POP_EMAX", "POP_GAM", "POP_KE0", "PPV_BASE", "PPV_C50", "PPV_GAM", 
                 "PPV_KE0", "RUV_ADD", "SIGMA")

  # MLE
  # ---------------
  MLE <- output[["MLE"]]
  expect_true(all(colnames(MLE) == c("Parameter", "MLE")),
           info = "MLE should have correct column names.")

  expect_true(all(MLE[["Parameter"]] == PARAMETERS),
           info = "MLE should contain statistics of correct parameter names.")

  # Check all number columns are of type numeric 
  coltypes <- sapply(MLE[, setdiff(colnames(MLE), "Parameter")], class)
  expect_true(all(coltypes == "numeric"), info = "Column types for values should be numeric.")

  # Check the values 
  expectedValues <- structure(list(MLE = c(0.00772375, 966.974, 57.5868, 0.0726516, 
    0.510841, 3.27451, 2.10992, 0.00867582, 0.0798188, 0.118566, 
    0.0261995, 5.11201, 1)), .Names = c("MLE"), row.names = c(NA, -13L), class = "data.frame")
  expect_true(all.equal(expectedValues, MLE[2]), info = "Values in dataframe should be as expected.")

  # Bootstrap
  # --------------
  Bootstrap <- output[["Bootstrap"]]
  expect_true(all(colnames(Bootstrap) == c("Parameter", "Mean", "Perc_2.5", "Perc_5", "Perc_95", "Perc_97.5")),
           info = "Bootstrap should have correct column names.")

  expect_true(all(Bootstrap[["Parameter"]] == PARAMETERS),
           info = "Bootstrap should contain statistics of correct parameter names.")

  # Check all number columns are of type numeric 
  coltypes <- sapply(Bootstrap[, setdiff(colnames(Bootstrap), "Parameter")], class)
  expect_true(all(coltypes == "numeric"), info = "Column types for values should be numeric.")

  # Check the values 
  expectedValues = structure(list(Mean = c(0.006758791, 958.9215, 57.62008, 0.07155076, 
    0.5069021, 3.402315, 2.115561, 0.008008466, 0.07631372, 0.1176205, 
    0.03163205, 5.046938, 1), Perc_2.5 = c(-0.0054543865, 832.9295, 
    54.46873, 0.05912859, 0.4619634, 2.361053, 1.707477, 0.003173246, 
    5e-06, 0.00017, 3.91641825e-05, 4.509949, 1), Perc_5 = c(-0.00253213875, 
    846.5632, 54.86693, 0.06304201, 0.4674843, 2.457718, 1.766692, 
    0.003553421, 5e-06, 0.00017, 6.3191005e-05, 4.673608, 1)), .Names = c("Mean", 
    "Perc_2.5", "Perc_5"), row.names = c(NA, -13L), class = "data.frame")
  expect_true(all.equal(expectedValues, Bootstrap[2:4]), info = "Values in dataframe should be as expected.")

 })

test_that("Test getPopulationParameters returns correct statistics using UPDRS1_CNS_ZINNIA.SO.xml sample data.", {
  
  data.path = system.file("tests//data//PharmMLSO/MachineGenerated//UPDRS1_CNS_ZINNIA.SO.xml", 
                          package = "DDMoRe")
  SOObject = LoadSOObject(data.path)
  
  # By specifying just estimates, and a keep.only for Bayesian and Bootstrap, only a named list is returned
  output = getPopulationParameters(SOObject, what="all", keep.only="mean")
  
  PARAMETERS = c("BETA_BASE", "CL_pop", "CLD_pop", "KTR_pop", "omega2_BASE", 
                  "omega2_C50", "omega2_CL", "omega2_CLD", "omega2_GAM", "omega2_KE0", 
                  "omega2_KTR", "omega2_POP_EMAX", "omega2_V1", "omega2_VT", "POP_BASE", 
                  "POP_C50", "POP_EMAX_pop", "POP_GAM", "POP_KE0", "RUV_ADD", "V1_pop", 
                  "VT_pop")

  # MLE
  # ---------------

  MLE = output[["MLE"]]
  expect_true(all(colnames(MLE) == c("Parameter", "MLE", "SE", "RSE")),
           info = "MLE should have correct column names.")

  expect_true(all(MLE[["Parameter"]] == PARAMETERS),
           info = "MLE should contain statistics of correct parameter names.")

  # Check all number columns are of type numeric 
  coltypes = sapply(MLE[, setdiff(colnames(MLE), "Parameter")], class)
  expect_true(all(coltypes == "numeric"), info = "Column types for values should be numeric.")

  # Check the values 
  expectedValues = structure(list(MLE = c(0.84735, 26, 6.8, 2.5, 0.0123, 0.12939, 
        0, 0, 0.20517, 3.42524, 0, 0, 0, 0, 23.65906, 983.96461, 0.54321, 
        2.92832, 2.57262, 5.58676, 53.5, 72.9), SE = c(0.07051, 0, 0, 
        0, 0.00492, 0.05482, 0, 0, 0.11878, 1.20232, 0, 0, 0, 0, 1.99079, 
        101.04072, 0.02841, 0.506, 1.13917, 0.29558, 0, 0), RSE = c(8.32, 
        0, 0, 0, 40.03, 42.37, 0, 0, 57.89, 35.1, 0, 0, 0, 0, 8.41, 10.27, 
        5.23, 17.28, 44.28, 5.29, 0, 0)), .Names = c("MLE", "SE", "RSE"
        ), row.names = c(NA, -22L), class = "data.frame")
  expect_true(all.equal(expectedValues, MLE[2:4]), info = "Values in dataframe should be as expected.")

 })

