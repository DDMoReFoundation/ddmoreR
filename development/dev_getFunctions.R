
# Clear workspace. 
rm(list=ls())

# Setup imports and directory
if ("DDMoRe.TEL" %in% .packages()) {
  detach("package:DDMoRe.TEL", unload=TRUE)
} 
install.packages("C:/Users/cmusselle/Projects/DDmore/TEL-R/.__artefacts/DDMoRe.TEL_0.0.3.tar.gz", repos = NULL, type = "source")
require("DDMoRe.TEL")

#source("tel-dev-mine.R")
# data.path = system.file("tests/data/PharmMLSO/MachineGenerated/pheno.SO.xml",  
#                         package = "DDMoRe.TEL")

data.path = system.file("tests/data/PharmMLSO/HandCoded/warfarin_PK_ODE_SO_FULL-v0_2.xml",  
                        package = "DDMoRe.TEL")

# data.path = system.file("tests/data/PharmMLSO/MachineGenerated/bootstrap.SO.xml",  
#                         package = "DDMoRe.TEL")
# 
# data.path = system.file("tests/data/PharmMLSO/MachineGenerated/run1.SO.xml",  
#                         package = "DDMoRe.TEL")


data.path = "C://Users//cmusselle/Projects/DDmore/TEL-R/ddmore.TEL//inst//tests//data//PharmMLSO/HandCoded//warfarin_PK_ODE_SO_FULL-v0_2.xml"


data.path

# Load in SO
SOObject <- LoadSOObject(data.path)

output = getPopulationParameters(SOObject)

# MLE.output = getMLEPopulationEstimates(SOObject)
# 
# Bayesian.output = getBayesianPopulationEstimates(SOObject)
# 
# Bootstrap.output = getBootstrapPopulationEstimates(SOObject)


out_structural = getPopulationParameters(SOObject, type="structural")
out_variability = getPopulationParameters(SOObject, type="variability")
out_estimates = getPopulationParameters(SOObject, what="estimates", keep.only="mean")
out_precision = getPopulationParameters(SOObject, what="precision")
out_intervals = getPopulationParameters(SOObject, what="intervals", keep.only="mean")


out_structural_estimates = getPopulationParameters(SOObject, type="structural", what="estimates")

# # Testing Low Level Getter Functions 
# tools = DDMoRe.TEL:::getToolSettings(SOObject)
# tools
# raw_results = DDMoRe.TEL:::getRawResults(SOObject)
# raw_results
# pop_est = DDMoRe.TEL:::getPopulationEstimates(SOObject)
# pop_est
# prec_pop_est = DDMoRe.TEL:::getPrecisionPopulationEstimates(SOObject)
# prec_pop_est
# ind_est = DDMoRe.TEL:::getIndividualEstimates(SOObject)
# ind_est
# prec_ind_est = DDMoRe.TEL:::getPrecisionIndividualEstimates(SOObject)
# prec_ind_est
# residuals= DDMoRe.TEL:::getResiduals(SOObject)
# residuals
# predictions = DDMoRe.TEL:::getPredictions(SOObject)
# predictions
# likelihood = DDMoRe.TEL:::getLikelihood(SOObject)
# likelihood
# 
# msgs = DDMoRe.TEL:::getSoftwareMessages(SOObject)
# msgs
# 
# # Test Higher Level getter functions 
# param = getParameterEstimates(SOObject)
# est_info = getEstimationInfo(SOObject)



