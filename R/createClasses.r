#######
## Creates R classes for TEL package.dependencies
## Author: Kate Hanley
#######


### Create dataObj class:

setClass("dataObj", 
  slots=c(
    DATA_INPUT_VARIABLES="list",
    SOURCE = "list",
    RSCRIPT = "list",
    HEADER = "list",
    FILE = "list",
    DESIGN = "list",
    DATA_DERIVED_VARIABLES = "vector"
    )    
)

### Create paObj class:

setClass("parObj", 
  slots= c(
  STRUCTURAL = "list",
  PRIOR = "list",
  VARIABILITY = "list"
  )
)

# Create modPred class:
setClass("modPred", 
  slots= c(
  ODE = "vector",
  LIBRARY = "vector"
  )
)


### Create mdlObj class:

setClass("mdlObj", 
  slots= c(
    MODEL_INPUT_VARIABLES = "list",
    STRUCTURAL_PARAMETERS = "vector",
    VARIABILITY_PARAMETERS = "vector",
    GROUP_VARIABLES = "vector",
    RANDOM_VARIABLE_DEFINITION ="vector",
    INDIVIDUAL_VARIABLES = "vector",
    MODEL_PREDICTION = "modPred",
    OBSERVATION = "list"
  )
)

### Create mclObj class:

setClass("mclObj", 
  slots= c(
  dataObj = "dataObj",
  parObj = "parObj",
  mdlObj = "mdlObj"
  )
)



