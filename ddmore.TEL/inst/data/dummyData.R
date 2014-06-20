#source("createClasses.R")

### Dummy dataObj:

dataObj <- new("dataObj",
    DATA_INPUT_VARIABLES= list(
    ID = list(type="categorical"), 
    TIME=list(type="continuous", units="h")
  ),
  SOURCE = list(
    myData = list(
    dataFile1 = list(file="tumour_exposure.csv",
      inputformat="nonmemFormat"),
    dataFile2 = list(file="ex_data_prolactin.csv",
      inputformat="nonmemFormat")  
    ),
    INLINE = "#ID TIM WT AGE SEX AMT DVID DVMDV
    1 0 6 6.7 50 1 100 0 1"),
  RSCRIPT = list(),
  HEADER = list(),
  FILE = list(),
  DESIGN = list(
    SizeArm=list(20,20,40,40),
    AGE=list(source="covdata"),
    SEX=list(source="covdata", units=""),
    WT=list(source="covdata")
  ),
  DATA_DERIVED_VARIABLES = "AGE=PNAY
    if (AGE>20) {
       WT= list(value=70,type=continuous)
       DOSE=MGKG # inherits attributes of DOSE
    } else {
       WT=WTKG # inherits attributes of WTKG
       DOSE=list(value=MGKG*WT,type=continuous)
    }"

)

dataObj2 <- new("dataObj",
    DATA_INPUT_VARIABLES= list(
    NID = list(type="categorical"), 
    TIME=list(type="continuous", units="h")
  ),
  SOURCE = list(
    myData = list(
    dataFile2 = list(file="ex_data_prolactin.csv",
      inputformat="nonmemFormat")  
    ),
    INLINE = "#ID TIM WT AGE SEX AMT DVID DVMDV
    1 0 6 6.7 50 1 100 0 1"),
  RSCRIPT = list(),
  HEADER = list(),
  FILE = list(),
  DESIGN = list(
    SizeArm=list(20,20,40,40),
    AGE=list(source="covdata"),
    SEX=list(source="covdata", units=""),
    WT=list(source="covdata")
  ),
  DATA_DERIVED_VARIABLES = "if(AMT<1){DV=999}"

)


### Dummy parObj:

parObj <- new("parObj", 
  STRUCTURAL = list(
      POP_SIZE0=list(value=6.66,lo=1,hi=10),
      POP_TOVER=list(value=18.9,lo=1,hi=200)
  ),
  PRIOR = list(),
  VARIABILITY = list(
    PPV_SIZE0=list(value=0.317,
        type="VAR"),
    PPV_TOVER=list(value=0.082,
        type="VAR")
  )
)

### Dummy modPred
modPred <- new("modPred", 
  ODE = c("DAE=DRUG"," DSIZE=TUMOUR"),
  LIBRARY = c("amount=nmadvan(model=13,output=list(A,F))")
)

### Dummy modObj

modObj <- new("modObj", 
    MODEL_INPUT_VARIABLES = list(
      ID=list(use="id",level=2),
      TIME=list(use="idv",units="h")
    ),
    STRUCTURAL_PARAMETERS = c("POP_SIZE0", "POP_TOVER",  "POP_AE50", "POP_TEQ"),
    VARIABILITY_PARAMETERS = c("POP_SIZE0", "POP_TOVER",  "POP_AE50",  "RUV_CV", "RUV_SD" ),
    GROUP_VARIABLES = list(
      c( GSIZE0 = "POP_SIZE0"),
      MIXTURE =list(
        mixtures=list("MIX1", "MIX2"),
        MIX1="POP_PROB_CYP_SLOW",
        MIX2="1-POP_PROB_CYP_SLOW"
      )
    ),
    RANDOM_VARIABLE_DEFINITION = c(
      "eta_PPV_SIZE0 ~ (type=normal, mean=0, var=PPV_SIZE0,level=ID)",
      "eta_PPV_TOVER ~ (type=normal, mean=0, var=PPV_TOVER,level=ID)"
    ),
    INDIVIDUAL_VARIABLES = c(
      SIZE0="GSIZE0*exp(eta_PPV_SIZE0)",
      GTOVER="POP_TOVER*168 # week -> h"
    ),
    MODEL_PREDICTION = modPred,
    OBSERVATION = list(
      c("Y = SIZE+SIZE*eps_RUV_CV+eps_RUV_SD"), 
      ESTIMATION = "if(DVID<=1) { estimate }",
      SIMULATION = "if(DVID<=1) { simulate }"
    )
)

# Dummy mclObj:

mogObj <- new("mogObj", 
  dataObj = dataObj,
  parObj = parObj,
  modObj = modObj
)



# String code example:
#test <- "parObj <- new('parObj', STRUCTURAL = list( POP_SIZE0=list(value=6.66,lo=1,hi=10),POP_TOVER=list(value=18.9,lo=1,hi=200)),PRIOR = list(),VARIABILITY = list(PPV_SIZE0=list(value=0.317,type=VAR),PPV_TOVER=list(value=0.082,type=VAR)))"
#eval(parse(text=test))





