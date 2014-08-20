

myMog <-new("mogObj"
    , dataObj = new("dataObj"
    , DATA_INPUT_VARIABLES = structure(list(ID = structure(list(type = "categorical"), .Names = "type"), 
    TIME = structure(list(type = "continuous", units = "\"h\""), .Names = c("type", 
    "units")), AMT = structure(list(type = "continuous", units = "\"mg\""), .Names = c("type", 
    "units")), DV = structure(list(type = "continuous"), .Names = "type")), .Names = c("ID", 
"TIME", "AMT", "DV"))
    , SOURCE = structure(list(file = "tumour_exposure.csv", symbolName = "myData", 
    identifier = "SOURCE", inputformat = "nonmemFormat"), .Names = c("file", 
"symbolName", "identifier", "inputformat"))
    , RSCRIPT = list()
    , HEADER = list()
    , FILE = list()
    , DESIGN = list()
    , DATA_DERIVED_VARIABLES = list()
)
    , parObj = new("parObj"
    , STRUCTURAL = structure(list(POP_SIZE0 = structure(list(value = "6.66", lo = "1", 
    hi = "10"), .Names = c("value", "lo", "hi")), POP_TOVER = structure(list(
    value = "18.9", lo = "1", hi = "200"), .Names = c("value", 
"lo", "hi")), POP_AE50 = structure(list(value = "11700.", lo = "1", 
    hi = "100000"), .Names = c("value", "lo", "hi")), POP_TEQ = structure(list(
    value = "8.27", lo = "1", hi = "100000"), .Names = c("value", 
"lo", "hi"))), .Names = c("POP_SIZE0", "POP_TOVER", "POP_AE50", 
"POP_TEQ"))
    , PRIOR = list()
    , VARIABILITY = list(structure(list(PPV_SIZE0 = structure(list(value = "0.317", 
    type = "VAR"), .Names = c("value", "type"))), .Names = "PPV_SIZE0"), 
    structure(list(PPV_TOVER = structure(list(value = "0.082", 
        type = "VAR"), .Names = c("value", "type"))), .Names = "PPV_TOVER"), 
    structure(list(PPV_AE50 = structure(list(value = "1.9", type = "VAR"), .Names = c("value", 
    "type"))), .Names = "PPV_AE50"), structure(list(RUV_CV = structure(list(
        value = "0.0358", type = "VAR"), .Names = c("value", 
    "type"))), .Names = "RUV_CV"), structure(list(RUV_SD = structure(list(
        value = "0.0125", type = "VAR"), .Names = c("value", 
    "type"))), .Names = "RUV_SD"))
)
    , mdlObj = new("mdlObj"
    , MODEL_INPUT_VARIABLES = structure(list(ID = structure(list(use = "id", level = "2"), .Names = c("use", 
"level")), TIME = structure(list(use = "idv", units = "\"h\""), .Names = c("use", 
"units")), AMT = structure(list(use = "amt", units = "\"mg\""), .Names = c("use", 
"units")), DV = structure(list(type = "continuous", use = "dv", 
    level = "1"), .Names = c("type", "use", "level"))), .Names = c("ID", 
"TIME", "AMT", "DV"))
    , STRUCTURAL_PARAMETERS = structure(list(POP_SIZE0 = structure(list(), .Names = character(0)), 
    POP_TOVER = structure(list(), .Names = character(0)), POP_AE50 = structure(list(), .Names = character(0)), 
    POP_TEQ = structure(list(), .Names = character(0))), .Names = c("POP_SIZE0", 
"POP_TOVER", "POP_AE50", "POP_TEQ"))
    , VARIABILITY_PARAMETERS = structure(list(PPV_SIZE0 = structure(list(), .Names = character(0)), 
    PPV_TOVER = structure(list(), .Names = character(0)), PPV_AE50 = structure(list(), .Names = character(0)), 
    RUV_CV = structure(list(), .Names = character(0)), RUV_SD = structure(list(), .Names = character(0))), .Names = c("PPV_SIZE0", 
"PPV_TOVER", "PPV_AE50", "RUV_CV", "RUV_SD"))
    , GROUP_VARIABLES = "GSIZE0 = POP_SIZE0\r\n"
    , RANDOM_VARIABLE_DEFINITION = structure(list(eta_PPV_SIZE0 = structure(list(type = "normal", 
    mean = "0", var = "PPV_SIZE0", level = "ID"), .Names = c("type", 
"mean", "var", "level")), eta_PPV_TOVER = structure(list(type = "normal", 
    mean = "0", var = "PPV_TOVER", level = "ID"), .Names = c("type", 
"mean", "var", "level")), eta_PPV_AE50 = structure(list(type = "normal", 
    mean = "0", var = "PPV_AE50", level = "ID"), .Names = c("type", 
"mean", "var", "level")), eps_RUV_CV = structure(list(type = "normal", 
    mean = "0", var = "RUV_CV", level = "DV"), .Names = c("type", 
"mean", "var", "level")), eps_RUV_SD = structure(list(type = "normal", 
    mean = "0", var = "RUV_SD", level = "DV"), .Names = c("type", 
"mean", "var", "level"))), .Names = c("eta_PPV_SIZE0", "eta_PPV_TOVER", 
"eta_PPV_AE50", "eps_RUV_CV", "eps_RUV_SD"))
    , INDIVIDUAL_VARIABLES = "SIZE0 = GSIZE0*exp(eta_PPV_SIZE0)\r\n        GTOVER = POP_TOVER*168\r\n        TOVER = GTOVER*exp(eta_PPV_TOVER)\r\n        GAE50 = POP_AE50/263*1000\r\n        AE50 = GAE50*exp(eta_PPV_AE50)\r\n        GTEQ = POP_TEQ*168\r\n        TEQ = GTEQ\r\n        KOVER = 1/TOVER\r\n        RIN = SIZE0*KOVER\r\n        KPD = ln(2)/TEQ\r\n        S1 = 1\r\n"
    , MODEL_PREDICTION = new("modPred"
    , ODE = "            DAE = DRUG\r\n            DSIZE = TUMOUR\r\n            PD = 1-DAE/(AE50+DAE)\r\n            DRUG = ode(deriv = -KPD*DRUG)\r\n            TUMOUR = ode(deriv = (RIN*PD-DSIZE*KOVER)*DSIZE, init = SIZE0)\r\n"
    , LIBRARY = "            amount=nmadvan(model = 13, output = list(A, F))\n"
    , content = "        AE = DRUG\r\n        SIZE = TUMOUR\r\n"
)
    , OBSERVATION = "Y = SIZE+SIZE*eps_RUV_CV+eps_RUV_SD\r\n"
    , ESTIMATION = character(0)
    , MODEL_OUTPUT_VARIABLES = structure(list(), .Names = character(0))
)
    , taskObj = new("taskObj"
    , content = "\nTARGET_CODE(target=NMTRAN_CODE,location=\"$PROBLEM\",first=true){***\r\n$PROB TUMOUR EXPOSURE\r\n; Tham LS, Wang L, Soo RA, Lee SC, Lee HS, Yong WP, Goh BC, Holford NH.\r\n; A pharmacodynamic model for the time course of tumor shrinkage by gemcitabine +\r\n; carboplatin in non-small cell lung cancer patients.\r\n; Clin Cancer Res 2008; 14: 4213-8.\r\n***}\n\nMODEL{\ntolrel = 9\r\n}\n\nmyEST=function(t,m,p,d) {\n\nEXECUTE{\ncommand = \"call nmgo tumour_size_ORG\"\r\n}\n\nESTIMATE{\ntarget = t\r\nmodel = m\r\nparameter = p\r\ndata = d\r\n\nTARGET_CODE(target=NMTRAN_CODE,location=\"$ESTIMATION\"){***\r\n$EST MAX=9990 NSIG=3 SIGL=9 NOABORT PRINT=1\r\nMETHOD=CONDITIONAL INTERACTION\r\nMSFO=size.msf\r\n$COV\r\n***}\nalgo = list(\"FOCE  INTERACTION\")\r\ncov = true\r\n}\n\n}\n"
)
)
