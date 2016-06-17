
mogData <- new("mogObj"
    , dataObj = new("dataObj"
    , SOURCE = structure(list(srcfile = structure(list(file = "warfarin_conc.csv", 
    inputFormat = "nonmemFormat", ignore = "#"), .Names = c("file", 
"inputFormat", "ignore"))), .Names = "srcfile")
    , DECLARED_VARIABLES = list(structure(list(.subtype = "EquationDef", name = "GUT"), .Names = c(".subtype", 
"name")), structure(list(.subtype = "EquationDef", name = "Y"), .Names = c(".subtype", 
"name")), structure(list(.subtype = "EnumDefn", name = "Y2", 
    categories = c("c0", "c1", "c2", "c3")), .Names = c(".subtype", 
"name", "categories")), structure(list(.subtype = "EquationDef", 
    name = "DOSE"), .Names = c(".subtype", "name")), structure(list(
    .subtype = "EquationDef", name = "CP_obs"), .Names = c(".subtype", 
"name")), structure(list(.subtype = "EquationDef", name = "PCA_obs"), .Names = c(".subtype", 
"name"))), 
    DATA_INPUT_VARIABLES = structure(
        list(
            ID = structure(list(use = "id"), .Names = "use"), 
            TIME = structure(list(use = "idv"), .Names = "use"), 
            WT = structure(list(use = "covariate"), .Names = "use"), 
            AMT = structure(list(use = "amt"), .Names = "use"), 
            DVID = structure(list(use = "dvid", variable = "Y2"), .Names = c("use", "variable")), 
            DV = structure(list(use = "dv", define = "{Y2.c0 when 0, Y2.c1 when 1, Y2.c2 when 2, Y2.c3 when 3}"), .Names = c("use", "define")), 
            MDV = structure(list(use = "mdv"), .Names = "use"), 
            logtWT = structure(list(use = "covariate"), .Names = "use")), 
        .Names = c("ID", "TIME", "WT", "AMT", "DVID", "DV", 
            "MDV", "logtWT")), 
    DATA_DERIVED_VARIABLES = list(
        structure(
            list(
                DT = structure(list(use = "doseTime", idvColumn = "TIME", amtColumn = "AMT"), 
                    .Names = c("use", "idvColumn", "amtColumn"))), .Names = "DT"))
    , name = "fully_populated_dat")
    , parObj = NULL
    , mdlObj = new("mdlObj", 
        IDV = "T", 
        COVARIATES = list(
            structure(
                list(.subtype = "EquationDef", name = "DT"), .Names = c(".subtype", "name")), 
            structure(
                list(.subtype = "EquationDef", name = "D"), .Names = c(".subtype", "name")), 
            structure(
                list(.subtype = "EquationDef", name = "WT"), .Names = c(".subtype", "name")), 
            structure(
                list(.subtype = "EquationDef", name = "AGE"), .Names = c(".subtype", "name")), 
            structure(
                list(.subtype = "EquationDef", name = "logtWT", expr = "ln(WT/70)"), .Names = c(".subtype", "name", "expr")), 
            structure(
                list(.subtype = "EnumDefn", name = "SEX", categories = c("female", "male")), .Names = c(".subtype", "name", "categories")), 
            structure(
                list(.subtype = "EquationDef", name = "tAGE", expr = "AGE-40"), .Names = c(".subtype", "name", "expr")), 
            structure(
                list(.subtype = "EquationDef", name = "tSEX", expr = "if (SEX==SEX.female) then 1 else 0"), .Names = c(".subtype", "name", "expr"))), 
        VARIABILITY_LEVELS = structure(
            list(
                ID = structure(
                    list(level = "2", type = "parameter"), .Names = c("level", "type")), 
                DV = structure(list(level = "1", type = "observation"), .Names = c("level", "type"))), 
                .Names = c("ID", "DV")), 
        STRUCTURAL_PARAMETERS = c("POP_CL", "POP_V", "POP_KA", "POP_TLAG", "BETA_CL_WT", "BETA_CL_AGE", 
            "BETA_V_WT", "POP_FCL_FEM", "RUV_PROP", "RUV_ADD"), 
        VARIABILITY_PARAMETERS = c("PPV_CL", "PPV_V", "PPV_KA", "PPV_TLAG"), 
        RANDOM_VARIABLE_DEFINITION = list(
            structure(
                list(.subtype = "RandVarDefn", 
                    blkAttrs = structure(list(level = "DV"), .Names = "level"), 
                    args = structure(list(mean = "0", var = "1"), 
                        .Names = c("mean", "var")), 
                    name = "EPS_Y", 
                    distType = "Normal"), 
                .Names = c(".subtype", "blkAttrs", "args", "name", "distType")), 
            structure(
                list(.subtype = "RandVarDefn", 
                    blkAttrs = structure(list(level = "ID"), .Names = "level"), 
                    args = structure(list(mean = "0", sd = "PPV_CL"), 
                        .Names = c("mean", "sd")), 
                    name = "ETA_CL", 
                    distType = "Normal"), 
                .Names = c(".subtype", "blkAttrs", "args", "name", "distType")), 
            structure(
                list(.subtype = "RandVarDefn", 
                    blkAttrs = structure(list(level = "ID"), .Names = "level"), 
                    args = structure(list(mean = "0", sd = "PPV_V"), 
                        .Names = c("mean", "sd")), 
                    name = "ETA_V", distType = "Normal"), 
                .Names = c(".subtype", "blkAttrs", "args", "name", "distType")), 
            structure(
                list(.subtype = "RandVarDefn", 
                    blkAttrs = structure(list(level = "ID"), .Names = "level"), 
                    args = structure(list(mean = "0", sd = "PPV_KA"), 
                        .Names = c("mean", "sd")), 
                    name = "ETA_KA", 
                    distType = "Normal"), 
                .Names = c(".subtype", "blkAttrs", "args", "name", "distType")), 
            structure(
                list(.subtype = "RandVarDefn", 
                    blkAttrs = structure(list(level = "ID"), .Names = "level"), 
                    args = structure(list(mean = "0", sd = "PPV_TLAG"), 
                        .Names = c("mean", "sd")), 
                    name = "ETA_TLAG", 
                    distType = "Normal"), 
                .Names = c(".subtype", "blkAttrs", "args", "name", "distType"))), 
        INDIVIDUAL_VARIABLES = list(structure(list(transform = "ln", .subtype = "TransDefn", 
    args = structure(list(trans = "ln", pop = "POP_CL", fixEff = "[{coeff=BETA_CL_WT, cov=logtWT},{coeff=POP_FCL_FEM, cov=FCLSEX},{coeff=BETA_CL_AGE, cov=tAGE}]", 
        ranEff = "[ETA_CL]"), .Names = c("trans", "pop", "fixEff", 
    "ranEff")), name = "CL", func = "linear"), .Names = c("transform", 
".subtype", "args", "name", "func")), structure(list(transform = "ln", 
    .subtype = "TransDefn", args = structure(list(trans = "ln", 
        pop = "POP_V", fixEff = "[{coeff=BETA_V_WT, cov=logtWT}]", 
        ranEff = "[ETA_V]"), .Names = c("trans", "pop", "fixEff", 
    "ranEff")), name = "V", func = "linear"), .Names = c("transform", 
".subtype", "args", "name", "func")), structure(list(transform = "ln", 
    .subtype = "TransDefn", args = structure(list(trans = "ln", 
        pop = "POP_CL", fixEff = "[{coeff=BETA_CL_WT, cov=logtWT}]", 
        ranEff = "[ETA_CL,ETA_V]"), .Names = c("trans", "pop", 
    "fixEff", "ranEff")), name = "CL2", func = "linear"), .Names = c("transform", 
".subtype", "args", "name", "func")), structure(list(transform = "ln", 
    .subtype = "TransDefn", args = structure(list(trans = "ln", 
        pop = "POP_KA", ranEff = "[ETA_KA]"), .Names = c("trans", 
    "pop", "ranEff")), name = "KA", func = "linear"), .Names = c("transform", 
".subtype", "args", "name", "func")), structure(list(transform = "ln", 
    .subtype = "TransDefn", args = structure(list(trans = "ln", 
        pop = "POP_TLAG", ranEff = "[ETA_TLAG]"), .Names = c("trans", 
    "pop", "ranEff")), name = "TLAG", func = "linear"), .Names = c("transform", 
".subtype", "args", "name", "func")), structure(list(.subtype = "EquationDef", 
    funcArgs = structure(list(grp = "BASE", ranEff = "[ETA_V]"), .Names = c("grp", 
    "ranEff")), funcName = "general", name = "indiv_BASE"), .Names = c(".subtype", 
"funcArgs", "funcName", "name")), structure(list(.subtype = "EquationDef", 
    name = "HBASE", expr = "V/365"), .Names = c(".subtype", "name", 
"expr")), structure(list(.subtype = "EquationDef", name = "KA", 
    expr = "ln(2)/AGE"), .Names = c(".subtype", "name", "expr"
)), structure(list(.subtype = "EquationDef", name = "TLAG", expr = "exp(POP_TLAG+ETA_TLAG)"), .Names = c(".subtype", 
"name", "expr")), structure(list(.subtype = "EquationDef", name = "F1", 
    expr = "1"), .Names = c(".subtype", "name", "expr")), structure(list(
    transform = "ln", .subtype = "TransDefn", args = structure(list(
        trans = "ln", pop = "POP_V", ranEff = "[ETA_CL]"), .Names = c("trans", 
    "pop", "ranEff")), name = "BM0", func = "linear"), .Names = c("transform", 
".subtype", "args", "name", "func")), structure(list(transform = "ln", 
    .subtype = "TransDefn", args = structure(list(trans = "ln", 
        pop = "POP_CL", ranEff = "[ETA_CL]"), .Names = c("trans", 
    "pop", "ranEff")), name = "BM02", func = "linear"), .Names = c("transform", 
".subtype", "args", "name", "func")))
    , MODEL_PREDICTION = list(structure(list(.subtype = "EquationDef", name = "DOSE"), .Names = c(".subtype", 
"name")), structure(list(.subtype = "EquationDef", name = "IMAX"), .Names = c(".subtype", 
"name")), structure(list(.subtype = "EquationDef", name = "IC50"), .Names = c(".subtype", 
"name")), structure(list(.subtype = "EquationDef", name = "AUC", 
    expr = "DOSE/CL"), .Names = c(".subtype", "name", "expr")), 
    structure(list(.subtype = "EquationDef", name = "lnLAMBDA", 
        expr = "ln(indiv_BASE)+POP_TLAG*CL"), .Names = c(".subtype", 
    "name", "expr")), structure(list(.subtype = "EquationDef", 
        name = "LAMBDA", expr = "exp(lnLAMBDA)"), .Names = c(".subtype", 
    "name", "expr")), structure(list(.subtype = "EquationDef", 
        name = "CC", expr = "if (T-DT<TLAG) then 0 elseif (T-DT==TLAG) then 1 else (D/V)*KA/(KA-T)*(exp(-(T-DT-TLAG))-exp(-KA*(T-DT-TLAG)))"), .Names = c(".subtype", 
    "name", "expr")), structure(list(DEQ = list(structure(list(
        .subtype = "EquationDef", name = "RATEIN", expr = "if (T>=TLAG) then GUT*KA else 0"), .Names = c(".subtype", 
    "name", "expr")), structure(list(GUT = structure(list(deriv = "(-RATEIN)", 
        init = "0", x0 = "0"), .Names = c("deriv", "init", "x0"
    ))), .Names = "GUT"), structure(list(CENTRAL = structure(list(
        deriv = "(RATEIN-CL*CENTRAL/V)", init = "0", x0 = "0"), .Names = c("deriv", 
    "init", "x0"))), .Names = "CENTRAL"), structure(list(.subtype = "EquationDef", 
        name = "EFF", expr = "IMAX*AUC/(IC50+AUC)"), .Names = c(".subtype", 
    "name", "expr")), structure(list(.subtype = "EquationDef", 
        name = "PCA", expr = "EFF"), .Names = c(".subtype", "name", 
    "expr")), structure(list(VEGF = structure(list(deriv = "KA-CL2*(1-EFF)*VEGF", 
        init = "BM0"), .Names = c("deriv", "init"))), .Names = "VEGF"), 
        structure(list(sVEGFR2 = structure(list(deriv = "KA*(1-AUC)-VEGF", 
            init = "BM02", wrt = "T"), .Names = c("deriv", "init", 
        "wrt"))), .Names = "sVEGFR2")), .subtype = "BlockStmt"), .Names = c("DEQ", 
    ".subtype")), structure(list(.subtype = "BlockStmt", COMPARTMENT = list(
        structure(list(.subtype = "EquationDef", name = "CC", 
            expr = "CENTRAL/V"), .Names = c(".subtype", "name", 
        "expr")), structure(list(INPUT_KA = structure(list(type = "depot", 
            modelCmt = "1", to = "CENTRAL", ka = "KA", tlag = "TLAG"), .Names = c("type", 
        "modelCmt", "to", "ka", "tlag"))), .Names = "INPUT_KA"), 
        structure(list(CENTRAL = structure(list(type = "compartment", 
            modelCmt = "2"), .Names = c("type", "modelCmt"))), .Names = "CENTRAL"), 
        structure(list(structure(list(type = "elimination", modelCmt = "2", 
            from = "CENTRAL", v = "V"), .Names = c("type", "modelCmt", 
        "from", "v"))), .Names = ""), structure(list(PERIPHERAL = structure(list(
            type = "distribution", modelCmt = "3", from = "CENTRAL", 
            kin = "KA", kout = "KA"), .Names = c("type", "modelCmt", 
        "from", "kin", "kout"))), .Names = "PERIPHERAL"))), .Names = c(".subtype", 
    "COMPARTMENT")), structure(list(.subtype = "EquationDef", 
        name = "F", expr = "CENTRAL/AGE"), .Names = c(".subtype", 
    "name", "expr")), structure(list(.subtype = "EquationDef", 
        name = "CC2", expr = "F"), .Names = c(".subtype", "name", 
    "expr")), structure(list(.subtype = "EquationDef", name = "HAZ", 
        expr = "HBASE*(1+CC2)"), .Names = c(".subtype", "name", 
    "expr")), structure(list(.subtype = "EquationDef", name = "Prob0", 
        expr = "DV"), .Names = c(".subtype", "name", "expr")), 
    structure(list(.subtype = "EquationDef", name = "Prob1", 
        expr = "CC-DV"), .Names = c(".subtype", "name", "expr"
    )), structure(list(.subtype = "EquationDef", name = "Prob2", 
        expr = "DV-TLAG"), .Names = c(".subtype", "name", "expr"
    )), structure(list(.subtype = "EquationDef", name = "Prob3", 
        expr = "TLAG"), .Names = c(".subtype", "name", "expr"
    )))
    , OBSERVATION = list(structure(list(.subtype = "EquationDef", funcArgs = structure(list(
    additive = "RUV_ADD", proportional = "RUV_PROP", eps = "EPS_Y", 
    prediction = "CC"), .Names = c("additive", "proportional", 
"eps", "prediction")), funcName = "combinedError1", name = "Y1"), .Names = c(".subtype", 
"funcArgs", "funcName", "name")), structure(list(Y2 = structure(list(
    type = "count", distn = "Poisson(lambda=LAMBDA)"), .Names = c("type", 
"distn"))), .Names = "Y2"), structure(list(Y3 = structure(list(
    type = "discrete", distn = "Binomial(probabilityOfSuccess=Prob0, numberOfTrials=1)"), .Names = c("type", 
"distn"))), .Names = "Y3"), structure(list(Y4 = structure(list(
    type = "categorical withCategories {c0 when Prob0, c1 when Prob1, c2 when Prob2, c3 when Prob3}"), .Names = "type")), .Names = "Y4"), 
    structure(list(Y5 = structure(list(type = "tte", hazard = "HAZ", 
        event = "intervalCensored"), .Names = c("type", "hazard", 
    "event"))), .Names = "Y5"), structure(list(.subtype = "EquationDef", 
        funcArgs = structure(list(trans = "ln", additive = "0", 
            proportional = "RUV_PROP", prediction = "KA"), .Names = c("trans", 
        "additive", "proportional", "prediction")), funcName = "combinedError2", 
        name = "LNsKIT_obs"), .Names = c(".subtype", "funcArgs", 
    "funcName", "name")), structure(list(.subtype = "EquationDef", 
        funcArgs = structure(list(additive = "RUV_ADD", proportional = "RUV_PROP", 
            eps = "EPS_Y", prediction = "CC"), .Names = c("additive", 
        "proportional", "eps", "prediction")), funcName = "combinedError1", 
        name = "CP_obs"), .Names = c(".subtype", "funcArgs", 
    "funcName", "name")), structure(list(.subtype = "EquationDef", 
        funcArgs = structure(list(additive = "RUV_ADD", prediction = "PCA"), .Names = c("additive", 
        "prediction")), funcName = "additiveError", name = "PCA_obs"), .Names = c(".subtype", 
    "funcArgs", "funcName", "name")))
    , GROUP_VARIABLES = list(structure(list(.subtype = "EquationDef", name = "BASE", 
    expr = "ln(POP_CL/(1-POP_CL))"), .Names = c(".subtype", "name", 
"expr")), structure(list(.subtype = "EquationDef", name = "HBASE", 
    expr = "POP_V/365"), .Names = c(".subtype", "name", "expr"
)), structure(list(.subtype = "EquationDef", name = "FCLSEX", 
    expr = "if (SEX==SEX.female) then 1 else 0"), .Names = c(".subtype", 
"name", "expr")), structure(list(.subtype = "EquationDef", name = "GRPCL", 
    expr = "ln(POP_CL*POP_V*DV)+BETA_CL_WT*logtWT"), .Names = c(".subtype", 
"name", "expr")), structure(list(.subtype = "EquationDef", name = "GRPV", 
    expr = "POP_V*(WT/70)^BETA_V_WT"), .Names = c(".subtype", 
"name", "expr")))
    , name = "fully_populated_mdl"
)
    , taskObj = new("taskObj"
    , ESTIMATE = structure(list(algo = "saem", solver = "stiff", reltol = "6"), .Names = c("algo", 
"solver", "reltol"))
    , SIMULATE = structure(list(solver = "stiff"), .Names = "solver")
    , EVALUATE = NULL
    , name = "fully_populated_task"
)
    , priorObj = NULL
    , designObj = NULL
    , name = "mogData"
)
