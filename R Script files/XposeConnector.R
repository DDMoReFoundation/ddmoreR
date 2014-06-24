as.xpdb <- function(outputObject=NULL, ...) {
  
  modelfile <- outputObject$modelFile
  
  file.name.base = gsub('.mdl', '', modelfile)
  file.name.base = gsub('.xml', '', file.name.base)
  file.name.base = gsub('.ctl', '', file.name.base)
  
  table.names = c("_data", "mutab", 
                  "_params", "catab", "cotab", "mytab", "extra", "xptab", 
                  "cwtab")
  
  TEL.xpose.data(runno="", tab.suffix=".tab", mod.prefix=file.name.base, mod.suffix=".ctl", table.names=table.names, ...)
  
}

TEL.xpose.data <- function (runno, tab.suffix = "", sim.suffix = "sim", cwres.suffix = "", 
                            directory = "", quiet = TRUE, table.names = c("sdtab", "mutab", 
                                                                          "patab", "catab", "cotab", "mytab", "extra", "xptab", 
                                                                          "cwtab"), cwres.name = c("cwtab"), mod.prefix = "run", 
                            mod.suffix = ".mod", phi.suffix = ".phi", phi.file = NULL, 
                            nm7 = NULL, ...)  {
  match.pos <- match(cwres.name, table.names)
  if (!is.na(match.pos)) 
    table.names <- table.names[-match.pos]

  myfun <- function(x, directory, mod.prefix, cwres.suffix, sim.suffix, 
                    tab.suffix) {
    paste(directory, mod.prefix, x, cwres.suffix, sim.suffix, 
          tab.suffix, sep = "")
  }
  tab.files <- sapply(table.names, myfun, directory, mod.prefix, 
                      cwres.suffix = "", sim.suffix = "", tab.suffix)
  cwres.files <- sapply(cwres.name, myfun, directory, mod.prefix, 
                        cwres.suffix, sim.suffix = "", tab.suffix)
  sim.files <- sapply(table.names, myfun, directory, mod.prefix, 
                      cwres.suffix = "", sim.suffix, tab.suffix)
  cwres.sim.files <- sapply(cwres.name, myfun, directory, mod.prefix, 
                            cwres.suffix, sim.suffix, tab.suffix)
  tab.files <- c(tab.files, cwres.files)
  sim.files <- c(sim.files, cwres.sim.files)
  cat("\nLooking for NONMEM table files\n")
  tmp <- read.nm.tables(table.files = tab.files, quiet = quiet, 
                        ...)
  if (is.null(tmp)) {
    cat("Table files not read!\n")
    return(NULL)
  }
  if (is.null(nm7)) {
    if (any(!is.na(match(c("IPRED", "IWRES"), names(tmp))))) {
      nm7 <- T
    }
    else {
      nm7 <- F
    }
  }
  createXposeClasses(nm7 = nm7)
  xpobj <- new("xpose.data", Runno = runno, Doc = NULL, Data = NULL)
  if (is.readable.file("xpose.ini")) {
    xpobj <- xpose.read(xpobj, file = "xpose.ini")
  }
  else {
    rhome <- R.home()
    xdefini <- paste(rhome, "\\library\\xpose4\\xpose.ini", 
                     sep = "")
    if (is.readable.file(xdefini)) {
      xpobj <- xpose.read(xpobj, file = xdefini)
    }
    else {
      xdefini2 <- paste(rhome, "\\library\\xpose4data\\xpose.ini", 
                        sep = "")
      if (is.readable.file(xdefini2)) {
        xpobj <- xpose.read(xpobj, file = xdefini2)
      }
    }
  }
  Data(xpobj) <- tmp
  cat("Table files read.\n")
  ind.data <- NULL
  nsim.phi <- NULL
  if (nm7) {
    phi.data <- read.phi(phi.file = phi.file, phi.prefix = mod.prefix, 
                         runno = runno, phi.suffix = phi.suffix, quiet = quiet, 
                         nm7 = nm7, directory = directory, ...)
    if (!is.null(phi.data)) {
      if (dim(phi.data)[1] == dim(unique(xpobj@Data[xvardef("id", 
                                                            xpobj)]))[1]) {
        xpobj@Data.firstonly <- phi.data
      }
      else {
        first.phi.data <- phi.data[!duplicated(phi.data[, 
                                                        xvardef("id", xpobj)]), ]
        sim.phi.data <- phi.data[duplicated(phi.data[, 
                                                     xvardef("id", xpobj)]), ]
        xpobj@Data.firstonly <- first.phi.data
        nsim.phi.nrows <- dim(sim.phi.data)[1]
        first.phi.nrows <- dim(first.phi.data)[1]
        if (regexpr("\\.", as.character(nsim.phi.nrows/first.phi.nrows)) != 
              -1) {
          cat("The length of the Phi data and the Phi simulated data do not match!\n")
          return(xpobj)
        }
        nsim.phi <- nsim.phi.nrows/first.phi.nrows
      }
    }
  }
  cat("\nLooking for NONMEM simulation table files.\n")
  gosim <- TRUE
  simct <- FALSE
  for (i in 1:length(sim.files)) {
    if (is.readable.file(sim.files[i])) {
      simct <- TRUE
    }
  }
  if (simct) {
    for (i in 1:length(tab.files)) {
      if ((is.readable.file(tab.files[i])) && (!is.readable.file(sim.files[i]))) {
        err.mess <- paste(sim.files[i], "not found!")
        gosim <- FALSE
        break
      }
    }
  }
  else {
    gosim <- FALSE
  }
  if (gosim == FALSE) {
    if (!simct) {
    }
    else {
      cat("  There is not the same number of normal and \n")
      cat("  simulation table files for the current run number:\n")
      cat(paste("  ", err.mess, "\n", sep = ""))
    }
    cat("No simulated table files read.\n\n")
  }
  if (gosim == TRUE) {
    simtmp <- read.nm.tables(sim.files, quiet = quiet)
    if (!is.null(tmp)) {
      SData(xpobj) <- simtmp
      cat("Simulation table files read.\n")
    }
    else {
      cat("There was a problem reading the simulation tables!\n")
      cat("Simulation tables not read!\n")
      return(NULL)
    }
    if (!is.null(nsim.phi)) {
      if (!(xpobj@Nsim == nsim.phi)) {
        cat("\nThere are not the same number of simulations\n", 
            "in the table files and the phi file.\n", "Something is wrong with the phi file.\n", 
            "It will not be used.\n", sep = "")
        xpobj@Data.firstonly <- NULL
      }
      else {
        xpobj@SData.firstonly <- sim.phi.data
      }
    }
  }
  if (is.readable.file("xpose.ini")) {
    xpobj <- xpose.read(xpobj, file = "xpose.ini")
  }
  else {
    rhome <- R.home()
    xdefini <- paste(rhome, "\\library\\xpose4\\xpose.ini", 
                     sep = "")
    if (is.readable.file(xdefini)) {
      xpobj <- xpose.read(xpobj, file = xdefini)
    }
  }
  if (file.exists(".sdtab.names.tmp")) 
    file.remove(".sdtab.names.tmp")
  if (file.exists(".catab.names.tmp")) 
    file.remove(".catab.names.tmp")
  if (file.exists(".cotab.names.tmp")) 
    file.remove(".cotab.names.tmp")
  if (file.exists(".patab.names.tmp")) 
    file.remove(".patab.names.tmp")
  tmp.obj <- read.vpctab(object = xpobj, tab.suffix = tab.suffix, 
                         ...)
  if (!is.null(tmp.obj)) 
    xpobj <- tmp.obj
  if (is.null(check.vars(c("idv"), xpobj))) {
    cat("\n*********PLEASE NOTE: idv NOT IDENTIFIED************\n")
    cat("The independent variable (idv) has not been identified\n")
    cat("in the table files!  Please use the command line function\n")
    cat("'change.xvardef' (use '?change.xvardef' for help) or the classic\n")
    cat("menu system (select: Preferences/Manage variables/Change idv)\n")
    cat("to identify the name of the idv\n")
    cat("****************************************************\n")
  }
  return(xpobj)
}


XposeGOF<-function(...){
  ## ----setupRunnoforXpose--------------------------------------------------
  #runno <- as.numeric(gsub("[a-z]", "", list.files(pattern="^sdtab")[1]))
  
  
  ## ----createXpdb----------------------------------------------------------
  base.xpdb<- XposeConnector(...) #xpose.data(runno)
  #save(base.xpdb, file="Xpose database.RData")
  
  ## ----xposeGOF------------------------------------------------------------
  #dv.vs.pred.ipred(base.xpdb)
  pred.vs.idv(base.xpdb)
  #ipred.vs.idv(base.xpdb)
  #cwres.vs.idv(base.xpdb)
  #cwres.vs.pred(base.xpdb)
  ranpar.hist(base.xpdb)
  parm.splom(base.xpdb)
  #parm.vs.cov(base.xpdb)
  #ind.plots(base.xpdb, layout=c(4,4))
}
