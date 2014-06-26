## ----defineFunctions-------------------------------------------------------

## Helper Functions reading NM-TRAN control streams
getNMBlocks<-function(RNMImportObject){
  Raw<-RNMImportObject[[1]]
  blocks<-grep("^\\$",Raw)
  nextBlock<-c(blocks[-1],length(Raw))
  ## Drop commented out lines
  ## blocks<-blocks[-grep("[;]",blocks)]
  ### Get first "word" to determine order
  blocks2<-sub( " +.*", "", Raw[blocks] )   
  blocks3<-sub("$","",blocks2, fixed=T)
  data.frame(Blocks=blocks2,Search=blocks3,firstRow=blocks,nextBlockRow=nextBlock)
}

## get<<...>>> functions
getNMDataObjects<-function(RNMImportObject){
  Raw<-RNMImportObject[[1]]
  Parsed<-RNMImportObject[[4]][[1]]
  
  blockInfo<-getNMBlocks(RNMImportObject)
  rows<-unlist(blockInfo[as.character(blockInfo$Search)=="DATA",c("firstRow","nextBlockRow")])
  rawDataRows<-Raw[rows[1]:(rows[2]-1)]
  
  rows<-unlist(blockInfo[as.character(blockInfo$Search)=="INPUT",c("firstRow","nextBlockRow")])
  rawInputRows<-Raw[rows[1]:(rows[2]-1)]
  
  RAW<-c(rawDataRows,rawInputRows)
  if(length(grep("^\\;",RAW))>0){
    RAW<-RAW[-grep("^\\;",RAW)]
  }
  
  list(RAW=RAW,
       HEADER=Parsed$Input,
       FILE=Parsed$Data)
}

getNMParameterObjects<-function(RNMImportObject){
  Raw<-RNMImportObject[[1]]
  Parsed<-RNMImportObject[[4]][[1]]
  
  blockInfo<-getNMBlocks(RNMImportObject)
  rows<-unlist(blockInfo[as.character(blockInfo$Search)=="THETA",c("firstRow","nextBlockRow")])
  rawThetaRows<-Raw[rows[1]:(rows[2]-1)]
  
  rows<-unlist(blockInfo[as.character(blockInfo$Search)=="OMEGA",c("firstRow","nextBlockRow")])
  rawOmegaRows<-Raw[rows[1]:(rows[2]-1)]
  
  rows<-unlist(blockInfo[as.character(blockInfo$Search)=="SIGMA",c("firstRow","nextBlockRow")])
  rawSigmaRows<-Raw[rows[1]:(rows[2]-1)]
  
  RAW<-c(rawThetaRows,rawOmegaRows,rawSigmaRows)
  if(length(grep("^\\;",RAW))>0){
    RAW<-RAW[-grep("^\\;",RAW)]
  }
  
  list(RAW=RAW,
       STRUCTURAL=Parsed$Theta,
       VARIABILITY=list(IIV=Parsed$Omega,
                        RUV=Parsed$Sigma))
}

getNMTaskPropertiesObjects<-function(RNMImportObject){
  Raw<-RNMImportObject[[1]]
  Parsed<-RNMImportObject[[4]][[1]]
  
  blockInfo<-getNMBlocks(RNMImportObject)
  rows<-unlist(blockInfo[as.character(blockInfo$Search)=="EST",c("firstRow","nextBlockRow")])
  rawEstRows<-Raw[rows[1]:(rows[2]-1)]
  
  rows<-unlist(blockInfo[as.character(blockInfo$Search)=="COV",c("firstRow","nextBlockRow")])
  rawCovRows<-Raw[rows[1]:(rows[2]-1)]
  
  RAW<-c(rawEstRows,rawCovRows)
  if(length(grep("^\\;",RAW))>0){
    RAW<-RAW[-grep("^\\;",RAW)]
  }
  
  list(RAW=RAW,
       TARGET_CODE=list(Parsed$Estimates, 
                        Parsed$Cov))
}

getNMObjects<-function(RNMImportObject,what=c("Data","Parameters","TaskProperties","All")){
  ## TO BE WRITTEN
}

readNMData<-function(mclDataObject){
  data<-read.csv(file=mclDataObject$FILE[,"File"])
  names(data)<-mclDataObject$HEADER[,"Label"]
  data
}

### ----estimate.NM-----------------------------------------------------------
estimate.NM<-function(modelfile=NULL,nonmem.exe="nonmem-7.2",modelextension=".mod",reportextension=".lst",addargs="",...){
  arg<-paste(nonmem.exe,paste(modelfile,modelextension,sep=""),paste(modelfile,reportextension,sep=""))
  if(.Platform$OS.type == "windows")shell(arg,invisible=F)
  if(.Platform$OS.type != "windows")system(arg,wait=T)
}

### ----execute.PsN-----------------------------------------------------------
execute.PsN<-function(modelfile=NULL,addargs="",...){
  arg<-paste("execute-3.5.4",shQuote(paste(modelfile,".mod",sep="")),addargs)
  shell(arg)
}

basicGOF.Xpose<-function(){
  ## ----setupRunnoforXpose--------------------------------------------------
  runno <- as.numeric(gsub("[a-z]", "", list.files(pattern="^sdtab")[1]))
  
  
  ## ----createXpdb----------------------------------------------------------
  base.xpdb<-xpose.data(runno)
  #save(base.xpdb, file="Xpose database.RData")
  
  ## ----xposeGOF------------------------------------------------------------
  dv.vs.pred.ipred(base.xpdb)
  pred.vs.idv(base.xpdb)
  ipred.vs.idv(base.xpdb)
  cwres.vs.idv(base.xpdb)
  cwres.vs.pred(base.xpdb)
  ranpar.hist(base.xpdb)
  parm.splom(base.xpdb)
  parm.vs.cov(base.xpdb)
  ind.plots(base.xpdb, layout=c(4,4))
  # etc. etc.
}

### ----VPC-----------------------------------------------------------------
VPC.PsN<-function(modelfile,lstfile,nsamp,seed,addargs,...){
  arg<-paste("vpc-3.5.4 ",modelfile," --lst=",lstfile," --samples=",nsamp," --seed=",seed," ",addargs,sep="")
  cat(arg)
  if(.Platform$OS.type == "windows")shell(arg,wait=T,invisible=F,translate=T)
  if(.Platform$OS.type != "windows")system(arg,wait=T)
}

## ----Bootstrap-----------------------------------------------------------
bootstrap.PsN<-function(modelfile,nsamp,seed,addargs=NULL,...){
  arg<-paste("bootstrap-3.5.4 ",modelfile," --samples=",nsamp," --seed=",seed," ",addargs,sep="")
  shell(shQuote(arg))
}

## ----simUncVPC,include=FALSE---------------------------------------------
simulate.PsN<-function(modelfile,nsamp,bootres,addargs=NULL,...){  
  arg<-paste("vpc-3.5.4 ",modelfile," --samples=",nsamp," --rawres_input=",bootres," ",addargs,sep="")
  shell(shQuote(arg))
}

### ----waitForFiles-----------------------------------------------------------------
waitForFiles<-function(file){
  if(!(file.exists(file)))Sys.sleep(30)
  for(i in 1:10){
    if(!(file.exists(file)))Sys.sleep(i*5)
  }## Get R to wait for creation of the VPC results!
}

### ----Change model attributes-------------------------------------------------
updateMOG<-function(parsedObject,
                    theta=parsedObject$Theta,
                    omega=parsedObject$Omega,
                    sigma=parsedObject$Sigma,
                    task=parsedObject$Estimates,
                    data=parsedObject$Data,
                    dataNames=parsedObject$Input,
                    tables=parsedObject$Tables){
  newObject<-parsedObject
  newObject$Theta<-theta
  newObject$Omega<-omega
  newObject$Sigma<-sigma
  newObject$Estimates<-task
  newObject$Data<-data
  newObject$Input<-dataNames
  newObject$Tables<-tables
  newObject
}

### ----Change model attributes-------------------------------------------------
updateModel<-function(parsedObject,
                      theta=parsedObject$Theta,
                      omega=parsedObject$Omega,
                      task=parsedObject$Estimates,
                      data=parsedObject$Data,
                      dataNames=parsedObject$Input){
  newObject<-parsedObject
  newObject$Theta<-theta
  newObject$Omega<-omega
  newObject$Estimates<-task
  newObject$Data<-data
  newObject$Input<-dataNames
  newObject
}

### ----writeModel for execution------------------------------------------------------

writeControlText<-function(templateModel,parsedControl, modelfile,modelextension=".mod",
                           modelBlockNames=c("PK","PRE","SUB","MOD","DES","ERR")){
  
  ### Get RAW NM control stream items
  control<-templateModel
  
  ### Where do the various block statements occur?
  blockpos<-grep("^ *[$]",control)
  blocks<-control[blockpos]
  
  ## Drop commented out lines
  ## blocks<-blocks[-grep("[;]",blocks)]
  ### Get first "word" to determine order
  blocks1<-sub( " +.*", "", blocks ) 
  blocks2<-sub("$","",blocks1, fixed=T)
  orig1<-data.frame(block=blocks2,line=blockpos,stringsAsFactors=F)
  orig2<-orig1[!duplicated(orig1$block),]
  
  blocks3<-substr(orig2$block,1,3)
  orig.pos<-c(1:length(blocks3))
  orig<-data.frame(block.id=blocks3,orig.pos=orig.pos,orig.block=orig2$block,line=orig2$line,stringsAsFactors=F)
  
  ### Get list of objects from the parsed Control file
  control2<-parsedControl
  control2Blocks<-substr(casefold(names(control2),upper=T),1,3)
  RNMI.pos<-c(1:length(control2Blocks))
  RNMI<-data.frame(block.id=control2Blocks,RNMI.pos=RNMI.pos,RNMI.block=names(parsedControl),stringsAsFactors=F)
  
  ## Match blocks in control file to  items in the parsed list
  ctrlmerged<-merge(orig,RNMI,by="block.id",all=T)
  ctrlmerged<-ctrlmerged[order(ctrlmerged$orig.pos),]
  ctrlmerged$orig.block[is.na(ctrlmerged$orig.block)]<-casefold(ctrlmerged$RNMI.block[is.na(ctrlmerged$orig.block)],upper=T)
  
  ## Leave out model related blocks from parsedcontrol
  ## Will pick these up directly from Raw file.
  ## This means that we do not expect user to update the model!
  otherBlocks<-ctrlmerged[!(ctrlmerged$block.id%in%modelBlockNames),]
  control2<-control2[otherBlocks$RNMI.block]
  
  ## If blocks appear in the original, but not RNMImport parsed version
  ## then create RNMImport blocks.
  ## e.g. $DES
  
  modelBlockCode<-list(NULL)
  modelBlocks<-ctrlmerged[ctrlmerged$block.id%in%modelBlockNames,]
  for(i in 1:nrow(modelBlocks)){
    nextBlock<-ctrlmerged[modelBlocks$orig.pos[i]+1,]
    modelStart<-modelBlocks$line[i]
    modelEnd<-nextBlock$line-1
    
    codeLines <- control[modelStart:modelEnd]
    codeLines<-paste(codeLines,"\n")
    modelBlockCode[[i]]<-codeLines
    names(modelBlockCode)[[i]]<-modelBlocks$orig.block[i]
  }
  
  addBlocks<-list(NULL)
  missBlocks<-ctrlmerged[is.na(ctrlmerged$RNMI.pos)&!(ctrlmerged$block.id%in%modelBlockNames),]
  if(nrow(missBlocks)>0){
    for(i in 1:nrow(missBlocks)){
      nextBlock<-ctrlmerged[missBlocks$orig.pos[i]+1,]
      missStart<-missBlocks$line[i]+1  ## NOTE! The +1 here might cause trouble!
      missEnd<-nextBlock$line-1
      
      codeLines <- control[missStart:missEnd]
      addBlocks[[i]]<-codeLines
      names(addBlocks)[[i]]<-as.character(missBlocks$block.id[i])
      newRNMIpos<-max(ctrlmerged$RNMI.pos,na.rm=T)+i
      ctrlmerged[ctrlmerged$block.id==missBlocks[i,"block.id"],"RNMI.pos"]<-newRNMIpos
    }
  }
  
  ### Change $THETA -Inf and Inf values to missing
  ### Change $THETA values = 0 to "0 FIX"
  control2$Theta<-formatC(control2$Theta)
  control2$Theta<-apply(control2$Theta,2,function(x)sub("^ *Inf",NA,x))
  control2$Theta<-apply(control2$Theta,2,function(x)sub("^ *-Inf",NA,x))
  control2$Theta[control2$Theta[,1]==control2$Theta[,3],c(1,3)]<-NA
  control2$Theta[control2$Theta[,2]==0,c(1,3)] <- NA
  control2$Theta[control2$Theta[,2]==0,2] <- "0 FIX"
  
  ### Change $OMEGA values = 0 to "0 FIX"
  ### THIS NEEDS WORK!!!
  ### Turn Omega matrix into diagonal etc.
  ### and handle block structures
  
  Omega<-NULL
  
  ## Are only diagonals filled??
  OmegaDiag<-sum(control2$Omega[lower.tri(control2$Omega,diag=F)])==0
  if(OmegaDiag){
    Omega.blocksize<-NULL
    Omega<-diag(control2$Omega)
    Omega[Omega==0]<-"0 FIX"
    Omega<-sapply(Omega,function(x)paste(x,"\n"))
  }
  if(!OmegaDiag){
    ## Which Omegas are BLOCK
    Corr<-(apply(control2$Omega,2,sum)-diag(control2$Omega))!=0
    block<-paste("BLOCK(",sum(Corr),")\n",sep="")
    Omega1<-control2$Omega[Corr,Corr]
    Omega.1<-paste(Omega1[lower.tri(Omega1,diag=T)],"\n")
    Omega<-list(block,Omega.1)
    if(sum(Corr)!=length(diag(control2$Omega))){
      Omega.2<-diag(control2$Omega[!Corr,!Corr])
      Omega.2[Omega.2==0]<-"0 FIX"
      Omega.2<-sapply(Omega.2,function(x)paste(x,"\n"))
      Omega<-list(block,Omega.1,"\n$OMEGA\n",Omega.2)
    }
  }
  
  ## Overwrite control2$Omega with Omega above.
  control2$Omega<-Omega
  names(control2$Omega)<-NULL
  
  Sigma<-NULL
  
  ## Are only diagonals filled??
  SigmaDiag<-sum(control2$Sigma[lower.tri(control2$Sigma,diag=F)])==0
  if(SigmaDiag){
    Sigma.blocksize<-NULL
    Sigma<-diag(control2$Sigma)
    Sigma[Sigma==0]<-"0 FIX"
    Sigma<-sapply(Sigma,function(x)paste(x,"\n"))
  }
  if(!SigmaDiag){
    ## Which Sigmas are BLOCK
    Corr<-(apply(control2$Sigma,2,sum)-diag(control2$Sigma))!=0
    block<-paste("BLOCK(",sum(Corr),")\n",sep="")
    Sigma1<-control2$Sigma[Corr,Corr]
    Sigma.1<-paste(Sigma1[lower.tri(Sigma1,diag=T)],"\n")
    Sigma<-list(block,Sigma.1)
    if(sum(Corr)!=length(diag(control2$Sigma))){
      Sigma.2<-diag(control2$Sigma[!Corr,!Corr])
      Sigma.2[Sigma.2==0]<-"0 FIX"
      Sigma.2<-sapply(Sigma.2,function(x)paste(x,"\n"))
      Sigma<-list(block,Sigma.1,"\n$Sigma\n",Sigma.2)
    }
  }
  
  control2$Sigma<-Sigma
  names(control2$Sigma)<-NULL
  
  ####################################################################
  ### PREPARE ITEMS IN CONTROL2 FOR WRITING OUT
  ####################################################################
  
  ## $INPUT records - Paste together the variables names and labels
  ## e.g. SID=ID TIME=TIME AMT=AMT BWT=DROP MDV=MDV DV=DV
  ## More detail than necessary / usual, but consistent with RNMImport object
  
  #### If the two are equal then write only one
  
  Input<-control2$Input[,"nmName"]
  diffInput<-control2$Input[,"nmName"]!=control2$Input[,"Label"]
  if(any(diffInput)){
    Input[diffInput]<-paste(control2$Input[diffInput,"nmName"],control2$Input[diffInput,"Label"],sep="=")
  }
  
  control2$Input<-Input
  
  ## $DATA records - Paste together commands and attributes
  ##  e.g. THEO.DAT IGNORE=# etc.
  
  Data<-paste("'",control2$Data[1],"'",sep="")
  
  if(control2$Data[2]!="NONE"){
    colnames(control2$Data)[2]<-"IGNORE"
    ignoreAccept<-paste(colnames(control2$Data),control2$Data,sep="=")[c(2,3)]
    ignoreAccept<-ignoreAccept[grep(".",control2$Data[c(2,3)])]  ## Non-missing
    ### Change $DATA REWIND statement to NOREWIND rather than REWIND=FALSE
    control2$Data[4]<-ifelse(control2$Data[4]=="FALSE","NOREWIND","")
    
    Data<-c(control2$Data[1], ignoreAccept)
  }
  control2$Data<-Data
  
  ## Omit Data file commands that have no attributes
  
  ## Combine $THETA bounds into usual NONMEM format
  ## e.g. (0, 0.5, ) OR 0.5 OR (,0.5,1000)
  Theta<-paste("(",apply(control2$Theta,1,function(x){paste(x,collapse=",")}),")\n")
  Theta<-gsub("NA","",Theta)
  Theta[is.na(control2$Theta[,1]) & is.na(control2$Theta[,3])]<-paste(control2$Theta[is.na(control2$Theta[,1]) & is.na(control2$Theta[,3]),2],"\n")
  
  control2$Theta<-Theta
  
  ## Prepare $OMEGA for printings
  
  control2$Omega<-print(unlist(control2$Omega,as.character))
  
  ## Check for existence of $Tables in original code
  if(length(control2$Tables)){
    ## Collect $TABLE variable strings, delete comma separator, append ONEHEADER NOPRINT statements
    Tables<-
      apply(control2$Table,1,function(x){paste(
        "$TABLE ",
        gsub(",","",x[2])
        ," ONEHEADER NOPRINT FILE=",x[1],"\n",sep="")})
    ## First $Table statement doesn't need "$Table" since it comes from ctrlmerged if present
    if(!is.na(ctrlmerged$orig.block[ctrlmerged$block.id=="TAB"]))Tables[1]<-sub("^\\$TABLE","",Tables[1],perl=T)
    Tables<-gsub("ETA\\.","ETA\\(",Tables,perl=T)
    Tables<-gsub("\\.","\\)",Tables,perl=T)
    control2$Tables<-Tables
  }
  
  control3<-list(NULL)
  for(i in 1:nrow(ctrlmerged)){
    if(ctrlmerged$block.id[i]%in%otherBlocks$block.id)control3[[i]]<-control2[[ctrlmerged$RNMI.block[i]]]
    if(ctrlmerged$block.id[i]%in%modelBlockNames)control3[[i]]<-modelBlockCode[[ctrlmerged$orig.block[i]]]
    names(control3)[[i]]<-ctrlmerged$orig.block[i]
  }
  
  
  #####################################
  #####################################
  ## Writing out the control statements
  #####################################
  #####################################
  
  ### PROBABLY NEEDS BETTER HANDLING OF ORDER OF BLOCKS IN THE NONMEM CODE
  ### USE RULES FROM NONMEM HELP GUIDES?
  ### FOR NOW BASED ON ORDER IN ORIGINAL NM CODE
  ### IF ITEMS ADDED THROUGH updateMOG(...) THEN ADD THESE AT THE END?
  ### USUALLY TABLE ITEMS
  
  ## "special" blocks need $ statement on one line and content below
  special<-is.element(ctrlmerged$block.id,c("PK","PRED","ERR","THE","OME","SIG","DES","MOD"))
  model<-is.element(ctrlmerged$block.id,modelBlockNames)
  ctrlmerged$orig.block[special]<-paste(ctrlmerged$orig.block[special],"\n")
  ctrlmerged$orig.block[model]<-""
  
  sink(file=paste(modelfile,modelextension,sep=""))
  for (i in 1:nrow(ctrlmerged)){
    if(!ctrlmerged$block.id[i]%in%modelBlockNames)cat(paste("$",ctrlmerged$orig.block[i]," ",sep=""))
    cat(paste(cat(control3[[i]]),"\n"))
  }
  sink()
}

## ----Adapted from RNMImport------------------------------------------------
commentPop<-function (..., .depth = 2) pop(..., mode = "comments", .depth = .depth)

pop<-function (txt, option, mode = c("logical", "equal", "brackets","comments","objects"),
               inPlace = TRUE, absent = if (mode == "logical") FALSE else NULL, 
               ignore.case = TRUE, shortcut = FALSE, sep = "=", .depth = 1, 
               removeBrackets = (mode == "brackets"), numeric = FALSE, ...) 
{
  if (inPlace) 
    nameTxt <- deparse(substitute(txt))
  txt <- gsub("\\)[[:space:]]*", ") ", txt)
  mode <- match.arg(mode)
  if (shortcut && mode == "equal") {
    option <- sprintf("[^[:space:]]*%s[^[:space:]=]*", option)
  }
  rx <- switch(mode, 
               equal = sprintf("\\<%s%s([^[:space:]]*)[[:space:]]*",option, sep), 
               logical = sprintf("\\<%s\\>", option),
               brackets = "(\\([^\\)]*\\))", 
               comments = "#(.*$)")
  
  grep.out <- grep(rx, txt, ignore.case = ignore.case, ...)
  op.out <- if (length(grep.out)) {
    out <- switch(mode, 
                  logical = TRUE, 
                  equal = gsub("['\"]","", gsub(sprintf("^.*%s.*$", rx), "\\1", txt[grep.out],ignore.case = ignore.case, ...)), 
                  brackets = sub(sprintf("^[^\\(\\)]*%s.*$",rx), "\\1", txt[grep.out], ignore.case = ignore.case, ...),
                  comments = {com <- rep("", length(txt))
                              com[grep.out] <- sub(sprintf("^.*%s", rx), "\\1",txt[grep.out])
                              com})
    txt <- sub(sprintf("[[:space:]]*%s[[:space:]]*", rx)," ", txt)
    if (removeBrackets) {
      out <- gsub("[\\(\\)]", "", out)
    }
    out
  }
  else absent
  if (numeric) 
    op.out <- as.numeric(op.out)
  if (inPlace) {
    assign(nameTxt, txt, parent.frame(.depth))
    return(op.out)
  }
  list(op.out = op.out, txt = txt)
}

importRAWMDL<-function(file=NULL,path=getwd()){
  path <- RNMImport:::processPath(path)
  fileContents <- RNMImport:::scanFile(RNMImport:::.getFile(file, path))
  if (is.null(fileContents)) 
    RNMImport:::RNMImportStop(paste("Contents of the file", fileName, 
                                    "are empty \n"), match.call())
  #   fileContents <- RNMImport:::negGrep("^[;[:space:]]+$", fileContents, 
  #                                       value = TRUE)
  fileContents <- RNMImport:::killRegex(fileContents, "^[[:blank:]]*")
  comments <- commentPop(fileContents, inPlace = FALSE)$op.out
  logMessage(logName = "highLevelParse", msg)
  outList <- list(Raw = fileContents, Comments = comments)
  outList$controlFile <- RNMImport:::.getFile(file, path)
  outList
}

findObjects<-function(txt=NULL){
  dStart<-grep("dataobj",txt)
  pStart<-grep("parobj",txt)
  mStart<-grep("mdlobj",txt)
  tStart<-grep("taskobj",txt)
  ## what comes after each block?
  data<-data.frame(objStart=c(dStart,pStart,mStart,tStart))
  data$obj<-c(rep("data",length(dStart)),rep("par",length(pStart)),rep("model",length(mStart)),rep("task",length(tStart)))
  data<-data[order(data$objStart),]
  data$objEnd<-c(data$objStart[-1]-1,length(txt))
  data$name<-gsub(" = *.+","",txt[data$objStart])
  data
}

findBlocks<-function(txt=NULL){
  start<-grep("\\{",txt)
  ## what comes after each block?
  data<-data.frame(blockStart=start)
  data$blockEnd<-grep("\\}",txt)
  data$name<-gsub(" *\\{ *.*","",txt[start])
  data
}

extractMDLObjects<-function(txt=NULL,type="All",name=NULL,dropComments=F){
  objects<-findObjects(txt)
  objLines<-objects[objects$obj==type,]
  if(type=="All")objLines<-objects
  nObjs<-nrow(objLines)
  out<-NULL
  for(i in 1:nObjs){
    objLines2<-objLines[i,]
    objLines2<-txt[objLines2$objStart:objLines2$objEnd] 
    objLines2<-objLines2[-length(objLines2)]  ## Remove "obj{" line
    objLines2<-objLines2[-1]  ## Remove "} # end object" line
    if(dropComments)objLines2<-objLines2[-grep("^ *#",objLines2)]
    out[[i]]<-objLines2
  }
  names(out)<-objLines$name
  if(length(name))out<-out[[name]]
  out
}

extractMDLBlocks<-function(object=NULL,dropComments=F){
  txt<-object[[1]]
  blocks<-findBlocks(txt)
  blockLines<-blocks
  nBlocks<-nrow(blockLines)
  out<-NULL
  for(i in 1:nBlocks){
    blockLines2<-blockLines[i,]
    blockLines3<-txt[blockLines2$blockStart:blockLines2$blockEnd] 
    blockLines4<-blockLines3[-length(blockLines3)]  ## Remove "obj{" line
    blockLines5<-blockLines4[-1]  ## Remove "} # end object" line
    #     if(dropComments)blockLines2<-blockLines2[-grep("^ *#",blockLines2)]
    out<-c(out,list(blockLines5))
  }
  names(out)<-blockLines$name
  out<-list(out)
  names(out)<-names(object)
  out
}

as.MDLData<-function(dataBlocks=NULL){
  HEADER<-dataBlocks$HEADER
  colNames<-gsub("=.+","",HEADER)
  categorical<-colNames[grep("categorical",HEADER)]
  FILE<-dataBlocks$FILE
  fileSource<-grep("^source=",FILE,value=T)
  fileSource<-gsub('\\"',"",gsub("source=","",fileSource))
  fileSource<-gsub(',',"",fileSource)
  fileFormat<-grep("^inputformat=",FILE,value=T)
  fileFormat<-gsub('\\"',"",gsub("inputformat=","",fileFormat))
  fileFormat<-gsub(')+',"",fileFormat)
  #RNMImport:::.getFile(fileSource,test=T)
  out<-list(dataBlocks,SOURCE=fileSource,FORMAT=fileFormat,HEADER=colNames,CATEGORICAL=categorical)
  out
}

read.data<-function(dataBlocks=NULL,categoricalAsFactor=T){
  if(dataBlocks$FORMAT=="NONMEM")myData<-readNmData(dataBlocks$SOURCE)
  names(myData)<-dataBlocks$HEADER
  if(categoricalAsFactor){
    if(length(dataBlocks$CATEGORICAL)){
      for(i in 1:length(dataBlocks$CATEGORICAL)){
        myData[,dataBlocks$CATEGORICAL]<-lapply(myData[,dataBlocks$CATEGORICAL],as.factor)
      }
    }
  }
  myData
}

getDataObjects<-function(file=NULL,name=NULL,...){
  out<-NULL
  RawCode<-importRAWMDL(file=file,...)
  Objects<-extractMDLObjects(RawCode$Raw,type="data",name=name,...)
  rawData<-Objects
  names(rawData)<-names(Objects)
  if(length(name))rawData<-rawData[[name]]
  parsedData<-lapply(extractMDLBlocks(rawData),as.MDLData)
  out<-parsedData
  out
}

getParameterObjects<-function(file=NULL,name=NULL,...){
  RawCode<-importRAWMDL(file=file,...)
  Objects<-extractMDLObjects(RawCode$Raw,type="par",name=name,...)
  out<-Objects
  if(length(name))out<-out[[name]]
  out
}

getModelObjects<-function(file=NULL,name=NULL,...){
  RawCode<-importRAWMDL(file=file,...)
  Objects<-extractMDLObjects(RawCode$Raw,type="model",name=name,...)
  out<-Objects
  if(length(name))out<-out[[name]]
  out
}

getTaskObjects<-function(file=NULL,name=NULL,...){
  RawCode<-importRAWMDL(file=file,...)
  Objects<-extractMDLObjects(RawCode$Raw,type="task",name=name,...)
  out<-Objects
  if(length(name))out<-out[[name]]
  out
}


getObjects<-function(file=NULL,name=NULL,...){
  RawCode<-importRAWMDL(file=file,...)
  Objects<-extractMDLObjects(RawCode$Raw,type="All",name=name,...)
  out<-Objects
  if(length(name))out<-out[[name]]
  out
}

parseCovMatrix<-function (values, nCov, tol = 1e-06) 
{
  if (is.matrix(values)) {
    mat <- values
  }
  else {
    values <- parseCharInput(values, sort = FALSE)
    if (nCov == 1) {
      length(values) == 1 || ectdStop("Dimension problem")
      mat <- matrix(values[1], nrow = 1, ncol = 1)
    }
    else {
      if (length(values) == 1) 
        values <- rep(values, nCov)
      nValues <- length(values)
      if (nValues == nCov) {
        mat <- diag(values)
      }
      else if (nValues == (nCov) * (nCov + 1)/2) {
        mat <- matrix(0, ncol = nCov, nrow = nCov)
        mat[upper.tri(mat, diag = TRUE)] <- values
        mat <- t(mat)
        mat[upper.tri(mat, diag = TRUE)] <- values
      }
      else {
        ectdStop("Dimension Problem")
      }
    }
  }
  checkSymmetricPDMatrix(mat, tol)
  mat
}