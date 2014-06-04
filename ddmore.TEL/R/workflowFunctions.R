## ----defineFunctions-------------------------------------------------------

### ----estimate.NM-----------------------------------------------------------
estimate.NM<-function(modelfile=NULL,nonmem.exe="nonmem-7.2",modelextension=".mod",reportextension=".lst",addargs="",...){
  arg<-paste(nonmem.exe,paste(modelfile,modelextension,sep=""),paste(modelfile,reportextension,sep=""))
  shell(cmd=shQuote(arg))
}

### ----execute.PsN-----------------------------------------------------------
execute.PsN<-function(modelfile=NULL,addargs="",...){
  arg<-paste("execute-3.5.4",shQuote(paste(modelfile,".mod",sep="")),addargs)
  shell(arg)
}


### ----VPC-----------------------------------------------------------------
VPC<-function(modelfile,lstfile,nsamp,seed,addargs,...){
  arg<-paste("vpc-3.5.4 ",modelfile," --lst=",lstfile," --samples=",nsamp," --seed=",seed," ",addargs,sep="")
  cat(arg)
  shell(shQuote(arg),wait=T)
}

## ----Bootstrap-----------------------------------------------------------
bootstrap<-function(modelfile,nsamp,seed,addargs=NULL,...){
  arg<-paste("bootstrap-3.5.4 ",modelfile," --samples=",nsamp," --seed=",seed," ",addargs,sep="")
  shell(shQuote(arg))
}

## ----simUncVPC,include=FALSE---------------------------------------------
simunc.vpc<-function(modelfile,nsamp,bootres,addargs=NULL,...){
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

writeControlText<-function(templateModel,parsedControl, modelfile,modelextension=".mod"){
  
  ### Get RAW NM control stream items
  control<-templateModel
  
  ### Where do the various block statements occur?
  blocks<-control[grep("^[$]",control)]
    
  ## Drop commented out lines
  ## blocks<-blocks[-grep("[;]",blocks)]
  ### Get first "word" to determine order
  blocks2<-sub( " +.*", "", blocks ) 
  blocks2<-sub("$","",blocks2, fixed=T)
  blocks3<-unique(blocks2)
  blocks4<-substr(blocks3,1,3)
  orig.pos<-c(1:length(blocks4))
  
  ### Get list of objects from the parsed Control file
  control2<-parsedControl
  
  ## Match blocks in control file to  items in the parsed list
  ctrlmatch<-data.frame(block.id=blocks4, orig.block=paste("$",blocks3,sep=""), orig.pos=orig.pos,
                        RNMI.pos=charmatch(blocks4,casefold(names(control2),upper=T)),stringsAsFactors=F)
  
  special<-is.element(ctrlmatch$orig.block,c("$PK","$ERROR","$THETA","$SIGMA"))
  ctrlmatch$orig.block[special]<-paste(ctrlmatch$orig.block[special],"\n")
  
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
    Omega[1]<-paste("\n",Omega[1])
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
  
  
  Sigma<-control2$Sigma
  Sigma[Sigma==0]<-"0 FIX"
  Sigma<-sapply(Sigma,function(x)paste(x,"\n"))
  Sigma[control2$Sigma==1]<-"1 FIX \n"
  
  control2$Sigma<-Sigma
  names(control2$Sigma)<-NULL
  
  control3<-control2
  
  ####################################################################
  ### PREPARE ITEMS IN CONTROL3 FOR WRITING OUT
  ####################################################################

  ## $INPUT records - Paste together the variables names and labels
  ## e.g. SID=ID TIME=TIME AMT=AMT BWT=DROP MDV=MDV DV=DV
  ## More detail than necessary / usual, but consistent with RNMImport object
  
  #### If the two are equal then write only one
  
  control3$Input<-control2$Input[,"nmName"]
  diffInput<-control2$Input[,"nmName"]!=control2$Input[,"Label"]
  if(any(diffInput)){
    control3$Input[diffInput]<-paste(control2$Input[diffInput,"nmName"],control2$Input[diffInput,"Label"],sep="=")
  }
  
  ## $DATA records - Paste together commands and attributes
  ##  e.g. THEO.DAT IGNORE=# etc.
  
  colnames(control3$Data)[2]<-"IGNORE"
  ignoreAccept<-paste(colnames(control3$Data),control3$Data,sep="=")[c(2,3)]
  ignoreAccept<-ignoreAccept[grep(".",control2$Data[c(2,3)])]  ## Non-missing
  ### Change $DATA REWIND statement to NOREWIND rather than REWIND=FALSE
  control3$Data[4]<-ifelse(control2$Data[4]=="FALSE","NOREWIND","")

  control3$Data<-c(control2$Data[1], ignoreAccept)
  
  ## Omit Data file commands that have no attributes
  
  ## Set up $PK block for printing - new line at end of each item
  ## i.e. Write separate line for each item as in input code
  control3$PK<-paste(sapply(control2$PK,function(x){paste(x,"\n")}))
  
  ## Set up $ERROR block for printing - new line at end of each item
  control3$Error<-paste(sapply(control2$Error,function(x){paste(x,"\n")}))
  
  ## Combine $THETA bounds into usual NONMEM format
  ## e.g. (0, 0.5, ) OR 0.5 OR (,0.5,1000)
  control3$Theta<-paste("(",apply(control2$Theta,1,function(x){paste(x,collapse=",")}),")\n")
  control3$Theta<-gsub("NA","",control3$Theta)
  control3$Theta[is.na(control2$Theta[,1]) & is.na(control2$Theta[,3])]<-paste(control2$Theta[is.na(control2$Theta[,1]) & is.na(control2$Theta[,3]),2],"\n")
  
  ## Prepare $OMEGA for printings
    
  control3$Omega<-print(unlist(control2$Omega,as.character))

  ## Collect $TABLE variable strings, delete comma separator, append ONEHEADER NOPRINT statements
  control3$Tables<-
    apply(control3$Table,1,function(x){paste(
      "$TABLE ",
      gsub(",","",x[2])
      ," ONEHEADER NOPRINT FILE=",x[1],"\n",sep="")})
  ## First $Table statement doesn't need "$Table" since it comes from ctrlmatch
  control3$Tables[1]<-sub("^\\$TABLE","",control3$Tables[1],perl=T)
  control3$Tables<-gsub("ETA\\.","ETA\\(",control3$Tables,perl=T)
  control3$Tables<-gsub("\\.","\\)",control3$Tables,perl=T)
  
  #####################################
  #####################################
  ## Writing out the control statements
  #####################################
  #####################################
  
  sink(file=paste(modelfile,modelextension,sep=""))
  for (i in 1:length(blocks3)){
    cat(paste(ctrlmatch$orig.block[i]," "))
    cat(paste(cat(control3[[ctrlmatch$RNMI.pos[i]]]),"\n"))
  }
  sink()
}

post.process <- function() {
# Post-process the results 
myRun <- importNm(sprintf("%s/%s", local.out.dir, fn)) 
cat(sprintf('Plotting results graphs for job ID: %s ...\n', jobID)) 
myPlot1 <- xyplot(DV ~ Y | SEX, data = nmData(myRun), type = "b", groups = ID, titles = "Observed vs Differences by Gender") 
print(myPlot1) 
#myPlot2 <- nmBoxPlot(nmData(myRun), "DV", "TIME", bVar = "SEX", titles = "Observed vs Differences by Gender") 
#print(myPlot2) 
#cat('Done.\n\n')

myRun <- importNm("warf_base.ctl") 

# graph as a png 
png() 
xyplot(DV ~ Y | SEX, data = nmData(myRun), type = "b", groups = ID, titles = "Observed vs Differences by Gender") 

# close display 
dev.off()
}