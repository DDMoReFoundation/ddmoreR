# setwd("C:/PKPDStick/MDL IDE/64bits/workspace/warfarin/src/package")
# source("workflowFunctions.R")
# source("execute.R")
# source("fileUtils.R")
# source("mdlUtils.R")
# source("server.R")


## ----Initialise-------------------------------------------------------

## Uses Mango Solutions library RNMImport that reads in NONMEM files
## and creates an R object
library(rjson)
library(RCurl)
library(RNMImport)
library(xpose4)
library(MSToolkit)

## ----loadData---------------------------------------------------------
warfData<-readNmData("warfpk2.csv",records=0)
warfNames<-names(warfData)
warfNames[1]<-"ID"
warfData<-readNmData("warfpk2.csv")
names(warfData)<-warfNames
head(warfData)

## ----loadModel---------------------------------------------------------

warfBaseModel<-importNmMod("run1.mod")
warfBaseModelParsed<-warfBaseModel[[4]][[1]] ## Parsed control statements for Problem 1
warfBaseModelRaw<-warfBaseModel[[1]]

### Set up a list of THETA vectors to substitute into the THETA block through an sapply function

## Possible values of THETA, fixing ALAG1 and RUVs to zero
## CL, V, KA, ALAG1, RUV1(Additive), RUV2(Propnl)
myThetas=list(c(0.1,8.0,0.5,0,0,0.25),
		
		c(0.1,8.0,0.5,0.5,0,0.25),
		
		c(0.1,8.0,0.5,0.5,0.1,0.25),
		
		c(0.1,8.0,0.5,0.5,0.1,0))

THETA<-lapply(myThetas,function(x){
			newThetas<-warfBaseModelParsed$Theta
			newThetas[,2]<-x
			newThetas
		})

## Possible values of OMEGA, fixing some to zero to test IIV
## Off-diagonal included to test correlation in IIV
OMEGA=list(
		
		parseCovMatrix(c(0.1,0.1,0,0),4), ## diagonal, assumes off-diagonal=0
		
		parseCovMatrix(c(0.1,0.1,0.1,0),4), ## diagonal, assumes off-diagonal=0
		
		parseCovMatrix(c(0.1,0.1,0.1,0.1),4), ## diagonal, assumes off-diagonal=0
		
		parseCovMatrix(c(0.1,0.01,0.1,0,0,0,0,0,0,0),4), ## lower triangle
		
		parseCovMatrix(c(0.1,0.01,0.1,0,0,0.1,0,0,0,0),4), ## lower triangle
		
		parseCovMatrix(c(0.1,0.01,0.1,0,0,0.1,0,0,0,0.1),4) ## lower triangle
)

Run1<-updateModel(warfBaseModelParsed,theta=THETA[[1]],omega=OMEGA[[1]]) ## Base model (again) Propnl error
Run2<-updateModel(warfBaseModelParsed,theta=THETA[[2]],omega=OMEGA[[1]]) ## Test ALAG1 Propnl error
Run3<-updateModel(warfBaseModelParsed,theta=THETA[[3]],omega=OMEGA[[1]]) ## Test additive & propnl error
Run4<-updateModel(warfBaseModelParsed,theta=THETA[[4]],omega=OMEGA[[1]]) ## Test additive error only
Run5<-updateModel(warfBaseModelParsed,theta=THETA[[2]],omega=OMEGA[[2]]) ## Test IIV on KA
Run6<-updateModel(warfBaseModelParsed,theta=THETA[[2]],omega=OMEGA[[3]]) ## Test IIV on ALAG1
Run7<-updateModel(warfBaseModelParsed,theta=THETA[[2]],omega=OMEGA[[4]]) ## Test corr on CL_V without IIV on KA, ALAG1
Run8<-updateModel(warfBaseModelParsed,theta=THETA[[2]],omega=OMEGA[[5]]) ## Test corr on CL_V with IIV on KA only
Run9<-updateModel(warfBaseModelParsed,theta=THETA[[2]],omega=OMEGA[[6]]) ## Test IIV on ALAG1 with IIV on KA and ALAG1

runpath<-file.path(getwd(),"Run1")
dir.create(runpath)
file.copy(Run1$Data[,"File"],file.path(runpath))
setwd(runpath)
writeControlText(templateModel=warfBaseModelRaw,parsedControl=Run1,file=file.path(getwd(),"Run1.mod"))

##----fit NONMEM model----------------------------------------------------
estimate.NM(modelfile="Run1.mod")

##----readNMoutput--------------------------------------------------------
base<-importNm("run1.mod")
#save(base,file="RNMImport database.RData")
#attributes(base)

## ----getFunctions--------------------------------------------------------
getObjective(base)

#getThetas(base)

getThetas(base,what=c("initial","final","stderrors"))

getOmegas(base)

getOmegas(base,what="shrinkage")

#getSigmas(base)

getEstimateCov(base)


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

VPC(modelfile="run1.mod",lstfile="run1.lst",nsamp=1000,seed=123,
		
		addargs="--bin_by_count=0 --bin_array=0.125,0.375,0.75,1.25,1.75,2.5,4.5,7.5,10.5,18,30,42,60,84,108,125 --dir=VPCdir")

waitForFiles(file="./VPCdir/vpctab1")

xpose.VPC(vpc.info="./VPCdir/vpc_results.csv",vpctab="./VPCdir/vpctab1")


bootstrap(modelfile="run1.mod",nsamp=100,seed=123)

simunc.vpc(modelfile="run1.mod",nsamp=100,bootres=".\bootstrap_dir1\raw_results1.csv")
xpose.VPC(vpc.info="./VPCdir/vpc_results.csv",vpctab="./VPCdir/vpctab1")
