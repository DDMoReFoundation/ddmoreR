## Prepared by Mike K Smith
## 22 Oct 2014

library(xpose4)

## Monolix output available via DDMoRe repository:
## http://wwwdev.ebi.ac.uk/biomodels/model-repository/model/DDMODEL00000374#Files 
## File: Warfarin_PK_ODE.zip

## READ MONOLIX output
if(Sys.getenv("USER")=="msmith") setwd("~/../desktop/IOG/Lixoft/warfarin_PK_ODE/warfarin_PK_ODE_project")

if(Sys.getenv("USER")=="ahooker") setwd("/Users/ahooker/Documents/_PROJECTS/Xpose/devel/pharmml_to_xpdb/warfarin_PK_ODE/warfarin_PK_ODE_project")

setwd("C:\\Users\\cmusselle\\Projects\\DDmore\\TEL-R\\development data\\warfarin_PK_ODE\\warfarin_PK_ODE_project")

# read PREDICTIONS
MLX.Pred.out <- read.table("predictions.txt",header=T)

plot.default(MLX.Pred.out$Y,MLX.Pred.out$popPred);abline(0,1,col="red")
plot.default(MLX.Pred.out$Y,MLX.Pred.out$indPred_mean);abline(0,1,col="red")

## Separate MLX.Pred.out: Mapping to SO object item <Predictions> equivalent
Predictions<-MLX.Pred.out[,c("ID","time","popPred","indPred_mean")]
names(Predictions)<-c("ID","TIME","PRED","IPRED")

## Separate MLX.Pred.out: Mapping to SO object item <Residuals> equivalent
Residuals<-MLX.Pred.out[,c("ID","time","popWRes","indWRes_mean")]
names(Residuals)<-c("ID","TIME","WRES","IWRES")

# read individual parameters
MLX.indiv.Par <- read.table("indiv_parameters.txt",header=T)

## Mapping to SO object item <IndividualEstimates> <Estimates> equivalent
Estimates <- MLX.indiv.Par[,c("ID","V_mean","KA_mean","CL_mean","TLAG_mean")]
names(Estimates) <- c("ID","V","KA","CL","TLAG")

# read individual parameters
MLX.indiv.Eta <- read.table("indiv_eta.txt",header=T)

## Mapping to SO object item <IndividualEstimates> <RandomEffects> equivalent
RandomEffects <- MLX.indiv.Eta[,c("ID","eta_V_mean","eta_KA_mean","eta_CL_mean","eta_TLAG_mean")]
names(RandomEffects) <- c("ID","ETA_V","ETA_KA","ETA_CL","ETA_TLAG")

## Combine Estimates and RandomEffects to create SO object item <IndividualEstimates> equivalent
IndividualEstimates <- list(Estimates=Estimates, RandomEffects=RandomEffects)

### READ Raw (input) dataset. 
### Ultimately we should use the readDataObjects function to apply the data variable names as per MCL

#warf<-read.csv("warfarin_conc.csv")
data<-read.csv("../warfarin_PK_ODE_data.txt",na.strings=".")
names(data)<-c("ID","TIME","WT","AMT","DVID","DV","MDV","logtWT")
head(data)

## Merge together data to get Xpose Database @Data item
data1 <- merge(data,Predictions)
data2 <- merge(data1,Residuals)
data3 <- merge(data2, IndividualEstimates$Estimates)
data4 <- merge(data3, IndividualEstimates$RandomEffects)

data.out <- data4[order(data4$ID,data4$TIME),]
head(data.out)


### CREATE new Xpose database
myXpdb<-new("xpose.data",Runno=0,Doc=NULL)

###############################################################
## LESS CERTAINTY IN THIS SECTION!
## In this section I'm making (slightly informed) guesses. Needs checking.
###############################################################

## Map data.out to xpdb@Data
Data(myXpdb)<-data.out

## Create xpdb@Data.firstonly using RandomEffects (?)
## not really needed here, only used if table files have different lengths.
##myXpdb@Data.firstonly <- RandomEffects

## Update xpdb@Prefs@Xvardef (variable definitions)
myXpdb@Prefs@Xvardef

## Fill in / Confirm information from PharmML
myXpdb@Prefs@Xvardef$id<-"ID"
myXpdb@Prefs@Xvardef$idv<-"TIME"
myXpdb@Prefs@Xvardef$occ<-NA
myXpdb@Prefs@Xvardef$dv<-"DV"

## Fill in / Confirm information from SO
myXpdb@Prefs@Xvardef$pred<-"PRED"
myXpdb@Prefs@Xvardef$ipred<-"IPRED"
myXpdb@Prefs@Xvardef$wres <- "WRES"
myXpdb@Prefs@Xvardef$iwres <- "IWRES"
myXpdb@Prefs@Xvardef$parms <- c("V","CL","KA","TLAG")
myXpdb@Prefs@Xvardef$covariates <- "logtWT"
myXpdb@Prefs@Xvardef$ranpar <- c("ETA_V","ETA_CL","ETA_KA","ETA_TLAG")

## Ideally would also update xpdb@Prefs@Labels (variable labels for plots)
myXpdb@Prefs@Labels

## update runno
myXpdb@Runno <- 1

###############################################################
### MORE CERTAINTY FROM THIS POINT!
###############################################################

##  Now create the diagnostic plots using Xpose!
basic.gof(myXpdb)

## PRED and IPRED vs DV
dv.vs.pred(myXpdb)
dv.vs.ipred(myXpdb)
dv.vs.pred.ipred(myXpdb)

# IWRES plots vs IPRED, TIME
absval.iwres.vs.ipred(myXpdb)
absval.iwres.vs.idv(myXpdb)
iwres.dist.hist(myXpdb)
iwres.dist.qq(myXpdb)

# WRES plots vs IPRED, TIME
wres.vs.pred(myXpdb)
wres.vs.idv(myXpdb)
wres.dist.hist(myXpdb)
wres.dist.qq(myXpdb)

## Individual plots of observed data, PRED, IPRED
ind.plots(myXpdb)

## Plots of parameters, ETAs
parm.splom(myXpdb)
ranpar.splom(myXpdb)
parm.hist(myXpdb)
ranpar.hist(myXpdb)
parm.qq(myXpdb)
ranpar.qq(myXpdb)
ranpar.vs.cov(myXpdb)

