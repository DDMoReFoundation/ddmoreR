# Course TEL workflow

#set working directory
setwd("C:\\Users\\khanley\\Documents\\Projects\\ddmore\\TEL\\training")

## Load TEl package and other useful packages
library(xpose4)
library(lattice)
library(tables)
library(DDMoRe.TEL)

## 

# We can see the functions available in the TEL package:
objects("package:DDMoRe.TEL")

# We have the 3 example mdl files available in our working directory:
list.files()

# Parse the MDL and create an R object containig the information:
parsed <- getMDLObjects("tumour_size_25June2014_OAM.mdl")

# Look at the names of the parsed objects
names(parsed)

# We can now extract the objects from the list manually:
tumour_size_ORG_dat <- parsed$tumour_size_ORG_dat
tumour_size_ORG_mdl <- parsed$tumour_size_ORG_mdl
tumour_size_ORG_par <- parsed$tumour_size_ORG_par

# Look at the type of objects:
class(tumour_size_ORG_dat)
class(tumour_size_ORG_mdl)
class(tumour_size_ORG_par)

# We can also just extract the data objects directly from the MDL:
dat <- getDataObjects("tumour_size_25June2014_OAM.mdl")

# This returns a list of objects, so we can extract the first one:
dat <- dat[[1]]
class(dat)

# We can then import the data from the data object, so it can be plotted in R. Note 
# that the .csv file needs to be in the working directory for this to work
myData <- read(dat)
head(myData)

# The data is now loaded into memory in R, so we can use any R functions we like:
mean(myData$AMT)
boxplot(myData$AMT)

xyplot(DV~TIME,groups=ID,data=myData,type="b",ylab="Tumour size",xlab="Time (weeks)")



# We can also import the parameter objects and model objects:
par <- getParameterObjects("tumour_size_25June2014_OAM.mdl")[[1]]
mod <- getModelObjects("tumour_size_25June2014_OAM.mdl")[[1]]

class(par)
class(mod)

# Let's look more closely at the parameter object:
str(par)

# There are 3 "slots" in the object: STRUCTURAL, VARIABILITY and "PRIOR"
# We can update this object using the "update" function:

parUpdated <- update(par, block="STRUCTURAL", type="POP_SIZE0", with=list(value=99999))

# Once we have updated this, we can create a MOG. A MOG requires one object only of
# type dataObj, parObj, taskObj and mdlObj. Currently we do not have a method of
# extracting task properties objects from MDL code, so for now we can create one manually. 
# This would not usually be required, as we will be able to import directly from the 
# mdl

taskObj1 <- new("taskObj", 
  IMPORT = list(nmadvan = list(target = "NMTRAN_CODE", name="ADVAN", 
    param=list(model=0,trans=0, ncmt=0))),
  DATA = list(),
  PARAMETER = list(),
  MODEL = list(tolrel=9),
  TASK_FUNCTION = list(),
  TARGET_CODE = list("$EST MAX=9990 NSIG=3 SIGL=9 NOABORT PRINT=1
METHOD=CONDITIONAL INTERACTION
MSFO=SIZE.MSF
$COV")
)

class(taskObj1)

# Now we have all 4 objects, we can create a MOG:
myMog <- createMogObj(dataObj = dat, taskObj = taskObj1, mdlObj = mod, parObj = par)


################## If the write method is finished:#############################
# We can then write the MOG back out to MDL:
writeMcl(myMog, file="myMog.mdl")

# We can then execute this mdl file using NONMEM:
results <- estimate("myMog.mdl", target="NONMEM")

# Alternatively, we can execute the MOG directly without creating the mdl file:
results <- estimate(myMog, target="NONMEM")
################################################################################

# We can execute an mdl file directly:
results <- estimate("tumour_size_25June2014_OAM.mdl", target="NONMEM")


# For now, the results received will be in the form of an NMRun object 
# (from the RNMImport package). In the future, a standardised object will be 
# created which is compatible with other software (e.g. BUGS, PsN)
class(results)

# As part of the TEL package, there will be many functions for working with the 
# standardised output object. These functions are currently in development, but as
# an example we can use the getEstimationInfo function:
getEstimationInfo(results)






