# Course TEL workflow

# SET THIS: The path (either absolute, or relative to your current directory) to the MDL file
modelFile <- "C:/runtime-mdl-editor/testproj/mdl/tumour/tumour_size_25June2014_OAM.mdl"


## Load TEL package and other useful packages
library(xpose4)
library(lattice)
#library(tables)
library(DDMoRe.TEL)

## 

# We can see the functions available in the TEL package:
objects("package:DDMoRe.TEL")

# We have the 3 example mdl files available in our working directory:
list.files()

# Parse the MDL and create an R object containig the information:
parsed <- getMDLObjects(modelFile)

# Look at the names of the parsed objects
names(parsed)

# We can now extract the objects from the list manually:
tumour_size_ORG_dat <- parsed$tumour_size_ORG_dat
tumour_size_ORG_mdl <- parsed$tumour_size_ORG_mdl
tumour_size_ORG_par <- parsed$tumour_size_ORG_par
tumour_size_ORG_task <- parsed$tumour_size_ORG_task

# Look at the type of objects:
class(tumour_size_ORG_dat)
class(tumour_size_ORG_mdl)
class(tumour_size_ORG_par)
class(tumour_size_ORG_task)

# We can also just extract the data objects directly from the MDL:
dat <- getDataObjects(modelFile)

# This returns a list of objects, so we can extract the first one:
dat <- dat[[1]]
class(dat)

# We can then import the data from the data object, so it can be plotted in R.
# The sourceDir parameter specifies the directory in which the data files live,
# in case they are not within the current directory.
myData <- read(dat, sourceDir=parent.folder(modelFile))
head(myData)

# The data is now loaded into memory in R, so we can use any R functions we like:
mean(myData$AMT)
boxplot(myData$AMT)

xyplot(DV~TIME,groups=ID,data=myData,type="b",ylab="Tumour size",xlab="Time (weeks)")


# We can also import the parameter objects and model objects:
par <- getParameterObjects(modelFile)[[1]]
mod <- getModelObjects(modelFile)[[1]]

class(par)
class(mod)

# Let's look more closely at the parameter object:
str(par)

# There are 3 "slots" in the object: STRUCTURAL, VARIABILITY and "PRIOR"
# We can update this object using the "update" function:

parUpdated <- update(par, block="STRUCTURAL", type="POP_SIZE0", with=list(value=99999))



# Now we have all 4 objects, we can create a MOG:
myMog <- createMogObj(dataObj = dat, mdlObj = mod, parObj = par, taskObj = tumour_size_ORG_task)


# We can then write the MOG back out to MDL:
modelFileModified <- file.path(parent.folder(modelFile), "outputMog")
write(myMog, f=modelFileModified)
# ... we will use this later



# We can execute an mdl file directly:
results1 <- estimate(modelFile, target="NONMEM")


# For now, the results received will be in the form of an NMRun object 
# (from the RNMImport package). In the future, a standardised object will be 
# created which is compatible with other software (e.g. BUGS, PsN)
class(results1)



# We can try executing the amended mdl file that was written out earlier, using NONMEM:
results2 <- estimate(paste0(modelFileModified,".mdl"), target="NONMEM")

# Alternatively, we can execute the MOG directly without creating the MDL file:
# TODO: Commented out at the moment since it currently writes an output.mdl file
# to the current directory which is probably not what is desired
#results3 <- estimate(myMog, target="NONMEM")



# As part of the TEL package, there will be many functions for working with the 
# standardised output object. These functions are currently in development, but as
# an example we can use the getEstimationInfo function:
#getEstimationInfo(results1)

