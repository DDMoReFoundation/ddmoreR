
root = "C:\\Users\\cmusselle\\Projects\\DDmore\\TEL-R"

if ("ddmore" %in% .packages()) {
	detach("package:ddmore", unload=TRUE)
}

setwd(root)

install.packages("C:\\Users\\cmusselle\\Projects\\DDmore\\TEL-R\\.__artefacts/ddmore_0.0.4.tar.gz", repos=NULL, type="source")

require(ddmore)

#----------------------------------------------

# Set file path definitions 
setwd('C:\\see-installer\\SEE\\MDL_IDE\\workspace\\development\\debugging\\SF-428')
mdlfile = 'UseCase2_TIMEchange_fixed.mdl'

# Examine MDL Object
myMDLObj <- getMDLObjects(mdlfile)

# Run the Model
data_file = "warfarin_conc.csv"

nonmem <- estimate(mdl_file, target="NONMEM", subfolder="Nonmem")

SO_file = "UseCase2_TIMEchange_fixed.SO.xml"

# Load SO object
DDMORE.setServer(createFISServer(startupScript = ""))


SO = LoadSOObject(SO_file)

df <- as.data(SO, data_file)
xpdb <- as.xpdb(SO, data_file)

out = getPopulationParameters(SO)




# ---------------------------------

SOObject <- SO
inputDataPath <- data_file

MDLObjs <- ddmore:::.getMdlInfoFromSO(SOObject, what="mdl")

# Find the variable names being used as ID and TIME
input.var.use.definitions = lapply(MDLObjs@DATA_INPUT_VARIABLES, FUN= function(x) {x[['use']]})

ID.index = input.var.use.definitions == "id"
TIME.index = input.var.use.definitions == "idv"

ID.colName = names(input.var.use.definitions[ID.index])
TIME.colName = names(input.var.use.definitions[TIME.index])

if (length(ID.colName) > 1) {
	stop(paste0("Multiple DATA_INPUT_VARIABLES have use defined as 'id' in MDL file, ", 
					"cannot determine correct column name for ID from MDL file. "))
} else if (length(ID.colName) == 0) {
	stop(paste0("No DATA_INPUT_VARIABLES have a 'use' parameter defined as 'id' in the MDL file", 
					"cannot determine correct column name for ID from MDL file."))
}
if (length(TIME.colName) > 1) {
	stop(paste0("Multiple DATA_INPUT_VARIABLES have use defined as 'idv' in MDL file, ", 
					"cannot determine correct column name for TIME from MDL file."))
} else if (length(TIME.colName) == 0) {
	stop(paste0("No DATA_INPUT_VARIABLES have a 'use' parameter defined as 'idv' in the MDL file", 
					"cannot determine correct column name for TIME from MDL file."))
}

# Pass in the rawData file 
colNames <- toupper(names(MDLObjs@DATA_INPUT_VARIABLES))
rawData <- read.NONMEMDataSet(inputDataPath, colNames=colNames)
# Convert all column headers to upper case 
names(rawData) <- toupper(names(rawData))

# Checks for Column format
rawData[[ID.colName]] <- as.numeric(rawData[[ID.colName]]) 
rawData[[TIME.colName]] <- as.numeric(rawData[[TIME.colName]]) 

# Reorder data frame to have ID and TIME column as first two. 
ID.col = names(rawData) == ID.colName
TIME.col = names(rawData) == TIME.colName
remaining.names = setdiff(names(rawData),c(ID.colName,TIME.colName))
rawData = cbind(rawData[, ID.col], 
		rawData[, TIME.col], 
		rawData[, remaining.names],
		deparse.level = 0)
# Update names for first two columns 
names(rawData) <- c(ID.colName, TIME.colName, remaining.names) 

mergedDataFrame <- rawData


# ---------------------------------


















