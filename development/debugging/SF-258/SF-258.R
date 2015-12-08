full_install = TRUE
root = "C:\\Users\\cmusselle\\Projects\\DDmore\\TEL-R"

if ("ddmore" %in% .packages()) {
	detach("package:ddmore", unload=TRUE)
}

setwd(root)
if (full_install) {
	install.packages("C:\\Users\\cmusselle\\Projects\\DDmore\\TEL-R\\.__artefacts/ddmore_0.0.4.tar.gz", repos=NULL, type="source")
} else {
	ddmore = as.package("DDMoRe.TEL")
	# document(ddmore)
	load_all(ddmore)
}

library(testthat)
require(devtools)
require(ddmore)

#----------------------------------------------

# Set file path definitions 
setwd('C:\\see-installer\\SEE\\MDL_IDE\\workspace\\development\\debugging\\SF-258')
mdlfile = 'UseCase2.mdl'

# Examine MDL Object
myMDLObj <- getMDLObjects(mdlfile)

# Run the Model
data_file = "warfarin_conc.csv"

nonmem <- estimate(mdl_file, target="NONMEM", subfolder="Nonmem")

SO_file = "UseCase2.SO.xml"

# Load SO object
DDMORE.setServer(createFISServer(startupScript = ""))


SO = LoadSOObject(SO_file)

df <- as.data(SO, data_file)
xpdb <- as.xpdb(SO, data_file)

out = getPopulationParameters(SO)





