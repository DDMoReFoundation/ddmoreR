
# https://sourceforge.net/p/ddmore/tickets/425/

rm(list=ls())
full_install = FALSE
root = "C:\\Users\\cmusselle\\Projects\\DDmore\\TEL-R"

if ("DDMoRe" %in% .packages()) {
  detach("package:DDMoRe", unload=TRUE)
} 

setwd(root)
if (full_install) {
  install.packages("C:\\Users\\cmusselle\\Projects\\DDmore\\TEL-R\\.__artefacts/DDMoRe_0.0.4.tar.gz", repos=NULL, type="source")
} else {
  ddmore = as.package("DDMoRe")
  # document(ddmore)
  load_all(ddmore)
}

require(DDMoRe.TEL)

# --------------------------

# file path definitions 
setwd('C:\\Users/cmusselle/Downloads/')
SO_file = "UseCase3.SO.xml"
mdl_file = 'C:\\Users/cmusselle/Downloads/UseCase3.mdl'
data_file = "warfarin_conc_pca.csv"

# Load SO object
SO = LoadSOObject(SO_file)
df <- as.data(SO, data_file)
xpdb <- as.xpdb(SO, data_file)