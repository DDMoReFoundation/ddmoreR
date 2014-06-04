require(DDMoRe.TEL)

cmdArgs <- commandArgs(TRUE)
if(length(cmdArgs) >0 ) doXML <- cmdArgs else doXML <- FALSE

doXML <- if(tolower(doXML) == "xmlreport") doXML <- TRUE else doXML <- FALSE

###########
# Uncomment the following when we have unit tests to run
#
#unitResults <- runDDMoRe.TELTests()
#
#if(doXML) 
#{
#	
#	source("printXMLProtocol.R")
#	printXMLProtocol(unitResults, "DDMoRe.TEL_internalunit.xml")
#}
