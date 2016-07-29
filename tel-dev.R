################################################################################
# Copyright (C) 2016 Mango Business Solutions Ltd, http://www.mango-solutions.com
#
# This program is free software: you can redistribute it and/or modify it under
# the terms of the GNU Affero General Public License as published by the
# Free Software Foundation, version 3.
#
# This program is distributed in the hope that it will be useful,
# but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY
# or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License
# for more details.
#
# You should have received a copy of the GNU Affero General Public License along
# with this program. If not, see <http://www.gnu.org/licenses/agpl-3.0.html>.
################################################################################

# Usage:
#
# Source this file from the test script to run tests from R IDE.
# N.B. add 'source' function call after any rm(list=ls()) calls.
#

##
## Load in the R files so that the workflow functions can be modified on-the-fly,
## rather than having to re-build the DDMoRE package every time a change is made.
##

rm(list=ls())

reqdLibs <- c('rjson','RCurl','RNMImport','XML','stringr', 'testthat')

# Unload libraries if they're already loaded, this is required because
# loading in the XML library messes up the environment that source()
# receives, namely that sys.frame(1)$ofile becomes undefined.
# TODO: This still doesn't work quite right, need to re-load tel-dev.R twice
sapply(search()[ search() %in% paste0('package:',reqdLibs) ],
	function(x) { print(x); detach(x, character.only=TRUE) }
)

# Uncomment to see the environment (whether $ofile is defined or not)
#print(ls(envir=sys.frame(1)))

if (is.null(sys.frame(1)$ofile)) {
	stop("There is a known intermittent problem loading in the R scripts after loading in libraries; try re-running that source() command again.")
}

location <- normalizePath(dirname(file.path(sys.frame(1)$ofile)))
print(sprintf("DDMoRe TEL R project root: %s", location))
scripts.dir <- paste0(location, "/ddmore.TEL/R/")
libPath <- file.path(location, ".lib")
print(sprintf("DDMoRe R source files location: %s", scripts.dir))
print(sprintf("DDMoRe R libraries directory: %s", libPath))
.libPaths(libPath)
          
script.files = c(
    "Classes", "createMogObj",
    "utils", "import", "execute", "conversion", "readDataObj", "updateParObj",
    "getDataObjects", "getParameterObjects", "getDesignObjects", "getPopulationParameters", "getModelObjects", "getTaskPropertiesObjects", "getMDLObjects",
    "FISServer", "FISJob", "server", "startingValues", "jobExecution", "psnWrappers",
	"StandardOutputObjectXmlParsers", "StandardOutputObjectCommonClasses",
	"Estimation-Class", "Simulation-Class", "OptimalDesign-Class", "ModelDiagnostic-Class", "RawResults-Class", "TaskInformation-Class",
	"StandardOutputObject-Class", "StandardOutputObjectDataFunctions", "LoadSOObject", "pfim"
)

sapply(script.files, function(s) {
    script.file <- paste0(scripts.dir, s, ".R");
    message(paste("Loading in", script.file, "..."))
    source(script.file)
})

# Load required libraries
sapply(reqdLibs,
	function(x) { library(x, character.only=TRUE) }
)

message("\n\nSuccessfully loaded the R scripts.")

