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

# Function setting up a FISService mock
# used for testing only
# @examples
# mockServer <- createMockFISServer(jobStatusPollingDelay = 1)
# suppressWarnings(DDMORE.setServer(mockServer))
# DDMORE.getServer()

setClass(
    "MockFISServer",
    contains = "FISServer"
)
createMockFISServer <-
    function(url = "http://localhost:9010", 
        operationalUrl = "http://localhost:9011",
        startupScript = "MOCK",
        jobStatusPollingDelay = 20, 
        startupPollingMax = 60, 
        startupPollingDelay = 1) {
        new(
            "MockFISServer",
            url = url,
            operationalUrl = operationalUrl,
            startupScript = startupScript,
            jobStatusPollingDelay = jobStatusPollingDelay,
            startupPollingMax = startupPollingMax,
            startupPollingDelay = startupPollingDelay
        )
    }

setMethod("readMDL", signature = signature("MockFISServer"),
    function(fisServer, filePath) {
        path <- sub(x=filePath, pattern="\\.mdl$", replacement=".json")
        message("Trying to use ", path, " as conversion result.")
        con <- file(path, "r")
        json <- readLines(con)
        close(con)
        return(json)
    })


