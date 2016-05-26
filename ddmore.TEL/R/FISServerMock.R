

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


