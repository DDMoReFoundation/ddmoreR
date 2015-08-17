##
## Load in the R files so that the workflow functions can be modified on-the-fly,
## rather than having to re-build the DDMoRE.TEL package every time a change is made.
##

reqdLibs <- c('rjson','RCurl','RNMImport','XML','stringr')

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

scripts.dir <- paste0(dirname(sys.frame(1)$ofile), "/ddmore.TEL/R/")

script.files = c(
    "telClasses", "createMogObj",
    "utils", "import", "execute", "conversion", "read", "update",
    "getDataObjects", "getParameterObjects", "getPopulationParameters", "getModelObjects", "getTaskPropertiesObjects", "getMDLObjects",
    "FISServer", "FISJob", "server", "jobExecution", "psnWrappers", "xmlParsers", "StandardOutputSubClasses", "StandardOutputObject",
	"LoadSOObject"
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

