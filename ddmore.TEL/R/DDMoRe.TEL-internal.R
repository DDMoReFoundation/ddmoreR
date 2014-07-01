
# TODO: Reinstate these?
# The problem is that as part of building the package, the package will attempt
# to be loaded. Since the building is not taking place within an SEE environment,
# FIS home and MIF home won't be found, so starting the servers will throw an error,
# and this will cause the build to fail.


.onAttach <-
function(x,y) {
    # TEL.startServer()
}

.onUnload <-
function(x) {
    # TEL.safeStop()
}

.onDetach <-
function(x) {
    # TEL.safeStop()
}
