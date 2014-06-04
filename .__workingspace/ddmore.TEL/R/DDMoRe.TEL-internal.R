.onAttach <-
function(x,y) {
  TEL.startServer()
}

.onUnload <-
function(x) {
    TEL.safeStop()
  }

.onDetach <-
  function(x) {
    TEL.safeStop()
  }