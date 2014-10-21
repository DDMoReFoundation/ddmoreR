





# #' LoadPharmML
# #'
# setGeneric(name="LoadPharmML",
#            def=function(theObject,fileHandle)
#            {
#                standardGeneric("LoadPharmML")
#            }
# )


# #' @rdname getModelObjects-methods
# #' @aliases getModelObjects,mogObj,mogObj-method
# setMethod(f="setCoordinate",
#                       signature="FirstQuadrant",
#                       definition=function(theObject,xVal,yVal)
#                       {
#                               theObject@x <- xVal
#                               theObject@y <- yVal
#                               return(theObject)
#                       }
#                       )

# # #' 
# # #'
# # #' @include StandardOutputClasses.R
# # setGeneric("getTaskPropertiesObjects", function(x, name, HOST='localhost', PORT='9010') { 
# #   # create object in R from parser:
# #   res <- .parseMDLFile(x, type="taskobj", HOST=HOST, PORT=PORT)
  
# #   return(res)
# # })


# # #' @rdname getTaskPropertiesObjects-methods
# # #' @aliases getTaskPropertiesObjects,mogObj,mogObj-method
# # setMethod("getTaskPropertiesObjects", signature=signature(x="mogObj"), function(x){
# #    return(x@taskObj)
# # })