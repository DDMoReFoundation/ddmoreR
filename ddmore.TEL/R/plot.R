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

##############################################################
#' plot a mogObj object
#'
#' Plots the data contained in the data file referenced within the \code{dataObj}
#' object within the specified object of class \code{mogObj}. Delegates to
#' \link{plot.dataObj}.
#' 
#' @seealso \code{plot.dataObj}
#' 
#' @include Classes.R
#' @include utils.R
#' @method plot mogObj
#' @export
#' @rdname plot.mogObj
#' @aliases plot,mogObj,mogObj-method

plot.mogObj <-
  function(object, by, group, IDVVar="IDV", sourceDir=getwd(), deriveVariables=FALSE, 
    categoricalAsFactor=FALSE, recode=FALSE, ...) {

  # Extract out dataObj:
  obj <- object@dataObj
  
  # Then call the dataObj method
  plot(obj, by, group, IDVVar, sourceDir, deriveVariables, categoricalAsFactor, recode, ... )

}

##############################################################
#' plot a dataObj object
#' 
#' Plots the data contained in the data file referenced within the specified \code{dataObj}.
#' The DATA_INPUT_VARIABLES information from the \code{dataObj} is used to define the
#' dependent and independent variables.
#' 
#' Most of the options take their default values from xpose.data object but may be
#' overridden by supplying them as arguments.
#'
#' @usage plot(object, by, group, sourceDir=getwd(), deriveVariables=TRUE, categoricalAsFactor=TRUE, recode=TRUE, ...)
#'
#' @param object Object of class \code{dataObj}.
#' @param by (Optional) single character naming data variable in object. Defines the 
#' conditioning for each panel in the plot.
#' @param group (Optional) single character naming grouping variable within each panel, usually varying 
#' graphical parameters for each level of the grouping variable.
#' @param IDVVar (Optional) Character string denoting data column which is the independent
#' variable. Defaults to "IDV".
#' @param sourceDir (Optional) The directory in which the referenced data file is to be found.
#' Defaults to the current working directory.
#' @param deriveVariables (Optional) See \link{read}
#' @param categoricalAsFactor (Optional) See \link{read}
#' @param recode (Optional) See \link{read}
#' @param ... Other arguments to be passed through to the plotting function 
#' (lattice graphics options).
#'
#' @return If the result of the \code{plot()} is not assigned to a variable then the plot of
#' dependent variable vs independent variable (as defined in the \code{dataObj}) is
#' displayed on the default graphics device. Otherwise if the result of the \code{plot()}
#' is assigned to a variable then this variable will be a suitably populated object of
#' class \code{trellis} as returned from \code{xyplot()}.
#' 
#' @details A wide array of extra options controlling \code{xyplot} are available. See 
#' \code{xpose.plot.default} and \code{xpose.panel.default} for details.
#'
#' @examples
#' ## Create an S4 object of class mclobj.
#' ThamMDLObjects <- getMDLObjects("2008ThamJCCR.mdl",
#'                   names=c("tumour_size_dat","tumour_size_par","tumour_size_mdl","tumour_size_task"))
#'
#' myThamMOG <- as.mogObj(ThamMDLObjects)
#' plot(myThamMOG)
#' 
#' @seealso \code{readDataObj}, \code{xyplot}
#' 
#' @include Classes.R
#' @include utils.R
#' @method plot dataObj
#' @export
#' @importFrom lattice xyplot
#' @rdname plot.dataObj
#' @aliases plot,dataObj,dataObj-method

plot.dataObj <-
  function(object, by, group, IDVVar = "IDV", sourceDir = getwd(), deriveVariables = FALSE, 
    categoricalAsFactor = FALSE, recode = FALSE, ...) {
  
  validObject(object)
  # First, read in the data:
  dat <- readDataObj(object, sourceDir, deriveVariables, categoricalAsFactor, recode, ...)
  
  nam <- names(dat)
  
  # Create a formula string to evaluate
  cmd <- paste("DV", IDVVar, sep="~")
  
  # Function requires columns called DV and IDV, and will error if not:
  if (!"DV" %in% nam) {
    stop("Column DV is not present in the data set.")
  }
  if (!IDVVar %in% nam) {
    stop("Column specified by IDVVar argument is not present in the data set.")
  }
 
  if (!missing(by)) {
    if (!by %in% nam) {
      stop("Column specified by 'by' argument is not present in the data set.")
    }
    # If by variable included, add to formula string for evaluation
    cmd <- paste(cmd, "|", by)
  }
  
  if (!missing(group)) {
    if (!group %in% nam) {
      stop("Column specified by 'group' argument is not present in the data set.")
    }
    xyplot(formula(cmd), group = dat[,group], data = dat, ...)
  } else {
    xyplot(formula(cmd), data=dat, ...)
  }
}

