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


#' as.mdl
#' 
#' Translates NMTRAN to MDL using an online implementation of nt2mdl by Nick Holford. Writes file to current working directory and returns MDL file name.
#' 
#' @param nmFilePath A local file path for an NMTRAN control file to be translated. No default.
#' @param url The URL of the translation service form. Default is "http://nmtran-to-mdl.mango-solutions.com/process.php"
#' @param mdlHelpComments If MDL help comments are to be included in the output set to TRUE. Default is FASLE.
#' @param nmtranComments If original NMTRAN in comments are to be included in the output set to TRUE. Default is FASLE.
#' 
#' @export
as.mdl <- function(nmFilePath, url = "http://nmtran-to-mdl.mango-solutions.com/process.php", 
                   mdlHelpComments = FALSE, nmtranComments = FALSE){
  
  # Checking to make sure that R can find the input file
  .precondition.checkArgument(file.exists(nmFilePath), "nmFilePath", 
                              sprintf("NMTRAN file %s must exist.", nmFilePath))

  # Based on the parameters the checkbox options for the form are set to Yes or No 
  mdlHelpCommentsOpt <- ifelse(mdlHelpComments, "Yes", "No")
  nmtranCommentsOpt <- ifelse(nmtranComments, "Yes", "No")
  
  # Stripping the NM control file name and defining the MDL file name
  nmFileName <- basename(as.character(nmFilePath))
  nmFileNameNoExt <- sub("\\.[[:alnum:]]+$", "", nmFileName)
  mdlFileName <- paste0(nmFileNameNoExt, ".mdl")
  
  # Using RCurl to post the form
  postFormResult <- postForm(uri = url, file = fileUpload(filename = nmFilePath), 
                             mdl = mdlHelpCommentsOpt, nmtran = nmtranCommentsOpt, 
                             .checkParams = FALSE)

  # Some regular expression to extract the contents of the <textarea> in the returned HTML
  mdl <- gsub("</textarea.+", "", postFormResult)
  mdl <- gsub(".+<textarea id='code' name='code'>", "", mdl)

  # Write out the MDL text to file. 
  writeLines(text = mdl, con = mdlFileName)
  
  # Check to make sure that R can find the output file
  if (!file.exists(mdlFileName)) {
    stop("Failed to convert NMTRAN to MDL; expected MDL output file \"", 
         mdlFileName, "\" does not exist.")
  }
  
  # Get the absolute path and return it
  mdlFilePath <- file_path_as_absolute(mdlFileName)
  return(mdlFilePath)
}

