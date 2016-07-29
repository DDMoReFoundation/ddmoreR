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

#' ---cleanup: function to remove NONMEM cruft.
#' Based on Andy Hooker's cleanup.R function https://github.com/andrewhooker/MBAOD/blob/master/R/cleanup.R
cleanUp <- function(workingDir = NULL, pattern = NULL, removeFolders = F, ...) {
  
  wd <- getwd()
  if(!is.null(workingDir)) wd <- file.path(getwd(),workingDir)
  
  print("- Cleaning up..")
  
  # remove old files before new run
  nmFiles <- c("_L", "_R", "INTER", "LINK", "nul", "nmprd4p.mod", "nonmem", "FCON", "FDATA", "FMSG", "fort.6", "FREPORT", "FSIZES", "FSTREAM", 
               "FSUBS", "fsubs.f90", "fsubs.o", "FSUBS.MU.F90", "GFCOMPILE.BAT", "linkc", "nmfe72", "set", "newline", "gfortran", "prsizes", "trash", "compile", 
               "garbage.out", "PRSIZES.f90", "fort.2002", "temporaryfile.xml")
  files <- nmFiles
  if(length(pattern) > 0) files <- pattern
  
  foo <- sapply(files, function(x) unlink(list.files(path=wd,pattern = x, full.names=T,...)	) )
  nFilesRemoved <- sum(foo>0)
  print(paste("-", nFilesRemoved, "files removed")) 
  
  unlink(file.path(getwd(), "temp_dir"), recursive = T)
  
  # remove folders
  if (removeFolders) {
    all <- list.dirs(path=wd, recursive=T,...)
    alldirs <- all[file.info(all)$isdir]
    matchdirs <- alldirs
    if (length(pattern) > 0) 
      matchdirs <- alldirs[grep(pattern, alldirs)]
    unlink(matchdirs, recursive = T,...)
    nDirsRemoved <- length(matchdirs)
    print(paste("-", nDirsRemoved, "directories removed")) 
  }
} 