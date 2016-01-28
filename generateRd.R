library(utils)
library(methods)
library(roxygen2)
cmds <- commandArgs(TRUE)
pkgp <- cmds[1]
# avoid collate_roclet() - topological sort, very slow
# avoid namespace_roclet() - we need to export everything
roxygenize(package.dir = pkgp,
  #roxygen.dir = pkgp,
  roclets = c('rd', 'namespace'))
