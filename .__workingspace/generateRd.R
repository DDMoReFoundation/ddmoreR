require(utils)
require(methods)
library(roxygen2)
cmds = commandArgs(TRUE)
pkgp = cmds[1]
# avoid collate_roclet() - topological sort, very slow
# avoid namespace_roclet() - we need to export everything
roxygenize(pkgp,pkgp,roclets='rd')
