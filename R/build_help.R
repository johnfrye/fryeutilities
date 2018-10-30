#' Build Static HTML Help with knitr
#'
#' @param pkgname package for which you want to build static html help
#' @export
build_help <- function(pkgname) {
  library(knitr)
  wd_orig <- getwd()
  setwd(system.file("html", package = pkgname))
  knit_rd(pkgname)
  setwd(wd_orig)
}