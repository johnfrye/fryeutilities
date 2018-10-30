#' Update this package from source
#' 
#' @export
update_fryeutilities <- function() {
  rm(list = ls())
  library("devtools")
  unload(pkgload::inst("fryeutilities"))
  wd.orig <- getwd()
  (wd <- path.expand("~"))
  wd
  setwd(wd)
  setwd("./R/Packages/fryeutilities")
  document()
  setwd("..")
  install("fryeutilities")
  library("fryeutilities")
  setwd(wd.orig)
}

