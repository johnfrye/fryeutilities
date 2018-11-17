#' update_worksheetfunctions - Update, Compile, and Re-Install the 'WorksheetFunctions' package.
#' @keywords WorksheetFunctions, xlsx
#' @examples
#' library(WorksheetFunctions)
#' update_worksheetfunctions()
#'@export
update_worksheetfunctions <- function() {
  rm(list=ls())
  library("devtools")
  unload(pkgload::inst("WorksheetFunctions"))
  wd.orig <- getwd()
  (wd <- path.expand("~"))
  wd
  setwd(wd)
  setwd("./WorksheetFunctions")
  document()
  setwd("..")
  install("WorksheetFunctions")
  library("WorksheetFunctions")
  setwd(wd.orig)
}
