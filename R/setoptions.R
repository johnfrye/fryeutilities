#' Set Default Project Options
#' @keywords project workflow, options
#'
setoptions <- function () {
  rm(list=ls())
  options(scipen=999)
  set.seed=1234
  options(stringsAsFactors=FALSE)
  opar <- par()
  #on.exit(par(opar))
}
