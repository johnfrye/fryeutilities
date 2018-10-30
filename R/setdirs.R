#' Set Project Directories
#'
#' @keywords project workflow
#' @export
setdirs <- function () {
  wd <- getwd()
  dirs <- c("analysis", "data", "graphics", "reports")
  ad <- file.path(wd, dirs[1])
  dd <- file.path(wd, dirs[2])
  gd <- file.path(wd, dirs[3])
  rptdir <- file.path(wd, dirs[4])
  return(c(ad, dd, gd, rptdir))
}
