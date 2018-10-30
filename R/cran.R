#' Install from CRAN
#'
#' @param x package name in quotes
#' @param .libwork work R Library location
#' @keywords package installation
cran <- function (x, .libwork) {
  install.packages(c(x), lib=.libwork, dependencies = TRUE)
}