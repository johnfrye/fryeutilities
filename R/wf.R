#' write dataframe to feather file
#'
#' @param df dataframe you want to save as a feather file
#' @param dfname name for the saved file, in quotes, without an extension
#' @param directory directory to save file to
#' @export
#' @examples
#' library(smutilities)
#' df <- iris
#' fd <- here::here("feather")
#' wf(df, "iris", fd)

wf <- function(df, dfname, directory = here::here("feather")){
  directory <- here::here("feather")
  (filename <- paste0(dfname, ".feather"))
  feather::write_feather(df, fp(directory, filename))
}


