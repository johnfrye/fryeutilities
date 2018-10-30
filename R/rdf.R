#' read feather file
#'
#' @param dfname name of the file, in quotes, without an extension
#' @param directory directory to read from
#' @export
#' @examples
#' library(smutilities)
#' df <- iris
#' fd <- here::here("feather")
#' wf(df, "iris", fd)
#' df <- rdf("iris", fd)

rdf <- function(dfname, directory = here::here("feather")){
  directory <- here::here("feather")
  (filename <- paste0(dfname, ".feather"))
  feather::read_feather(fp(directory, filename))
}

