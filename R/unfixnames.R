#' prepare column names for presentation
#'
#' @param df dataframe whose colnames you want to standardize
#' @export
#' @examples
#' library(smutilities)
#' df <- iris
#' names(df)
#' names(df) <- fixnames(df)
#' names(df)
#' names(df) <- unfixnames(df)
#' names(df)

unfixnames <- function(df){
  names_orig <- trimws(names(df))
  new_names <- names_orig
  new_names <- gsub("_", " ", new_names)
  new_names <- sttt(new_names)
  names(df) <- new_names
}


