#' Manage NA Values
#'
#' @param df the data frame you're working with
#' @param na_val what should NAs be changed to? (e.g. 0, "", NA)
#' @keywords project workflow
#' 
setNA <- function (df, na_val) {
  df[is.na(df)] <- na_val
}
