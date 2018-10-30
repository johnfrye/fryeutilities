#' Move a set of columns to the left-hand side of a data frame
#' 
#' @param df A data frame
#' @param refcols A vector of the column names you would like to move to the left side of the dataframe (in order)
#' @export
arrange_cols <- function(df=df, refcols=refcols){
  df <- df[, c(refcols, setdiff(names(df), refcols))]
}