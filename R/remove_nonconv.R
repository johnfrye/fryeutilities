#' Remove Nonconvertible Text
#'
#' @param df a dataframe
#' @param column.name name of the column to process, in quotes

remove_nonconv <- function(df, column.name) {
  # arg 'sub' = character string. If not NA, 
  # it is used to replace any non-convertible bytes in the input
  chg.col <- which(colnames(df)==column.name)
  nonconvertible.text.sub <- "nonconvtext"
  df[, chg.col] <- iconv(df[, chg.col], from="latin1", to="ASCII", 
                         sub=nonconvertible.text.sub)

  # replace non-ASCII characters with blank spaces
  df[, chg.col] <- gsub(nonconvertible.text.sub, "", df[, chg.col])
  return(df)
}