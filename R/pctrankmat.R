#' Create a percent-ranked data frame from 
#' a subset of columns of an existing data frame
#' @param df A data frame with one or more numeric columns
#' @param begin The first numeric column.  All columns beyond this one must be numeric.
#' @keywords numdigits The number of significant digits for use in rounding the results
#'
pctrankmat <- function(df, begin, numdigits){
  end <- ncol(df)
  if(begin==end){
    X1 <- data.frame(df[, begin])
    colnames(X1) <- colnames(df)[begin]
  }else {
    X1 <- df[, c(begin:end)]
  } 
  X2 <- data.frame(apply(X1, 2, FUN = percent_rank))
  X2 <- round(X2 *100, digits = numdigits)
  colnames(X2) <- paste0(colnames(X2), "_pctrank")
  
  if(begin==end){
    X3 <- cbind(df, X2)
  }else {
    X3 <- cbind(df[, c(1:(begin-1))], X2)
  } 
  return(X3)
}