#' fileTable
#' @keywords create a table of files in a folder with file dates (from filename)
#' @param df dataframe suitable for conversion to correlation matrix
#' @export
#' @examples
#' df <- mtcars[, 1:7]
#' corrtable(df)
corrtable <- function(df) {
  ## Correlation matrix with p-values. See http://goo.gl/nahmV for documentation of this function
  cor.prob <- function (X, dfr = nrow(X) - 2) {
    R <- cor(X, use="pairwise.complete.obs")
    above <- row(R) < col(R)
    r2 <- R[above]^2
    Fstat <- r2 * dfr/(1 - r2)
    R[above] <- 1 - pf(Fstat, 1, dfr)
    R[row(R) == col(R)] <- NA
    R
  }
  
  flattenSquareMatrix <- function(m) {
    if( (class(m) != "matrix") | (nrow(m) != ncol(m))) stop("Must be a square matrix.") 
    if(!identical(rownames(m), colnames(m))) stop("Row and column names must be equal.")
    ut <- upper.tri(m)
    data.frame(i = rownames(m)[row(m)[ut]],
               j = rownames(m)[col(m)[ut]],
               cor=t(m)[ut],
               p=m[ut])
  }
  
  # "flatten" that table
  corrtable <- flattenSquareMatrix(cor.prob(df))
  
  corrtable2 <- corrtable %>% 
    rename(j = i,
           i = j) %>% 
    select(i, j, cor, p) %>% 
    rbind(corrtable) %>% 
    distinct() %>% 
    arrange(i, desc(cor))
}
