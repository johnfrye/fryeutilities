#' Create a Basic Table DF (tbl_df)
#' 
#' @keywords make results dataframe
#' @param fill_value Default value for all data frame 'cells'
#' @param nrows Number of rows desired
#' @param ncols Number of columns desired
#' @importFrom dplyr tbl_df
#' @export
#' @examples
#' create_tbl_df(0, 1, 5)

create_tbl_df <- function(fill_value, nrows, ncols) {
  results <- tbl_df(as.data.frame(matrix(fill_value, nrow = nrows, ncol = ncols)))
  return(results)
}