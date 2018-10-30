#' standardize text within columns of a dataframe
#'
#' @param df dataframe whose column text you want to standardize
#' @export
#' @examples

fixtext <- function(df, colnames){
  for (i in 1:length(colnames)){
    x <- df[, colname[i]]
    x = trimws(x)
    x = str_replace_all(x, "[[:punct:]]", " ")
    x = sttt(x)
    df[, colname[i]] <- x
  }
  df
}



