#' find data frame column names containing specific text
#'
#' @param df dataframe whose colnames you want to check
#' @param text text to match on
#' @export

names_with <- function(df, text){
  vars_sel <- names(df)[grep(text, names(df), ignore.case = TRUE)]
}