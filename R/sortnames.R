#' display sorted column names for a dataframe
#'
#' @param df dataframe whose colnames you want to see sorted
#' @export

sortnames <- function(df){
nm <- sort(names(df))
nm
}