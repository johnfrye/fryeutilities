#' standardize column names for a dataframe
#'
#' @param df dataframe whose colnames you want to standardize
#' @export
#' @examples
#' # Handling of periods in names:
#' # Assume that a period should be replaced by an underscore
#' # to preserve whatever separation of words was intended
#' # by the original inclusion of periods,  Don't
#' # want to simply remove them, but they're inconvenient.
#' # Replace periods with an underscore:
#' df <- mtcars
#' head(df)
#' names(df) <- paste0("_.", names(df), ")")
#' head(df)
#' df2 <- df %>% 
#' fixnames2()
#' names(df2)

fixnames2 <- function(df){
  df2 <- df %>% 
    setNames(nm = fixnames(df))
  df2
} 
  