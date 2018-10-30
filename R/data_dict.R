#' build an initial data dictionary for a dataframe
#'
#' @param df dataframe whose columns you would like to list and class
#' @importFrom dplyr mutate
#' @export
#' @examples
#' df <- mtcars
#' dd <- data_dict(df)
#' dd
data_dict <- function(df){
  vartable <- tibble(index = as.integer(1:ncol(df)), 
                     variable = names(df),
                     verbose_name = gsub("_", " ", variable),
                     type = sapply(df, class)) %>% 
    mutate(verbose_name = sttt(verbose_name))
}