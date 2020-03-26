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

data_dict_na <- function(df){
  na_table <- dff(natable2(df)) %>% 
    mutate(variable = as.character(variable))
  
  data_dictionary <- dff(data_dict(df)) %>% 
    arrange(variable) %>% 
    mutate(variable = as.character(variable)) %>% 
    left_join(na_table, by = 'variable')
}