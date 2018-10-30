#' build an detailed data dictionary with summary statistics
#'
#' @param df dataframe whose columns you would like to summarize
#' @importFrom dplyr mutate
#' @export
#' @examples
#' df <- skim_sum(mtcars)
#' df
skim_sum_levels <- function(df){
  data_dict <- data_dict(df) %>% 
    select(-type)
  
  skim_df <- dff(skimr::skim(df))
  
  skim_hist <- skim_df %>% 
    filter(stat == 'hist') %>% 
    dplyr::rename(histogram = formatted) %>% 
    select(variable, histogram)
  
  skim_tbl0 <- skim_df %>%
    filter(stat != 'hist') %>% 
    select(variable, type, level, stat, value) %>% 
    spread(stat, value) %>% 
    left_join(skim_hist, by = "variable") %>% 
    mutate(mean = round(mean, 2),
           sd = round(sd, 2)) %>% 
    left_join(data_dict, by = "variable") %>% 
    mutate(percent_complete = round((complete * 100)/n, 2)) %>% 
    select(index, variable, verbose_name, type, level, n, complete,
           missing, percent_complete, mean, p0, p25, p50, p75, p100,
           sd, histogram) %>% 
    arrange(index)
  
  ind_col <- grep('index', names(skim_tbl0), ignore.case = TRUE)
  orig_name <- names(skim_tbl0)[ind_col]
  names(skim_tbl0)[ind_col] <- 'Index'
  skim_tbl <- skim_tbl0 %>%
    arrange(Index)
  names(skim_tbl)[ind_col] <- orig_name
  skim_tbl
}

skim_sum <- function(df){
  data_dict <- data_dict(df) %>% 
    select(-type)
  
  skim_df <- dff(skimr::skim(df))
  
  skim_hist <- skim_df %>% 
    filter(stat == 'hist') %>% 
    dplyr::rename(histogram = formatted) %>% 
    select(variable, histogram)
  
  skim_tbl0 <- skim_df %>%
    filter(!stat %in% c('count', 'hist')) %>% 
    select(variable, type, stat, value) %>% 
    spread(stat, value) %>% 
    left_join(skim_hist, by = "variable") %>% 
    mutate(mean = round(mean, 2),
           sd = round(sd, 2)) %>% 
    left_join(data_dict, by = "variable") %>% 
    mutate(percent_complete = round((complete * 100)/n, 2)) %>% 
    select(index, variable, verbose_name, type, n, complete,
           missing, percent_complete, mean, p0, p25, p50, p75, p100,
           sd, histogram) %>% 
    arrange(index)
  
  ind_col <- grep('index', names(skim_tbl0), ignore.case = TRUE)
  orig_name <- names(skim_tbl0)[ind_col]
  names(skim_tbl0)[ind_col] <- 'Index'
  skim_tbl <- skim_tbl0 %>%
    arrange(Index)
  names(skim_tbl)[ind_col] <- orig_name
  skim_tbl
}
