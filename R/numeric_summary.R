#' numeric_summary
#' @keywords create a table summarizing the numeric columns of a dataframe
#' @param df a dataframe containing at least one numeric column
#' @export
#' @examples
#' df <- numeric_summary(mtcars)
#' head(df)
numeric_summary <- function(df){
  df_numeric <- df[, sapply(df, is.numeric)]
  df2 <- dff(summary(df_numeric))
  a <- stri_locate_all_regex(df2$Freq, ":")
  b <- sapply(a, function(x) max(x))
  df2 <- df2 %>% 
    select(-Var1) %>% 
    rename(Variable = Var2) %>% 
    mutate(statistic = str_sub(Freq, 1, (b-1)),
           statistic = trimws(statistic),
           statistic = gsub("Qu.", "Quartile", statistic),
           statistic = gsub("\\.", "", statistic),
           value = str_sub(Freq, (b+1), nchar(Freq)),
           value = as.numeric(value),
           value = round(value, 1),
           statistic = factor(statistic, 
                              levels = c("Min", "1st Quartile", "Median", "Mean", "3rd Quartile", "Max"))) %>%
    select(-Freq) %>% 
    dcast(formula = Variable~statistic)
} 