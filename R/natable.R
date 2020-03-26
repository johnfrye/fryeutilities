#' Summarize Number of NAs by Variable
#'
#' @param df data frame from which you want the NA table
#' @export
#' @examples
#' natable(mtcars)
natable <- function (df) {
  vars <- names(df)
  na_count <- sapply(df[vars], function(x) sum(is.na(x)))
  nact <- data.frame(na_count = na_count)
  nact <- data.frame(variable = rownames(nact), nact)
  rownames(nact) <- NULL
  View(nact)
  missing <- which(nact$na_count > 0)
  if (length(missing) > 0){
    missing <- nact[missing, ]
    return(missing)
  }
  if (length(missing) == 0) print("No Missing Values in Data Frame")
}

natable2 <- function (df) {
  vars <- names(df)
  na_count <- sapply(df[vars], function(x) sum(is.na(x)))
  nact <- data.frame(na_count = na_count)
  nact <- data.frame(variable = rownames(nact), nact) %>% 
    mutate(row_count = nrow(df)) %>% 
    mutate(pct_missing = round(na_count*100/row_count, 1))
  rownames(nact) <- NULL
  missing <- which(nact$na_count > 0)
  if (length(missing) > 0){
    missing <- nact[missing, ]
    # return(missing)
  }
  if (length(missing) == 0) print("No Missing Values in Data Frame")
  return(nact)
}