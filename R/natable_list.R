natable_list <- function (df) {
  vars <- names(df)
  na_count <- sapply(df[vars], function(x) sum(is.na(x)))
  nact <- data.frame(na_count = na_count)
  nact <- data.frame(variable = rownames(nact), nact)
  rownames(nact) <- NULL
  missing <- which(nact$na_count > 0)
  if (length(missing) > 0){
    missing <- nact[missing, ] %>% 
    mutate(rows = nrow(df),
           pct_missing = round(100*na_count/rows, 1)) %>% 
      arrange(desc(pct_missing))
  }
  missing
}