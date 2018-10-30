#' Remove Links from a String
#'
#' @param df a dataframe
#' @param column.name name of the column to process, in quotes
#' @export

processLinks <- function(x) {
  x2 <- paste0(x, " ")
  link.count <- nrow(data.frame(str_locate_all(x2, "http")))
  if(link.count>0){
    for(i in 1:link.count){
      a <- dff(str_locate(x2, "http"))
      b <- dff(str_locate_all(x2, " "))
      a$endlink <- min(b$start[b$start>a$end])
      x3 <- str_sub(x2, start=0, end=(a$start-1))
      x4 <- str_sub(x2, start=a$endlink, end=nchar(x2))
      x2 <- paste0(x3, x4)
    }
    x <- x2
  }
  x <- trimws(x)
  return(x)
}

#' this next chunk works fine but should be vectorized
removeLinks <- function(df, column.name) {
  chg.col <- which(colnames(df)==column.name)
  for(i in 1:nrow(df)){
    df[, chg.col][i] <- processLinks(df[, chg.col][i])
  }
  return(df)
}
