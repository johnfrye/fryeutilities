#' save excel worksheet with simple defaults
#'
#' @param df dataframe you want to save in a worksheet
#' @param filename raw filename without datetime or file extension
#' @export


savexl <- function(df, dir = dd, filename){
  fname <- paste0(filename, ' ', time, '.xlsx')
  xlsx::write.xlsx2(df, file = fp(dir, fname))
}