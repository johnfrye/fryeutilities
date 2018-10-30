#' findtext
#' @keywords find min or max position of a text string within a character vector
#' @param vec_name vector to search within
#' @param string text to locate, in quotes
#' @param sel supports either 'min' or 'max
#' @export
findtext <- function(vec_name, string, sel = 'max'){
  a <- str_locate_all(vec_name, string)
  if(sel == 'max') b <- sapply(a, function(x) max(x))
  if(sel == 'min') b <- sapply(a, function(x) min(x))
  b
}