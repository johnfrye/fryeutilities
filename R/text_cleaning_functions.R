#' String Cleaning Functions
#'
#' @param x character vector
#' 
# remove content inside html tags from string variables
UnTag <- function(x){gsub("<[^>]*>", " ", x)}

# remove leading and trailing spaces from a string
spaceTrim <- function(x){gsub("(^ +)|( +$)", "", x)}
