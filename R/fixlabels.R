#' fixlabels:  Standardize a vector of character values for use as dataframe column names
#'
#' @param x character vector whose elements you want to standardize
#' @export
#' @examples
#' varnames <-  dff(variable = names(iris))
#' varnames
#' varnames <- varnames %>% 
#'   mutate(variable = fixlabels(variable))
#' varnames

fixlabels <- function (x) {
  names_orig <- trimws(x)
  new_names <- names_orig
  new_names <- tolower(new_names)
  new_names <- gsub("[.]+", "_", new_names)
  new_names <- gsub("\"", "", new_names)
  new_names <- gsub("\n", "_", new_names)
  new_names <- gsub(" ", "_", new_names)
  new_names <- gsub("'", "", new_names)
  new_names <- gsub(",", "_", new_names)
  new_names <- gsub("%", "percent", new_names)
  new_names <- gsub("\\+", "_plus", new_names)
  new_names <- gsub("\\:", "_", new_names)
  new_names <- gsub("#", "number_", new_names)
  new_names <- gsub("\\?", "", new_names)
  new_names <- gsub("\\(", "", new_names)
  new_names <- gsub("\\)", "", new_names)
  new_names <- gsub("[_]+", "_", new_names)
  for (i in 1:length(new_names)) {
    if (str_sub(new_names[i], 1, 1) == "_") 
      new_names[i] <- str_sub(new_names[i], 2, nchar(new_names[i]))
    if (str_sub(new_names[i], -1, -1) == "_") 
      new_names[i] <- str_sub(new_names[i], 1, nchar(new_names[i] - 
                                                       1))
  }
  dupe_count <- sapply(1:length(new_names), function(i) {
    sum(new_names[i] == new_names[1:i])
  })
  new_names[dupe_count > 1] <- paste(new_names[dupe_count > 
                                                 1], dupe_count[dupe_count > 1], sep = "_")
  x <- new_names
}