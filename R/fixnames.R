#' standardize column names for a dataframe
#'
#' @param df dataframe whose colnames you want to standardize
#' @export
#' @examples
#' # Handling of periods in names:
#' # Assume that a period should be replaced by an underscore
#' # to preserve whatever separation of words was intended
#' # by the original inclusion of periods,  Don't
#' # want to simply remove them, but they're inconvenient.
#' # Replace periods with an underscore:
#' df <- mtcars
#' head(df)
#' names(df) <- paste0("_.", names(df), ")")
#' head(df)
#' names(df) <- fixnames(df)
#' head(df)
#' df
fixnames <- function(df){
  names_orig <- trimws(names(df))
  new_names <- names_orig
  new_names <- tolower(new_names)
  new_names <- gsub("\r", "_", new_names, fixed = TRUE) #remove newlines
  new_names <- gsub("[.]+", "_", new_names) # convert 1+ periods to single _
  new_names <- gsub("\"", "", new_names) # remove quotation marks
  new_names <- gsub("\n", "_", new_names, fixed = TRUE)
  new_names <- gsub(" ", "_", new_names, fixed = TRUE)
  new_names <- gsub("-", "_", new_names, fixed = TRUE)
  new_names <- gsub(".", "", new_names, fixed = TRUE)
  new_names <- gsub("'", "", new_names, fixed = TRUE)
  new_names <- gsub(",", "_", new_names, fixed = TRUE)
  new_names <- gsub("%", "percent", new_names, fixed = TRUE)
  new_names <- gsub("+", "_plus", new_names, fixed = TRUE)
  new_names <- gsub(":", "_", new_names, fixed = TRUE)
  new_names <- gsub("#", "number_", new_names, fixed = TRUE)
  new_names <- gsub("?", "", new_names, fixed = TRUE)
  new_names <- gsub("(", "", new_names, fixed = TRUE)
  new_names <- gsub(")", "", new_names, fixed = TRUE)
  new_names <- gsub("/", "_", new_names, fixed = TRUE)
  new_names <- gsub("[_]+", "_", new_names) # fix cases of multiple consecutive underscores
  # mechanical way of dropping leading and trailing underscores
  for (i in 1:length(new_names)){
    sub1 <- str_sub(new_names[i], 2, (nchar(new_names[i])))
    sub2 <- str_sub(new_names[i], 1, nchar(new_names[i]) -1)
    new_names[i] <- ifelse(str_sub(new_names[i], 1, 1) == "_", sub1, new_names[i])
    new_names[i] <- ifelse(str_sub(new_names[i], -1, -1) == "_", sub2, new_names[i])
  }
  new_names <- trimws(new_names)
  # Handle duplicated names (this code is from 'janitor' package)
  # This appends the column number to repeated instances of duplicate variable names
  dupe_count <- sapply(1:length(new_names), function(i) { sum(new_names[i] == new_names[1:i]) })
  new_names[dupe_count > 1] <- paste(new_names[dupe_count > 1],
                                     dupe_count[dupe_count > 1],
                                     sep = "_")
  new_names <- trimws(new_names)
  names(df) <- new_names
}