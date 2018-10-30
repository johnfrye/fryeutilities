#' fileTable
#' @keywords create a table of files in a folder with file dates (from filename)
#' @param folderpath the directory to search, defaults to dd
#' @param search.pattern a string indicating search qualifier
#' @export
#' @examples
#' ft <- fileTable("~")
#' ft
fileTable <- function (fpath = dd, search.pattern = "") {
  oldw <- getOption("warn")
  options(warn = -1)
  files <- tbl_df(data.frame(file_path = list.files(fpath, 
                                                    search.pattern, 
                                                    full.names = TRUE))) %>% 
    mutate(file_path = as.character(file_path),
           file_name = list.files(fpath, search.pattern, full.names = FALSE))
  
  a <- stringr::str_locate_all(files$file_path, "\\.")
  b <- sapply(a, function(x) max(x))
  
  files2 <- files %>%        
    mutate(file_extension = stringr::str_sub(file_path, b + 1, nchar(file_path)),
           last_modified = file.info(file_path)$mtime) %>% 
    arrange(desc(last_modified))
  
  options(warn = oldw)
  files2
}

