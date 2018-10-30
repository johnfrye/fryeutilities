#' Format list name and save path
#' 
#' @param list_name Desired list name, no trailing spaces required
#' @param full_listname List name with extension (as returned by listname function)
#' @export
listname <- function(list_name, time=time_stamp()) {
  res_name <- paste0(list_name, " ", time, ".rds")
}

respath <- function(full_listname) {
  res_path <- fp(dd, full_listname)
}