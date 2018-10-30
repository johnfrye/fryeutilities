#' Time Stamp Function, Formatted for Titles & Filenames
#'
#' @keywords project workflow, time

time_stamp <- function(){
  time <- Sys.time()
  time <- gsub("\\:", "", time)
  time <- stringr::str_sub(time, start=1, end=nchar(time)-2)
  time <- gsub("-", "_", time)
  time <- gsub(" ", "_", time)
  return(time)
}

#' timestamp formatted yyyy_mm_dd
# time_stamp_form <- function () {
#   time <- time_stamp()
#   date_txt <- stri_sub(time, from=1, to=8)
#   yr_txt <- stri_sub(date_txt, from=1, to=4)
#   mo_txt <- stri_sub(date_txt, from=5, to=6)
#   day_txt <- stri_sub(date_txt, from=7, to=8)
#   date2_txt <- paste(yr_txt, mo_txt, day_txt, sep="_")
#   return(date2_txt)
# }