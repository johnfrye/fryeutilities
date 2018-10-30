#' create_cal
#' @keywords create df of monthly start and end dates
#' @param start_yr 4-digit integer start year (no quotes needed)
#' @param end_yr 4-digit integer end year (no quotes needed)
#' @importFrom lubridate ymd
#' @importFrom lubridate days
#' @importFrom lubridate ceiling_date
#' @export
#' @examples
#' cal_test <- create_cal(start_yr = 2005, end_yr = 2016)
#' View(cal_test)
#' 
create_cal <- function(start_yr, end_yr){
  last_day <- function(date) {
    lubridate::ceiling_date(date, "month") - lubridate::days(1)
  }
  
  yrs <- seq(start_yr, end_yr, 1)
  yrs12 <- rep(yrs, 12)
  yrs12 <- sort(yrs12)
  months <- seq(1:12)
  month_beg <- paste(yrs12, months, "01", sep="-")
  
  cal <- dff(month_start = lubridate::ymd(month_beg)) %>% 
    mutate(month_end = last_day(month_start))
}