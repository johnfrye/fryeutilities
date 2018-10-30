#' shortcuts
#' @keywords shortcuts
#' @param df a dataframe
#' @export

s <- base::summary
h <- utils::head
dft <- tibble::as_data_frame
dff <- base::data.frame
tdf <- dplyr::tbl_df
sttt <- stringi::stri_trans_totitle
sttl <- stringi::stri_trans_tolower
sttu <- stringi::stri_trans_toupper
fp <- base::file.path


# ht==headtail, i.e., show the first and last 10 items of an object
ht <- function(df) rbind(head(df, 10), tail(df, 10))
