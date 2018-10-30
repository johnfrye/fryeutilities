#' write_vartable
#' @param df a dataframe with variables as columns and obs as rows
#' @param dd the target save directory.  dd by default
#' @param filename  The filename and file extension, in quotes
#' @keywords variable table
#'
write_vartable <- function(df, dd=dd, filename) {
  vartable <- dff(row.order=as.integer(1:ncol(df)), variable=names(df))
  WriteXLS("vartable", ExcelFileName = file.path(dd, filename), row.names = F,
           AdjWidth = T, BoldHeaderRow = T, col.names = T, FreezeRow = 1)
}