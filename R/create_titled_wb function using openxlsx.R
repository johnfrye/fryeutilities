#' create_titled_wb - Create a Workbook with Multiple Titled Worksheets Using 'openxlsx'
#' @keywords xlsx
#' @param filename The worksheet name you would like, including .xlsx file extension.
#' @param dataframe_list A list of dataframes (without quotes) created outside the function call.
#' @param titles A vector of worksheet titles, 1 for each worksheet tab.
#' @param tabnames A vector of tabnames, current maximum # tabs supported = 8.
#' @param format_column_labels TRUE or FALSE.
#' @param ws_type "table" or "standard".
#' @param output_path Location where you want the workbook to be saved.
#' @param open_wb If TRUE, open the workbook for review.
#' @examples
#' library(WorksheetFunctions)
#' df1 <- smutilities::create_tbl_df(fill_value=0, nrows=10, ncols=10)
#' df2 <- smutilities::create_tbl_df(fill_value="A2", nrows=10, ncols=10)
#' df3 <- smutilities::create_tbl_df(fill_value="Test", nrows=10, ncols=10)
#' stub_colnames <- rep(c("Test.Name", "Test_Name", "test.Name",
#'                        "test_NAMe", "test.NAME"), 2)
#' stub_colnames <- paste(stub_colnames, 1:length(stub_colnames), sep=" ")
#' names(df1) <- stub_colnames
#' names(df2) <- stub_colnames
#' names(df3) <- stub_colnames
#'
#' dataframes <- list(df1, df2, df3)
#'
#' create_titled_wb(filename = "Test_New",
#'                  dataframe_list = dataframes,
#'                  titles = paste("Title ", 1:length(dataframe_list)),
#'                  tabnames = LETTERS[1:length(dataframe_list)],
#'                  format_column_labels = TRUE,
#'                  ws_type = "table",
#'                  output_path = getwd(),
#'                  open_wb = TRUE,
#'                  overwrite_existing = TRUE)
#' @export

create_titled_wb <- function(filename="Test",
                             dataframe_list,
                             titles=LETTERS[1:length(dataframe_list)],
                             tabnames=LETTERS[1:length(dataframe_list)],
                             format_column_labels=TRUE,
                             ws_type = "table",
                             output_path = getwd(),
                             open_wb = TRUE,
                             overwrite_existing=TRUE){

  time <- time_stamp()
  filename <- paste0(filename, "_", time, ".xlsx")
  num_tabs <- length(dataframe_list)
  wb <- NULL
  wb <- createWorkbook(filename)

  # max colors in this palette is 8.  Could recycle if need more tabs
  tabcolors <- RColorBrewer::brewer.pal(num_tabs, "Set3")

  # define default header style
  hs1 <- createStyle(fontSize = 12, fgFill = "#4F81BD", halign = "center",
                     textDecoration = "Bold", border = "Bottom",
                     wrapText=TRUE, fontColour = "white")

  # create worksheets
  for (i in 1:num_tabs){
    addWorksheet(wb, tabnames[i], tabColour=tabcolors[i])
    if(ws_type == "table"){

      # format data frame
      dt <- data.frame(dataframe_list[i])

      # format column labels (names)
      if(format_column_labels==TRUE){
        names(dt) <- gsub("\\.", " ", names(dt))
        names(dt) <- gsub("\\_", " ", names(dt))
        names(dt) <- stringi::stri_trans_totitle(names(dt))
      }

      writeDataTable(wb, i, dt, startRow = 3, startCol = 1,
                     headerStyle = hs1, tableStyle = "TableStyleLight2")
    }else if(ws_type=="standard"){
      writeData(wb, i, dt, startRow = 3, startCol = 1, borders = "all",
                headerStyle = hs1, borderStyle = "thin")
    }else{
      print(paste0("Worksheet Type Not Specified: Tab ", i))
    }

    # auto-extend column width
    setColWidths(wb, sheet = i, cols = 1:ncol(dt), widths = "auto")

    # for some reason, freezepane doesn't work on firstActivecol="A"
    freezePane(wb, i, firstActiveRow = 2, firstActiveCol = "B")
  }

  # add titles
  # do this after setting column widths to prevent title
  # text from expanding first column too far

  hs2 <- createStyle(fontColour = "darkblue", fontSize = 18,
                     halign = "LEFT", textDecoration = "Bold")

  for (i in 1:num_tabs){
    title <- data.frame(val="")
    names(title) <- titles[i]
    writeData(wb, i, title, startRow = 1, startCol = 1, headerStyle = hs2)
  }

  # open workbook for review
  if(open_wb) openXL(wb)

  # save
  saveWorkbook(wb, file = file.path(output_path, filename),
               overwrite = overwrite_existing)
}



