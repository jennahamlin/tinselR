#' read_data Function
#'
#' @title read_data
#'
#' @description function to read in the data using readr::read_delim
#' filePath is the path to the location of the file you want to read in
#' sep is the specified delimiter, probably either a tab or comma
#' the other bits here help with reading in the file: trim whitespace, skip
#' empty row, column names, and how to read in the data; default is
#' set at column as characters
#'
#' @rdname read_data
#' 
#' @param file_path path of where to look for file
#' @param sep the file separator
#' 
#' @export
#' 
read_data <- function(file_path, sep) {
  readr::read_delim(file_path,
                    sep,
                    trim_ws = TRUE,
                    skip_empty_rows = TRUE,
                    col_names = TRUE,
                    col_types =
                      readr::cols(.default = readr::col_character()))
}
