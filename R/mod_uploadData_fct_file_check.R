######################################
#### uploadData server functions ####
######################################

#function to confirm the type of file uploaded, matches the selected type
#this uses the fill uploaded (file_up), the type of file delimited selected
#(file_type - either a csv or tsv), and the file separate from input$sep, which
#the user specifies on the interface -so this is ultimately a reactive
file_check <- function(file_up, file_type, file_sep) {
  my_file <- req(file_up$datapath)
  my_lines <- readLines(con = my_file, n = 3)
  file_chk <- validate(
    need(
      length(strsplit(my_lines[2],
                      file_type)[[1]]) ==
        length(strsplit(my_lines[3], file_type)[[1]]),
      paste("Error: the delimiter chosen does not match the file type uploaded:
            ", file_up[1], sep = "")),
    need(
      length(strsplit(my_lines[2], file_type)[[1]]) > 1,
      paste("Error: the delimiter chosen does not match the file type uploaded:
            ", file_up[1], sep = "")))
  if (is.null(file_chk) == TRUE) {
    file_name <- read_data(file_path = file_up$datapath, sep = file_sep)
  } else {
    return(file_chk)
  }
}