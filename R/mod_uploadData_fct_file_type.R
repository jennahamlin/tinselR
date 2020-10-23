######################################
#### uploadData server functions ####
######################################

#function which maps the type of file uploaded based on user selection. For
#example, in_var could be input$gene_sep
file_type <- function(in_var) {
  if (in_var == "\t") {
    return("\t")
  } else if (in_var == ",") {
    return(",")
  }
}