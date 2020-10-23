######################################
#### uploadData server functions ####
######################################

#change column1, row1 to the id of label
#necessary for downstream steps
replace_column_header <- function(gene_file_in) {
  . <- NULL
  dplyr::rename(gene_file_in, label = 1)
  #rename column 1 to label for joining of data sets later
}