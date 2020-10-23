######################################
#### displayData server functions ####
######################################

#this combines the genetic distance file and the tree data by the 'label'
combine_g_and_t <- function(tree_file, gene_file) {
  dplyr::full_join(tree_file, gene_file, by = "label")
}