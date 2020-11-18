#' @title Example Meta 1
#'
#' This is a meta data file for tip correction and adding a heatmap for 19
#' isolates of Salmonella enterica to be used with the Newick phylogenetic tree
#' (example tree 1) and the optional genetic distance file (example gene 1).
#'
#' @docType data
#'
#' @rdname meta1
#'
#' @usage data(meta1)
#'
#' @keywords datasets
#'
#' @examples
#' data(meta1)
#'  #convert the column Display labels to the row name
#' meta1 <- meta1 %>% tibble::column_to_rownames(var = "Display.labels")%>%
#' dplyr::select(-Tip.labels)%>% #exclude column Tip.labels
#' ncol() #get number of columns
"meta1"
