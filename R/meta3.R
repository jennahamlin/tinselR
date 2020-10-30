#' @title Example Meta 3
#'
#' This is a meta data file for tip correction and adding a heatmap for 19
#' isolates of Salmonella enterica to be used with the Newick phylogenetic tree
#' (example tree 3) and the optional genetic distance file (example gene 3).
#'
#' @docType data
#'
#' @rdname meta3
#'
#' @usage data(meta3)
#'
#' @keywords datasets
#'
#' @examples
#' data(meta3)
#'  #convert the column Display labels to the row name
#' meta3 <- meta3 %>% tibble::column_to_rownames(var = "Display.labels")%>%
#' dplyr::select(-Tip.labels)%>% #exclude column Tip.labels
#' ncol() #get number of columns
"meta3"
