#' @title Example Gene 1
#'
#' This is a genetic distance matrix of SNPs for 19 isolates of Salmonella
#' enterica to be used with the Newick phylogenetic tree (example tree 1) and
#' the optional meta data file (example meta 1).
#'
#' @docType data
#'
#' @rdname gene1
#'
#' @usage data(gene1)
#'
#' @keywords datasets
#'
#' @examples
#' data(gene1)
#' #convert to three column tibble by  label column and remove self comparisons
#' gene1 <- gene1 %>% dplyr::rename( label = 1) %>%
#'  tidyr::pivot_longer(-label )%>%
#'  .[which(.$label != .$name),] %>%
#'  dplyr::mutate(value=ifelse(value=="-", 0,value))
#'
"gene1"
