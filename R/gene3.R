#' @title Example Gene 3
#'
#' This is a genetic distance matrix of SNPs for 19 isolates of Salmonella
#' enterica to be used with the Newick phylogenetic tree (example tree 3) and
#' the optional meta data file (example meta 3).
#'
#' @docType data
#'
#' @rdname gene3
#'
#' @usage data(gene3)
#'
#' @keywords datasets
#'
#' @examples
#' data(gene3)
#' #convert to three column tibble by  label column and remove self comparisons
#' gene3 <- gene3 %>% dplyr::rename( label = 1) %>%
#'  tidyr::pivot_longer(-label )%>%
#'  .[which(.$label != .$name),] %>%
#'  dplyr::mutate(value=ifelse(value=="-", 0,value))
#'
"gene3"
