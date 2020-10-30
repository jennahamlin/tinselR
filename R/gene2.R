#' @title Example Gene 2
#'
#' This is a genetic distance matrix of SNPs for 14 isolates of Salmonella
#' enterica sub species enterica to be used with the Newick phylogenetic tree
#' (example tree 2) and the optional meta data file (example meta 2).
#'
#' @docType data
#'
#' @rdname gene2
#'
#' @usage data(gene2)
#'
#' @keywords datasets
#'
#' @examples
#' data(gene2)
#' #convert to three column tibble by the label column
#' gene2 <- gene2 %>% dplyr::rename( label = 1) %>% tidyr::pivot_longer(-label)
#'
"gene2"
