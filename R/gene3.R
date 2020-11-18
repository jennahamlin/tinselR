#' @title Example Gene 3
#'
#' This is a genetic distance matrix of SNPs for 14 isolates of Salmonella
#' enterica sub species enterica to be used with the Newick phylogenetic tree
#' (example tree 3) and the optional meta data file (example meta 3).
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
#' #convert to three column tibble by the label column
#' gene3 <- gene3 %>% dplyr::rename( label = 1) %>% tidyr::pivot_longer(-label)
#'
"gene3"
