#'@title Example Gene 2
#'
#' This is a genetic distance matrix of SNPs for 19 isolates of Eschericia coli
#' to be used with the Newick phylogenetic tree (example tree 2) and the
#' optional meta data file (example meta 2).
#'
#' @docType data
#'
#' @rdname gene2
#'
#' @usage data(gene2)
#'
#' @keywords datasets
#'
#'@examples
#'data(gene2)
#'#renames column 2, row 2 from '.' to 'center' for further manipulation
#'gene2 <- gene2 %>% dplyr::rename(center = 2)
#'
"gene2"
