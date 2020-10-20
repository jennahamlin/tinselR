#'@title Example Gene 1
#'
#' This is a genetic distance matrix of SNPs for 19 isolates of Eschericia coli 
#' to be used with the newick phylogenetic tree (example tree 1) and the
#' optional meta data file (example meta 1)
#'
#' @docType data
#' 
#' @rdname gene1
#'
#' @usage data(gene1)
#' 
#' @keywords datasets
#' 
#'@examples
#'data(gene1)
#'#renames column 1, row 1 from '.' to 'center' for further manipulation
#'gene1 <- gene1 %>% dplyr::rename(center = 1)  
#' 
"gene1"
