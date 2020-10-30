#' @title Example Tree 3
#'
#' This is a Newick phylogenetic tree for 19 isolates of Salmonella enterica
#' to be used with the optional genetic distance matrix (example gene 3) and
#'  the optional meta data file (example meta 3).
#'
#' @docType data
#'
#' @rdname tree3
#'
#' @usage data(tree3)
#'
#' @keywords datasets
#'
#' @examples
#' data(tree3)
#' treeout <- ggtree::ggtree(tree3)
#' treeout <- treeout + ggtree::geom_tiplab() #add labels to the tree
"tree3"
