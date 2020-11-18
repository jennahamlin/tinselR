#' @title Example Tree 1
#'
#' This is a Newick phylogenetic tree for 19 isolates of Salmonella enterica
#' to be used with the optional genetic distance matrix (example gene 1) and
#'  the optional meta data file (example meta 1).
#'
#' @docType data
#'
#' @rdname tree1
#'
#' @usage data(tree1)
#'
#' @keywords datasets
#'
#' @examples
#' data(tree1)
#' treeout <- ggtree::ggtree(tree1)
#' treeout <- treeout + ggtree::geom_tiplab() #add labels to the tree
"tree1"
