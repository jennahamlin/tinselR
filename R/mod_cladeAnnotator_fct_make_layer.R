#####################################
## cladeAnnotator server functions ##
#####################################

#function to add an annotation layer, uses findMRCA from phylotools to get
#the MRCA (node) for the selected tips. Takes tree and brushed tips; allows
#user to adjust the color and offset of annotations. Label = the annotation
#drawn on with the snp_mean calculated
make_layer <- function(tree, tips, label, color, offset) {
  ggtree::geom_cladelabel(
    node = phytools::findMRCA(ape::as.phylo(tree), tips),
    label = label,
    color = color,
    angle = 0,
    offset = offset
  )
}