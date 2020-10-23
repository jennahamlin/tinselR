#####################################
## cladeAnnotator server functions ##
#####################################

#add map function; takes in a tree and the converted meta data file, along with
#two reactive values (r_value and matoff) which are initialized on start of the
#application. Only allows the inclusion of the map if the value of show_map is
#greater than 0; when add_map button is pushed then this makes r_value > 0
add_map <- function(tree, metaFile, r_value, matOff) {
  if (r_value > 0 & !is.null(metaFile)) {
    tree <- ggtree::gheatmap(tree,
                             metaFile,
                             offset = matOff,
                             width = 0.2,
                             colnames_angle = 45,
                             colnames_offset_y = -1,
                             hjust = 0.5)
    #+ ggplot2::scale_color_viridis_d(option = matCol())
    #will add in this option to change the color; which i want to add
  }
  return(tree)
}