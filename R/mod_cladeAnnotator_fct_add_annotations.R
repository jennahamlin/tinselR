#this functions calculates the mean # snps and adds that layer as
#annotation. Additionally, it checks for overlap in annotations and adjusts
#as necessary
add_annotations <- function(tree_plot, tip_vector_in, r_n_values, r_tip_vec,
                            label_off, geneObjectForSNP, lab_color) {
  g <- tree_plot

  if (r_n_values > 0) {
    #this is the i'th list, for which we are calculating the offset
    for (i in seq_along(tip_vector_in)) {
      current_tips <- r_tip_vec[[i]]
      n_overlap <- 0     # start by assuming no overlap
      if (i > 1) {         # for the first set of tips no comparisons needed
        # otherwise do comparisons
        #this is the j'th list, against which we need to compare if i
        #overlaps it
        for (j in 1:(i - 1)) {
          compare_tips <- r_tip_vec[[j]]  #tips to compare to
          # for every match, count it
          n_overlap <- n_overlap + any(current_tips %in% compare_tips)
        }
      }

      # set the clade label offset based on how many sets of previous tips
      #it overlaps and provide user #option to adjust the position of
      #all annotations
      label_offset <- label_off + n_overlap * 0.004

      #uses the snpAnno function to calculate the mean # of snps for
      #brushed tips
      snp_mean <-
        snp_anno(gene_file = geneObjectForSNP,
                 tips = current_tips)

      #generates the layer for the set of brushed tips
      g <- g +
        make_layer(
          tree_plot,
          tips = tip_vector_in[[i]],
          label = paste0("Range \nof \nSNP(s)- \n",
                         paste0(min(snp_mean), sep = ",", max(snp_mean))),
          color = lab_color,
          offset = label_offset
        )
    }
  }
  return(g)
}
