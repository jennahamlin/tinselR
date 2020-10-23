#####################################
## cladeAnnotator server functions ##
#####################################

#this function takes in the genetic distance matrix which has been transformed
#to three columns with all pairwise comparisons listed. The tips parameter is
#tips that have been selected/brushed by the user and the output is a vector
#of snp differences that can be used to get the range for a clade of interest. 
snp_anno <- function(gene_file, tips) {
  label <- name <- value <- NULL #deals with 'no visible binding' error
  snp_vector <- c()
  for (i in 1:(length(tips) - 1)) {
    for (j in (i + 1):length(tips)) {
      if (tips[i] == tips[j])
        next
      snp_vector <- append(snp_vector, gene_file %>%
                             dplyr::filter(label == tips[i] & name == tips[j])
                           %>% dplyr::pull(value)
      )
    }
  }
  return(as.numeric(snp_vector))
}