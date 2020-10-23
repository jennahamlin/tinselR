######################################
#### uploadData server functions ####
######################################


#additional manipulation of genetic distance matrix for ultimately getting the
#mean number of SNPs

gene_object_out  <- function(gene_file) {
  label <- . <- value <- NULL
  gene_file %>%
    #remove na
    stats::na.omit() %>%
    #convert to a three column data frame
    tidyr::pivot_longer(-label) %>%
    #remove self comparisons for this table - necessary for snp mean/median
    #calculation.
    .[which(.$label != .$name), ] %>%
    ##replace - with zero in the file; if zeros already infile, still works
    dplyr::mutate(value = ifelse(value == "-", 0, value))
}
