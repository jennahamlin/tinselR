################################
###### golem_utils server ######
################################

#This holds fuctions that are used in different modules.

#####################################################
#### uploadData and exampleData server functions ####
#####################################################

#change column1, row1 to the id of label
#necessary for downstream steps
replace_column_header <- function(gene_file_in) {
  . <- NULL
  dplyr::rename(gene_file_in, label = 1)
  #rename column 1 to label for joining of data sets later
}

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

######################################################
###### tipCheck and exampleData server function ######
######################################################

#function to read in the meta data file; transform and determine if there is a
#column that can be plotted for a matrix. The input is the meta data file; while
#the output gets used to send a message to the user.
m_file_conversion <- function(m_file) {
  Tip.labels <- NULL
  meta <- m_file %>%
    #convert the column Display labels to the row name
    tibble::column_to_rownames(var = "Display.labels") %>%
    #do not include the column of 'ugly' tip labels
    dplyr::select(-Tip.labels)
}