###functions

######################################
#### uploadData server functions ####
######################################

#function to read in the data using readr::read_delim
#filePath is the path to the location of the file you want to read in
#sep is the specified delimiter, probably either a tab ("\t") or comma (",")
#the other bits here help with reading in the file: trim whitespace, skip
#empty row, column names, and how to read in the data; default is set at column
#as characters
read_data <- function(file_path, sep) {
  readr::read_delim(file_path,
                    sep,
                    trim_ws = TRUE,
                    skip_empty_rows = TRUE,
                    col_names = TRUE,
                    col_types = readr::cols(.default = readr::col_character()))
}

#function which maps the type of file uploaded based on user selection. For
#example, inVar could be input$genesep
file_type <- function(in_var) {
  if (in_var == "\t") {
    return("\t")
    } else if (in_var == ",") {
    return(",")
      }
}

#function to confirm the type of file uploaded, matches the selected type
#this uses the fill uploaded (file_up), the type of file delimited selected
#(file_type - either a csv or tsv), and the file separate from input$sep, which
#the user specifies on the interface -so this is ultimately a reactive
file_check <- function(file_up, file_type, file_sep) {
  my_file <- req(file_up$datapath)
  my_lines <- readLines(con = my_file, n = 3)
  file_chk <- validate(
    need(
      length(strsplit(my_lines[2],
                      file_type)[[1]]) ==
        length(strsplit(my_lines[3], file_type)[[1]]),
      paste("Error: the delimiter chosen does not match the file type uploaded:
            ", file_up[1], sep = "")),
    need(
      length(strsplit(my_lines[2], file_type)[[1]]) > 1,
      paste("Error: the delimiter chosen does not match the file type uploaded:
            ", file_up[1], sep = "")))
  if (is.null(file_chk) == TRUE) {
    file_name <- read_data(file_path = file_up$datapath, sep = file_sep)
  } else {
    return(file_chk)
  }
}

#change column1, row1 to the id of label and replace - with a 0 within the file
#necessary for downstream steps
replace_h_with_zeros <- function(gene_file_in) {
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


######################################
#### displayData server functions ####
######################################

#this combines the genetic distance file and the tree data by the 'label'
combine_g_and_t <- function(tree_file, gene_file) {
  dplyr::full_join(tree_file, gene_file, by = "label")
  #%>%
  #  treeio::as.treedata()
}

# treePlot <- function(inputFile, align, layout, fontface, width, node,
# limit, nudge_x) {
#   label <- NULL
#   ggtree::ggtree(inputFile, layout)+
#     ggplot2::xlim(NA, limit)+
#     ggtree::geom_tiplab(align, fontface, family="Helvetica")+
#     ggtree::geom_treescale(width, x = 0.005, y = -1 )+
#     ggtree::geom_text2(ggplot2::aes(label=label,
# subset = !is.na(as.numeric(label)) & as.numeric(label) > node), nudge_x )
# }

#####################################
## cladeAnnotator server functions ##
#####################################

## cladeAnnotator server functions
#function which gets the snps for two tips and puts them into the snpVector


snp_anno <- function(gene_file, tips) {
  #adding this helps with devtools::check() note of 'no visible binding for
  #global variables
  label <- name <- value <- NULL 
  snp_vector <- c()
  for (i in 1:(length(tips) - 1)) {
    for (j in (i + 1):length(tips)) {
      if (tips[i] == tips[j])
        next
      snp_vector <- append(snp_vector, gene_file %>%
                          dplyr::filter(label == tips[i] & name == tips[j]) %>%
                          dplyr::pull(value)
      )
    }
  }
  return(as.numeric(snp_vector))
}

#function to add layer, uses findMRCA to get the MRCA (node) for the
#selected tips
make_layer <- function(tree, tips, label, color, offset) {
  ggtree::geom_cladelabel(
    node = phytools::findMRCA(ape::as.phylo(tree), tips),
    label = label,
    color = color,
    angle = 0,
    offset = offset
  )
}
