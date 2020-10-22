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
read_data<-function(filePath, sep) {
  readr::read_delim(filePath,
                    sep,
                    trim_ws = TRUE,
                    skip_empty_rows = TRUE,
                    col_names = TRUE,
                    col_types = readr::cols(.default = readr::col_character()))
}

#function which maps the type of file uploaded based on user selection. For 
#example, inVar could be input$genesep
file_type <- function(in_var) {
  if(in_var == "\t") {
    return("\t") }
  else if (in_var == ",") {
    return(",") }
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
    file_name <- read_data(filePath = file_up$datapath, sep = file_sep)
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

gene_object_out  <- function (gene_file) {
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
    dplyr::mutate(value = ifelse(value=="-", 0, value))
}

######################################
###### tipCheck server function ######
######################################

## tipCheck server function
# Function to check imported data files for tip label agreement. If no tip label
#agreement, tells user what is problematic
sanity <- function(m_file, g_file, t_file) {
  
  #meta data get tips
  m_file_tips <- m_file %>% dplyr::pull(1) %>% sort
  
  #genetic data get tips
  g_file_tips <- g_file %>% dplyr::pull(1) %>% sort
  
  #tree file get tips
  t_file_hold <- treeio::read.newick(file = t_file$datapath)
  t_file_tips <- sort(t_file_hold$tip.label)
  
  # Check for required column names in meta data file
  if ("Tip.labels" %in% colnames(m_file) != TRUE) {
    return(HTML('<span style = "color:gray">Your metadata file does not contain
                the correct column headers. Please correct and try again.
                </span>'))
  } else if("Display.labels" %in% colnames(m_file) != TRUE) {
    return(HTML('<span style = "color:gray">Your metadata file does not contain
                the correct column headers. Please correct and try again.
                </span>'))
  }
  
  # Check for the same number of tips for all three files
  if (length(t_file_tips) != length(g_file_tips) |
     length(t_file_tips) != length(m_file_tips) |
     length(g_file_tips) != length(m_file_tips)) {
    return(HTML(paste(
      '<span style = "color:gray">The number of tip labels in your input files
      are unequal, please correct.</span>',
      '<span style = "color:gray">No. of labels in tree file:</span>',
      length(t_file_tips),
      '<span style="color:gray">No. of labels in distance file:</span>',
      length(g_file_tips),
      '<span style = "color:gray">No. of labels in meta data file:</span>',
      length(m_file_tips),
      sep = "<br/>")))
  } else {
    return(HTML('<span style = "color:gray">All three files pass checks and
                contain the same tip labels!</span>'))}
}

#function to read in the meta data file; transform and determine if there is a
#column that can be plotted for a matrix 
m_file_conversion <- function(m_file) {
  Tip.labels <- NULL
  meta <-m_file %>%
    #convert the column Display labels to the row name
    tibble::column_to_rownames(var = "Display.labels") %>% 
    #do not include the column of 'ugly' tip labels
    dplyr::select(-Tip.labels)
}

#get the number of columns of the meta data file. Here columns should be 1 or
#more after transformation of meta data
not_columns <- function (file){
  col_n_file<- ncol(file)
  #colHFile <- colnames(file) #could include what the column headers are

  if (col_n_file < 1 ) {
    return("And looks like there is not a column for matrix plotting")
  } else {
    return(
      paste("And looks like the number of columns for matrix plotting is: ",
            col_n_file))
  }
}

######################################
#### displayData server functions ####
######################################

#this combines the genetic distance file and the tree data by the 'label' 
combine_g_and_t <- function(tree_file, gene_file){
  dplyr::full_join(tree_file, gene_file, by = "label")%>%
    treeio::as.treedata()
}

# treePlot <- function(inputFile, align, layout, fontface, width, node,
# limit, nudge_x){
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
      if(tips[i] == tips[j]) 
        next
      snp_vector<-append(snp_vector, gene_file %>%
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
