################################
###### golem_utils server ######
################################

#This holds functions that are used in different modules.

################################################
#### tipCheck and testthat server functions ####
################################################

## tipCheck server function
#Function to check imported data files for tip label agreement. If no tip label
#agreement, tells user what is problematic; this can include if number of tips
#does not match when all three files are uploaded. The input paramters include
#tree file, genetic file, and meta file
sanity <- function(m_file, g_file, t_file) {
  
  #meta data get tips
  m_file_tips <- m_file %>% dplyr::pull(1) %>% sort
  
  #genetic data get tips
  g_file_tips <- g_file %>% dplyr::pull(1) %>% sort
  
  #tree file get tips
  t_file_tips <- sort(t_file$tip.label)
  
  # Check for required column names in meta data file
  if ("Tip.labels" %in% colnames(m_file) != TRUE) {
    return(HTML('<span style = "color:gray">Your metadata file does not contain
                the correct column headers. Please correct and try again.
                </span>'))
  } else if ("Display.labels" %in% colnames(m_file) != TRUE) {
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


#get the number of columns of the meta data file. Here columns should be 1 or
#more after transformation of meta data. Tell user how many columns there are
#as the output here allows the user to use the add heatmap/matrix button.
not_columns <- function(file) {
  col_n_file <- ncol(file)
  
  if (col_n_file < 1) {
    return("And looks like there is not a column for adding a heatmap")
  } else {
    return(
      paste("And looks like the number of columns for adding a heatmap is: ",
            col_n_file))
  }
}

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
    tidyr::drop_na() %>%
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


###################################################
#### displayData server and testthat functions ####
###################################################

#this combines the genetic distance file and the tree data by the 'label'
combine_g_and_t <- function(tree_file, gene_file) {
  dplyr::full_join(tree_file, gene_file, by = "label")
}


######################################################
#### cladeAnnotator server and testthat functions ####
######################################################

#function which gets the snps for two tips and puts them into the snpVector
#input is the manipulated genetic distance file and the user selected tips
snp_anno <- function(gene_file, tips) {
  #adding this helps with devtools::check() note of 'no visible binding for
  #global variables
  label <- name <- value <- NULL
  snp_vector <- c()
  for (i in 1:(length(tips) - 1)) { #this goes over a three column dataframe
    for (j in (i + 1):length(tips)) { #i and j are the ids of tips
      if (tips[i] == tips[j] | is.na(tips[i]) | is.na(tips[j])) 
        #if (tips[i] == tips[j]) #don't include self comparisons
        return(NULL)
      #next
      snp_vector <- append(snp_vector, gene_file %>% #add snps to vector 
                             dplyr::filter(
                               label == tips[i] & name == tips[j]) %>%
                             dplyr::pull(value)
      )
    }
  }
  return(as.numeric(snp_vector))
}
