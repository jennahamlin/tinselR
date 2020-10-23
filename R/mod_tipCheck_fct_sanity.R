######################################
###### tipCheck server function ######
######################################

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
  #t_file_hold <- treeio::read.newick(file = t_file$datapath)
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