################################
###### golem_utils server ######
################################

#This holds fuctions that are used in different modules. 


######################################
###### tipCheck server function ######
######################################

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