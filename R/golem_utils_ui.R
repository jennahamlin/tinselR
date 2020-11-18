########################
#### golem_utils ui ####
########################

#This holds functions that are used in the module uploadData ui

#function to upload a file. file_id is the id assigned to the file. file_label 
#is the label assigned to the file as visible in the ui
file_upload <- function(file_id, file_label) {
  fileInput(file_id,
            file_label,
            multiple = FALSE,
            accept = c("text/csv",
                       "text/comma-separated-values,text/plain",
                       ".csv",
                       ".tsv"))
  }

#function which holds the type of input separators. file_id is the id assigned 
#to the file. file_label is the label assigned to the file as visible in the ui
input_separator <- function(file_id, file_label) {
  radioButtons(file_id,
               file_label,
               choices = c(Comma = ",",
                           Tab = "\t"),
               selected = "\t")
}
