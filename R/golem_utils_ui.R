###functions

##uploadData ui functions

#function to upload a file
file_upload <- function(file_id, file_label) {
  fileInput(file_id,
            file_label,
            multiple = FALSE,
            accept = c("text/csv",
                       "text/comma-separated-values,text/plain",
                       ".csv",
                       ".tsv"))
  }

input_separator <- function(file_id, file_label){
  radioButtons(file_id,
               file_label,
               choices = c(Comma = ",",
                           Tab = "\t"),
               selected = "\t")
}
