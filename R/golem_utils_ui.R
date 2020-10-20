###functions

##uploadData ui functions

#function to upload a file 
fileUpload <- function(fileId, fileLabel){
  fileInput(fileId,
            fileLabel,
            multiple = FALSE,
            accept = c("text/csv",                           
                       "text/comma-separated-values,text/plain",
                       ".csv",
                       ".tsv"))
  }

inputSeparator <- function(fileId, fileLabel){
  radioButtons(fileId,
               fileLabel,
               choices = c(Comma = ",",
                           Tab = "\t"),
               selected = "\t")
  }