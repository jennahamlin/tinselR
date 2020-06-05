###functions

##uploadData server functions

#function to read in the data using readr::read_delim
readData<-function(filepath, sep)
{readr::read_delim(filepath,
                   sep,
                   trim_ws = T,
                   skip_empty_rows = T,
                   col_names = T,
                   col_types = readr::cols(.default = readr::col_character())
)
}

#function which maps the type of file uploaded based on user selection. For example, inVAr could be input$genesep
fileType <- function(inVar){
  if(inVar == "\t")
  {
    return("\t")
  }
  else (inVar == ",")
  {
    return(",")
  }
}


#function to confirm the type of file uploaded matches the selected type 
# this uses the fille uploaded (FileUp), the type of file selected (FileType - either a csv or tsv), and the file seperate from input$sep
fileCheck<- function(FileUp, FileType, FileSep){
  myLines <- readLines(con = FileUp$datapath,
                       n = 3)
  filechk <- validate(
    need(
      length(strsplit(myLines[2], FileType)[[1]]) == length(strsplit(myLines[3], FileType)[[1]]),
      "Error: the delimiter chosen does not match the file type uploaded."
    ),
    need(
      length(strsplit(myLines[2], FileType)[[1]]) > 1,
      "Error: the delimiter chosen does not match the file type uploaded.")
  )
  if (is.null(filechk) == TRUE) {
    FileName <- readData(filepath = FileUp$datapath, sep = FileSep)
    #return(FileName)
  }
  else {
    return(filechk)
  }
}


## cladeAnnotator server functions

#function which gets the snps for two tips and puts them into the snpVector
snpVector <- c()

snp_anno <- function(geneFile, tips){
  for (i in 1:length(tips)){
    for (j in 1:length(tips)){
      if(tips[i] == tips[j]) next #https://stackoverflow.com/questions/36329183/exclude-one-fixed-variable-in-for-loop
      snpVector[i]<- geneFile%>%
        dplyr::filter(label == tips[i] & name == tips[j]) %>%
        dplyr::pull(value)
    }
  }
  return(as.numeric(snpVector))
}

#function which makes the annotation layer(s)
make_layer <- function(tree, tips, label, color, offset) {
  ggtree::geom_cladelabel(
    node = phytools::findMRCA(ape::as.phylo(tree), tips),
    label = label,
    color = color,
    offset 
  )
}




# # Inverted versions of in, is.null and is.na
# `%not_in%` <- Negate(`%in%`)
# 
# not_null <- Negate(is.null)
# 
# not_na <- Negate(is.na)
# 
# # Removes the null from a vector
# drop_nulls <- function(x){
#   x[!sapply(x, is.null)]
# }
# 
# # If x is null, return y, otherwise return x
# "%||%" <- function(x, y){
#   if (is.null(x)) {
#     y
#   } else {
#     x
#   }
# }
# # If x is NA, return y, otherwise return x
# "%|NA|%" <- function(x, y){
#   if (is.na(x)) {
#     y
#   } else {
#     x
#   }
# }
# 
# # typing reactiveValues is too long
# rv <- shiny::reactiveValues
# rvtl <- shiny::reactiveValuesToList