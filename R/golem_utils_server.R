###functions

##uploadData server functions

#function to read in the data using readr::read_delim
readData<-function(filePath, sep)
{readr::read_delim(filePath,
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
# this uses the fille uploaded (fileUp), the type of file selected (fileType - either a csv or tsv), and the file seperate from input$sep
fileCheck<- function(fileUp, fileType, fileSep){
  myLines <- readLines(con = fileUp$datapath,
                       n = 3)
  fileChk <- validate(
    need(
      length(strsplit(myLines[2], fileType)[[1]]) == length(strsplit(myLines[3], fileType)[[1]]),
      #paste(fileUp[1])
      paste("Error: the delimiter chosen does not match the file type uploaded: ", fileUp[1], sep = "")
    ), 
    need(
      length(strsplit(myLines[2], fileType)[[1]]) > 1,
      paste("Error: the delimiter chosen does not match the file type uploaded: ", fileUp[1], sep = "")))
  if (is.null(fileChk) == TRUE) {
    FileName <- readData(filePath = fileUp$datapath, sep = fileSep)
    #return(FileName)
  }
  else {
    return(fileChk)
  }
}


## cladeAnnotator server functions

#function which gets the snps for two tips and puts them into the snpVector
snpVector <- c()

snpAnno <- function(geneFile, tips){
  #str(tips)
  label <- name <- value <- NULL # adding this helps with devtools::check() note of 'no visible binding for global variables as noted here https://www.r-bloggers.com/no-visible-binding-for-global-variable/
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



#function to add layer, uses findMRCA to get the MRCA(node) for the selected tips
make_layer <- function(tree, tips, label, color, offset) {
  ggtree::geom_cladelabel(
    node = phytools::findMRCA(ape::as.phylo(tree), tips),
    label = label,
    color = color,
    angle = 0,
    offset = offset
  )
}
# checkOverlap <- function(previous_plot, incoming_tips) {
#   preG <- ggplot2::ggplot_build(previous_plot)
#   
#   tip_labels <- preG$data[[3]]
#   incoming_y_coords <-
#     tip_labels[tip_labels$label %in% incoming_tips, "y"]
#   
#   if (length(preG$data) < 4) {
#     any_overlap <- FALSE
#   } else {
#     cladeSegments <- preG$data[[4]]
#     overlaps <- sapply(1:nrow(cladeSegments), function(i) {
#       X <- DescTools::Overlap(
#         x = c(cladeSegments[i, "y"], cladeSegments[i, "yend"]), 
#         y = incoming_y_coords)
#       Y <- X > 0
#       #return(Y)
#     })
#     #print(cladeSegments)
#     #any_overlap <- any(overlaps)
#   }
#   #return(any_overlap)
# }







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