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

#change column1, row1 to the id of label and replace - with a 0 within the file; necessary for downstream steps
toThreeColumns <- function(geneFileIn){
  center <- NULL 
  dplyr::rename(geneFileIn, label = 1) %>%
    replace(., .=="-", 0)
}

#additional manipulation of genetic distance matrix for ultimately getting the mean number of SNPs 
geneObjectOut  <- function (geneFile) {
  label <- . <- NULL
    geneFile%>%
    stats::na.omit()%>%
    tidyr::pivot_longer(-label)%>%  #convert to a three column data frame 
    .[which(.$label != .$name),] 
}

##displayData server functions
combineGandT <- function(treeFile, geneFile){
  dplyr::full_join(treeFile, geneFile, by = "label")%>%
    treeio::as.treedata()
}

## cladeAnnotator server functions
#function which gets the snps for two tips and puts them into the snpVector
snpVector <- c()

snpAnno <- function(geneFile, tips){
  label <- name <- value <- NULL # adding this helps with devtools::check() note of 'no visible binding for global variables as noted here https://www.r-bloggers.com/no-visible-binding-for-global-variable/
  
  for (i in 1:(length(tips)-1)){
    for (j in (i+1):length(tips)){
      if(tips[i] == tips[j]) next #https://stackoverflow.com/questions/36329183/exclude-one-fixed-variable-in-for-loop
      snpVector<-append(snpVector, geneFile%>%
        dplyr::filter(label == tips[i] & name == tips[j]) %>%
        dplyr::pull(value)
      )
    }
  }
  return(as.numeric(snpVector))
}

#function to add layer, uses findMRCA to get the MRCA (node) for the selected tips
make_layer <- function(tree, tips, label, color, offset) {
  ggtree::geom_cladelabel(
    node = phytools::findMRCA(ape::as.phylo(tree), tips),
    label = label,
    color = color,
    angle = 0,
    offset = offset
  )
}



