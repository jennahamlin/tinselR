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
readData<-function(filePath, sep)
{readr::read_delim(filePath,
                   sep,
                   trim_ws = TRUE,
                   skip_empty_rows = TRUE,
                   col_names = TRUE,
                   col_types = readr::cols(.default = readr::col_character())
)
}

#function which maps the type of file uploaded based on user selection. For 
#example, inVar could be input$genesep
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

#function to confirm the type of file uploaded, matches the selected type 
#this uses the fille uploaded (fileUp), the type of file delimited selected 
#(fileType - either a csv or tsv), and the file seperate from input$sep, which
#the user specifies on the interface -so this is ultimately a reactive
fileCheck<- function(fileUp, fileType, fileSep){
  myFile <- req(fileUp$datapath)
  myLines <- readLines(con = myFile, n = 3)
  fileChk <- validate(
    need(
      length(strsplit(myLines[2],
                      fileType)[[1]]) == 
        length(strsplit(myLines[3], fileType)[[1]]),
      paste("Error: the delimiter chosen does not match the file type uploaded: 
            ", fileUp[1], sep = "")
    ), 
    need(
      length(strsplit(myLines[2], fileType)[[1]]) > 1,
      paste("Error: the delimiter chosen does not match the file type uploaded: 
            ", fileUp[1], sep = "")))
  if (is.null(fileChk) == TRUE) {
    FileName <- readData(filePath = fileUp$datapath, sep = fileSep)
  }
  else {
    return(fileChk)
  }
}

#change column1, row1 to the id of label and replace - with a 0 within the file
#necessary for downstream steps
replaceHwithZeros <- function(geneFileIn){
  . <- NULL 
  dplyr::rename(geneFileIn, label = 1) 
  #rename column 1 to label for joining of data sets later
}

#additional manipulation of genetic distance matrix for ultimately getting the 
#mean number of SNPs 

geneObjectOut  <- function (geneFile) {
  label <- . <- value <- NULL
  geneFile%>%
    #remove na
    stats::na.omit()%>%
    #convert to a three column data frame
    tidyr::pivot_longer(-label)%>%
    #remove self comparisons for this table - necessary for snp mean/median 
    #calculation.
    .[which(.$label != .$name),] %>% 
    ##replace - with zero in the file; if zeros already infile, still works
    dplyr::mutate(value=ifelse(value=="-", 0,value))
}

######################################
###### tipCheck server function ######
######################################

## tipCheck server function
# Function to check imported data files for tip label agreement. If no tip label
#agreement, tells user what is problematic
sanity <- function(mFile, gFile, tFile) { 
  #function(impMeta, metSep, impGene, genSep, impTree) {
  
  #meta data get tips
  mFileTips <- mFile %>% dplyr::pull(1) %>% sort
  #print(mFileTips)
  
  #genetic data get tips
  gFileTips <- gFile %>% dplyr::pull(1) %>% sort
  #print(gFileTips)
  
  #tree file get tips
  tFileHold <- treeio::read.newick(file = tFile$datapath)
  tFileTips <- sort(tFileHold$tip.label)
  
  # Check for required column names in meta data file
  if("Tip.labels" %in% colnames(mFile) != TRUE) {
    return(HTML('<span style="color:gray">Your metadata file does not contain
                the correct column headers. Please correct and try again.
                </span>'))
  } else if("Display.labels" %in% colnames(mFile) != TRUE) {
    return(HTML('<span style="color:gray">Your metadata file does not contain 
                the correct column headers. Please correct and try again.
                </span>'))
  } 
  
  # Check for the same number of tips for all three files
  if(length(tFileTips) != length(gFileTips) |
     length(tFileTips) != length(mFileTips) |
     length(gFileTips) != length(mFileTips)) {
    return(HTML(paste(
      '<span style="color:gray">The number of tip labels in your input files 
      are unequal, please correct.</span>', 
      '<span style="color:gray">No. of labels in tree file:</span>', 
      length(tFileTips),
      '<span style="color:gray">No. of labels in distance file:</span>',
      length(gFileTips),
      '<span style="color:gray">No. of labels in meta data file:</span>',
      length(mFileTips),
      sep = "<br/>")))
  } else {
    return(HTML('<span style="color:gray">All three files pass checks and 
                contain the same tip labels!</span>'))}
}

#function to read in the meta data file; transform and determine if there is a 
#column that can be plotted
#for a matrix 
mFileConversion <- function(mFile){
  Tip.labels <- NULL
  meta2 <-mFile %>%
    #convert the column Display labels to the row name
    tibble::column_to_rownames(var = "Display.labels")%>% 
    #do not include the column of 'ugly' tip labels  
    dplyr::select(-Tip.labels) 
  
}

#get the number of columns of the meta data file. Here columns should be 1 or 
#more after transformation of meta data
notColumns <- function (file){
  colNFile<- ncol(file)
  #colHFile <- colnames(file) #could include what the column headers are
  
  print("L135 checking meta file")
  if(colNFile < 1 ){
    return("And looks like there is not a column for matrix plotting")
  } else {
    return(
      paste("And looks like the number of columns for matrix plotting is: ",
            colNFile))
  }
}

######################################
#### displayData server functions ####
######################################

#this combines the genetic distance file and the tree data by the 'label' 
combineGandT <- function(treeFile, geneFile){
  dplyr::full_join(treeFile, geneFile, by = "label")%>%
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
snpVector <- c()

snpAnno <- function(geneFile, tips){
  #adding this helps with devtools::check() note of 'no visible binding for
  #global variables
  label <- name <- value <- NULL 
  
  for (i in 1:(length(tips)-1)){
    for (j in (i+1):length(tips)){
      if(tips[i] == tips[j]) 
        next
      snpVector<-append(snpVector, geneFile%>%
                          dplyr::filter(label == tips[i] & name == tips[j]) %>%
                          dplyr::pull(value)
      )
    }
  }
  return(as.numeric(snpVector))
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
