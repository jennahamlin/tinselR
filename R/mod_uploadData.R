#' uploadData UI Function
#' 
#' @title   mod_uploadData_ui and mod_uploadData_server
#' @description  A shiny Module. This module allows the user to upload three
#'  different types of files and does file checking to confirm the correct 
#'  delimiter is selected
#'
#' @param id shiny id
#' @param input internal
#' @param output internal
#' @param session internal
#'
#' @rdname mod_uploadData
#'
#' @keywords internal
#' @export 
#' @importFrom shiny NS tagList 
mod_uploadData_ui <- function(id){
  ns <- NS(id)
  tagList(
    
    tags$table(width ="100%",
               tags$th("Upload Files", colspan="3",
                       style="font-size:20px; color:#444444;")),
    tags$hr(style="border-color: #99b6d8;"),
    
    #upload tree file 
    fileInput(ns("treeFile"), label = tags$b("1. Upload a newick file",
                                             style="color:#afafae")),
    
    #upload genetic distance file using a function 
    fileUpload(ns("geneFile"), fileLabel = 
                 tags$b("2. Upload a genetic distance file",
                        style="color:#afafae")),
    
    #this decreases the distance between the two buttons (upload and separator)
    div(style = "margin-top:-2em", 
        
        #specify the type of separator for the genetic distance file uploaded
        inputSeparator(ns("geneSep"), fileLabel = tags$em(
          "Separator for genetic distance file", style="color:#afafae"))),
    
    fileUpload(ns("metaFile"), fileLabel = tags$b(
      "3. Upload an optional meta data file", style="color:#afafae")),
    
    div(style = "margin-top:-2em",
        inputSeparator(ns("metaSep"), fileLabel = tags$em(
          "Separator for optional meta data file", style="color:#afafae"))
    ),
    tags$hr(style="border-color: #99b6d8;")
    
  )
}


#' uploadData Server Function
#' 
#' @rdname mod_uploadData
#' @export
#' @keywords internal
mod_uploadData_server <- function(input, output, session){
  ns <- session$ns
  
  #reactive which holds just the tree file, this is used in the tipcheck module
  treeFileOut <- reactive({req(input$treeFile)})
  
  #reactive which holds just the gene file, this is used in tipcheck module
  genFile <- reactive({ fileCheck(fileUp = geneFileUp(),
                                  fileType = geneFileType(), 
                                  fileSep = geneFileType())  })
  
  #reactive expression that holds the genetic distance file
  geneFileUp <- reactive({
    #validate(need(input$geneFile !="", "Please import a genetic distance file")) 
    input$geneFile
  })
  
  #reactive which holds the input selected for delimiter for gene file using
  #fileType function - this will check for correctly selected delimiter
  geneFileType <- eventReactive(input$geneSep, {
    fileType(input$geneSep)
  })
  
  #additional manipulation of genetic distance matrix for ultimately 
  #getting the mean number of SNPs for either the corrected or uncorrected file;
  #uses two functions located in goloem_utils_server.R file and has a
  #description of those functions within. 
  geneObject <-reactive({
    label <- NULL
    geneObjectOut(replaceHwithZeros(geneFileCorOrUn()))
  })
  
  #reactive which holds just the meta file, this is used tipcheck and matrix
  #check in their module
  metaFileOut <- reactive({ 
    fileCheck(fileUp = metaFileUp(), fileType = metaFileType(),
              fileSep = metaFileType())
  })
  
  #this performs file conversion for the meta file if there is matrix data, 
  #and is a reactive that is ultimately send to the cladeAnnotator
  mFileMat <- reactive({
    if(!is.null(metaFileUp())){
      mFileConversion(mFile = metaFileOut() )
    } else {
      #skip
    }
  })
  
  #reactive expression that holds the meta data file
  metaFileUp <- reactive({
    input$metaFile
  })
  
  #reactive which holds the input selected for delimiter for meta file
  metaFileType <- eventReactive(input$metaSep, {
    fileType(input$metaSep)
  })
  
  
  #reactive expression that uploads the newick tree and allows the optional
  #upload of meta data to correct tree tip labels this also performs a check to
  #see if the correct delimiter is selected before reading in the file and 
  #provides users with an error message if the delimiter is in correctly selected
  treeFileUp <- reactive({
    
    . <- NULL #this refers to the file that is passed through
    
    validate(need(input$treeFile !="", "Please import newick tree file"))
    req(input$treeFile)
    
    if (is.null(metaFileUp()$datapath)) { #if no meta file still upload the tree
      treeio::read.newick(input$treeFile$datapath)
    } else { 
      
      #if metafile do an error check and then correct tip labels using
      #phylotools sub.taxa.label function
      metaFileSeperate <- fileCheck(fileUp = metaFileUp(), 
                                    fileType = metaFileType(),
                                    fileSep = metaFileType()) 
      
      treeio::read.newick(input$treeFile$datapath)%>%
        phylotools::sub.taxa.label(., as.data.frame(metaFileSeperate))
      #this above line converts tip labels to pretty labels based on user upload  
      #of meta data file
    }
  })
  
  #reactive expression that uploads the genetic distance file and allows the
  #optional upload of meta data to correct tree tip labels this also performs a 
  #check to see if the correct delimitor is selected before reading in the files
  #and provides users with error output
  
  geneFileCorOrUn <- reactive({ 
    #if no meta file, error check delimitor choosen for genetic distance file 
    #uploaded to be able to use clade annotator function
    if (is.null(metaFileUp()$datapath)) { 
      
      geneFileUncorrected <- fileCheck(fileUp = geneFileUp(), 
                                       fileType = geneFileType(),
                                       fileSep = geneFileType())
    } 
    
    else { #if meta file uploaded do an error check, then do an error check for 
      #genetic distance and then correct the distance file to match meta file
      #tip labels
      
      metaFileComb <- fileCheck(fileUp = metaFileUp(),
                                fileType = metaFileType(),
                                fileSep = metaFileType()) 
      
      geneFileCorrected <- fileCheck(fileUp = geneFileUp(),
                                     fileType = geneFileType(),
                                     fileSep = geneFileType()) %>%
        dplyr::rename(center = 1) 
      #rename column to center; necessary for next step. 
      
      #the next two lines essentially map the perferred tip lab display in the 
      #meta data file to that in the genetic distance file, which has the long 
      #tip display names so essentially replacing the long tip labels with
      #whatever the user prefers. 
      
      colnames(geneFileCorrected)[2:ncol(geneFileCorrected)] = metaFileComb$Display.labels[which(metaFileComb$Tip.labels %in% colnames(geneFileCorrected)[2:ncol(geneFileCorrected)])]
      
      geneFileCorrected$center = metaFileComb$Display.labels[which(metaFileComb$Tip.labels %in% geneFileCorrected$center)]
      
      return(geneFileCorrected)
    }
  })
  
  #return these reactive objects to be used in particular modules 
  return(
    list(
      #for adding on a heatmap
      mFileMatOut = reactive(mFileMat()),
      
      #for tip check and dealing with matrix in above code
      mFileOut = reactive(metaFileOut()),
      
      #for tip check; unclear why mFileOut can be used, but without this a user
      #message doesn't get displayed
      metaFileOut = reactive(metaFileUp()),
      
      #for display tree - holds tree with or without converted tip labels
      treeFileOut = reactive(treeFileUp()),
      
      #for display tree to make a combined tree and genetic distance matrix
      geneObjectOutForS4 = reactive(geneObjectOut()), 
      
      #for clade annotator to get snp differences and calculate the mean
      geneObjectForSNP = reactive(geneObject()), 
      
      #for tip check 
      gFileOut = reactive(genFile()),
      
      #for tip check 
      tFileOut = reactive(treeFileOut()) 
    ))
}

## To be copied in the UI
# mod_uploadData_ui("uploadData_ui_1")

## To be copied in the server
# callModule(mod_uploadData_server, "uploadData_ui_1")

