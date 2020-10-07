# Module UI

#' @title   mod_uploadData_ui and mod_uploadData_server
#' @description  A shiny Module. This module allows the user to upload three different types of files and does file checking to confirm the correct delimiter is selected
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
               tags$th("Upload Files", colspan="3", style="font-size:20px; color:#7ab567;")),
    tags$hr(style="border-color: #99b6d8;"),
    
    #upload tree file 
    fileInput(ns("treeFile"), label = tags$b("1. Upload a newick file", style="color:#afafae")),
    
    #upload genetic distance file using a function 
    fileUpload(ns("geneFile"), fileLabel = tags$b("2. Upload a genetic distance file", style="color:#afafae")),
    
    div(style = "margin-top:-2em", #this decreases the distance between the two buttons (fileupload and inputseparator)
        #specify the type of separator for the genetic distance file uploaded
        inputSeparator(ns("geneSep"), fileLabel = tags$em("Separator for genetic distance file", style="color:#afafae"))),
    
    fileUpload(ns("metaFile"), fileLabel = tags$b("3. Upload an optional meta data file", style="color:#afafae")),
    
    div(style = "margin-top:-2em",
        inputSeparator(ns("metaSep"), fileLabel = tags$em("Separator for optional meta data file", style="color:#afafae"))
    ),
    tags$hr(style="border-color: #99b6d8;")
    
  )
}


# Module Server

#' @rdname mod_uploadData
#' @export
#' @keywords internal
mod_uploadData_server <- function(input, output, session){
  ns <- session$ns
  
  #reactive expression that until a file is uploaded, the below message is displayed if attempting to use the clade annotator button
  geneFileUp <- reactive({
    validate(need(input$geneFile !="", "Please import a genetic distance file to use the clade annotator"))
    input$geneFile
  })
  
  #reactive which holds the input selected for delimiter for gene file using fileType function - this will check for correctly selected delimitor
  geneFileType <- eventReactive(input$geneSep, {
    fileType(input$geneSep)
  })
  
  #reactive expression that holds the meta data file, as this is optional not necessary to validate with informative text display
  metaFileUp <- reactive({
    input$metaFile
  })
  
  #reactive which holds the input selected for delimiter for meta file
  metaFileType <- eventReactive(input$metaSep, {
    fileType(input$metaSep)
  })
  
  #reactive expression that uploads the newick tree and allows the optional upload of meta data to correct tree tip labels
  #this also performs a check to see if the correct delimitor is selected before reading in the file and provides users with error output
  treeFileUp <- reactive({
    
    validate(need(input$treeFile !="", "Please import newick tree file"))
    req(input$treeFile)
    
    if (is.null(metaFileUp()$datapath)) { #if no meta file still upload the tree
      treeio::read.newick(input$treeFile$datapath)
    } 
    else { #if metafile do an error check and then correct tip labels using phylotools sub.taxa.label function
    
      metaFileSeperate <- fileCheck(fileUp = metaFileUp(), fileType = metaFileType(), fileSep = input$metaSep) 
      
      treeio::read.newick(input$treeFile$datapath)%>%
        phylotools::sub.taxa.label(., as.data.frame(metaFileSeperate)) #this line converts tip labels to pretty labels based on user upload of meta data file
    }
  })
  
  #reactive expression that uploads the genetic distance file and allows the optional upload of meta data to correct tree tip labels
  #this also performs a check to see if the correct delimitor is selected before reading in the files and provides users with error output
  
  geneFileCorOrUn <- reactive({ 
    if (is.null(metaFileUp()$datapath)) { #if no meta file, error check delimitor choosen for genetic distance file uploaded to be able to use clade annotator function
      
      geneFileUncorrected <- fileCheck(fileUp = geneFileUp(), fileType = geneFileType(), fileSep = input$geneSep)
    } 
    
    else { #if meta file uploaded do an error check, then do an error check for genetic distance and then correct the distance file to match meta file tip labels
      
      . <- NULL
      
      metaFileComb <- fileCheck(fileUp = metaFileUp(), fileType = metaFileType(), fileSep = input$metaSep) 
      
      geneFileCorrected <- fileCheck(fileUp = geneFileUp(), fileType = geneFileType(), fileSep = input$geneSep) %>%
        dplyr::rename(center = 1)
      
      #geneFileCorrected %>% dplyr::rename(center = 1)
      
      colnames(geneFileCorrected)[2:ncol(geneFileCorrected)] = metaFileComb$Display.labels[which(metaFileComb$Tip.labels %in% colnames(geneFileCorrected)[2:ncol(geneFileCorrected)])]
      
      geneFileCorrected$center = metaFileComb$Display.labels[which(metaFileComb$Tip.labels %in% geneFileCorrected$center)]
      
      return(geneFileCorrected)
    }
  })
  
  #additional manipulation of genetic distance matrix for ultimately getting the mean number of SNPs; uses two functions located in
  #goloem_utils_server.R file 
  geneObject <-reactive({
    label <- NULL
    geneObjectOut(toThreeColumns(geneFileCorOrUn()))
  })
  

  #return these reactive objects to be used in tree display module 
  return(
    list(
      metaFileOut = reactive(metaFileUp()),
      treeFileOut = reactive(treeFileUp()),
      geneObjectOutForS4 = reactive(geneObjectOut()),
      geneObjectForSNP = reactive(geneObject())))
}

## To be copied in the UI
# mod_uploadData_ui("uploadData_ui_1")

## To be copied in the server
# callModule(mod_uploadData_server, "uploadData_ui_1")

