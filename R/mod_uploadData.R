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
mod_uploadData_ui <- function(id) {
  ns <- NS(id)
  tagList(

    tags$table(width = "100%",
               tags$th("Upload Files", colspan = "3",
                       style = "font-size:20px; color:#444444;")),
    tags$hr(style = "border-color: #99b6d8;"),

    #upload tree file
    fileInput(ns("treeFile"), label = tags$b("1. Upload a newick file",
                                             style = "color:#afafae")),

    #upload genetic distance file using a function 
    fileUpload(ns("geneFile"), fileLabel =
                 tags$b("2. Upload a genetic distance file",
                        style = "color:#afafae")),

    #this decreases the distance between the two buttons (upload and separator)
    div(style = "margin-top:-2em",

    #specify the type of separator for the genetic distance file uploaded
        inputSeparator(ns("geneSep"), fileLabel = tags$em(
          "Separator for genetic distance file", style = "color:#afafae"))),

    fileUpload(ns("metaFile"), fileLabel = tags$b(
      "3. Upload an optional meta data file", style="color:#afafae")),

    div(style = "margin-top:-2em",
        inputSeparator(ns("metaSep"), fileLabel = tags$em(
          "Separator for optional meta data file", style = "color:#afafae"))
    ),
    tags$hr(style = "border-color: #99b6d8;")
  )
}

#' uploadData Server Function
#'
#' @rdname mod_uploadData
#' @export
#' @keywords internal
mod_uploadData_server <- function(input, output, session) {
  ns <- session$ns

  ############
  ### META ###
  ############
  
  #1. reactive expression that holds meta data file and sends tipcheck message
  metaFileUp <- reactive({
    input$metaFile
  })
  
  #2. to perform check for correctly selected delimiter using fileType function
  metaFileType <- eventReactive(input$metaSep, {
    fileType(input$metaSep)
  })
  
  #3. read in the file using file up and file type reactive 
  metaFile <- reactive({
    fileCheck(fileUp = metaFileUp(), fileType = metaFileType(),
              fileSep = metaFileType())
  })
  
  #this performs file conversion for the meta file if there is matrix data,
  #and is a reactive that is ultimately send to the cladeAnnotator
  mFileMat <- reactive({
    if(!is.null(metaFileUp())) { 
      mFileConversion(mFile = metaFile())
    } else {
      #skip
    }
  })
  
  ###############
  ### GENETIC ###
  ###############
  
  #1. reactive expression that holds the genetic distance file
  geneFileUp <- reactive({
    input$geneFile
  })
  
  #2. to perform check for correctly selected delimiter using fileType function
  geneFileType <- eventReactive(input$geneSep, {
    fileType(input$geneSep)
  })
  
  #3. read in the file using file up and file type reactive 
  geneFile <- reactive(
    {fileCheck(fileUp = geneFileUp(),
                                  fileType = geneFileType(),
                                  fileSep = geneFileType())  })

  ##############
  ### TREES ###
  ##############

  #reactive expression that uploads the newick tree and allows the optional
  #upload of meta data to correct tree tip labels 
  treeFileUp <- reactive({
    
    . <- NULL #this refers to the file that is passed through
    
    validate(need(input$treeFile != "", "Please import newick tree file"))
    req(input$treeFile)
    
    if (is.null(metaFileUp()$datapath)) { #if no meta file still upload the tree
      treeio::read.newick(input$treeFile$datapath)
    } else {
      metaFileSeperate <- metaFile()

      treeio::read.newick(input$treeFile$datapath) %>%
        phylotools::sub.taxa.label(., as.data.frame(metaFileSeperate))
      #above line converts tip labels to pretty labels based on user meta upload 
    }
  })
 
 


  
  #additional manipulation of genetic distance matrix for ultimately
  #getting the mean number of SNPs for either the corrected or uncorrected file;
  #uses two functions located in goloem_utils_server.R file and has a
  #description of those functions within.
  geneObject <- reactive({
    label <- NULL
    geneObjectOut(replaceHwithZeros(geneFileCorOrUn()))
  })
  
  
  

  #reactive expression that uploads the genetic distance file and allows the
  #optional upload of meta data to correct tree tip labels this also performs a
  #check to see if the correct delimiter is selected before reading in the files
  #and provides users with error output

  #if no meta file, error check delimiter chosen for genetic distance file
  #uploaded to be able to use clade annotator function
  geneFileCorOrUn <- reactive({
    if (is.null(metaFileUp()$datapath)) {
      geneFileUncorrected <- geneFile()
    }

    else { #if meta file uploaded do an error check, then do an error check for
      #genetic distance and then correct the distance file to match meta file
      #tip labels

      metaFileComb <- metaFile()

      geneFileCorrected <- geneFile() %>% dplyr::rename(center = 1)
      #rename column to center; necessary for next step.

      #the next two lines essentially map the perferred tip lab display in the
      #meta data file to that in the genetic distance file, which has the long
      #tip display names so essentially replacing the long tip labels with
      #whatever the user prefers.

      colnames(geneFileCorrected)[2:ncol(geneFileCorrected)] <-
        metaFileComb$Display.labels[which(metaFileComb$Tip.labels %in%
                                            colnames(geneFileCorrected)
                                          [2:ncol(geneFileCorrected)])]

      geneFileCorrected$center <-
        metaFileComb$Display.labels[which(metaFileComb$Tip.labels
                                          %in% geneFileCorrected$center)]

      return(geneFileCorrected)
    }
  })

  #return these reactive objects to be used in particular modules
  return(
    list(
      #for adding on a heatmap; sent to cladeAnnotator module
      mFileMatOut = reactive(mFileMat()),
      
      #checks for file and sends user message; sent to tipCheck
      metaFileOut = reactive(metaFileUp()),
      
      #used for sanity (concordant check between files) and mFileConversion
      #(convert for heatmap) functions
      mFileOut = reactive(metaFile()),

      #checks for file and sends user message; sent to tipCheck
      geneFileOut = reactive(geneFileUp()),
      
      #used for sanity (concordant check between files); sent to tipCheck
      gFileOut = reactive(geneFile()),
      
      #for display tree to make a combined tree and genetic distance matrix
      geneObjectOutForS4 = reactive(geneObjectOut()),
      
      #for clade annotator to get snp differences and calculate the mean
      geneObjectForSNP = reactive(geneObject()),
      
      
      #for display tree - holds tree with or without converted tip labels
      treeFileOut = reactive(treeFileUp()),

      #sent to tipCheck Module 
      tFileOut = reactive({
        req(input$treeFile)})
    ))
}

## To be copied in the UI
## mod_uploadData_ui("uploadData_ui_1")

## To be copied in the server
## callModule(mod_uploadData_server, "uploadData_ui_1")
