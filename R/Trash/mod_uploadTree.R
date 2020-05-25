# Module UI

#' @title   mod_uploadTree_ui and mod_uploadTree_server
#' @description  A shiny Module. This module will upload a newick tree and is read in by treeio package with the function read.newick and allows one to midpoint root the tree
#'
#' @param id shiny id
#' @param input internal
#' @param output internal
#' @param session internal
#'
#' @rdname mod_uploadTree
#'
#' @keywords internal
#' @export 
#' @importFrom shiny NS tagList 
mod_uploadTree_ui <- function(id, label){
  ns <- NS(id)
  tagList(
    
    fileInput(ns("treeFile"), label ="1. Upload a newick file, please"),
    
    # checkboxInput(ns("midPoint"), "Midpoint Root Tree", TRUE),
    
    # Input: Select a file ----
    fileInput(ns("geneFile"), 
              label = "2. Upload a genetic distance file ",     #label here is specified and is called in the app_ui with the tags$div section 
              multiple = FALSE,                                 #does not all multiple files to be uploaded
              accept = c("text/csv",                            #accept - this bypasses the  need to do validation as in the web brower only the files with these extensions are selectable
                         "text/comma-separated-values,text/plain",
                         ".csv",
                         ".tsv")),
    
    # Input: Select separator ----
    radioButtons(ns("geneSep"), "Separator for genetic data",
                 choices = c(Comma = ",",
                             Tab = "\t"),
                 selected = "\t"),
    
    # Input: Select a file ----
    fileInput(ns("metaFile"), 
              label= "3. Upload an optional meta data file",     #label here is specified and is called in the app_ui with the tags$div section 
              multiple = FALSE,                                  #does not all multiple files to be uploaded
              accept = c("text/csv",                             #accept - this bypasses the  need to do validation as in the web brower only the files with these extensions are selectable
                         "text/comma-separated-values,text/plain",
                         ".csv",
                         ".tsv")),
    
    # Input: Select separator ----
    radioButtons(ns("metaSep"), "Separator for meta data",
                 choices = c(Comma = ",",
                             Tab = "\t"),
                 selected = "\t"))
}

# Module Server

#' @rdname mod_uploadTree
#' @export
#' @keywords internal

mod_uploadTree_server <- function(input, output, session){
  ns <- session$ns
  
 #function to read data in either genetic distance matrix or meta data file 
  readData<-function(filepath, delim)
   {readr::read_delim(filepath,
                    delim,
                    trim_ws = T,
                    skip_empty_rows = T,
                    col_names = T)}
  
  
  #reactive expression that until a file is uploaded, the below message is displayed
  geneFileUp <- reactive({
    validate(need(input$geneFile !="", "Please import a genetic distance file to use the clade annotator"))
    input$geneFile
  }) 
  
  #reactive expression that holds the meta data file 
  metaFileUp <- reactive({
    input$metaFile
  })
  
  #reactive expression that upload the newick tree and allows the optional upload of meta data to correct tree tip labels 
  treeFileUp <- reactive({
    
    validate(need(input$treeFile !="", "Please import newick tree file"))
    req(input$treeFile)
    
    if (is.null(metaFileUp()$datapath)) {
      treeio::read.newick(input$treeFile$datapath)
    } else {
      
      dataFile <- readData(filepath =metaFileUp()$datapath,
                           delim =  input$metaSep)
      
      treeio::read.newick(input$treeFile$datapath)%>%
        phylotools::sub.taxa.label(., as.data.frame(dataFile))
    }
  })
  
  #
  geneFileCorOrUn <- reactive({ 
    if (is.null(metaFileUp()$datapath)) {
      
      geneFileUncorrected <- readData(filepath = geneFileUp()$datapath,
                                      delim =  input$geneSep)
    } 
    else {
      
      metaFileComb <- readData(filepath =metaFileUp()$datapath,
                                delim =  input$metaSep)
        
      geneFileCorrected <- readData(filepath = geneFileUp()$datapath,
                                      delim =  input$geneSep)
      
      colnames(geneFileCorrected)[2:ncol(geneFileCorrected)] = metaFileComb$Display.labels[which(metaFileComb$Tip.labels %in% colnames(geneFileCorrected)[2:ncol(geneFileCorrected)])]
      
      geneFileCorrected$. = metaFileComb$Display.labels[which(metaFileComb$Tip.labels %in% geneFileCorrected$.)]
      
      return(geneFileCorrected)
    }
  })
  
  #return these reactive objects to be used in tree display module 
  return(
    list(
      treeFileOut = reactive(treeFileUp()),
      geneFileCorOrUnOut = reactive(geneFileCorOrUn())
      #metaFileOut = reactive(metaFileUp()),
      #metaSepOut = reactive(input$metaSep)
    ))

  # midTree <- reactive({
  #   if(input$midPoint == TRUE) {
  #     return(phytools::midpoint.root(treeFile()))
  #   }
  #   else {
  #     return(treeFile)
  #   }
  # })
  # 
  
  
}

## To be copied in the UI
# mod_uploadTree_ui("uploadTree_ui_1")

## To be copied in the server
# callModule(mod_uploadTree_server, "uploadTree_ui_1")
