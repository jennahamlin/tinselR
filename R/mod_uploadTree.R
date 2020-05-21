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
mod_uploadTree_ui <- function(id, label ="Upload a newick file, please"){
  ns <- NS(id)
  tagList(
    
    fileInput(ns("treeFile"), label),
    checkboxInput(ns("midPoint"), "Midpoint Root Tree", TRUE),
    # Input: Select a file ----
    fileInput(ns("metaFile"), 
              label= "Upload a meta data file",     #label here is specified and is called in the app_ui with the tags$div section 
              multiple = FALSE,     #does not all multiple files to be uploaded
              accept = c("text/csv",     #accept - this bypasses the  need to do validation as in the web brower only the files with these extensions are selectable
                         "text/comma-separated-values,text/plain",
                         ".csv",
                         ".tsv")),
    
    # Input: Select separator ----
    radioButtons(ns("sep"), "Separator",
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
  
  # Create your own reactive values that you can modify because input is read only
  rv <- reactiveValues()

  # Do something when input$file1 changes
  # * set rv$file1, remove rv$file2
  observeEvent(input$treeFile, {
    rv$treeFile=input$treeFile
  })

  treeFile <- reactive({
    
    validate(need(input$treeFile !="", "Please import tree file"))
    req(input$treeFile)
    #treeio::read.newick(input$treefile$datapath)
    
    if (is.null(input$metaFile$datapath)) {
      treeio::read.newick(input$treeFile$datapath)
    } else {
      
      dataFile <- readr::read_delim(input$metaFile$datapath,
                                    delim = ",",
                                    trim_ws = T,
                                    skip_empty_rows = T,
                                    col_names = T)
      
      treeio::read.newick(input$treeFile$datapath)%>%
        phylotools::sub.taxa.label(., as.data.frame(dataFile))
    }
  })
  
  midTree <- reactive({
    if(input$midPoint == TRUE) {
      return(phytools::midpoint.root(treeFile()))
    }
    else {
      return(treeFile)
    }
  })
  
  
  
}

## To be copied in the UI
# mod_uploadTree_ui("uploadTree_ui_1")

## To be copied in the server
# callModule(mod_uploadTree_server, "uploadTree_ui_1")
