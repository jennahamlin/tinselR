# Module UI

#' @title   mod_uploadTree_ui and mod_uploadTree_server
#' @description  A shiny Module. This module will upload a newick tree and is read in by treeio package with the function read.newick
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
    
    fileInput(ns("treefile"), label),
    checkboxInput(ns("midPoint"), "Midpoint Root Tree", TRUE))
}

# Module Server

#' @rdname mod_uploadTree
#' @export
#' @keywords internal

mod_uploadTree_server <- function(input, output, session){
  ns <- session$ns
  
  treeFile <- reactive({
    #validate(need(input$treefile !="", "Please import tree file"))
    req(input$treefile)
    treeio::read.newick(input$treefile$datapath)
  })
  
  midTree <- reactive({
    if(input$midPoint == TRUE) {
      return(phytools::midpoint.root(treeFile()))
    }
    else {
      return(treeFile())
    }
  })
}

## To be copied in the UI
# mod_uploadTree_ui("uploadTree_ui_1")

## To be copied in the server
# callModule(mod_uploadTree_server, "uploadTree_ui_1")
