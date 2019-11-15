# Module UI

#' @title   mod_treeInput_ui and mod_treeInput_server
#' @description  A shiny Module.
#'
#' @param id shiny id
#' @param input internal
#' @param output internal
#' @param session internal
#'
#' @rdname mod_treeInput
#'
#' @keywords internal
#' @export 
#' @importFrom shiny NS tagList 
mod_treeInput_ui <- function(id){
  ns <- NS(id)
  tagList(
    
    fileInput(ns("upload_tree"),
              label = "Select Tree File:")
  )
}

# Module Server

#' @rdname mod_treeInput
#' @export
#' @keywords internal

mod_treeInput_server <- function(input, output, session){
  ns <- session$ns
  
  userTree <- reactive({
    validate(need(input$upload_tree !="", "Please import a tree file"))
    req(input$upload_tree)
  })   
  
  outTree <- reactive({
    ape::read.tree(userTree()$datapath)
      })
  
  midTree<- reactive({
    #midpoint root the tree file
    phytools::midpoint.root(outTree())
  })
}

## To be copied in the UI
# mod_treeInput_ui("treeInput_ui_1")

## To be copied in the server
# callModule(mod_treeInput_server, "treeInput_ui_1")

