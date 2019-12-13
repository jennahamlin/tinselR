# Module UI
  
#' @title   mod_treeDisplay_ui and mod_treeDisplay_server
#' @description  A shiny Module.
#'
#' @param id shiny id
#' @param input internal
#' @param output internal
#' @param session internal
#'
#' @rdname mod_treeDisplay
#'
#' @keywords internal
#' @export 
#' @importFrom shiny NS tagList 
mod_treeDisplay_ui <- function(id, name){
  ns <- NS(id)
  tabPanel(
    name, plotOutput(ns("tree"))
  )
}
    
# Module Server
    
#' @rdname mod_treeDisplay
#' @export
#' @keywords internal
    
mod_treeDisplay_server <- function(input, output, session, file){
  ns <- session$ns

    output$tree<-renderPlot({
     file()
  })
  
  
}
    
## To be copied in the UI
# mod_treeDisplay_ui("treeDisplay_ui_1")
    
## To be copied in the server
# callModule(mod_treeDisplay_server, "treeDisplay_ui_1")
 
