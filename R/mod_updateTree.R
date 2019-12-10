# Module UI
  
#' @title   mod_updateTree_ui and mod_updateTree_server
#' @description  A shiny Module.
#'
#' @param id shiny id
#' @param input internal
#' @param output internal
#' @param session internal
#'
#' @rdname mod_updateTree
#'
#' @keywords internal
#' @export 
#' @importFrom shiny NS tagList 
mod_updateTree_ui <- function(id){
  ns <- NS(id)
  tagList(
    
    plotOutput(ns("midtree"))
  
  )
}
    
# Module Server
    
#' @rdname mod_updateTree
#' @export
#' @keywords internal
    
mod_updateTree_server <- function(input, output, session){
  ns <- session$ns
  
  midTree <- reactive({
    
    if(input$midp == TRUE) {
      return(phytools::midpoint.root(outTree()))
    }
    else {
      
      return(outTree())
    }
  })
}
    
## To be copied in the UI
# mod_updateTree_ui("updateTree_ui_1")
    
## To be copied in the server
# callModule(mod_updateTree_server, "updateTree_ui_1")
 
