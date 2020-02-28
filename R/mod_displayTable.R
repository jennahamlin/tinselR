# Module UI
  
#' @title   mod_displayTable_ui and mod_displayTable_server
#' @description  A shiny Module.
#'
#' @param id shiny id
#' @param input internal
#' @param output internal
#' @param session internal
#'
#' @rdname mod_displayTable
#'
#' @keywords internal
#' @export 
#' @importFrom shiny NS tagList 
mod_displayTable_ui <- function(id){
  ns <- NS(id)
  tagList(
  #tabPanel(
    #name,
    tableOutput(ns("contents"))
  )
}

# Module Server
    
#' @rdname mod_displayTable
#' @export
#' @keywords internal
    
mod_displayTable_server <- function(input, output, session, file){
  ns <- session$ns
  output$contents <- renderTable({
    file()
  })
}
    
## To be copied in the UI
# mod_displayTable_ui("displayTable_ui_1")
    
## To be copied in the server
# callModule(mod_displayTable_server, "displayTable_ui_1")
 
