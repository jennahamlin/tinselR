#' cladeAnnotatorButtons UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd 
#'
#' @importFrom shiny NS tagList 
mod_cladeAnnotatorButtons_ui <- function(id){
  ns <- NS(id)
  tagList(
    actionButton(ns("add_tree"), "Visualize Tree")
  )
}
    
#' cladeAnnotatorButtons Server Function
#'
#' @noRd 
mod_cladeAnnotatorButtons_server <- function(input, output, session){
  ns <- session$ns
 
  return(list(
    addTree <- reactive(input$add_tree),
    print(addTree)
  ))
  
}
    
## To be copied in the UI
# mod_cladeAnnotatorButtons_ui("cladeAnnotatorButtons_ui_1")
    
## To be copied in the server
# callModule(mod_cladeAnnotatorButtons_server, "cladeAnnotatorButtons_ui_1")
 
