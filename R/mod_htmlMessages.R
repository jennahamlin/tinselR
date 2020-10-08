#' htmlMessages UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd 
#'
#' @importFrom shiny NS tagList 
mod_htmlMessages_ui <- function(id){
  ns <- NS(id)
  tagList(
    # #uiOutput(ns('fileChecking'))
    # htmlOutput(ns('fileChecking'))
  )
}
    
#' htmlMessages Server Function
#'
#' @noRd 
mod_htmlMessages_server <- function(input, output, session, fileTest){
  ns <- session$ns
 
  # output$fileChecking <- renderUI({
  # ns <- session$ns
  #   if(is.null(fileTest())) {
  #     HTML('<span style="color:green"><font size=4><strong>No errors were detected in the input files. Please continue with Tinsel by clicking the Update Plot button at the bottom of the Decorate Your Tree section below.</strong></font></span>')
  #   } else {
  #    HTML(fileTest())
  #   }
  # })
  
}
    
## To be copied in the UI
# mod_htmlMessages_ui("htmlMessages_ui_1")
    
## To be copied in the server
# callModule(mod_htmlMessages_server, "htmlMessages_ui_1")
 
