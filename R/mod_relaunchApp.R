#' relaunchApp UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd 
#'
#' @importFrom shiny NS tagList 
mod_relaunchApp_ui <- function(id){
  ns <- NS(id)
  tagList(
    actionButton(ns("reloadSession"), HTML("Relaunch App"),
                 style="color: #fff; background-color:#9c461e ; border-color: #9c461e; width: 200px;", icon("refresh")))
}
##c85f66
#HTML("Relaunch the <br/> Application")
    
#' relaunchApp Server Function
#'
#' @noRd 
mod_relaunchApp_server <- function(input, output, session){
  ns <- session$ns
  
  #this will reload the session and clear exisiting info - good if you want to start TOTALLY new
  observeEvent(input$reloadSession,{
    session$reload()
  })
 
}
    
## To be copied in the UI
# mod_relaunchApp_ui("relaunchApp_ui_1")
    
## To be copied in the server
# callModule(mod_relaunchApp_server, "relaunchApp_ui_1")
 
