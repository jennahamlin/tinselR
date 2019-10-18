# Module UI
  
#' @title   mod_read_in_data_ui and mod_read_in_data_server
#' @description  A shiny Module.
#'
#' @param id shiny id
#' @param input internal
#' @param output internal
#' @param session internal
#'
#' @rdname mod_read_in_data
#'
#' @keywords internal
#' @export 
#' @importFrom shiny NS tagList 
mod_read_in_data_ui <- function(id){
  ns <- NS(id)
  tagList(
    fileInput("csvFile", "Drag cars.csv over here!"),
    tableOutput("rawData")
  )
}
    
# Module Server
    
#' @rdname mod_read_in_data
#' @export
#' @keywords internal
    
mod_read_in_data_server <- function(input, output, session){
  ns <- session$ns
  rawData <- eventReactive(input$csvFile, {
    read.csv(input$csvFile$datapath)
  })
  
  output$rawData <- renderTable({
    rawData() %>% head
  })
}
    
## To be copied in the UI
    
## To be copied in the server
 
