#' @import shiny
app_server <- function(input, output,session) {
  # List the first level callModules here
  datafile <- callModule(mod_dataInput_server, "dataInput_ui_1")
  
  output$contents <- renderTable({
    datafile()
  })
  
}
