#' @import shiny
app_server <- function(input, output,session) {
  # List the first level callModules here
  datafile <- callModule(mod_read_in_data_server, "read_in_data_ui_1",
                         stringsAsFactors = FALSE)
  
  output$table <- renderDataTable({
    datafile()
  })
}


