#' @import shiny
app_server <- function(input, output,session) {
  # List the first level callModules here
  metadatafile <- callModule(mod_csvFileInput_server, "csvFileInput_ui_1",
                         stringsAsFactors = FALSE)
  
  output$table <- renderDataTable({
    metadatafile()
  })
  genedatafile <- callModule(mod_csvFileInput_server, "csvFileInput_ui_1",
                         stringsAsFactors = FALSE)
  
  output$table <- renderDataTable({
    genedatafile()
  })
}


