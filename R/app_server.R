#' @import shiny
app_server <- function(input, output,session) {
  # List the first level callModules here
  metadatafile <- callModule(mod_csvFileInput_server, "csvFileInput_ui_meta",
                         stringsAsFactors = FALSE)
  
  output$metatable <- renderDataTable({
    metadatafile()
  })
  genedatafile <- callModule(mod_csvFileInput_server, "csvFileInput_ui_genetic",
                         stringsAsFactors = FALSE)
  
  output$genetable <- renderDataTable({
    genedatafile()
  })
}


