#' @import shiny
app_server <- function(input, output, session) {
  # List the first level callModules here
  metafile <- callModule(mod_dataInput_server, "dataInput_ui_meta")
  
  output$metacontents <- renderTable({
    metafile()
    
  })
  
  genefile <- callModule(mod_dataInput_server, "dataInput_ui_gene")
  
  output$genecontents <- renderTable({
    genefile()
    
  })
  
  
  
 
  
  callModule(mod_treeUpdate_server, "treeUpdate_ui_1")
  
  callModule(mod_downloadImage_server, "downloadImage_ui_1")
}
