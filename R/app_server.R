#' @import shiny
app_server <- function(input, output,session) {
  # List the first level callModules here
  metafile <- callModule(mod_dataInput_server, "dataInput_ui_meta")
  
  output$metacontents <- renderTable({
    metafile()
  })
  
  genefile <- callModule(mod_dataInput_server, "dataInput_ui_gene")
  
  output$genecontents <- renderTable({
    genefile()
  })
  
  treefile <-callModule(mod_treeInput_server, "treeInput_ui_1")
  
  output$tree <- renderPlot({
    ape::plot.phylo(treefile())
    
  })
  
}
