#' @import shiny
app_server <- function(input, output, session) {
  # List the first level callModules here
  
  metafile <- callModule(mod_dataInput_server, "dataInput_ui_meta")
  
  callModule(mod_displayTable_server, "displayTable_ui_1", metafile)
  
  genefile <- callModule(mod_dataInput_server, "dataInput_ui_gene")
  
  callModule(mod_displayTable_server, "displayTable_ui_2", genefile)
  
  treefile <- callModule(mod_treeInput_server, "treeInput_ui_1")
  
  updatetree <- callModule(mod_displayTree_server, "displayTree_ui_1", treefile)
  
  output$tree2 <- renderPlot({
    ape::plot.phylo(updatetree(), 
                    align.tip.label = input$aligntiplabels)})
}