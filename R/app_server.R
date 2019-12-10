#' @import shiny
app_server <- function(input, output, session) {
  # List the first level callModules here
  
  metafile <- callModule(mod_dataInput_server, "dataInput_ui_meta")
  
  callModule(mod_displayTable_server, "displayTable_ui_1", metafile)
  
  genefile <- callModule(mod_dataInput_server, "dataInput_ui_gene")
  
  callModule(mod_displayTable_server, "displayTable_ui_2", genefile)
  
  treefile <- callModule(mod_treeInput_server, "treeInput_ui_1")
  
  callModule(mod_treeUpdate_server, "treeUpdate_ui_1", treefile)
  
  #output$tree <- renderPlot({
  #  ape::plot.phylo(treefile(),
  #                  align.tip.label = input$aligntiplabels,
  #                  show.node.label = input$shownodelabels,
  #                  edge.width = input$edgewidth)
  #  if(input$scalebar) ape::add.scale.bar()
  #})
  
}