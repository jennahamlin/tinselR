#' @import shiny
app_server <- function(input, output, session) {
  # List the first level callModules here
  metafile <- callModule(mod_dataInput_server, "dataInput_ui_meta")
  
  output$metacontents <- renderTable({
    if(input$disp == "head") {
      return(head(metafile()))
    }
    else {
      return(metafile())
    }
    
  })
  
  genefile <- callModule(mod_dataInput_server, "dataInput_ui_gene")
  
  output$genecontents <- renderTable({
    if(input$disp == "head") {
      return(head(genefile()))
    }
    else {
      return(genefile())
    }
    
  })
  
  treefile <-callModule(mod_treeInput_server, "treeInput_ui_1")
  
  output$tree <- renderPlot({
    ape::plot.phylo(treefile())
    
  })
  
}
