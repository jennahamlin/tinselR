#' @import shiny
#' @import shiny
app_server <- function(input, output,session) {
  # List the first level callModules here
  
  #uplodad the data (tree, genetic distance and meta data) and call that module output by dataDisplay$
  dataDisplay <- callModule(mod_uploadData_server, "uploadData_ui_1")
  
  #module which holds the tree viz parameters and referenced with params$
  params <- callModule(mod_paramsTree_server, "paramsTree_ui_data")
  
  #displays the tree and uses the params as input to change tree viz
  plot <- callModule(mod_displayTree_server, "displayTree_ui_data", dataDisplay$treeFileOut, dataDisplay$geneFileCorOrUnOut, params$treeformat, params$lim, params$align, params$font, params$numscale, params$node, params$midP)
  
  #annotates tree with incorporated tree viz parameters
  treeWLayers <- callModule(mod_cladeAnnotator_server, "cladeAnnotator_ui_data", plot$geneObjectOut, plot$make_treeOut)
  
  #allows tree with annotation and viz parameters to be changed to be donwloaded
  callModule(mod_downloadImage_server, "downloadImage_ui_1", treeWLayers)
  
  ##repeated for preloaded data 
  #uplodad the tree and call that module dataDisplay
  exampleTree <- callModule(mod_exampleData_server, "exampleData_ui_1")
  
  exampleParams <- callModule(mod_paramsTree_server, "paramsTree_ui_example")
  
  #callModule(mod_exampleDisplay_server, "exampleDisplay_ui_1", exampleTree$extreeFileOut,  
  examplePlot <- callModule(mod_displayTree_server, "displayTree_ui_example",exampleTree$extreeFileOut, exampleTree$exgeneFileCorOrUnOut,
             exampleParams$treeformat, exampleParams$lim, exampleParams$align, exampleParams$font, exampleParams$numscale, exampleParams$node, exampleParams$midP)
  
  callModule(mod_cladeAnnotator_server, "cladeAnnotator_ui_example", examplePlot$geneObjectOut, examplePlot$make_treeOut)
  
}

