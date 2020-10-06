#' @import shiny
#' @import shiny
app_server <- function(input, output,session) {
  # List the first level callModules here
  
  #uplodad the data (tree, genetic distance and meta data) and call that module output by dataDisplay$
  dataDisplay <- callModule(mod_uploadData_server, "uploadData_ui_1")
  
  #module which holds the tree viz parameters and referenced with params$
  params <- callModule(mod_paramsTree_server, "paramsTree_ui_data")
  
  buttons <- callModule(mod_pushButtons_server, "pushButtons_ui_data")
  
  #displays the tree and uses the params as input to change tree viz
  plot <- callModule(mod_displayTree_server, "displayTree_ui_data", dataDisplay$treeFileOut, dataDisplay$ geneObjectOutForS4, 
                     params$treeformat, params$lim, params$align, params$font, params$numscale, params$node, params$midP)
  
  #annotates tree with incorporated tree viz parameters
  treeWLayers <- callModule(mod_cladeAnnotator_server, "cladeAnnotator_ui_data", buttons$addTree, buttons$addAnno, buttons$removeAnno, 
                            plot$makeTreeOut,  dataDisplay$geneObjectForSNP, params$labelOff,  params$labColor)
  
  #allows tree with annotation and viz parameters to be donwloaded
  callModule(mod_downloadImage_server, "downloadImage_ui_1", treeWLayers)
  
  ##repeated for preloaded data; no need to donwload so that module is not included below
  #uplodad the data (tree, genetic, and meta) and call that module dataDisplay. This is the only module that is re-written, the rest of the same 
  #used in the above section. 
  exampleData <- callModule(mod_exampleData_server, "exampleData_ui_1")
  
  exampleParams <- callModule(mod_paramsTree_server, "paramsTree_ui_example")
  
  exampleButtons <- callModule(mod_pushButtons_server, "pushButtons_ui_example")
  
  examplePlot <- callModule(mod_displayTree_server, "displayTree_ui_example",exampleData$extreeFileOut, exampleData$exGeneFileCorOrUnOut,
             exampleParams$treeformat, exampleParams$lim, exampleParams$align, exampleParams$font, exampleParams$numscale, exampleParams$node, exampleParams$midP)
  
  callModule(mod_cladeAnnotator_server, "cladeAnnotator_ui_example", exampleButtons$addTree, exampleButtons$addAnno, exampleButtons$removeAnno,
             exampleData$exGeneObjectForSNP, examplePlot$makeTreeOut, exampleParams$labelOff, exampleParams$labColor)
  
}

