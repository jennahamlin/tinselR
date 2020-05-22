#' @import shiny
#' @import shiny
app_server <- function(input, output,session) {
  # List the first level callModules here
  
  #uplodad the tree and call that module treeDisplay
  treeDisplay <- callModule(mod_uploadTree_server, "uploadTree_ui_1")
  
  #module which holds the tree viz parameters
  params <- callModule(mod_paramsTree_server, "paramsTree_ui_1")
  
  #displays the tree and uses the params as input to change tree viz
  plot <-callModule(mod_displayTree_server, "displayTree_ui_1", treeDisplay$treeFileOut, geneFile, params$treeformat, params$lim, params$align, params$font, params$numscale, params$node)
  
  #metaFile <- callModule(mod_dataInput_server, "dataInput_ui_meta")
  
  #callModule(mod_displayTable_server, "displayTable_ui_1", metaFile)
  
  geneFile <- callModule(mod_dataInput_server, "dataInput_ui_gene", treeDisplay$metaFileOut, treeDisplay$metaSepOut)
  
  callModule(mod_displayTable_server, "displayTable_ui_2", geneFile)
  
  #annotates tree with incorporated tree viz parameters
  treeWLayers <- callModule(mod_cladeAnnotator_server, "cladeAnnotator_ui_1", plot$geneObjectOut, plot$make_treeOut)
  
  #allows tree with annotation and viz parameters to be changed to be donwloaded
  callModule(mod_downloadImage_server, "downloadImage_ui_1", treeWLayers)
}

