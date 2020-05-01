<<<<<<< HEAD
#' @import shiny
#' @import shiny
app_server <- function(input, output,session) {
  # List the first level callModules here
  
  treeDisplay <- callModule(mod_uploadTree_server, "uploadTree_ui_1")
  
  params <- callModule(mod_paramsTree_server, "paramsTree_ui_1")
  
  plot <-callModule(mod_displayTree_server, "displayTree_ui_1", treeDisplay, geneFile, params$treeformat, params$lim, params$align, params$font, params$numscale, params$node)
  
  callModule(mod_downloadImage_server, "downloadImage_ui_1", plot)
  
  metafile <- callModule(mod_dataInput_server, "dataInput_ui_meta")
  
  callModule(mod_displayTable_server, "displayTable_ui_1", metafile)
  
  geneFile <- callModule(mod_dataInput_server, "dataInput_ui_gene")
  
  callModule(mod_displayTable_server, "displayTable_ui_2", geneFile)
  
  callModule(mod_combineTandG_server, "combineTandG_ui_1", plot)
  
}

=======
#' @import shiny
#' @import shiny
app_server <- function(input, output,session) {
  # List the first level callModules here
  
  treeDisplay <- callModule(mod_uploadTree_server, "uploadTree_ui_1")
  
  params <- callModule(mod_paramsTree_server, "paramsTree_ui_1")
  
  plot <-callModule(mod_displayTree_server, "displayTree_ui_1", treeDisplay, geneFile, params$treeformat, params$lim, params$align, params$font, params$numscale, params$node)
  
  callModule(mod_downloadImage_server, "downloadImage_ui_1", plot)
  
  metafile <- callModule(mod_dataInput_server, "dataInput_ui_meta")
  
  callModule(mod_displayTable_server, "displayTable_ui_1", metafile)
  
  geneFile <- callModule(mod_dataInput_server, "dataInput_ui_gene")
  
  callModule(mod_displayTable_server, "displayTable_ui_2", geneFile)
  
  plot2 <- callModule(mod_combineTandG_server, "combineTandG_ui_1", plot)
  
  #callModule(mod_cladeAnnotator_server, "cladeAnnotator_ui_1", plot)
}

>>>>>>> 13d0bd1d1bbae398d0b41cb91af7f1338170f51f
