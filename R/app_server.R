#' @import shiny
#' @import shiny
app_server <- function(input, output,session) {
  # List the first level callModules here
  treeDisplay <- callModule(mod_uploadTree_server, "uploadTree_ui_1")
  
  params <- callModule(mod_paramsTree_server, "paramsTree_ui_1")
  
  callModule(mod_displayTree_server, "displayTree_ui_1", treeDisplay, 
             params$align, params$numscale, params$treeformat, params$font, params$node)
  
  metafile <- callModule(mod_dataInput_server, "dataInput_ui_meta")
  
  callModule(mod_displayTable_server, "displayTable_ui_1", metafile)
  
  genefile <- callModule(mod_dataInput_server, "dataInput_ui_gene")
  
  callModule(mod_displayTable_server, "displayTable_ui_2", genefile)
  
}

