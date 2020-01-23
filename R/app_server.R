#' @import shiny
app_server <- function(input, output, session) {
  # List the first level callModules here
  
  metafile <- callModule(mod_dataInput_server, "dataInput_ui_meta")
  
  callModule(mod_displayTable_server, "displayTable_ui_1", metafile)
  
  genefile <- callModule(mod_dataInput_server, "dataInput_ui_gene")
  
  callModule(mod_displayTable_server, "displayTable_ui_2", genefile)
  
  treefile <- callModule(mod_treeInput_server, "treeInput_ui_1")
  
  callModule(mod_treeDisplay_server, "treeDisplay_ui_1", treefile)

    }
  
  