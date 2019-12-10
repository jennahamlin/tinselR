# Module UI

#' @title   mod_treeUpdate_ui and mod_treeUpdate_server
#' @description  A shiny Module.
#'
#' @param id shiny id
#' @param input internal
#' @param output internal
#' @param session internal
#'
#' @rdname mod_treeUpdate
#'
#' @keywords internal
#' @export 
#' @importFrom shiny NS tagList 
mod_treeUpdate_ui <- function(id){
  ns <- NS(id)
  tagList(
    plotOutput("tree")
    
    
  )
}

# Module Server

#' @rdname mod_treeUpdate
#' @export
#' @keywords internal

mod_treeUpdate_server <- function(input, output, session, outTree){
  ns <- session$ns
  
  
  
}

## To be copied in the UI
# mod_treeUpdate_ui("treeUpdate_ui_1")

## To be copied in the server
# callModule(mod_treeUpdate_server, "treeUpdate_ui_1")

