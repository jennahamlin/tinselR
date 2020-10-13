#' addMatrix UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd 
#'
#' @importFrom shiny NS tagList 
mod_addMatrix_ui <- function(id){
  ns <- NS(id)
  tagList(
    plotOutput(ns("treeDisplay"))
  )
}
    
#' addMatrix Server Function
#'
#' @noRd 
mod_addMatrix_server <- function(input, output, session, addMatrix, makeTreeOut, metaFileOut, metaSep){
  ns <- session$ns
 
  mFile <- fileCheck(fileUp = metaFileOut(), fileType = metaSep(), fileSep = metaSep())
  
  observeEvent(addMatrix() == T, {
  ggtree::gheatmap(makeTreeOut())
  })
}
    
## To be copied in the UI
# mod_addMatrix_ui("addMatrix_ui_1")
    
## To be copied in the server
# callModule(mod_addMatrix_server, "addMatrix_ui_1")
 
