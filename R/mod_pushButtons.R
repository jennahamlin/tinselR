#' pushButtons UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd 
#'
#' @importFrom shiny NS tagList 
mod_pushButtons_ui <- function(id){
  ns <- NS(id)
  tagList(
    actionButton(ns("add_tree"), "Visualize Tree"),
    actionButton(ns("add_annotation"), HTML("Add Annotation <br/> to Tree"), icon("plus"), class = "btn btn-primary"),
    actionButton(ns("remove_annotation"), HTML("Remove Previous <br/> Annotation(s) on Tree"),icon("minus"),class = "btn btn-primary")
  )
}
    
#' pushButtons Server Function
#'
#' @noRd 
mod_pushButtons_server <- function(input, output, session){
  ns <- session$ns
  

  list(
    addTree = reactive(input$add_tree),
    addAnno = reactive(input$add_annotation),
    removeAnno = reactive(input$remove_annotation)
  )

}
    
## To be copied in the UI
# mod_pushButtons_ui("pushButtons_ui_1")
    
## To be copied in the server
# callModule(mod_pushButtons_server, "pushButtons_ui_1")
 
