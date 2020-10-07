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
    column(
      actionButton(ns("add_tree"),  tags$b("Visualize Tree"), 
                   style="color: #fff; background-color: #d1ad5b; border-color: #d1ad5b; width: 200px;",  icon("tree")),
      
      actionButton(ns("add_annotation"), "Add Annotation(s) ", 
                   style="color: #fff; background-color: #7ab567; border-color: #7ab567; width: 200px;",  icon("plus")),
      
      actionButton(ns("remove_annotation"), HTML("Remove Previous <br/> Annotation(s)"), icon("minus"),
                   style="color: #fff; background-color: #463d60; border-color: #463d60; width: 200px;"), 
      
      mod_relaunchApp_ui("relaunchApp_ui_data"), width =3)
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
 
