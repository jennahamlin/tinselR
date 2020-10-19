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
    actionButton(ns("add_tree"),  "Visualize Tree", 
                 style="color: #fff; background-color:#334b38 ; border-color: #334b38; width: 200px;",  icon("tree")),
    ##c4b6b0
    actionButton(ns("add_annotation"), "Add Annotation(s) ", 
                 style="color: #fff; background-color:#e49600 ; border-color: #e49600; width: 200px;",  icon("plus")),
    ##7ab567
    actionButton(ns("remove_annotation"), "Remove Annotation(s)", icon("minus"),
                 style="color: #fff; background-color:#275a5c ; border-color: #275a5c; width: 200px;"),
    #463d60
    actionButton(ns("add_matrix"), "Add heatmap", icon("bars"),
                 style="color: #fff; background-color: #a4b46b ; border-color:#a4b46b; width: 200px;"),
    
    actionButton(ns("remove_matrix"), "Remove heatmap", icon("bars"),
                 style="color: #fff; background-color:#899097 ; border-color:#899097 ; width: 200px;")
    #d8b189
  )
}

#' pushButtons Server Function
#'
#' @noRd 
mod_pushButtons_server <- function(input, output, session){
  ns <- session$ns
  
  #all of these buttons are 'pushed' and activated in the clade annotator module
  list(
    addTree = reactive(input$add_tree), 
    addAnno = reactive(input$add_annotation),
    removeAnno = reactive(input$remove_annotation),
    addMatrix = reactive(input$add_matrix),
    removeMatrix = reactive(input$remove_matrix)
  )
  
}

## To be copied in the UI
# mod_pushButtons_ui("pushButtons_ui_1")

## To be copied in the server
# callModule(mod_pushButtons_server, "pushButtons_ui_1")

