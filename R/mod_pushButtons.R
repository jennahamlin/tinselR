#' pushButtons UI Function
#'
#' @title mod_pushButtons_ui mod_pushButtons_server
#'
#' @description A shiny Module. This module generates the displayed action
#' buttons for add/remove annotations and add/remove heatmap.
#'
#' @rdname mod_pushButtons
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#'
#' @importFrom shiny NS tagList
mod_pushButtons_ui <- function(id) {
  ns <- NS(id)
  tagList(

    actionButton(ns("add_tree"),  "Visualize Tree",
                 style = "color: #fff; background-color:#334b38 ;
                 border-color: #334b38; width: 200px;",  icon("tree")),

    actionButton(ns("add_annotation"), "Add Annotation(s) ",
                 style = "color: #fff; background-color:#e49600 ;
                 border-color: #e49600; width: 200px;",  icon("plus")),

    actionButton(ns("remove_annotation"), "Remove Annotation(s)", icon("minus"),
                 style = "color: #fff; background-color:#275a5c ;
                 border-color: #275a5c; width: 200px;"),

    actionButton(ns("add_heatmap"), "Add heatmap", icon("bars"),
                 style = "color: #fff; background-color: #a4b46b ;
                 border-color:#a4b46b; width: 200px;"),

    actionButton(ns("remove_heatmap"), "Remove heatmap", icon("bars"),
                 style = "color: #fff; background-color:#899097 ;
                 border-color:#899097 ; width: 200px;")
  )
}

#' pushButtons Server Function
#'
#' @rdname mod_pushButtons
mod_pushButtons_server <- function(input, output, session) {
  ns <- session$ns

  #all of these buttons are 'pushed' and activated in the clade annotator module
  list(
    add_tree = reactive(input$add_tree),
    add_anno = reactive(input$add_annotation),
    remove_anno = reactive(input$remove_annotation),
    add_heatmap = reactive(input$add_heatmap),
    remove_heatmap = reactive(input$remove_heatmap)
  )
}
