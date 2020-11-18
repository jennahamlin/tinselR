#' pushButtons Function
#'
#' @title mod_pushButtons_ui mod_pushButtons_server
#'
#' @description A shiny Module. This module generates the displayed action
#' buttons for adding the tree, add/remove annotations, or add/remove heatmap.
#'
#' @rdname mod_pushButtons
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @keywords internal
#' @importFrom shiny NS tagList
mod_pushButtons_ui <- function(id) {
  ns <- NS(id)
  tagList(

    actionButton(ns("add_tree"),  HTML("Visualize <br/> Tree"),
                 style = "color: #fff; background-color:#334b38;
                 border-color: #334b38; width: 150px;",  icon("tree")),

    actionButton(ns("add_annotation"), HTML("Add <br/> Annotation(s)"),
                 style = "color: #fff; background-color:#e49600;
                 border-color: #e49600; width: 150px;",  icon("plus")),

    actionButton(ns("remove_annotation"), HTML("Remove <br/> Annotation(s)"),
                 icon("minus"), style = "color: #fff; background-color:#275a5c;
                 border-color: #275a5c; width: 150px;"),

    actionButton(ns("add_heatmap"), HTML("Add <br/> heatmap"), icon("plus"),
                 style = "color: #fff; background-color: #a4b46b;
                 border-color:#a4b46b; width: 150px;"),

    actionButton(ns("remove_heatmap"), HTML("Remove <br/>  heatmap"),
                 icon("minus"), style = "color: #fff; background-color:#899097;
                 border-color:#899097 ; width: 150px;")
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
