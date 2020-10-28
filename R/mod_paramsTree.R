#' paramsTree UI Function
#'
#' @title   mod_paramsTree_ui and mod_paramsTree_server
#'
#' @description  A shiny Module. This module holds all of the parameters that
#' can be adjusted for the visualization of a phylogenetic tree.
#'
#' @param id shiny id
#' @param input internal
#' @param output internal
#' @param session internal
#'
#' @rdname mod_paramsTree
#'
#' @keywords internal
#' @export
#' @importFrom shiny NS tagList
mod_paramsTree_ui <- function(id) {
  ns <- NS(id)
  tagList(
    tags$table(width = "100%",
               tags$th("Alter Tree Visual Parameters", colspan = "3",
                       style = "font-size:20px; color:#444444;")),
    tags$hr(style = "border-color: #99b6d8;"),

    column(
      numericInput(ns("number_scale"), tags$b("Size of Scale Bar - ",
                                          style = "color:#afafae"),
                   value = 0.001, step = 0.001),
      numericInput(ns("node_display"), tags$b("Min. Value of Bootstrap -",
                                             style = "color:#afafae"), value =
                     50, max = 100),
      numericInput(ns("boot_position"), tags$b("Bootstrap Positions - ",
                                              style = "color:#afafae"),
                   value = 0.00025, max = 1, step = 0.01), width = 3),

    column(
      numericInput(ns("tip_lim"), tags$b("Add Spacing to Plot - ",
                                        style  = "color:#afafae"), value = 0.02,
                   max = 1, step = 0.01),
      numericInput(ns("label_offset"), tags$b("Move All Annotations -",
                                             style = "color:#afafae"),
                   value = 0.005, step = 0.01),
      numericInput(ns("mat_offset"), tags$b("Move Matrix -",
                                           style = "color:#afafae"),
                   value = 0.05, step = 0.01), width = 3),
    column(
      checkboxInput(ns("midPoint"), tags$b("Midpoint Root",
                                           style = "color:#afafae"), TRUE),
      checkboxInput(ns("align_tips"), tags$b("Align the tips",
                                            style = "color:#afafae"), FALSE),
      selectInput(ns("color"), tags$b("Annotation Label Color - ",
                                      style = "color:#afafae"),
                  c("blue", "red", "black", "gray")),
      selectInput(ns("heatmap_color"), tags$b("Heatmap Color Options - ",
                                         style="color:#afafae"),
                  c("A", "B", "C", "D", "E")), width = 3),
    column(
      radioButtons(ns("font_format"), tags$b("Font Format",
                                            style = "color:#afafae"),
                   choices = list(
                     "bold" = "bold", "italic" = "italic",
                     "bold+italic" = "bold.italic"), selected = "bold"),
      radioButtons(ns("tree_format"), tags$b("Tree layout",
                                            style = "color:#afafae"),
                   choices = list(
                     "rectangular" = "rectangular", "slanted" = "slanted",
                     "circular" = "circular"), selected = "rectangular"),
      width = 3),

    tags$hr(style = "border-color: #99b6d8;"),
    tags$table(width = "100%",
               tags$th("Tree Display", colspan = "3", style = "font-size:20px;
                       color:#444444;")),
    tags$hr(style = "border-color: #99b6d8;")
  )
}

#' paramsTree Server Function
#'
#' @rdname mod_paramsTree
#' @export
#' @keywords internal

mod_paramsTree_server <- function(input, output, session) {
  ns <- session$ns

  # list of tree visualization parameters that can be changed.
  # if you want to include more tree viz parameters for a user to adjust,
  # add them to the ui above and return them here with name to be used in
  # data display module
  return(list(
    align = reactive(input$align_tips),
    tree_format = reactive(input$tree_format),
    font = reactive(input$font_format),
    num_scale = reactive(input$number_scale),
    node = reactive(input$node_display),
    lim = reactive(input$tip_lim),
    boot_pos = reactive(input$boot_position),
    mid_p = reactive(input$midPoint),
    label_off = reactive(input$label_offset),
    lab_color = reactive(input$color),
    mat_off = reactive(input$mat_offset),
    heat_col = reactive(input$heatmap_color)
  ))
}
