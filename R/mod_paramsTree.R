# Module UI

#' @title   mod_paramsTree_ui and mod_paramsTree_server
#' @description  A shiny Module. This module holds all of the parameters that can be adjusted for the visualization of a phylogenetic tree.
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
mod_paramsTree_ui <- function(id){
  ns <- NS(id)
  tagList(
    
    tags$table(width ="100%",
               tags$th("Alter tree viz parameters", colspan="3", style="font-size:20px; color:#afafae")),
    tags$hr(style="border-color: black;"),
    column(
      checkboxInput(ns("alignTips"), ("Align the tips"), FALSE),
      numericInput(ns("numScale"), "Size of Scale Bar - ", value = 0.001, step = 0.001),
      numericInput(ns("nodeDisplay"), "Min. Value of Bootstrap -", value = 50, max = 100), width = 3),
    
    column(
      checkboxInput(ns("midPoint"), "Midpoint Root", TRUE),
      numericInput(ns("tipLim"), 'Add Spacing to Plot - ', value = 0.02, max = 1, step = 0.01 ), 
      numericInput(ns("labelOffset"), "Move All Annotations -", value = 0.005, step = 0.01), width = 3),
    
    column(
      radioButtons(ns("treeFormat"), "Tree layout -", 
                   choices = list(
                     "rectangular" = "rectangular", "slanted" = "slanted", 
                     "circular" = "circular", "fan" = "fan"), selected = "rectangular"),
      selectInput(ns("color"), "Tip Label Color - ", c("blue", "red", "black", "gray")), width =3),
    
    column(
      radioButtons(ns("fontFormat"), "Font Format - ", 
                   choices = list(
                     "bold" = "bold", "italic" = "italic", 
                     "bold+italic" = "bold.italic"), selected = "bold"), width = 3),     
    tags$table(width ="100%",
               tags$th("tree display", colspan="3", style="font-size:20px; color:#afafae")),
    tags$hr(style="border-color: black;")
    
  )
}

# Module Server

#' @rdname mod_paramsTree
#' @export
#' @keywords internal

mod_paramsTree_server <- function(input, output, session){
  ns <- session$ns
  
  list(
    align = reactive(input$alignTips),
    treeformat = reactive(input$treeFormat),
    font = reactive(input$fontFormat),
    numscale = reactive(input$numScale),
    node = reactive(input$nodeDisplay),
    lim = reactive(input$tipLim),
    midP = reactive(input$midPoint), 
    labelOff = reactive(input$labelOffset),
    labColor = reactive(input$color))
}

## To be copied in the UI
# mod_paramsTree_ui("paramsTree_ui_1")

## To be copied in the server
# callModule(mod_paramsTree_server, "paramsTree_ui_1")
