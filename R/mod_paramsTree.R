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
    checkboxInput(ns("alignTips"), "Align tip labels", FALSE),
    checkboxInput(ns("midPoint"), "Midpoint Root Tree", TRUE),
    
    numericInput(ns("tipLim"), 'Add spacing to plot - ', value = 0.02, max = 1, step = 0.01 ),
    numericInput(ns("numScale"), "Size of the scale bar - ", value = 0.001, step = 0.001),
    numericInput(ns("nodeDisplay"), "Minimum value of bootstrap -", value = 50, max = 100),
    radioButtons(ns("treeFormat"), "What tree layout?", 
                       choices = list(
                         "rectangular" = "rectangular", "slanted" = "slanted", 
                         "circular" = "circular", "fan" = "fan"), selected = "rectangular"),
    radioButtons(ns("fontFormat"), "What font format for tip labels?", 
                       choices = list(
                         "bold" = "bold", "italic" = "italic", 
                         "bold+italic" = "bold.italic"), selected = "bold")
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
    midP = reactive(input$midPoint))
}

## To be copied in the UI
# mod_paramsTree_ui("paramsTree_ui_1")

## To be copied in the server
# callModule(mod_paramsTree_server, "paramsTree_ui_1")
