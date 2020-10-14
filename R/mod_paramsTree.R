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
               tags$th("Alter Tree Visual Parameters", colspan="3", style="font-size:20px; color:#7ab567;")),
    tags$hr(style="border-color: #99b6d8;"),
    column(
      numericInput(ns("numScale"), tags$b("Size of Scale Bar - ", style="color:#afafae"), value = 0.001, step = 0.001),
      numericInput(ns("nodeDisplay"), tags$b("Min. Value of Bootstrap -", style="color:#afafae"), value = 50, max = 100), 
      numericInput(ns('bootPosition'), tags$b("Bootstrap Positions - ", style="color:#afafae"), value = 0.00025, max =1, step = 0.01), width = 3),
    
    column(
      numericInput(ns("tipLim"), tags$b("Add Spacing to Plot - ", style="color:#afafae"), value = 0.02, max = 1, step = 0.01 ), 
      numericInput(ns("labelOffset"), tags$b("Move All Annotations -", style="color:#afafae"), value = 0.005, step = 0.01),
      selectInput(ns("color"), tags$b("Tip Label Color - ", style="color:#afafae"), c("blue", "red", "black", "gray")), width = 3),
    #numericInput(ns('overlapAdjust'), tags$b("Adjust overlap - ", style="color:#afafae"), value = 0.004, max =1, step = 0.01), 
    
    column(
      radioButtons(ns("fontFormat"), tags$b("Font Format", style="color:#afafae"), 
                   choices = list(
                     "bold" = "bold", "italic" = "italic", 
                     "bold+italic" = "bold.italic"), selected = "bold"),
      checkboxInput(ns("midPoint"), tags$b("Midpoint Root", style="color:#afafae"), TRUE), 
      checkboxInput(ns("alignTips"), tags$b("Align the tips", style="color:#afafae"), FALSE), width = 3),
    
    column(
      radioButtons(ns("treeFormat"), tags$b("Tree layout", style="color:#afafae"), 
                   choices = list(
                     "rectangular" = "rectangular", "slanted" = "slanted", 
                     "circular" = "circular"), selected = "rectangular"), width = 3)
  )
}

# Module Server

#' @rdname mod_paramsTree
#' @export
#' @keywords internal

mod_paramsTree_server <- function(input, output, session){
  ns <- session$ns
  
  # list of tree visulazation parameters that can be changed. 
  # if you want to include more tree viz parameters for a user to adjust, 
  # add them to the ui above and return them here with name to be used in 
  # data display module
  return(list(
    align = reactive(input$alignTips),
    treeformat = reactive(input$treeFormat),
    font = reactive(input$fontFormat),
    numscale = reactive(input$numScale),
    node = reactive(input$nodeDisplay),
    lim = reactive(input$tipLim),
    bootPos = reactive(input$bootPosition), 
    midP = reactive(input$midPoint), 
    labelOff = reactive(input$labelOffset),
    overAdd = reactive(input$overlapAdjust),
    labColor = reactive(input$color)
    ))
}

## To be copied in the UI
# mod_paramsTree_ui("paramsTree_ui_1")

## To be copied in the server
# callModule(mod_paramsTree_server, "paramsTree_ui_1")
