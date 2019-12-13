# Module UI

#' @title   mod_treeInput_ui and mod_treeInput_server
#' @description  A shiny Module.
#'
#' @param id shiny id
#' @param input internal
#' @param output internal
#' @param session internal
#'
#' @rdname mod_treeInput
#'
#' @keywords internal
#' @export 
#' @importFrom shiny NS tagList 
mod_treeInput_ui <- function(id){
  ns <- NS(id)
  tagList(
    
    fileInput(ns("file"),
              label = "Select Tree File:"),
    checkboxInput(ns("midp"), "Midpoint Root", TRUE),
    checkboxInput(ns("alignTipLabels"), "Align tip labels", TRUE),
    checkboxInput(ns("showNodeLabels"), "Show node labels", FALSE)
    #checkboxInput("scalebar", "Add scale bar", FALSE),
    #numericInput("edgewidth", "Edge width", value=1, min=1)
  )
}

# Module Server

#' @rdname mod_treeInput
#' @export
#' @keywords internal

mod_treeInput_server <- function(input, output, session){
  ns <- session$ns
  
  userFile <- reactive({
    validate(need(input$file !="", "Please import a tree file"))
    input$file
  })   
  
  datafile <- reactive({
    ape::read.tree(userFile()$datapath)
  })
  
  tips <- FALSE
  nodes <- FALSE
  
  headfile <- reactive({
    if (input$showNodeLabels == TRUE) {
      nodes <- TRUE
    }
    if (input$alignTipLabels == TRUE) {
      tips <- TRUE
    }
    # if (input$addScaleBar == TRUE) {
    #   scale <- TRUE
    # }
    return(ape::plot.phylo(datafile(),
                           align.tip.label = tips,
                           show.node.label = nodes)
    )
  })
  
}

#if(input$scalebar) ape::add.scale.bar()

## To be copied in the UI
# mod_treeInput_ui("treeInput_ui_1")

## To be copied in the server
# callModule(mod_treeInput_server, "treeInput_ui_1")

