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
mod_treeInput_ui <- function(id, label){
  ns <- NS(id)
  tagList(
    
    fileInput(ns("file"),
              label),
    checkboxInput(ns("midp"), label = "Midpoint Root", TRUE),
    checkboxInput(ns("alignTipLabels"), "Align tip labels", TRUE),
    checkboxInput(ns("showNodeLabels"), "Show node labels", FALSE),
    checkboxInput("addScaleBar", label = "Add scale bar", FALSE),
    #checkboxInput(ns("edgeL"), label = "Use edge length", FALSE)
    #numericInput("edgeW", "Edge width", value=1, min=1)
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
  
  return(datafile)
  # values <-reactiveValues()
  # 
  # values$tips <- FALSE
  # values$nodes <- FALSE
  # edgelength <- FALSE
  # 
  # headfile <- reactive({
  #   
  #     if (input$showNodeLabels == TRUE) {
  #     values$nodes <- TRUE
  #   }
  #    if (input$alignTipLabels == TRUE) {
  #      values$tips <- TRUE
  #    }
    #  if (input$midp == TRUE){
    #    return(phytools::midpoint.root(datafile()))
    #  }
     # if(input$edgeL == TRUE){
     #     edgelength <- TRUE
     #   }
# if(input$midp) datafile <- phytools::midpoint.root(datafile())
  # return(ape::plot.phylo(datafile(),
  #                       align.tip.label = values$tips,
  #                       show.node.label = nodes
  #                       #use.edge.length = edgelength
  #   )
  #   )
#  })

}


# if(input$scalebar) ape::add.scale.bar()

#if(input$midp == TRUE) {
#  return(phytools::midpoint.root(outTree()))
# }

## To be copied in the UI
# mod_treeInput_ui("treeInput_ui_1")

## To be copied in the server
# callModule(mod_treeInput_server, "treeInput_ui_1")

