# Module UI
  
#' @title   mod_displayTree_ui and mod_displayTree_server
#' @description  A shiny Module.
#'
#' @param id shiny id
#' @param input internal
#' @param output internal
#' @param session internal
#'
#' @rdname mod_displayTree
#'
#' @keywords internal
#' @export 
#' @importFrom shiny NS tagList 
mod_displayTree_ui <- function(id){
  ns <- NS(id)

  plotOutput(ns("tree"))
  
}
    
# Module Server
    
#' @rdname mod_displayTree
#' @export
#' @keywords internal
    
mod_displayTree_server <- function(input, output, session, outTree){
 ns <- session$ns
 
   output$tree <- renderPlot({
    ape::plot.phylo(outTree(), 
                    #align.tip.label = input$aligntiplabels,
                    #show.node.label = input$shownodelabels,
                    #edge.width = input$edgewidth
                    #if(input$scalebar) ape::add.scale.bar()
    )
  })
}

    
## To be copied in the UI
# mod_displayTree_ui("displayTree_ui_1")
    
## To be copied in the server
# callModule(mod_displayTree_server, "displayTree_ui_1")
 
