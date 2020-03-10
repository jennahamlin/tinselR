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
  tagList(
    plotOutput(ns("treeDisplay"), brush =ns("plot_brush"))
    , #this displays the tree and allows one to brush tips
    tableOutput(ns("selectedIndivs")) #this displays the brushed tips
  )
}

# Module Server

#' @rdname mod_displayTree
#' @export
#' @keywords internal

mod_displayTree_server <- function(input, output, session, 
                                   treeFile, treeformat, align, font, numscale, node){
  ns <- session$ns
  
  make_tree <- reactive({
    ggtree::ggtree(treeFile(), layout = treeformat())+
      ggtree::geom_tiplab(align = align(), fontface = font(), family="Arial") + 
      ggtree::geom_treescale(width = numscale())+
      ggtree::geom_text2(ggplot2::aes(label=label, subset=!is.na(as.numeric(label)) & label >node()), nudge_x = 0.0002)
  })
  
  output$treeDisplay <- renderPlot({
    make_tree()
  })
  
  dataWithSelection <- reactive({
    brushedPoints(make_tree()$data, input$plot_brush)
  })
  
  output$selectedIndivs <- renderText({ #renderText instead of renderPrint to exclude quotes around output
    ifelse(dataWithSelection()$isTip == TRUE, dataWithSelection()$label, "") 
  })
  
  return(make_tree)
}

## To be copied in the UI
# mod_displayTree_ui("displayTree_ui_1")

## To be copied in the server
# callModule(mod_displayTree_server, "displayTree_ui_1"