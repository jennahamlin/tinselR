# Module UI

#' @title   mod_displayTree_ui and mod_displayTree_server
#' @description  A shiny Module. This module combines the tree with genetic distance as an S4 object that allows tree plotting and accessing the combined data. 
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
  )
  
}

# Module Server

#' @rdname mod_displayTree
#' @export
#' @keywords internal

mod_displayTree_server <- function(input, output, session, 
                                   treeFileOut, geneObjectOutForS4, treeformat, lim, align, font, numscale, node, midP){
  ns <- session$ns
  
  midTree <- reactive({
    if(midP() == TRUE) {
      return(phytools::midpoint.root(treeFileOut()))
    }
    else {
      return(treeFileOut())
    }
  })

  #convert phylogenetic tree (midpoint rooted or not) to tibble to join tree and genetic distance matrix
  treeObject<-reactive({
    tibble::as_tibble(midTree())
  })
  
  #join the treeobject and updated genetic distance file by label and convert to s4 object
  gandTS4 <- reactive({
    print("Line 49")
    print("This is okay for order tree, genetic, annotate, then meta")
    combineGandT(treeObject(), geneObjectOutForS4())
  })
  
  ## displayTree server functions
  treePlot <- function(inputFile){
    label <- NULL
    ggtree::ggtree(inputFile, layout = treeformat())+
      ggplot2::xlim(NA, lim())+
      ggtree::geom_tiplab(align = align(), fontface = font(), family="Helvetica") +
      ggtree::geom_treescale(width = numscale(), x = 0.005, y = -1 )+
      ggtree::geom_text2(ggplot2::aes(label=label, subset = !is.na(as.numeric(label)) & as.numeric(label) > node()), nudge_x = 0.00025)
  }
  
  ########additional reactive tree parameters to possibly include
  #these could be parameters to increase/decrease how far tree fills
  #to the margins 
  #ggplot2::theme(plot.margin=ggplot2::margin(10,10,10,10))
  
  #nudge_x for bootstrap
  
  
  
  #major plotting reactive using an S4 object called above (gandTS4) or the base midTree reactive made from import of treeFileOut and the  Upload data module 
  makeTree <- reactive({
    
    if(is.null(input$id)){ # this disconnects the need for genetic distance file to be uploaded. #not sure why I use input$id here 
      treePlot(midTree())
    } 
    else{
      treePlot(gandTS4())
    }
  })
  
  #return these reactive objects to be used in cladeAnnotator module 
  return(
    list(
      makeTreeOut = reactive(makeTree())
    ))
}

## To be copied in the UI
# mod_displayTree_ui("displayTree_ui_1")

## To be copied in the server
# callModule(mod_displayTree_server, "displayTree_ui_1"