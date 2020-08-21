#' exampleDisplay UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd 
#'
#' @importFrom shiny NS tagList 
mod_exampleDisplay_ui <- function(id){
  ns <- NS(id)
  tagList(
    actionButton(ns("add_tree"),"Visualize Tree"), 
    # Show a plot of the generated distribution
    mainPanel(
      plotOutput(ns("treeDisplay"))
      
    )
 
  )
}
    
#' exampleDisplay Server Function
#'
#' @noRd 
mod_exampleDisplay_server <- function(input, output, session, extreeFileOut, treeformat, lim, align, font, numscale, node, midP){
  ns <- session$ns
  
  
  midTree <- reactive({
    if(midP() == TRUE) {
      return(phytools::midpoint.root(extreeFileOut()))
    }
    else {
      return(extreeFileOut())
    }
  })
  
  treePlot <- function(inputFile){
    ggtree::ggtree(inputFile, layout = treeformat())+
      ggplot2::xlim(NA, lim())+
      ggtree::geom_tiplab(align = align(), fontface = font(), family="Helvetica") +
      ggtree::geom_treescale(width = numscale())+
      ggtree::geom_text2(ggplot2::aes(label=label, subset = !is.na(as.numeric(label)) & as.numeric(label) > node()), nudge_x = 0.00025)
    
  }
  
  #major plotting reactive using an S4 object called above (gandTS4) or the base midTree reactive made from import of treeFileOut and the  Upload data module 
  exmake_tree <- reactive({
    
      treePlot(midTree())
     
      })
  
 
  
  
   # exmake_tree <- reactive({
   #  ggtree::ggtree(extreeFileOut())+
   #    ggtree::geom_tiplab()+
   #    ggplot2::xlim(NA, 0.02)})
   # 
  
  #displays the tree plot, uses output from the displayTree module 
  observeEvent(input$add_tree, {output$treeDisplay <- renderPlot({
    exmake_tree()})
  })
 
}
    
## To be copied in the UI
# mod_exampleDisplay_ui("exampleDisplay_ui_1")
    
## To be copied in the server
# callModule(mod_exampleDisplay_server, "exampleDisplay_ui_1")
 
