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
mod_exampleDisplay_server <- function(input, output, session, extreeFileOut){
  ns <- session$ns
  
  
  exmake_tree <- reactive({
    ggtree::ggtree(extreeFileOut())+
      ggtree::geom_tiplab()+
      ggplot2::xlim(NA, 0.02)})
  
  
  #displays the tree plot, uses output from the displayTree module 
  observeEvent(input$add_tree, {output$treeDisplay <- renderPlot({
    exmake_tree()})
  })
 
}
    
## To be copied in the UI
# mod_exampleDisplay_ui("exampleDisplay_ui_1")
    
## To be copied in the server
# callModule(mod_exampleDisplay_server, "exampleDisplay_ui_1")
 
