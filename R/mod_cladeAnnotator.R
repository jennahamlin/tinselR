#' cladeAnnotator UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd 
#'
#' @importFrom shiny NS tagList 
mod_cladeAnnotator_ui <- function(id){
  ns <- NS(id)
  tagList(
    actionButton(ns("add_annotation"),"Add clade annotation")
    
  )
}
    
#' cladeAnnotator Server Function
#'
#' @noRd 
mod_cladeAnnotator_server <- function(input, output, session, make_tree){
  ns <- session$ns
  
  observeEvent(input$add_annotation,{
    # add new layer
    plot.dat$layer1 <<- ggtree::geom_cladelabel(node=28, label = "a clade") 
  })
  
  observe({
    #print("render")
    output$plot <- renderPlot({make_tree() + plot.dat$layer1})
  })
  
  
  # tip.lab <- reactive({
  #    dataWithSelection()
  #  })
  #  
  # make_tree_annot<- reactive ({
  # makeplot() +  ggtree::geom_cladelabel(node= phytools::findMRCA(ape::as.phylo(tree), tip.lab()[[1]]), label = "clade") 
  # })
  # 
  # output$treeDisplay <- renderPlot({
  #   #str(tip.lab())
  #   make_pl()
  # })
  # 
}
    
## To be copied in the UI
# mod_cladeAnnotator_ui("cladeAnnotator_ui_1")
    
## To be copied in the server
# callModule(mod_cladeAnnotator_server, "cladeAnnotator_ui_1")
 
