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
    plotOutput("plot"),
    
    actionButton("add_annotation","Add clade annotation")
    
  )
}
    
#' cladeAnnotator Server Function
#'
#' @noRd 
mod_cladeAnnotator_server <- function(input, output, session, make_tree){
  ns <- session$ns
  
  observe({
    print("render")
    output$plot <- renderPlot({ make_tree() + plot.dat$layer1 })
  })
  
  
  observeEvent(input$add_annotation,{
    # Calculate standard deviation
    plot.dat$layer1 <- geom_errorbar(aes(ymin=mean-sd,ymax=mean+sd))
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
 
