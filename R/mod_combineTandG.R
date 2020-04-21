# Module UI

#' @title   mod_combineTandG_ui and mod_combineTandG_server
#' @description  A shiny Module.
#'
#' @importFrom magrittr %>%
#' @name %>%
#' @param id shiny id
#' @param input internal
#' @param output internal
#' @param session internal
#'
#' @rdname mod_combineTandG
#'
#' @keywords internal
#' @export
#' @importFrom shiny NS tagList
mod_combineTandG_ui <- function(id){
  ns <- NS(id)
  tagList(
    #actionButton(ns("add_tree"),"Visualize tree"),
    actionButton(ns("select_tips"),"Select tips"),
    actionButton(ns("add_annotation"),"Add clade annotation"),
    actionButton(ns("update_tree"),"update tree"),
    
    plotOutput(ns("treeDisplay"), brush =ns("plot_brush")),
    tableOutput(ns("selectedIndivs")),
    tableOutput(ns("selectedIndivsSNPs")) #this displays the brushed tips
  )
}

# Module Server

#' @rdname mod_combineTandG
#' @export
#' @keywords internal

mod_combineTandG_server <- function(input, output, session, make_tree){
  ns <- session$ns

  #makes the tree plot, uses output from the displayTree module - note to self: do i want this in this module or in the displayTree module
    output$treeDisplay <- renderPlot({
      make_tree()
  })
  
  # initialize reactiveValues to hold brushed tip labels
  rv <- reactiveValues()
  
  observeEvent(input$select_tips,{
    rv$selected_points <- NULL
    # add clicked
    rv$selected_points <- rbind(isolate(rv$selected_points), 
                                brushedPoints(make_tree()$data, input$plot_brush))
    # remove _all_ duplicates (toggle)
    # http://stackoverflow.com/a/13763299/3817004
    rv$selected_points <- isolate(
      rv$selected_points[!(duplicated(rv$selected_points) | 
                             duplicated(rv$selected_points, fromLast = TRUE)), ])
    str(rv$selected_points)
  })
  
  #add label to vector if isTip == True for the brushed tips, this creates a list to run the findMRCA function 
  dataWithSelection2 <- reactive({
    tipVector <- c()
    for (i in 1:length(rv$selected_points$label)){ if(rv$selected_points$isTip[i] == TRUE) tipVector <- c(tipVector, rv$selected_points$label[i])}
    return(tipVector)
  })
  
  # incorporate the tipVector information for adding layer
  annotation <-eventReactive(input$add_annotation,{
    ggtree::geom_cladelabel(node=phytools::findMRCA(ape::as.phylo(make_tree()), dataWithSelection2()), label = "Clade", color = "red")
  })
  
  observeEvent(input$update_tree,{
    output$treeDisplay <- renderPlot({isolate(make_tree() +
                                                annotation())
    })
  })
  
  #this reactive value call holds the layers
  rvLayers <- reactiveValues()
  
  observeEvent(input$select_tips, {
    rvLayers$selected <-rbind(isolate(rvLayers$selected),
                              a = make_tree()$layers)
    str(rvLayers$selected)
  })
  
  
#   #reactive that holds the brushed points on a plot
#   dataWithSelection <- reactive({
#     brushedPoints(make_tree()$data, input$plot_brush)
#   })
# 
#   dataWithSelection2 <- reactive({
#     tipVector <- c()
#     for (i in 1:length(dataWithSelection()$label)){ if(dataWithSelection()$isTip[i] == TRUE) tipVector <- c(tipVector,dataWithSelection()$label[i])}
#     return(tipVector)
#   })
# 
#   # add new layer using this reactive
#   layer <- reactive({
#     ggtree::geom_cladelabel(node=phytools::findMRCA(ape::as.phylo(make_tree()), dataWithSelection2()), label = "Clade")
#   })
# 
#   #add that layer onto the displayed tree
#   observeEvent(input$add_annotation, {
#     output$treeDisplay <- renderPlot({make_tree() + layer()})
#   })
#   
   #add that layer onto the displayed tree
   #observeEvent(input$add_annotation, {
  #   output$treeDisplay <- renderPlot({make_tree() + layer()})
  # })

  # Remove and reset all annotations - give user the base tree
  #observeEvent(input$exclude_reset, {
  #  output$treeDisplay <- renderPlot({
  #    make_tree()})
  #})

  #displays the output from brushed points - makeplot is of class "ggtree" "gg" and "ggplot"
  output$selectedIndivs<-renderTable(
    ifelse(dataWithSelection2()$isTip == TRUE, dataWithSelection2()$label, "")
    #,
    #caption="Above are the individuals that you selected. Would you like to annotate?"
  )

  #converts the brushed points data into a long data table - displays all possible combinations
  gandT <-reactive({
    dataWithSelection2()%>%
      na.omit() %>%
      dplyr::select(-c(parent, node, branch.length, isTip, x, y, branch, angle))%>%
      tidyr::pivot_longer(-label)
  })

  gandTreduced <- reactive({
    gandT()%>%
      dplyr::filter(label == dataWithSelection2()$label[1] & name == dataWithSelection2()$label[2])%>%
      dplyr::pull(value)
  })

  output$selectedIndivsSNPs <- renderTable({ #renderTable makes a table of values
    gandTreduced()
  })

  #return(dataWithSelection)
  return(make_tree)

}

## To be copied in the UI
# mod_combineTandG_ui("combineTandG_ui_1")

## To be copied in the server
# callModule(mod_combineTandG_server, "combineTandG_ui_1")
