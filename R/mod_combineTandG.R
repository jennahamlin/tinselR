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
    actionButton(ns("add_tree"),"Visualize Tree"),
    actionButton(ns("add_annotation"),"Add Annotation to Tree"),
    actionButton(ns("tree_reset"),"Remove All Annotations on Tree"),
    selectInput(ns("fileType"), label = "Type", choices = c("png", "pdf", "tiff")),
    numericInput(ns("width"), "Width of Image (inches)", value = 6),
    numericInput(ns("height"), "Height of Images (inches)", value = 8),
    downloadButton(ns("download")),
    
    
    
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
  observeEvent(input$add_tree, {output$treeDisplay <- renderPlot({
    make_tree()})
  })

  # Initialize a reactive value and set to zero
  n_annotations <- reactiveVal(0)
  annotations <- reactiveValues()
  
  #reactive that holds the brushed points on a plot
  dataWithSelection <- reactive({
    brushedPoints(make_tree()$data, input$plot_brush)
  })
  
  #add to label to vector if isTip == True this is necessary to exclude the NA in the tip vector
  dataWithSelection2 <- eventReactive(input$plot_brush, {
    tipVector <- c()
    for (i in 1:length(dataWithSelection()$label)) {
      if (dataWithSelection()$isTip[i] == TRUE)
        tipVector <- c(tipVector, dataWithSelection()$label[i])
    }
    return(tipVector)
  })
  
  #function which is the layer
  make_layer <- function(tree, tips, label, color, offset ) {
    ggtree::geom_cladelabel(
      node = phytools::findMRCA(ape::as.phylo(tree), tips),
      label = label,
      color = color, 
      offset = max(tree_plot$data$x) - 0.002
    )
  }
  
  #display that layer onto the tree
  anno_plot <- eventReactive(input$add_annotation, {
    # update the reactive value
    new <- n_annotations() + 1
    n_annotations(new)
    annotations[[paste0("ann", n_annotations())]] <- dataWithSelection2()
    
    plt <-
      #make_tree() +
      lapply(1:n_annotations(), function(i)
        make_layer(
          make_tree(),
          tips = annotations[[paste0("ann", i)]],
          label = paste("Clade", "\nSNP Differences"),
          color = "red", 
          offset = tip_vector[[make_layer]]
        ))
    return(plt)
  })
  
  observeEvent(input$add_annotation,{
    output$treeDisplay <- renderPlot({
      make_tree() + anno_plot()
    })
  })
  
  #Remove and reset all annotations - give user the base tree
  observeEvent(input$tree_reset, {
    output$treeDisplay <- renderPlot({
      make_tree()})
  })
  
  
  treeWLayers <- reactive ({make_tree() + anno_plot()})
  
  output$download <- downloadHandler(
    filename = function() {
      paste("treePlot", '.', Sys.Date(), '.', input$fileType, sep = '')}, #as is this does not include end of file designation (i.e. .pdf, when)
    
    content = function(file) {
      ggplot2::ggsave(file, treeWLayers(), width = input$width, height = input$height)}
  )
  
  # #displays the output from brushed points - makeplot is of class "ggtree" "gg" and "ggplot"
  # output$selectedIndivs<-renderTable(
  #   ifelse(dataWithSelection2()$isTip == TRUE, dataWithSelection2()$label, "")
  #   #,
  #   #caption="Above are the individuals that you selected. Would you like to annotate?"
  # )
  # 
  # #converts the brushed points data into a long data table - displays all possible combinations
  # gandT <-reactive({
  #   dataWithSelection2()%>%
  #     na.omit() %>%
  #     dplyr::select(-c(parent, node, branch.length, isTip, x, y, branch, angle))%>%
  #     tidyr::pivot_longer(-label)
  # })
  # 
  # gandTreduced <- reactive({
  #   gandT()%>%
  #     dplyr::filter(label == dataWithSelection2()$label[1] & name == dataWithSelection2()$label[2])%>%
  #     dplyr::pull(value)
  # })
  # 
  # output$selectedIndivsSNPs <- renderTable({ #renderTable makes a table of values
  #   gandTreduced()
  # })
  # 
  # #return(dataWithSelection)
  # return(make_tree)
  
}

## To be copied in the UI
# mod_combineTandG_ui("combineTandG_ui_1")

## To be copied in the server
# callModule(mod_combineTandG_server, "combineTandG_ui_1")
