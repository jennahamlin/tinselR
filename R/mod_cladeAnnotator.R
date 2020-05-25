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
    actionButton(ns("add_tree"),"Visualize Tree"),
    actionButton(ns("add_annotation"),"Add Annotation to Tree"),
    actionButton(ns("tree_reset"),"Reset"),
    actionButton(ns("reload"),"Reload"),
    
    
    plotOutput(ns("treeDisplay"), brush =ns("plot_brush"))
  )
}

#' cladeAnnotator Server Function
#'
#' @noRd 
mod_cladeAnnotator_server <- function(input, output, session, geneObjectOut, make_treeOut){
  ns <- session$ns
  
  # #convert to long data frame - three columns. This takes as input the genetic distance object from display tree module
  geneFile <-reactive({ 
    geneObjectOut()%>%
      na.omit()%>%
      tidyr::pivot_longer(-label)
  })
  
  #remove self comparisons for this table - necessary for snp mean/median calculation. 
  geneFileSNP <-reactive({
    geneFile()[which(geneFile()$label != geneFile()$name),]
  })
  
  #displays the tree plot, uses output from the displayTree module 
  observeEvent(input$add_tree, {output$treeDisplay <- renderPlot({
    make_treeOut()})
  })
  
  # Initialize a reactive value and set to zero
  n_annotations <- reactiveVal(0)
  annotations <- reactiveValues()
  
  #reactive that holds the brushed points on a plot
  dataWithSelection <- reactive({
    brushedPoints(make_treeOut()$data, input$plot_brush)
  })
  
  tipVector <- c()
  
  #add to label to vector if isTip == True
  dataWithSelection2 <- eventReactive(input$plot_brush, {
    for (i in 1:length(dataWithSelection()$label)) {
      if (dataWithSelection()$isTip[i] == TRUE)
        tipVector <- c(tipVector, dataWithSelection()$label[i])
    }
    return(tipVector)
  })
  
  snpVector <- c()
  
  snp_anno <- function(geneFile, tips){
    for (i in 1:length(tips)){
      for (j in 1:length(tips)){
        if(tips[i] == tips[j]) next #https://stackoverflow.com/questions/36329183/exclude-one-fixed-variable-in-for-loop
        snpVector[i]<- geneFile%>%
          dplyr::filter(label == tips[i] & name == tips[j]) %>%
          dplyr::pull(value)
      }
    }
    return(as.numeric(snpVector))
  }
  
  #function which makes the annotation layer(s)
  make_layer <- function(tree, tips, label, color) {
    ggtree::geom_cladelabel(
      node = phytools::findMRCA(ape::as.phylo(tree), tips),
      label = label,
      color = color,
      offset = max(make_treeOut()$data$x)
    )
  }
  
  snpMean <- eventReactive(input$add_annotation, {

    lapply(1:n_annotations(), function(i)
      snp_anno(geneFile = geneFileSNP(),
               tips = annotations[[paste0("ann", i)]]
      ))
  })
  
  #display that layer onto the tree
  anno_plot <- eventReactive(input$add_annotation, {
    
    # update the reactive value as a count
    new <- n_annotations() + 1
    n_annotations(new)
    
    #add the tip vector (aka label) to the annotation reactive value
    annotations[[paste0("ann", n_annotations())]] <- dataWithSelection2()
    
    #list apply over the make_layer function to add the annotation
    plt <-
      lapply(1:n_annotations(), function(i)
        make_layer(
          make_treeOut(),
          tips = annotations[[paste0("ann", i)]],
          label = paste("Clade", "\nSNP(s) -", lapply(snpMean()[i], mean)),  
          color = "red"
        ))
    return(plt)
  })
  
  #this will reload the session and clear exisiting info - good if you want to start TOTALLY new 
  observeEvent(input$reload,{
      session$reload()
    })
  
  #add the annotations when selection is brushed
  observeEvent(input$add_annotation,{
    output$treeDisplay <- renderPlot({
      make_treeOut() + anno_plot()
    })
  })
  
  # # #remove a reactive annotation one by one
  # # #note to self - must have something be brushed
  # anno_plot_undo<- eventReactive(input$tree_reset, {
  #   # update the reactive value as a count of - 1
  # 
  #   new <- n_annotations() - 1
  #   n_annotations(new)
  # 
  #   #list apply over the make_layer function to add the annotation
  #   plt <-
  #     lapply(1:n_annotations(), function(i)
  #       make_layer(
  #         make_treeOut(),
  #         tips = annotations[[paste0("ann", i)]],
  #         label = paste("Clade", "\nSNP(s) -", lapply(snpMean()[i], mean)),
  #         color = "red"
  #       ))
  #   return(plt)
  # })
  # 
  # #remove the annotations
  # observeEvent(input$tree_reset,{
  #   output$treeDisplay <- renderPlot({
  #     if(n_annotations() == 1){
  #       
  #       make_treeOut()
  #     }
  #     else{
  #     make_treeOut() + anno_plot_undo()
  #     }
  #   })
  # })

  #reactive to send tree with annoations to downloadImage module 
  treeWLayers <- reactive ({make_treeOut() +  anno_plot()})
}

## To be copied in the UI
# mod_cladeAnnotator_ui("cladeAnnotator_ui_1")

## To be copied in the server
# callModule(mod_cladeAnnotator_server, "cladeAnnotator_ui_1")

