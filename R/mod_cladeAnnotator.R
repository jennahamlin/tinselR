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
    shinyjs::useShinyjs(),
    actionButton(ns("add_tree"),"Visualize Tree"),
    actionButton(ns("add_annotation"),"Add Annotation to Tree"),
    actionButton(ns("tree_reset"),"Remove Previous Annotation(s) on Tree"),
    
    plotOutput(ns("treeDisplay"), brush =ns("plot_brush")),
    tableOutput(ns("contents"))
  )
}
    
#' cladeAnnotator Server Function
#'
#' @noRd 
mod_cladeAnnotator_server <- function(input, output, session, make_treeOut, geneObjectOut){
  ns <- session$ns
 

  # #convert to long data frame - three columns. This takes as input the genetic distance object from display tree module 
   geneFile <-  reactive({
     geneObjectOut()%>%
     na.omit()%>%
     tidyr::pivot_longer(-label)
   })
   
   #remove self comparisons for this table - necessary for snp mean/median calculation. 
   geneFileCheck <- reactive({
     geneFile()[which(geneFile()$label != geneFile()$name),]
   })

   output$contents <- renderTable({
     geneFileCheck()
   })
   
  #displays the tree plot, uses output from the displayTree module 
  observeEvent(input$add_tree, {output$treeDisplay <- renderPlot({
    make_treeOut()})
  })
  
  # Initialize a reactive value and set to zero for annotations
  n_annotations <- reactiveVal(0)
  annotations <- reactiveValues()
  
  #reactive that holds the brushed points on a plot
  dataWithSelection <- reactive({
    brushedPoints(make_treeOut()$data, input$plot_brush)
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
  
  #function which makes the annotation layer(s)
  make_layer <- function(tree, tips, label, color, offset ) {
    ggtree::geom_cladelabel(
      node = phytools::findMRCA(ape::as.phylo(tree), tips),
      label = label,
      color = color, 
      offset = max(make_treeOut()$data$x)
    )
  }
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

    snpMean <- eventReactive(input$add_annotation, {lapply(1:n_annotations(), function(i)
      snp_anno(geneFile = geneFile4(),
               tips= tip_vector[[i]]))
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
          label = paste("Clade", "\nSNP Differences"),
          color = "red", 
          offset = tipVector[[i]] #can be make_layer
        ))
    return(plt)
  })
  
  #add the annotations when selection is brushed
  observeEvent(input$add_annotation,{
    output$treeDisplay <- renderPlot({
      make_treeOut() + anno_plot()
    })
  })
  
  #remove a reactive annotation one by one
  #note to self - must have something be brushed 
  anno_plot_undo <- eventReactive(input$tree_reset, {
    # update the reactive value as a count of - 1
    
    new <- n_annotations() - 1
    n_annotations(new)
    
    #list apply over the make_layer function to add the annotation
    plt <-
      lapply(1:n_annotations(), function(i)
        make_layer(
          make_treeOut(),
          tips = annotations[[paste0("ann", i)]],
          label = paste("Clade", "\nSNP Differences"),
          color = "red", 
          offset = tip_vector[[i]] #can be make_layer
        ))
    return(plt)
  })
  
  
  #remove the annotations 
  observeEvent(input$tree_reset,{
    output$treeDisplay <- renderPlot({
      make_treeOut() + anno_plot_undo()
    })
  })
  
  # #Remove and reset all annotations - give user the base tree
  # observeEvent(input$tree_reset, {
  #   output$treeDisplay <- renderPlot({
  #     #shinyjs::reset("add_annotation")
  #     make_treeOut() 
  #     })
  # })
  
  #reactive to send tree with annoations to downloadImage module 
  treeWLayers <- reactive ({make_treeOut() + anno_plot()})
}
    
## To be copied in the UI
# mod_cladeAnnotator_ui("cladeAnnotator_ui_1")
    
## To be copied in the server
# callModule(mod_cladeAnnotator_server, "cladeAnnotator_ui_1")
 
