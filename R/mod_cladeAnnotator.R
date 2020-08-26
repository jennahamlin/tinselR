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
    actionButton(ns("add_annotation"),"Add Annotation to Tree", icon("refresh"),
                 class = "btn btn-primary"),
    actionButton(ns("tree_reset"),"Remove Previous Annotation(s) on Tree", icon("refresh"),
                 class = "btn btn-primary"),
    actionButton(ns("reload"), "Reload the Shiny application session"), 
    plotOutput(ns("treeDisplay"), brush = ns("plot_brush"))
  )
}

#' cladeAnnotator Server Function
#'
#' @noRd 
mod_cladeAnnotator_server <- function(input, output, session, geneObjectOut, make_treeOut){
  ns <- session$ns
  
  #convert to long data frame - three columns.
  #This takes as input the genetic distance object from display tree module
  geneFile <-reactive({
    label <- NULL
    geneObjectOut()%>%
      stats::na.omit()%>%
      tidyr::pivot_longer(-label)
  })

  #remove self comparisons for this table - necessary for snp mean/median calculation.
  geneFileSNP <-reactive({
    label <- NULL
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
  
  #add label to tipVector if isTip == True
  dataWithSelection2 <- eventReactive(input$plot_brush, {
    label <- NULL
    for (i in 1:length(dataWithSelection()$label)) {
      if (dataWithSelection()$isTip[i] == TRUE)
        tipVector <- c(tipVector, dataWithSelection()$label[i])
    }
    return(tipVector)
  })
  
  #output$textDisplay <-renderText(dataWithSelection2())
  
  makeLayer <- function(tree, tips, label, color, offset) {
    ggtree::geom_cladelabel(
      node = phytools::findMRCA(ape::as.phylo(tree), tips),
      label = label,
      color = color,
      angle = 0,
      offset = offset
    )
  }
  
  #use snp_anno function to get the snps differences between compared tips
  snpMean <- eventReactive(input$add_annotation, {
    lapply(1:n_annotations(), function(i)
      snpAnno(geneFile = geneFileSNP(),
              tips = annotations$data[[paste0("ann", i)]]
      ))
  })
  
  checkOverlap <- function(previous_plot, incoming_tips) {
    pre_g <- ggplot2::ggplot_build(previous_plot)
    
    tip_labels <- pre_g$data[[3]]
    
    incoming_y_coords <-
      tip_labels[tip_labels$label %in% incoming_tips, "y"]
    
    if (length(pre_g$data) < 4) {
      any_overlap <- FALSE
    } else {
      clade_segments <- pre_g$data[[4]]
      
      overlaps <- sapply(1:nrow(clade_segments), function(i) {
        X <- DescTools::Overlap(
          x = c(clade_segments[i, "y"], clade_segments[i, "yend"]), 
          y = incoming_y_coords)
        Y <- X > 0})
    }
  }
  
  addAnnotations <- function(tree_plot, tip_vector) {
    g <- tree_plot
    
    for (i in seq_along(tip_vector)) {
      any_overlap <- checkOverlap(previous_plot = g, incoming_tips = tip_vector[[i]])
      print(tip_vector[[i]])
      
      print(any_overlap)                                                                           
      #print(current_offset)
      
      g <- g +
        makeLayer(
          tree_plot,
          tips = tip_vector[[i]],
          label = paste("Clade","\nrange of SNP(s) -", lapply(snpMean()[i], function(x){round(range(x),0)})),
          color = rev(colors())[i],
          offset = current_offset <- ifelse(any_overlap, 0.011, 0.008)
        )
    }
    return(g)
  }
  
  #display that layer onto the tree
  observeEvent(input$add_annotation, {
    
    # update the reactive value as a count
    new <- n_annotations() + 1
    n_annotations(new)
    
    #add the tip vector (aka label) to the annotation reactive value
    annotations$data[[paste0("ann", n_annotations())]] <- dataWithSelection2()
    
    tips <- lapply(1:n_annotations(), function(i)
      annotations$data[[paste0("ann", i)]])
    
    output$treeDisplay <- renderPlot({
      addAnnotations(tree_plot = make_treeOut() , tip_vector =  tips)
      })
    })
  
  #display that layer onto the tree
  observeEvent(input$tree_reset, {
    
    # update the reactive value as a count
    new <- n_annotations() - 1
    n_annotations(new)
    
    #add the tip vector (aka label) to the annotation reactive value
    annotations$data[[paste0("ann", n_annotations())]] <- dataWithSelection2()
    
    tips <- lapply(1:n_annotations(), function(i)
      annotations$data[[paste0("ann", i)]])
    
    output$treeDisplay <- renderPlot({
      addAnnotations(tree_plot = make_treeOut() , tip_vector =  tips)
    })
  })
  
  
  # #display that layer onto the tree
  # anno_plot <- eventReactive(input$add_annotation, {
  # 
  #   # update the reactive value as a count
  #   new <- n_annotations() + 1
  #   n_annotations(new)
  # 
  #   #add the tip vector (aka label) to the annotation reactive value
  #   annotations$data[[paste0("ann", n_annotations())]] <- dataWithSelection2()
  #   
  #   #list apply over the makeLayer function to add the annotation
  #   plt <-
  #     lapply(1:n_annotations(), function(i)
  #       makeLayer(
  #         make_treeOut(),
  #         tips = annotations$data[[paste0("ann", i)]],
  #         label = paste("Clade", "\nSNP(s) -", lapply(snpMean()[i], function(x){round(mean(x),0)})),
  #         color = "red",
  #         offSet = max(make_treeOut()$data$x)
  #       ))
  #   return(plt)
  # 
  # })
  
  # #add the annotations when selection is brushed
  # observeEvent(input$add_annotation,{
  #   output$treeDisplay <- renderPlot({
  #     validate(need(input$plot_brush !="", "Please import a genetic distance file to use the clade annotator"))
  #     
  #     make_treeOut() + anno_plot()
  #   })
  # })
  
  
  #this will reload the session and clear exisiting info - good if you want to start TOTALLY new 
  observeEvent(input$reload,{
    session$reload()
  })
  
  # #remove a reactive annotation one by one
  # #note to self - must have something be brushed
  # anno_plot_undo<- eventReactive(input$tree_reset, {
  #   # update the reactive value as a count of - 1
  #   
  #   new <- n_annotations() - 1
  #   n_annotations(new)
  #   
  #   #list apply over the makeLayer function to add the annotation
  #   plt <-
  #     lapply(1:n_annotations(), function(i)
  #       makeLayer(
  #         make_treeOut(),
  #         tips = annotations$data[[paste0("ann", i)]],
  #         label = paste("Clade", "\nSNP(s) -", lapply(snpMean()[i], function(x){round(mean(x),0)})),
  #         color = "red",
  #         offSet = max(make_treeOut()$data$x)
  #       ))
  #   return(plt)
  # })
  # 
  # # remove the annotations
  # observeEvent(input$tree_reset, {
  #   
  #   output$treeDisplay <- renderPlot({
  #     if (n_annotations() == 1) {
  #       n_annotations<<-reactiveVal(0)
  #       return(make_treeOut())
  #       
  #     } else {
  #       make_treeOut() + anno_plot_undo()
  #     }
  #   })
  # })
  # 
  # 
  # #reactive to send tree with annoations to downloadImage module 
  # treeWLayers <- reactive ({
  #   if (!is.null(make_treeOut() + anno_plot_undo()) ){
  #     make_treeOut() + anno_plot_undo()
  #   } else {
  #     make_treeOut() +  anno_plot()
  #   }
  # })
}



## To be copied in the UI
# mod_cladeAnnotator_ui("cladeAnnotator_ui_1")

## To be copied in the server
# callModule(mod_cladeAnnotator_server, "cladeAnnotator_ui_1")
