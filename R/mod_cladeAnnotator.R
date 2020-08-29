#' cladeAnnotator UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
#'
#' @importFrom shiny NS tagList
mod_cladeAnnotator_ui <- function(id) {
  ns <- NS(id)
  tagList(
    actionButton(ns("add_tree"), "Visualize Tree"),
    actionButton(
      ns("add_annotation"),
      "Add Annotation to Tree",
      icon("plus"),
      class = "btn btn-primary"
    ),
    actionButton(
      ns("tree_reset"),
      "Remove Previous Annotation(s) on Tree",
      icon("refresh"),
      class = "btn btn-primary"
    ),
    actionButton(ns("reload"), "Reload the Shiny application session"),
    plotOutput(ns("treeDisplay"), brush = ns("plot_brush"))
  )
}

#' cladeAnnotator Server Function
#'
#' @noRd
mod_cladeAnnotator_server <-
  function(input,
           output,
           session,
           geneObjectOut,
           make_treeOut) {
    ns <- session$ns
    
    #displays the tree plot, uses output from the displayTree module
    observeEvent(input$add_tree, {
      output$treeDisplay <- renderPlot({
        make_treeOut()
      })
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
      # browser()
      for (i in 1:length(dataWithSelection()$label)) {
        if (dataWithSelection()$isTip[i] == TRUE)
          tipVector <- c(tipVector, dataWithSelection()$label[i])
      }
      return(tipVector)
    })
    
    output$textDisplay <- renderText(dataWithSelection2())
    
    make_layer <- function(tree, tips, label, color, offset) {
      ggtree::geom_cladelabel(
        node = phytools::findMRCA(ape::as.phylo(tree), tips),
        label = label,
        color = color,
        angle = 0,
        offset = offset
      )
    }
    
    check_overlap <- function(previous_plot, incoming_tips) {
      pre_g <- ggplot2::ggplot_build(previous_plot)
      
      tip_labels <- pre_g$data[[3]]
      
      incoming_y_coords <-
        tip_labels[tip_labels$label %in% incoming_tips, "y"]
      
      # if (length(pre_g$data) < 5) {
      #   any_overlap <- FALSE
      # } else {
      
      # adding this here because I am not sure if the order of elements 
      # in the `data` slot of the pre-built ggplot is always the same
      # so trying to detect the last data frame in `$data` that contains
      # segments by searching for a column named yend. 
      # The first two such elements of `$data` will have this, because the 
      # branches of the trees are drawn as segments, so we're extracting the
      # last objects. 
      
      # Note, this procedure might fail if future additions to the app
      # adorn the tree with other segments
      
        segments_index <- lapply(pre_g$data, colnames) %>% 
          sapply(., function(x) any(x == "yend")) %>% 
          data.frame(cs = ., nm = 1:length(.)) %>% 
          dplyr::filter(cs == TRUE) %>% 
          dplyr::slice_max(order_by = nm, n=1) %>% 
          dplyr::pull(nm)
        
        clade_segments <- pre_g$data[segments_index][[1]]
        # browser()
        overlaps <- sapply(1:nrow(clade_segments), function(i) {
          X <- DescTools::Overlap(x = c(clade_segments[i, "y"], clade_segments[i, "yend"]),
                                  y = incoming_y_coords)
          Y <- X > 0
          return(Y)
        })
      # }
    }
    
    addAnnotations <- function(tree_plot, tip_vector) {
      g <- tree_plot
      for (i in seq_along(tip_vector)) {
        any_overlap <- check_overlap(previous_plot = g, incoming_tips = tip_vector[[i]])
        current_offset <- ifelse(any_overlap, 0.009, 0.004)
        
        print(tip_vector[[i]])
        print(any_overlap)
        print(current_offset)
        
        g <- g +
          make_layer(
            tree_plot,
            tips = tip_vector[[i]],
            label = paste("Clade", i),
            color = rev(colors())[i],
            offset = current_offset
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
      annotations$data[[paste0("ann", n_annotations())]] <-
        dataWithSelection2()
      
      tips <- lapply(1:n_annotations(), function(i)
        annotations$data[[paste0("ann", i)]])
      # browser()
      output$treeDisplay <- renderPlot({
        addAnnotations(tree_plot = make_treeOut() , tip_vector =  tips)
      })
      
      
    })
    
    #this will reload the session and clear exisiting info - good if you want to start TOTALLY new
    observeEvent(input$reload, {
      session$reload()
    })
    
    # #remove a reactive annotation one by one
    # #note to self - must have something be brushed
    anno_plot_undo <- eventReactive(input$tree_reset, {
      # update the reactive value as a count of - 1
      
      new <- n_annotations() - 1
      n_annotations(new)
      
      #list apply over the make_layer function to add the annotation
      plt <-
        lapply(1:n_annotations(), function(i)
          make_layer(
            make_treeOut(),
            tips = annotations$data[[paste0("ann", i)]],
            label = paste("Clade", "\nSNP(s) -", lapply(snpMean()[i], function(x) {
              round(mean(x), 0)
            })),
            color = "red",
            offSet = max(make_treeOut()$data$x)
          ))
      return(plt)
    })
    
    # remove the annotations
    observeEvent(input$tree_reset, {
      output$treeDisplay <- renderPlot({
        if (n_annotations() == 1) {
          n_annotations <<- reactiveVal(0)
          return(make_treeOut())
          
        } else {
          make_treeOut() + anno_plot_undo()
        }
      })
    })
    
    
    #reactive to send tree with annoations to downloadImage module
    treeWLayers <- reactive ({
      if (!is.null(make_treeOut() + anno_plot_undo())) {
        make_treeOut() + anno_plot_undo()
      } else {
        make_treeOut() +  anno_plot()
      }
    })
  }



## To be copied in the UI
# mod_cladeAnnotator_ui("cladeAnnotator_ui_1")

## To be copied in the server
# callModule(mod_cladeAnnotator_server, "cladeAnnotator_ui_1")
