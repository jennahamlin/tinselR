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
    actionButton(ns("add_annotation"),"Add Annotation to Tree",icon("plus"),class = "btn btn-primary"),
    actionButton(ns("remove_annotation"),"Remove Previous Annotation(s) on Tree",icon("refresh"),class = "btn btn-primary"),
    actionButton(ns("reload"), "Relaunch the Application"),
    plotOutput(ns("treeDisplay"), brush = ns("plot_brush"))
  )
}

#' cladeAnnotator Server Function
#'
#' @noRd
mod_cladeAnnotator_server <-
  function(input, output, session, geneObjectForSNP, make_treeOut) {
    ns <- session$ns
    
    #displays the tree plot, uses output from the displayTree module
    observeEvent(input$add_tree, {
      output$treeDisplay <- renderPlot({
        make_treeOut()})
    })
    
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
   
    # Initialize a reactive value and set to zero and an empty list
    Values <- reactiveValues()
    observe({
      Values[["n"]]   <- 0
      Values[["tip_vec"]] <- list()
    })
    
    #this functions calculates the mean # snps and adds that layer as annotation. Additionally, it checks
    #for overlap in annotations and adjusts as necessary
    addAnnotations <- function(tree_plot, tip_vector) {
      g <- tree_plot
      
      for (i in seq_along(tip_vector)) { #this is the i'th list, for which we are calculating the offset
        current_tips <- Values[["tip_vec"]][[ i ]]
        n_overlap = 0     # start by assuming no overlap
        if (i>1){         # for the first set of tips no comparisons needed
          # otherwise do comparisons
          for (j in 1:(i-1)){  #this is the j'th list, against which we need to compare if i overlaps it
            compare_tips <- Values[["tip_vec"]][[ j ]]  #tips to compare to
            n_overlap <- n_overlap + any(current_tips %in% compare_tips) # for every match, count it
          }
        }
        
        # set the clade label offset based on how many sets of previous tips it overlaps
        label_offset <- 0.005 + n_overlap*0.003
        
        #uses the snpAnno function to calculate the mean # of snps for brushed tips 
        snpMean <- lapply(1:Values[["n"]], function(i)
          snpAnno(geneFile = geneObjectForSNP(),
                  tips = current_tips))
        
        #generates the layer for the set of brushed tips
        g <- g +
          make_layer(
            tree_plot,
            tips = tip_vector[[i]],
            label = paste("Clade", "\nSNP(s) -", lapply(snpMean[i], function(x){round(mean(x),0)})),
            color = "blue",
            offset = label_offset
          )
      }
      return(g)
    }
    
    #event reactive which holds the tips information 
    anno_plot<- eventReactive(input$add_annotation, {
      
      # update the reactive value as a count of + 1
      Values[["n"]] <- Values[["n"]] + 1
      
      #add the tip vector (aka label) to the annotation reactive value
      Values[["tip_vec"]][[paste0("tips", Values[["n"]])]] <- dataWithSelection2()
      
      tips <- lapply(1:Values[["n"]], function(i)
        Values[["tip_vec"]][[paste0("tips", i)]])
      
      return(tips)
      
    })
    
    #display that layer onto the tree
    observeEvent(input$add_annotation, {
      output$treeDisplay <- renderPlot({
        addAnnotations(tree_plot = make_treeOut() , tip_vector =  anno_plot() )
      })
    })

    #event reactive which holds the tips information 
    anno_plotUndo<- eventReactive(input$remove_annotation, {
      
      # update the reactive value as a count of - 1
      Values[["n"]] <- Values[["n"]] - 1
      
      tips <- lapply(1:Values[["n"]], function(i)
        Values[["tip_vec"]][[paste0("tips", i)]])
      
      return(tips)
      
    })
    
    #this will reload the session and clear exisiting info - good if you want to start TOTALLY new 
    observeEvent(input$reload,{
      session$reload()
    })
    
    #display that layer onto the tree
    observeEvent(input$remove_annotation, {
      output$treeDisplay <- renderPlot({
        addAnnotations(tree_plot = make_treeOut() , tip_vector =  anno_plotUndo() )
      })
    })
    
    
    # remove the annotations
    observeEvent(input$remove_annotation, {
      
      output$treeDisplay <- renderPlot({
        if (Values[["n"]] == 1) {
          Values[["n"]] <- 0
          return(make_treeOut())
          
        } else {
          addAnnotations(tree_plot = make_treeOut() , tip_vector =  anno_plotUndo())
        }
      })
    })
    
    
    # #send tree with annotations to the download module
    # treePlotOut <- reactive({
    #   addAnnotations(tree_plot = make_treeOut() , tip_vector =  anno_plot() )    })
    
    #reactive to send tree with annoations to downloadImage module
    treePlotOut <- reactive ({
          if (!is.null(anno_plotUndo())) {
            addAnnotations(tree_plot = make_treeOut() , tip_vector = anno_plotUndo() )
          } 
       else {
             addAnnotations(tree_plot = make_treeOut() , tip_vector =  anno_plot() )}
          })
    #uncomment this out to send tree for download. 
    return(treePlotOut)
   
  }





## To be copied in the UI
# mod_cladeAnnotator_ui("cladeAnnotator_ui_1")

## To be copied in the server
# callModule(mod_cladeAnnotator_server, "cladeAnnotator_ui_1")