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
  function(input, output, session, geneObjectForSNP, make_treeOut, labelOff, labColor){
    ns <- session$ns
    
    #this will reload the session and clear exisiting info - good if you want to start TOTALLY new 
    observeEvent(input$reload,{
      session$reload()
    })
    
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
   
    # Initialize a reactive value and set to zero (count) and an empty list for tip vector input
    Values <- reactiveValues()
    observe({
      Values[["n"]]   <- 0
      Values[["tip_vec"]] <- list() 
      Values[["annoUndoCount"]] <- 0
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
        label_offset <- labelOff() + n_overlap*0.003
        
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
            color = labColor(),
            offset = label_offset
          )
      }
      return(g)
    }
    
    #event reactive which holds the tips information and increments by + 1 for each user brushed set of tips
    anno_plot<- eventReactive(input$add_annotation, {
      #str(Values[["n"]])
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

    #event reactive which holds the tips information and increments by - 1 for each time user pushes the button
    anno_plotUndo<- eventReactive(input$remove_annotation, {
      
      # update the reactive value as a count of - 1
      Values[["n"]] <- Values[["n"]] - 1
      
      Values[["annoUndoCount"]] <- Values[["annoUndoCount"]] + 1
      
        tips <- lapply(1:Values[["n"]], function(i)
        Values[["tip_vec"]][[paste0("tips", i)]])
      
      return(tips)
      
    })
    
    
    anno_plotHold <-eventReactive(input$add_annotation, {
      addAnnotations(tree_plot = make_treeOut() , tip_vector =  anno_plot() )
    })

    # remove the annotations one by one, when number of values equals one, then display tree without annotations.
    # currently, when only two annotations are left, this will remove those last two annotations and give the blank tree
    # not sure how to change to allow when two annotations are left, then display one annotation
    observeEvent(input$remove_annotation, {

      output$treeDisplay <- renderPlot({
        if (Values[["n"]] >= 1) {
          addAnnotations(tree_plot = make_treeOut() , tip_vector =  anno_plotUndo())
        } else {
          Values[["n"]]   <- 0
          make_treeOut()
        }
      })

      # print("is anno_plotUndo null")
      # print(is.null(anno_plotUndoHold()))
      # print(anno_plotUndoHold())
    })
    
  

    #return(anno_plotUndoHold)
    
    #reactive to send tree with annoations to downloadImage module
    treePlotOut <- reactive ({
      if(Values[["annoUndoCount"]] >= 1){
        addAnnotations(tree_plot = make_treeOut() , tip_vector =  anno_plotUndo())
      }
      else if(Values[["n"]] >=1 ){
        addAnnotations(tree_plot = make_treeOut() , tip_vector = anno_plot())
      } else {
        make_treeOut()
      }
      }) 
      
      
    #   (is.null(anno_plotUndoHold())) {
    #     addAnnotations(tree_plot = make_treeOut() , tip_vector = anno_plot() )
    #   } 
    #   else {
    #     addAnnotations(tree_plot = make_treeOut() , tip_vector =  anno_plotUndo() ) 
    #    }
    # })
    
    #uncomment this out to send tree for download. 
    return(treePlotOut)
    
  }




## To be copied in the UI
# mod_cladeAnnotator_ui("cladeAnnotator_ui_1")

## To be copied in the server
# callModule(mod_cladeAnnotator_server, "cladeAnnotator_ui_1")