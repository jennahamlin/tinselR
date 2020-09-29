# Module UI

#' @title   mod_cladeAnnotator_ui and mod_cladeAnnotator_server
#' @description  A shiny Module. This module allows the user to add or remove annotations and checks for overlap between those annotaions. 
#'
#' @param id shiny id
#' @param input internal
#' @param output internal
#' @param session internal
#'
#' @rdname mod_cladeAnnotator
#'
#' @keywords internal
#' @export 
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

# Module Server

#' @rdname mod_cladeAnnotator
#' @export
#' @keywords internal
mod_cladeAnnotator_server <-
  function(input, output, session, geneObjectForSNP, makeTreeOut, labelOff, labColor){
    ns <- session$ns
    
    #this will reload the session and clear exisiting info - good if you want to start TOTALLY new 
    observeEvent(input$reload,{
      session$reload()
    })
    
    #displays the tree plot, uses output from the displayTree module
    observeEvent(input$add_tree, {
      output$treeDisplay <- renderPlot({
        makeTreeOut()})
    })
    
    #reactive that holds the brushed points on a plot
    dataWithSelection <- reactive({
      # if (!is.null(metaFileUp()$datapath)){
      #   
      # } else {
      
        brushedPoints(makeTreeOut()$data, input$plot_brush)
      # }
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
      Values[["annoUndoCount"]] <- 0 #use this as a count for sending the downloaded image with annotations removed 
    })
    
    #this functions calculates the mean # snps and adds that layer as annotation. Additionally, it checks
    #for overlap in annotations and adjusts as necessary
    addAnnotations <- function(treePlot, tipVectorIn) {
      g <- treePlot
      
      for (i in seq_along(tipVectorIn)) { #this is the i'th list, for which we are calculating the offset
        currentTips <- Values[["tip_vec"]][[ i ]]
        nOverlap = 0     # start by assuming no overlap
        if (i>1){         # for the first set of tips no comparisons needed
          # otherwise do comparisons
          for (j in 1:(i-1)){  #this is the j'th list, against which we need to compare if i overlaps it
            compareTips <- Values[["tip_vec"]][[ j ]]  #tips to compare to
            nOverlap <- nOverlap + any(currentTips %in% compareTips) # for every match, count it
          }
        }
        
        # set the clade label offset based on how many sets of previous tips it overlaps and provide user 
        #option to adjust the position of all annotations
        label_offset <- labelOff() + nOverlap*0.003
        
        #uses the snpAnno function to calculate the mean # of snps for brushed tips 
        snpMean <- lapply(1:Values[["n"]], function(i)
          snpAnno(geneFile = geneObjectForSNP(),
                  tips = currentTips))
        
        #generates the layer for the set of brushed tips
        g <- g +
          make_layer(
            treePlot,
            tips = tipVectorIn[[i]],
            label = paste("Clade", "\nSNP(s) -", lapply(snpMean[i], function(x){round(range(x),0)})),
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
      
      if (Values[["annoUndoCount"]] < 1){
        skip
      } else {
        Values[["annoUndoCount"]] <- Values[["annoUndoCount"]] - 1 }
      
      tips<-c()
      if (Values[["n"]] < 1 ) {
        skip
      } else {
        tips <- lapply(1:Values[["n"]], function(i)
          Values[["tip_vec"]][[paste0("tips", i)]])
      }
      return(tips)
    })
    
    #display that layer onto the tree
    observeEvent(input$add_annotation, {
      output$treeDisplay <- renderPlot({
        addAnnotations(treePlot = makeTreeOut() , tipVectorIn =  anno_plot() )
      })
    })

    #event reactive which holds the tips information and increments by - 1 for each time user pushes the button
    anno_plotUndo<- eventReactive(input$remove_annotation, {
      
      # update the reactive value as a count of - 1
      Values[["n"]] <- Values[["n"]] - 1
      
      if (Values[["annoUndoCount"]] == 1){
        skip
      } else {
        Values[["annoUndoCount"]] <- Values[["annoUndoCount"]] + 1 }
      
      tips<-c()
      if (Values[["n"]] < 1 ) {
        skip
      } else {
        tips <- lapply(1:Values[["n"]], function(i)
          Values[["tip_vec"]][[paste0("tips", i)]])
      }
      
      return(tips)
      
    })

    # remove the annotations one by one, when number of values equals one, then display tree without annotations.
    observeEvent(input$remove_annotation, {

      output$treeDisplay <- renderPlot({
         if (Values[["n"]] >= 1) {
          addAnnotations(treePlot = makeTreeOut() , tipVectorIn =  anno_plotUndo())
        } 
        else {
          makeTreeOut()
        }
      })
    })
    
    # annoUndoHold <- reactive({
    #  hold <-  addAnnotations(treePlot = makeTreeOut() , tipVectorIn =  anno_plotUndo())
    #  return(hold)
    # })
    # 
    
    #reactive to send tree with annoations to downloadImage module
    treePlotOut <- reactive ({
      if(Values[["annoUndoCount"]] == 1) {
        addAnnotations(treePlot = makeTreeOut() , tipVectorIn =  anno_plotUndo())
      } else if( 
        #Values[["annoUndoCount"]] >= 1) {
        Values[["n"]] >=1  ) {
        addAnnotations(treePlot = makeTreeOut() , tipVectorIn = anno_plot())
      } else {
        makeTreeOut()
      }
      }) 

      #uncomment this out to send tree for download. 
    return(treePlotOut)
    
  }

## To be copied in the UI
# mod_cladeAnnotator_ui("cladeAnnotator_ui_1")

## To be copied in the server
# callModule(mod_cladeAnnotator_server, "cladeAnnotator_ui_1")