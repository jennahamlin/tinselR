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
    mainPanel(
      plotOutput(ns("treeDisplay"), brush = ns("plot_brush")))
  )
}

# Module Server

#' @rdname mod_cladeAnnotator
#' @export
#' @keywords internal
mod_cladeAnnotator_server <-
  function(input, output, session, mFileOut, makeTreeOut, addTree, addAnno, removeAnno, 
           addMatrix, geneObjectForSNP, labelOff, labColor, matOff,  matCol){
    
    #add other tree viz parameters above 
    
    ns <- session$ns
    
    #displays the tree plot, uses output from the displayTree module
    observeEvent(addTree(), {
      #str(makeTreeOut())
      output$treeDisplay <- renderPlot({
        makeTreeOut()})
    })
    
    uploadOrder <- function(file) {
      if(is.null(file)) {
        return("please relaunch the application") 
      } else if (is.null(file))
        return(NULL)
    }
    
    #reactive that holds the brushed points on a plot
    dataWithSelection <- reactive({
      uploadOrder(mFileOut())
      brushedPoints(makeTreeOut()$data, input$plot_brush)
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
      Values[["annoUndoCount"]] <- 0 #use this as a count for sending the downloaded image with annotations removed essentially turn on/turn off
      Values[["matrixCount"]] <- 0 
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
        label_offset <- labelOff() + nOverlap*0.004
        
        #uses the snpAnno function to calculate the mean # of snps for brushed tips 
        snpMean <- 
          snpAnno(geneFile = geneObjectForSNP(),
                  tips = currentTips)
        
        #generates the layer for the set of brushed tips
        g <- g +
          make_layer(
            treePlot,
            tips = tipVectorIn[[i]], 
            label = paste0("Range \nof \nSNP(s)- \n", paste0(min(snpMean), sep =",", max(snpMean))),
            color = labColor(),
            offset = label_offset
          )
      }
      return(g)
    }
    
    #event reactive which holds the tips information and increments by + 1 for each user brushed set of tips
    anno_plot<- eventReactive(addAnno(), {
      
      # update the reactive value as a count of + 1
      Values[["n"]] <- Values[["n"]] + 1
      
      #add the tip vector (aka label) to the annotation reactive value
      Values[["tip_vec"]][[paste0("tips", Values[["n"]])]] <- dataWithSelection2()
      
      if (Values[["annoUndoCount"]] < 1){
        #skip
      } else {
        Values[["annoUndoCount"]] <- Values[["annoUndoCount"]] - 1 }
      
      tips<-c()
      
      if (Values[["n"]] < 1 ) {
        #skip
      } else {
        tips <- lapply(1:Values[["n"]], function(i)
          Values[["tip_vec"]][[paste0("tips", i)]])
      }
      return(tips)
    })
    
    #display that layer onto the tree
    observeEvent(addAnno(), {
      output$treeDisplay <- renderPlot({
        addAnnotations(treePlot = makeTreeOut() , tipVectorIn =  anno_plot() )
      })
    })
    
    #event reactive which holds the tips information and increments by - 1 for each time user pushes the button
    anno_plotUndo<- eventReactive(removeAnno(), {
      
      # update the reactive value as a count of - 1
      Values[["n"]] <- Values[["n"]] - 1
      
      
      if (Values[["annoUndoCount"]] == 1){
        #skip
      } else {
        Values[["annoUndoCount"]] <- Values[["annoUndoCount"]] + 1 }
      
      tips<-c()
      
      if (Values[["n"]] < 1 ) {
        #skip
      } else {
        tips <- lapply(1:Values[["n"]], function(i)
          Values[["tip_vec"]][[paste0("tips", i)]])
      }
      
      return(tips)
      
    })
    
    # remove the annotations one by one, when number of values equals one, then display tree without annotations.
    observeEvent(removeAnno(), {
      
      output$treeDisplay <- renderPlot({
        if (Values[["n"]] >= 1) {
          addAnnotations(treePlot = makeTreeOut() , tipVectorIn =  anno_plotUndo())
        }
        else {
          makeTreeOut()
        }
      })
    })
    
    mFile <- reactive({
      mFileConversion(mFile =mFileOut() )
    })
    
    matTree <- eventReactive(addMatrix(), {
      
      if (Values[["matrixCount"]] == 1){
        #skip
      } else {
        Values[["matrixCount"]] <- Values[["matrixCount"]] + 1 }
      
      ggtree::gheatmap(treePlotOut(), mFile(), offset =  matOff(), width = 0.2, colnames_angle = 45, colnames_offset_y = -1, hjust =  0.5)  +
        ggplot2::scale_fill_viridis_d(option= matCol())
    })
    
    observeEvent(addMatrix(),{
      if(Values[["n"]]>= 1)
      output$treeDisplay <- renderPlot({
        matTree()
      }) else if (Values[["n"]]==0){
        output$treeDisplay <- renderPlot({
          matTree()
    })}
    })
    
    #reactive to send tree with annoations to downloadImage module
    treePlotOut <- reactive ({
      #if (is.null(input$id))
      if(Values[["annoUndoCount"]] == 1) {
        addAnnotations(treePlot = makeTreeOut() , tipVectorIn =  anno_plotUndo())
      } else if(
        Values[["n"]] >=1  ) {
        addAnnotations(treePlot = makeTreeOut() , tipVectorIn = anno_plot())
      } else {
        makeTreeOut()
      }
    })
    
    #uncomment this out to send tree for download.
    treeOut <- reactive({
      if( Values[["matrixCount"]] != 1){
        return(treePlotOut())
      } else{
        return(matTree())
      }
    })
    
    #return(matTree)
    return(treeOut)
    
  }

## To be copied in the UI
# mod_cladeAnnotator_ui("cladeAnnotator_ui_1")

## To be copied in the server
# callModule(mod_cladeAnnotator_server, "cladeAnnotator_ui_1")