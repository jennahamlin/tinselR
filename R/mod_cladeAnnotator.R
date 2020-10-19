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
  function(input, output, session, mFileMatOut, makeTreeOut, addTree, addAnno, removeAnno, 
           addMatrix, removeMatrix, geneObjectForSNP, labelOff, labColor, matOff){
    
    #add other tree viz parameters above 
    
    ns <- session$ns
    
    # Initialize a reactive value and set to zero (count) and an empty list for tip vector input
    Values <- reactiveValues()
    observe({
      Values[["n"]]   <- 0
      Values[["tip_vec"]] <- list() 
      Values[["validMeta"]] <- 0
      Values[["showMap"]] <- 0
    })
    
    #reactive that holds the brushed points on a plot
    dataWithSelection <- reactive({
      brushedPoints(makeTreeOut()$data, input$plot_brush)
    })
    
    
    #add label to tipVector if isTip == True
    dataWithSelection2 <- eventReactive(input$plot_brush, {
      label <- NULL
      tipVector <- c()
      
      for (i in 1:length(dataWithSelection()$label)) {
        if (dataWithSelection()$isTip[i] == TRUE) 
          tipVector <- c(tipVector, dataWithSelection()$label[i])
      }
      return(tipVector)
    })
    
    #displays the tree plot, uses output from the displayTree module
    observeEvent(addTree(), {
      output$treeDisplay <- renderPlot({
        makeTreeOut()})
    })
    
    #display that user-brushed layer onto the tree
    observeEvent(addAnno(), {
      
      #this acts as a control for if the user accidently presses the addAnno button without the file loaded
      if (is.null(geneObjectForSNP())) {
        #skip
      } else {
        
        Values[["n"]] <- Values[["n"]] + 1
        
        #add the tip vector (aka label) to the annotation reactive value
        Values[["tip_vec"]][[paste0("tips", Values[["n"]])]] <- dataWithSelection2()
        
        #add to variable called tips
        tips <- createTipList()
        
        output$treeDisplay <- renderPlot({
          addMap(tree = addAnnotations(treePlot = makeTreeOut(), tipVectorIn =  tips ), metaFile = mFileMatOut())
        })
      }
    })
    
    # remove the annotations one by one, when number of values equals one, then display tree without annotations.
    observeEvent(removeAnno(), {
      
      if (is.null(geneObjectForSNP())) {
        #skip
      } else {
        
        if(Values[["n"]] > 0) {
          
          Values[["n"]] <- Values[["n"]] - 1
          
          tempTip <-Values[["tip_vec"]]
          
          #remove the last set of tips that the user selected
          Values[["tip_vec"]]<- tempTip[-length(tempTip)] }
        
        tips <- createTipList()
        
        output$treeDisplay <- renderPlot({
          addMap(tree = addAnnotations(treePlot = makeTreeOut() , tipVectorIn =  tips), metaFile = mFileMatOut())        
        })
      }
    })
    
    #allow the user to add a matrix to a tree; change showMap to the value of 1
    observeEvent(addMatrix(),{
      
        
        #display that layer onto the tree
        Values[["showMap"]] <-  1
        output$treeDisplay <- renderPlot({
          
          #render the plot using the currentTreeOut function. 
          currentTreeOut()
        })
    }) 
    
    #as above with add matrix but this allows the removal of the matrix by setting showMap to 0
    observeEvent(removeMatrix(),{
     
        
        #display that layer onto the tree
        Values[["showMap"]] <-  0
        output$treeDisplay <- renderPlot({
          currentTreeOut()
        })
    }) 
    
    #add map funciton takes in a tree and the converted meta data file. 
    #only allows the inclusion of the mapk if the value of showMap is greater than 0
    addMap <- function(tree, metaFile){
      if(Values[["showMap"]] > 0 & !is.null(metaFile) ) {
        tree <- ggtree::gheatmap(tree,
                                 metaFile,
                                 offset = matOff(),
                                 width = 0.2,
                                 colnames_angle = 45, 
                                 colnames_offset_y = -1,
                                 hjust = 0.5)
        #+
        #  ggplot2::scale_color_viridis_d(option = matCol()) #ideally, will add in this option to change the color
        
      }
      return(tree)
    }
    
    #function to create the tip list. list apply over the counter('n') and paste the values in the tip vector to the variable tips
    createTipList <- function(){
      tips <- c()
      if (Values[["n"]] < 1 ) {
        #skip
      } else {
        tips <- lapply(1:Values[["n"]], function(i)
          Values[["tip_vec"]][[paste0("tips", i)]])
      }
      return(tips)
    }
    
    #this functions calculates the mean # snps and adds that layer as annotation. Additionally, it checks
    #for overlap in annotations and adjusts as necessary
    addAnnotations <- function(treePlot, tipVectorIn) {
      g <- treePlot
      
      if(Values[["n"]] > 0) {
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
          # set the clade label offset based on how many sets of previous tips it overlaps and provide user #option to adjust the position of all annotations
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
      }
      return(g)
    }
    
    #function to create the tree.
    currentTreeOut <- function(){
      addMap(tree = addAnnotations(treePlot = makeTreeOut() , tipVectorIn =  createTipList() ), metaFile = mFileMatOut())
    }
    
    #reactive to send tree to downloadImage module
    treeOut <- reactive ({
      currentTreeOut()
    })
    
    return(treeOut)
    
  }

## To be copied in the UI
# mod_cladeAnnotator_ui("cladeAnnotator_ui_1")

## To be copied in the server
# callModule(mod_cladeAnnotator_server, "cladeAnnotator_ui_1")