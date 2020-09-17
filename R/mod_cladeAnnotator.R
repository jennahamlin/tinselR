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
      ns("remove_annotation"),
      "Remove Previous Annotation(s) on Tree",
      icon("refresh"),
      class = "btn btn-primary"
    ),
    actionButton(ns("reload"), "Remove all annotations"),
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
    
    
    make_layer <- function(tree, tips, label, color, offset) {
      ggtree::geom_cladelabel(
        node = phytools::findMRCA(ape::as.phylo(tree), tips),
        label = label,
        color = color,
        angle = 0,
        offset = offset
      )
    }
    
    Values <- reactiveValues()
    observe({
      Values[["n"]]   <- 0
      Values[["tip_vec"]] <- list()
    })
    
    #display the annotation layer onto the tree
    # offsetTip <- eventReactive(input$add_annotation, {
    #   # update the number of annotations
    #   Values[["n"]] <- Values[["n"]] + 1
    #   
    #   # #add label to tipVector if isTip == True
    #   # req(input$plot_brush)
    #   # tipVector2 <- c()
    #   # selected_tips <- brushedPoints(make_treeOut()$data, input$plot_brush)
    #   # for (i in 1:length(selected_tips$label)) {
    #   #   if (selected_tips$isTip[i])
    #   #     tipVector2 <- c(tipVector2, selected_tips$label[i])
    #   # }
    #   
    #   #Values[["tip_vec"]][[paste0("tips", Values[["n"]])]] <- tipVector2
    #   
    #   Values[["tip_vec"]][[paste0("tips", Values[["n"]])]] <- dataWithSelection2()
    #   print(Values[["tip_vec"]])
    #   print(dataWithSelection2())
    #   
    #   
    #   # check if the tips of the current annotation overlap with tips from previous annotations
    #   current_tips <- Values[["tip_vec"]][[  Values[["n"]] ]]
    #   previous_tips <- Values[["tip_vec"]][ -Values[["n"]] ]
    #   n_overlap <- sapply(previous_tips, function(a) any(current_tips %in% a)) %>% unlist %>% sum
    #   print(n_overlap)
    #   
    #   # set the clade label offset based on how many sets of previous tips it overlaps
    #   label_offset <- 0.004 + n_overlap*0.002
    #   
    #   return(label_offset)
    #   
    # })
    
   
    
    addAnnotations <- function(tree_plot, tip_vector) {
      print("tip_vector=") 
      print(tip_vector)
      g <- tree_plot
    
      #print(offsetTip())
      
      for (i in seq_along(tip_vector)) { #this is the i'th list, for which we are calculating the offset
        print("i=")
        print(i)
        current_tips <- Values[["tip_vec"]][[ i ]]
        print("current_tips=")
        print(current_tips)
        n_overlap = 0     # start by assuming no overlap
        if (i>1){         # for the first set of tips no comparisons needed
                          # otherwise do comparisons
          for (j in 1:(i-1)){  #this is the j'th list, against which we need to compare if i overlaps it
            print("   j=")
            print(j)
            compare_tips <- Values[["tip_vec"]][[ j ]]  #tips to compare to
            n_overlap <- n_overlap + any(current_tips %in% compare_tips) # for every match, count it
          }
        }
        
        # set the clade label offset based on how many sets of previous tips it overlaps
        label_offset <- 0.004 + n_overlap*0.002
        str(make_treeOut()$data)
        snpMean <- lapply(1:n_annotations(), function(i)
          snpAnno(geneFile = geneFileSNP(),
                  tips = annotations$data[[paste0("ann", i)]]
          ))
        
        g <- g +
          make_layer(
            tree_plot,
            tips = tip_vector[[i]],
            label = paste("Clade", "\nSNP(s) -", lapply(snpMean[i], function(x){round(mean(x),0)})),
            color = "blue",
            offset = label_offset
              #tipOut[i]
            #label_offset
            #0.006
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
      
      tips <- lapply(1:n_annotations(), function(i) annotations$data[[paste0("ann", i)]])
      
      Values[["n"]] <- Values[["n"]] + 1
      Values[["tip_vec"]][[paste0("tips", Values[["n"]])]] <- dataWithSelection2()
      #print(Values[["tip_vec"]])
 
      output$treeDisplay <- renderPlot({
        addAnnotations(tree_plot = make_treeOut() , tip_vector =  tips)
      })
    })
  }


#   #add the annotations when selection is brushed
#   observeEvent(input$add_annotation,{
#     output$treeDisplay <- renderPlot({
# validate(need(input$plot_brush !="", "Please import a genetic distance file to use the clade annotator"))
#       make_treeOut() + anno_plot()
#     })
#   })


###############calculates snpmean but overlap does not work 

# #convert to long data frame - three columns.
# #This takes as input the genetic distance object from display tree module
# geneFile <-reactive({
#   label <- NULL
#   geneObjectOut()%>%
#     stats::na.omit()%>%
#     tidyr::pivot_longer(-label)
# })
# 
# #remove self comparisons for this table - necessary for snp mean/median calculation.
# geneFileSNP <-reactive({
#   label <- NULL
#   geneFile()[which(geneFile()$label != geneFile()$name),]
# })
# 
# #displays the tree plot, uses output from the displayTree module
# observeEvent(input$add_tree, {
#   output$treeDisplay <- renderPlot({
#     make_treeOut()
#   })
# })
# 
# 
# # Initialize a reactive value and set to zero
# n_annotations <- reactiveVal(0)
# annotations <- reactiveValues()
# 
# #reactive that holds the brushed points on a plot
# dataWithSelection <- reactive({
#   brushedPoints(make_treeOut()$data, input$plot_brush)
# })
# 
# tipVector <- c()
# 
# #add label to tipVector if isTip == True
# dataWithSelection2 <- eventReactive(input$plot_brush, {
#   label <- NULL
#   for (i in 1:length(dataWithSelection()$label)) {
#     if (dataWithSelection()$isTip[i] == TRUE)
#       tipVector <- c(tipVector, dataWithSelection()$label[i])
#   }
#   return(tipVector)
# })
# 
# makeLayer <- function(tree, tips, label, color, offset) {
#   ggtree::geom_cladelabel(
#     node = phytools::findMRCA(ape::as.phylo(tree), tips),
#     label = label,
#     color = color,
#     offset = offset
#   )
# }
# 
# # #use snp_anno function to get the snps differences between compared tips
# # snpMean <- eventReactive(input$add_annotation, {
# #   lapply(1:n_annotations(), function(i)
# #     snpAnno(geneFile = geneFileSNP(),
# #             tips = annotations$data[[paste0("ann", i)]]
# #     ))
# # })
# Values <- reactiveValues()
# observe({
#   Values[["n"]]   <- 0
#   Values[["tip_vec"]] <- list()
# })
# 
# #display that layer onto the tree
# anno_plot <- eventReactive(input$add_annotation, {
# 
#   Values[["n"]] <- Values[["n"]] + 1
#   
#   # update the reactive value as a count
#   new <- n_annotations() + 1
#   n_annotations(new)
# 
#   #add the tip vector (aka label) to the annotation reactive value
#   annotations$data[[paste0("ann", n_annotations())]] <- dataWithSelection2()
#   
#   # check if the tips of the current annotation overlap with tips from previous annotations
#   # current_tips <-  annotations$data[[paste0("ann", n_annotations())]] 
# 
#    #add label to tipVector if isTip == True
#    tipVector2 <- c()
#    selected_tips <- brushedPoints(make_treeOut()$data, input$plot_brush)
#    for (i in 1:length(selected_tips$label)) {
#      if (selected_tips$isTip[i])
#        tipVector2 <- c(tipVector2, selected_tips$label[i])
#    }
#    
#   Values[["tip_vec"]][[paste0("tips", Values[["n"]])]] <- tipVector2
#   # check if the tips of the current annotation overlap with tips from previous annotations
#     current_tips <- Values[["tip_vec"]][[  Values[["n"]] ]]
#     previous_tips <- Values[["tip_vec"]][ -Values[["n"]] ]
#  
#     n_overlap <- sapply(previous_tips, function(a) any(current_tips %in% a)) %>% unlist %>% sum
#     
#    label_offset <- 0.004 + n_overlap*0.002
# 
#   snpMean <- lapply(1:n_annotations(), function(i)
#     snpAnno(geneFile = geneFileSNP(),
#             tips = annotations$data[[paste0("ann", i)]]
#     ))
#   
#   #list apply over the make_layer function to add the annotation
#   plt <-
#     lapply(1:n_annotations(), function(i)
#       makeLayer(
#         make_treeOut(),
#         tips = annotations$data[[paste0("ann", i)]],
#         label = paste("Clade", "\nSNP(s) -", lapply(snpMean[i], function(x){round(mean(x),0)})),
#         color = "red",
#         offset = label_offset
#       ))
#   return(plt)
# 
# })
# 
# #add the annotations when selection is brushed
# observeEvent(input$add_annotation,{
#   output$treeDisplay <- renderPlot({
#     validate(need(input$plot_brush !="", "Please import a genetic distance file to use the clade annotator"))
# 
#     make_treeOut() + anno_plot()
#   })
# })

####################################works to calculate snpmean but lots of functionality is lost 
#add the annotations when selection is brushed
# observeEvent(input$add_tree,{
#   Values[["phy"]] <- make_treeOut()
#   output$treeDisplay <- renderPlot({
#     Values[["phy"]]})
# })
# 
# 
# Values <- reactiveValues()
# observe({
#   #Values[["phy"]] <- make_treeOut()
#   Values[["n"]]   <- 0
#   Values[["tip_vec"]] <- list()
# })
# 
# #function which makes the annotation layer(s)
# make_layer <- function(tree, tips, label, color, offset) {
#   ggtree::geom_cladelabel(
#     node = phytools::findMRCA(ape::as.phylo(tree), tips),
#     label = label,
#     color = color,
#     angle = 0,
#     offset = offset
#   )
# }
# 
# #display the annotation layer onto the tree
# observeEvent(input$add_annotation, {
#   # update the number of annotations
#   Values[["n"]] <- Values[["n"]] + 1
#   
#   #add label to tipVector if isTip == True
#   req(input$plot_brush)
#   tipVector <- c()
#   selected_tips <- brushedPoints(Values[["phy"]]$data, input$plot_brush)
#   for (i in 1:length(selected_tips$label)) {
#     if (selected_tips$isTip[i])
#       tipVector <- c(tipVector, selected_tips$label[i])
#   }
#   
#   Values[["tip_vec"]][[paste0("tips", Values[["n"]])]] <- tipVector
#   
#   # check if the tips of the current annotation overlap with tips from previous annotations
#   current_tips <- Values[["tip_vec"]][[  Values[["n"]] ]]
#   
#   previous_tips <- Values[["tip_vec"]][ -Values[["n"]] ]
#   
#   n_overlap <- sapply(previous_tips, function(a) any(current_tips %in% a)) %>% unlist %>% sum
#   
#   # set the clade label offset based on how many sets of previous tips it overlaps
#   label_offset <- 0.004 + n_overlap*0.002
#   
#   snpMean <- 
#     lapply(1:length(Values[["n"]]), function(i)
#       snpAnno(geneFile = geneFileSNP(),
#               tips = current_tips))
#   
#   anno_phy <- Values[["phy"]] +
#     make_layer(
#       tree = Values[["phy"]],
#       tips =  current_tips,
#       color = "grey25",
#       label = paste("Clade", "\nSNP(s) -", round(mean(unlist(as.list(snpMean[1]))), 0)), 
#       offset = label_offset
#     )
#   
#   Values[["phy"]] <- anno_phy
# })
# 
# output$treeDisplay <- renderPlot({
#   Values[["phy"]]
# })
# 
# 
# 
# #display the layer onto the tree
# observeEvent(input$remove_annotation, {
#   # update the number of annotations
#   Values[["n"]] <- Values[["n"]] - 1
#   
#   current_tips <-   Values[["tip_vec"]][ Values[["n"]] ]
#   
#   previous_tips <- Values[["tip_vec"]][ -Values[["n"]] ]
#   
#   n_overlap <- sapply(previous_tips, function(a) any(current_tips %in% a)) %>% unlist %>% sum
#   
#   # set the clade label offset based on how many sets of previous tips it overlaps
#   label_offset <- 0.004 + n_overlap*0.002
#   
#   snpMean <- 
#     lapply(1:length(Values[["n"]]), function(i)
#       snpAnno(geneFile = geneFileSNP(),
#               tips = current_tips))
#   
#   anno_phy <- Values[["phy"]] +
#     make_layer(
#       tree = Values[["phy"]],
#       tips =  current_tips,
#       color = "grey25",
#       label = paste("Clade", "\nSNP(s) -", round(mean(unlist(as.list(snpMean()[1]))), 0)), 
#       offset = label_offset
#     )
#   
#   Values[["phy"]] <- anno_phy
# })
# 
# output$treeDisplay <- renderPlot({
#   Values[["phy"]]
# })
# 

# #add the annotations when selection is brushed
# observeEvent(input$add_annotation,{
# output$treeDisplay <- renderPlot({
# validate(need(input$plot_brush !="", "Please import a genetic distance file to use the clade annotator"))
#     make_treeOut() + anno_plot()
#   })
# })
# 





#  #    #this will reload the session and clear exisiting info - good if you want to start TOTALLY new
#  #    observeEvent(input$reload, {
#  #      session$reload()
#  #    })
#  #    
#  #    if (FALSE) {
#  #      
#  #      # disabling for now
#  #      # both this and the next reactive listen to the same event
#  #      # for now keep it simple and remove all annotations
#  #      
#  #      # #remove a reactive annotation one by one
#  #      # #note to self - must have something be brushed
#       anno_plot_undo <- eventReactive(input$remove_annotation, {
#         # update the reactive value as a count of - 1
# 
#         new <- n_annotations() - 1
#         n_annotations(new)
# 
#         #list apply over the make_layer function to add the annotation
#         plt <-
#           lapply(1:n_annotations(), function(i)
#             make_layer(
#               make_treeOut(),
#               tips = annotations$data[[paste0("ann", i)]],
#               label = paste("Clade", "\nSNP(s) -", lapply(snpMean()[i], function(x) {
#                 round(mean(x), 0)
#               })),
#               color = "red",
#               offSet = max(make_treeOut()$data$x)
#             ))
#         return(plt)
#       })
# 
#     }
#  #    
#  #    
#   # remove the annotations
#   observeEvent(input$reload, {
#     Values[["phy"]] <- make_treeOut()
#     Values[["n"]]   <- 0
#     Values[["tip_vec"]] <- list()
#   })
#   #  #    
#   #  #    #reactive to send tree with annoations to downloadImage module
#   #       treeWLayers <- reactive ({
#   #  #    # if (!is.null(make_treeOut() + anno_plot_undo())) {
#   #         make_treeOut() }) 
#   #         #+ anno_plot_undo()
#   #  #    # } else {
#   #  #    #   make_treeOut() +  anno_plot()
#   #  #    # }
#   #  #    # })
#   #  #  
#   
#   #send tree with annotations to the download module
#   treePlotOut <- reactive({
#     Values[["phy"]]
#   })
#   
#   return(treePlotOut)
# }

## To be copied in the UI
# mod_cladeAnnotator_ui("cladeAnnotator_ui_1")

## To be copied in the server
# callModule(mod_cladeAnnotator_server, "cladeAnnotator_ui_1")