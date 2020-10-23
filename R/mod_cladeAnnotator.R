#' cladeAnnotator UI Function
#'
#' @title   mod_cladeAnnotator_ui and mod_cladeAnnotator_server
#'
#' @description  A shiny Module. This module allows the user to add or
#' remove annotations and checks for overlap between those annotations and
#' allows the addition of the heatmap.
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
      plotOutput(ns("tree_display"), brush = ns("plot_brush")))
  )
}

#' cladeAnnotator Server Function
#'
#' @rdname mod_cladeAnnotator
#' @export
#' @keywords internal
mod_cladeAnnotator_server <-
  function(input, output, session, mFileMatOut, make_tree_out, add_tree,
           add_anno, remove_anno, add_matrix, remove_matrix, geneObjectForSNP,
           label_off, lab_color, mat_off) {
    
    #add other tree viz parameters above
    ns <- session$ns
    
    # Initialize a reactive value and set to zero (count) and an empty
    #list for tip vector input
    Values <- reactiveValues()
    observe({
      Values[["n"]]   <- 0
      Values[["tip_vec"]] <- list()
      Values[["show_map"]] <- 0
    })
    
    #reactive that holds the brushed points on a plot
    data_with_selection <- reactive({
      brushedPoints(make_tree_out()$data, input$plot_brush)
    })
    
    #add label to tipVector if isTip == True
    data_with_selection2 <- eventReactive(input$plot_brush, {
      label <- NULL
      tip_vector <- c()
      
      for (i in 1:length(data_with_selection()$label)) {
        if (data_with_selection()$isTip[i] == TRUE)
          tip_vector <- c(tip_vector, data_with_selection()$label[i])
      }
      return(tip_vector)
    })
    
    #displays the tree plot, uses output from the displayTree module
    observeEvent(add_tree(), {
      output$tree_display <- renderPlot({
        make_tree_out()})
    })
    
    #display that user-brushed layer onto the tree
    observeEvent(add_anno(), {
      
      #this acts as a control for if the user accidentally presses the
      #add_anno button without the file loaded
      if (is.null(geneObjectForSNP())) {
        #skip
      } else {
        
        Values[["n"]] <- Values[["n"]] + 1
        
        #add the tip vector (aka label) to the annotation reactive value
        Values[["tip_vec"]][[paste0("tips", Values[["n"]])]] <-
          data_with_selection2()
        
        #add to variable called tips
        tips <- create_tip_list(r_n_values = Values[["n"]],
                                r_tip_vec = Values[["tip_vec"]])
        
        output$tree_display <- renderPlot({
          add_map(tree = add_annotations(tree_plot = make_tree_out(),
                                         tip_vector_in =  tips,
                                         r_n_values = Values[["n"]],
                                         r_tip_vec = Values[["tip_vec"]], 
                                         label_off = label_off(), 
                                         geneObjectForSNP = geneObjectForSNP(),
                                         lab_color = lab_color()),
                  metaFile = mFileMatOut(), r_value = Values[["show_map"]],
                  matOff = mat_off())
        })
      }
    })
    
    # remove the annotations one by one, when number of values equals one,
    #then display tree without annotations.
    observeEvent(remove_anno(), {
      
      if (is.null(geneObjectForSNP())) {
        #skip
      } else {
        
        if (Values[["n"]] > 0) {
          
          Values[["n"]] <- Values[["n"]] - 1
          
          temp_tip <- Values[["tip_vec"]]
          
          #remove the last set of tips that the user selected
          Values[["tip_vec"]] <- temp_tip[-length(temp_tip)]
        }
        
        tips <- create_tip_list(r_n_values = Values[["n"]], 
                                r_tip_vec = Values[["tip_vec"]])
        
        output$tree_display <- renderPlot({
          add_map(tree = add_annotations(tree_plot = make_tree_out(),
                                         tip_vector_in =  tips,
                                         r_n_values = Values[["n"]],
                                         r_tip_vec = Values[["tip_vec"]], 
                                         label_off = label_off(), 
                                         geneObjectForSNP = geneObjectForSNP(),
                                         lab_color = lab_color()),
                  metaFile = mFileMatOut(), r_value = Values[["show_map"]], 
                  matOff = mat_off())
        })
      }
    })
    
    #allow the user to add a matrix to a tree; change show_map to the value of 1
    observeEvent(add_matrix(), {
      
      #display that layer onto the tree
      Values[["show_map"]] <-  1
      output$tree_display <- renderPlot({
        
        #render the plot using the  current_tree_out function.
        current_tree_out()
      })
    })
    
    #as above with add matrix but this allows the removal of the matrix by
    #setting show_map to 0
    observeEvent(remove_matrix(), {
      
      #display that layer onto the tree
      Values[["show_map"]] <-  0
      output$tree_display <- renderPlot({
        current_tree_out()
      })
    })
    
    #function to create the tree.
    current_tree_out <- function() {
      add_map(tree = add_annotations(tree_plot = make_tree_out(),
                                     tip_vector_in =
                                       create_tip_list(
                                         r_n_values = Values[["n"]],
                                         r_tip_vec =Values[["tip_vec"]]),
                                     r_n_values = Values[["n"]],
                                     r_tip_vec = Values[["tip_vec"]], 
                                     label_off = label_off(), 
                                     geneObjectForSNP = geneObjectForSNP(),
                                     lab_color = lab_color()),
              metaFile = mFileMatOut(), 
              r_value = Values[["show_map"]], 
              matOff = mat_off())
    }
    
    #reactive to send tree to downloadImage module
    tree_out <- reactive({
      current_tree_out()
    })
    
    return(tree_out)
  }
