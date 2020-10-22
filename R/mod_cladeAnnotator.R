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
        tips <- create_tip_list()

        output$tree_display <- renderPlot({
          add_map(tree = add_annotations(tree_plot = make_tree_out(),
                                       tip_vector_in =  tips),
                 metaFile = mFileMatOut())
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

        tips <- create_tip_list()

        output$tree_display <- renderPlot({
          add_map(tree = add_annotations(tree_plot = make_tree_out(),
                                       tip_vector_in =  tips),
                 metaFile = mFileMatOut())
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

    #add map funciton takes in a tree and the converted meta data file.
    #only allows the inclusion of the mapk if the value of show_map is
    #greater than 0
    add_map <- function(tree, metaFile) {
      if (Values[["show_map"]] > 0 & !is.null(metaFile)) {
        tree <- ggtree::gheatmap(tree,
                                 metaFile,
                                 offset = mat_off(),
                                 width = 0.2,
                                 colnames_angle = 45,
                                 colnames_offset_y = -1,
                                 hjust = 0.5)
        #+
        #  ggplot2::scale_color_viridis_d(option = matCol())
        #will add in this option to change the color

      }
      return(tree)
    }

    #function to create the tip list. list apply over the counter('n') and
    #paste the values in the tip vector to the variable tips
    create_tip_list <- function() {
      tips <- c()
      if (Values[["n"]] < 1) {
        #skip
      } else {
        tips <- lapply(1:Values[["n"]], function(i)
          Values[["tip_vec"]][[paste0("tips", i)]])
      }
      return(tips)
    }

    #this functions calculates the mean # snps and adds that layer as
    #annotation. Additionally, it checks for overlap in annotations and adjusts
    #as necessary
    add_annotations <- function(tree_plot, tip_vector_in) {
      g <- tree_plot

      if (Values[["n"]] > 0) {
        #this is the i'th list, for which we are calculating the offset
        for (i in seq_along(tip_vector_in)) {
          current_tips <- Values[["tip_vec"]][[i]]

          n_overlap <- 0     # start by assuming no overlap
          if (i > 1) {         # for the first set of tips no comparisons needed
            # otherwise do comparisons

            #this is the j'th list, against which we need to compare if i
            #overlaps it
            for (j in 1:(i - 1)) {
              compare_tips <- Values[["tip_vec"]][[j]]  #tips to compare to

              # for every match, count it
              n_overlap <- n_overlap + any(current_tips %in% compare_tips)
            }
          }

          # set the clade label offset based on how many sets of previous tips
          #it overlaps and provide user #option to adjust the position of
          #all annotations
          label_offset <- label_off() + n_overlap * 0.004

          #uses the snpAnno function to calculate the mean # of snps for 
          #brushed tips
          snp_mean <-
            snp_anno(gene_file = geneObjectForSNP(),
                    tips = current_tips)

          #generates the layer for the set of brushed tips
          g <- g +
            make_layer(
              tree_plot,
              tips = tip_vector_in[[i]],
              label = paste0("Range \nof \nSNP(s)- \n",
                             paste0(min(snp_mean), sep = ",", max(snp_mean))),
              color = lab_color(),
              offset = label_offset
            )
        }
      }
      return(g)
    }

    #function to create the tree.
     current_tree_out <- function() {
      add_map(tree = add_annotations(tree_plot = make_tree_out(),
                                   tip_vector_in =  create_tip_list()),
             metaFile = mFileMatOut())
    }

    #reactive to send tree to downloadImage module
    tree_out <- reactive({
       current_tree_out()
    })

    return(tree_out)
  }
