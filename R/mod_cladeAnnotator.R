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
    values <- reactiveValues()
    observe({
      values[["n"]]   <- 0
      values[["tip_vec"]] <- list()
      values[["show_map"]] <- 0
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
        
        values[["n"]] <- values[["n"]] + 1
        
        #add the tip vector (aka label) to the annotation reactive value
        values[["tip_vec"]][[paste0("tips", values[["n"]])]] <-
          data_with_selection2()
        
        #add to variable called tips
        tips <- create_tip_list(r_n_values = values[["n"]],
                                r_tip_vec = values[["tip_vec"]])
        
        output$tree_display <- renderPlot({
          add_map(tree = add_annotations(tree_plot = make_tree_out(),
                                         tip_vector_in =  tips,
                                         r_n_values = values[["n"]],
                                         r_tip_vec = values[["tip_vec"]],
                                         label_off = label_off(),
                                         geneObjectForSNP = geneObjectForSNP(),
                                         lab_color = lab_color()),
                  metaFile = mFileMatOut(), r_value = values[["show_map"]],
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
        
        if (values[["n"]] > 0) {
          
          values[["n"]] <- values[["n"]] - 1
          
          temp_tip <- values[["tip_vec"]]
          
          #remove the last set of tips that the user selected
          values[["tip_vec"]] <- temp_tip[-length(temp_tip)]
        }
        
        tips <- create_tip_list(r_n_values = values[["n"]],
                                r_tip_vec = values[["tip_vec"]])
        
        output$tree_display <- renderPlot({
          add_map(tree = add_annotations(tree_plot = make_tree_out(),
                                         tip_vector_in =  tips,
                                         r_n_values = values[["n"]],
                                         r_tip_vec = values[["tip_vec"]],
                                         label_off = label_off(),
                                         geneObjectForSNP = geneObjectForSNP(),
                                         lab_color = lab_color()),
                  metaFile = mFileMatOut(), r_value = values[["show_map"]],
                  matOff = mat_off())
        })
      }
    })
    
    #allow the user to add a matrix to a tree; change show_map to the value of 1
    observeEvent(add_matrix(), {
      
      #display that layer onto the tree
      values[["show_map"]] <-  1
      output$tree_display <- renderPlot({
        
        #render the plot using the  current_tree_out function.
        current_tree_out()
      })
    })
    
    #as above with add matrix but this allows the removal of the matrix by
    #setting show_map to 0
    observeEvent(remove_matrix(), {
      
      #display that layer onto the tree
      values[["show_map"]] <-  0
      output$tree_display <- renderPlot({
        current_tree_out()
      })
    })
    
    #####################################
    ## cladeAnnotator server functions ##
    #####################################
    
    #function to create the tip list. list apply over the counter('n') and
    #paste the values in the tip vector to the variable tips. The input in this
    #function is two reactive values (r_n_values and r_tip_vec); r_n_values is a
    #counter; while r_tip_vec is the actual tips the user has selected
    create_tip_list <- function(r_n_values, r_tip_vec) {
      tips <- c()
      if (r_n_values < 1) {
        #skip
      } else {
        tips <- lapply(1:r_n_values, function(i)
          r_tip_vec[[paste0("tips", i)]])
      }
      return(tips)
    }
    
    #this functions calculates the mean # snps and adds that layer as
    #annotation. Additionally, it checks for overlap in annotations and adjusts
    #as necessary
    add_annotations <- function(tree_plot, tip_vector_in, r_n_values, r_tip_vec,
                                label_off, geneObjectForSNP, lab_color) {
      g <- tree_plot
      
      if (r_n_values > 0) {
        #this is the i'th list, for which we are calculating the offset
        for (i in seq_along(tip_vector_in)) {
          current_tips <- r_tip_vec[[i]]
          n_overlap <- 0     # start by assuming no overlap
          if (i > 1) {         # for the first set of tips no comparisons needed
            # otherwise do comparisons
            #this is the j'th list, against which we need to compare if i
            #overlaps it
            for (j in 1:(i - 1)) {
              compare_tips <- r_tip_vec[[j]]  #tips to compare to
              # for every match, count it
              n_overlap <- n_overlap + any(current_tips %in% compare_tips)
            }
          }
          
          # set the clade label offset based on how many sets of previous tips
          #it overlaps and provide user #option to adjust the position of
          #all annotations
          label_offset <- label_off + n_overlap * 0.004
          
          #uses the snpAnno function to calculate the mean # of snps for
          #brushed tips
          snp_mean <-
            snp_anno(gene_file = geneObjectForSNP,
                     tips = current_tips)
          
          #generates the layer for the set of brushed tips
          g <- g +
            make_layer(
              tree_plot,
              tips = tip_vector_in[[i]],
              label = paste0("Range \nof \nSNP(s)- \n",
                             paste0(min(snp_mean), sep = ",", max(snp_mean))),
              color = lab_color,
              offset = label_offset
            )
        }
      }
      return(g)
    }
    
    #function to add an annotation layer, uses findMRCA from phylotools to get
    #the MRCA (node) for the selected tips. Takes tree and brushed tips; allows
    #user to adjust the color and offset of annotations. Label = the annotation
    #drawn on with the snp_mean calculated
    make_layer <- function(tree, tips, label, color, offset) {
      ggtree::geom_cladelabel(
        node = phytools::findMRCA(ape::as.phylo(tree), tips),
        label = label,
        color = color,
        angle = 0,
        offset = offset
      )
    }
    
    #this function takes in the genetic distance matrix which has been transformed
    #to three columns with all pairwise comparisons listed. The tips parameter is
    #tips that have been selected/brushed by the user and the output is a vector
    #of snp differences that can be used to get the range for a clade of interest.
    snp_anno <- function(gene_file, tips) {
      label <- name <- value <- NULL #deals with 'no visible binding' error
      snp_vector <- c()
      for (i in 1:(length(tips) - 1)) {
        for (j in (i + 1):length(tips)) {
          if (tips[i] == tips[j])
            next
          snp_vector <- append(snp_vector, gene_file %>%
                                 dplyr::filter(label == tips[i] & name == tips[j])
                               %>% dplyr::pull(value)
          )
        }
      }
      return(as.numeric(snp_vector))
    }
    
    #add map function; takes in a tree and the converted meta data file, along with
    #two reactive values (r_value and matoff) which are initialized on start of the
    #application. Only allows the inclusion of the map if the value of show_map is
    #greater than 0; when add_map button is pushed then this makes r_value > 0
    add_map <- function(tree, metaFile, r_value, matOff) {
      if (r_value > 0 & !is.null(metaFile)) {
        tree <- ggtree::gheatmap(tree,
                                 metaFile,
                                 offset = matOff,
                                 width = 0.2,
                                 colnames_angle = 45,
                                 colnames_offset_y = -1,
                                 hjust = 0.5)
        #+ ggplot2::scale_color_viridis_d(option = matCol())
        #will add in this option to change the color; which i want to add
      }
      return(tree)
    }
    
    #function to create the tree.
    current_tree_out <- function() {
      add_map(tree = add_annotations(tree_plot = make_tree_out(),
                                     tip_vector_in =
                                       create_tip_list(
                                         r_n_values = values[["n"]],
                                         r_tip_vec = values[["tip_vec"]]),
                                     r_n_values = values[["n"]],
                                     r_tip_vec = values[["tip_vec"]],
                                     label_off = label_off(),
                                     geneObjectForSNP = geneObjectForSNP(),
                                     lab_color = lab_color()),
              metaFile = mFileMatOut(),
              r_value = values[["show_map"]],
              matOff = mat_off())
    }
    
    ##################################
    ## cladeAnnotator server output ##
    ##################################
    
    #reactive to send tree to downloadImage module
    tree_out <- reactive({
      current_tree_out()
    })
    
    return(tree_out)
  }
