#' displayTree UI Function
#' 
#'   @title   mod_displayTree_ui and mod_displayTree_server
#' @description  A shiny Module. This module combines the tree with genetic
#'  distance as an S4 object that allows tree plotting and accessing the 
#'  combined data. 
#'
#' @param id shiny id
#' @param input internal
#' @param output internal
#' @param session internal
#'
#' @rdname mod_displayTree
#'
#' @keywords internal
#' @export 
#' @importFrom shiny NS tagList 
mod_displayTree_ui <- function(id){
  ns <- NS(id)
  tagList(
  )
}

#' displayTree Server Function
#' @rdname mod_displayTree
#' @export
#' @keywords internal

mod_displayTree_server <- function(input, output, session,
                                   tree_file_out, geneObjectOutForSNP, align,
                                   tree_format, font, num_scale, node, lim,
                                   boot_pos, mid_p, mat_off) {
  ns <- session$ns

  #midpoint root the tree based on reactive value, if not just display the tree
  mid_tree <- reactive({
    if (mid_p() == TRUE) {
      return(phytools::midpoint.root(tree_file_out()))
    } else {
      return(tree_file_out())
    }
  })

  #convert phylogenetic tree (midpoint rooted or not) to tibble to join tree
  #and genetic distance matrix
  tree_object<-reactive({
    tibble::as_tibble(mid_tree())
  })

  #join the treeobject and updated genetic distance file by label and
  #convert to s4 object
  g_and_t_s4 <- reactive({
    combine_g_and_t(tree_object(), geneObjectOutForSNP())
  })
  
  ########additional reactive tree parameters to possibly include
  #these could be parameters to increase/decrease how far tree fills
  #to the margins
  #ggplot2::theme(plot.margin=ggplot2::margin(10,10,10,10))

  ## displayTree server function. In theory this function should be able to be
  #moved over to the golem_utils_server.R script and updating the function to
  #take all of the reactive values but I have been unsuccessful at doing that
  #and get a strange error `Warning: Error in modifyList: is.list(val) is not
  #TRUE` so leaving here for now

  tree_plot <- function(input_file) {
    label <- NULL
    ggtree::ggtree(input_file, layout = tree_format()) +
      ggplot2::xlim(NA, lim()) +
      ggtree::geom_tiplab(align = align(), fontface = font(),
                          family="Helvetica", size = 3) +
      ggtree::geom_treescale(width = num_scale(), x = 0.005, y = -3 ) +
      ggtree::geom_text2(ggplot2::aes(label=label, 
                                      subset = !is.na(as.numeric(label)) &
                                        as.numeric(label) > node()),
                         nudge_x = boot_pos())
  }

  #major plotting reactive using an S4 object called above (gandTS4) or the
  #base mid_tree reactive made from import of treeFileOut and the  Upload data
  #module 
  make_tree <- reactive({

    # this disconnects the need for genetic distance file to be uploaded for
    #the tree viz to happen
    if (is.null(input$gandTS4)) {
      tree_plot(mid_tree())

      #what the call should look like if treePlot function was in the
      #golem_utils_server.R file
      #treePlot(inputFile = mid_tree(), align = align(), layout = treeformat(),
      #fontface = font(), width = numscale(), node = node(), limit = lim(),
      #nudge_x = bootPos())

    } else{
      tree_plot(g_and_t_s4())

      #what the call should look like if treePlot function was in the
      #golem_utils_server.R file
      #treePlot(inputFile = gandTS4(), align = align(), layout = treeformat(),
      #fontface = font(), width = numscale(), node = node(), limit = lim(),
      #nudge_x = bootPos())
    }
  })

  #return display tree reactive to be used in cladeAnnotator module 
  return(
    list(
      make_tree_out = reactive(make_tree())
    ))
}
