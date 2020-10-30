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
#' @importFrom phytools midpoint.root
#' @importFrom tidytree as_tibble
#' @importFrom ggplot2 xlim
#' @importFrom ggplot2 aes
#' @importFrom ggtree ggtree
#' @importFrom ggtree geom_tiplab
#' @importFrom ggtree geom_treescale
#' @importFrom ggtree geom_text2
mod_displayTree_ui <- function(id) {
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
  #inputs:
  # tree_file_out - from upload/example Data module. Has attribute of phylo
  # geneObjectOutForSNP - from upload/example Data module. Is a tibble
  # align - from paramsTree module; this is checkbox input (Y or N)
  # tree_format - from paramsTree module; checkbox input (multiple choice)
  # font - from paramsTree module; this is checkbox input (multiple choice)
  # num_scale - from paramsTree module; this is numeric input from user
  # node - from paramsTree module; this is numeric input from user
  # lim - from paramsTree module; this is numeric input from user
  # boot_pos - from paramsTree module; this is numeric input from user
  # mid_p - from paramsTree module; this is checkbox input (Y or N)
  # mat_off - from paramsTree module; this is numeric input from user

  ns <- session$ns

  #midpoint root the tree based on reactive value, if not just display the tree
  mid_tree <- reactive({
    if (mid_p() == TRUE) {
      return(phytools::midpoint.root(tree_file_out()))
    } else {
      return(tree_file_out())
    }
  })

  #convert phylogenetic tree (midpoint rooted or not) to tidytree to join tree
  #and genetic distance matrix
  tree_object <- reactive({
    tidytree::as_tibble(mid_tree())
  })

  #join the treeobject and updated genetic distance file by label and
  #convert to s4 object
  g_and_t_s4 <- reactive({
    combine_g_and_t(tree_object(), geneObjectOutForSNP())
  })

  #major plotting reactive using an S4 object called above (gandTS4) or the
  #base mid_tree reactive using the tree_file_out from the Upload or Example
  #data module
  make_tree <- reactive({

    # this disconnects the need for genetic distance file to be uploaded for
    if (is.null(input$gandTS4)) {
      tree_plot(mid_tree())
    } else{
      tree_plot(g_and_t_s4())
    }
  })

  ######################################
  #### displayTree server functions ####
  ######################################

  #this takes in the tree file and allows for various parameters to be adjusted
  #because those parameters are reactives (i.e. tree_format())
  tree_plot <- function(input_file) {
    label <- NULL
    g <- ggtree::ggtree(input_file, layout = tree_format()) +
      ggplot2::xlim(NA, lim()) +
      ggtree::geom_tiplab(align = align(), fontface = font(),
                          family = "sans", size = 3) +
      ggtree::geom_treescale(width = num_scale(), x = 0.005, y = -3) +
      ggtree::geom_text2(ggplot2::aes(label = label,
                                      subset = !is.na(as.numeric(label)) &
                                        as.numeric(label) > node()),
                         nudge_x = boot_pos())
    return(g)
  }

  ############################
  #### displayData output ####
  ############################

  #return display tree reactive to be used in cladeAnnotator module
  return(
    list(
      make_tree_out = reactive(make_tree())
    ))
}
