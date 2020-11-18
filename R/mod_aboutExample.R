#' aboutExample Function
#'
#' @title mod_aboutExample_ui mod_aboutExample_server
#'
#' @description A shiny Module. This module generates the tab, which 
#' contains information regarding the example data sets that is pre-loaded
#'  when the application launches.
#'
#' @rdname mod_aboutExample
#'
#' @param id Internal parameters for {shiny}.
#' @param input internal
#' @param output internal
#' @param session internal
#
#'
#' @keywords internal
#' @importFrom shiny NS tagList
mod_aboutExample_ui <- function(id) {
  ns <- NS(id)
  tagList(
    fluidRow(column(12, offset = 0,
                    mainPanel(h2(strong("Description of Example Data")),
                              tags$br(),
                              tags$strong("Three example datasets are included
                                          to test out the application."),
                              tags$p("The three datasets hopefully highlight how
                              to best use the application. Data descriptions
                                     below -",
                                     tags$br(),
                                     tags$br(),
                                     tags$strong("How to test with example data"
                                     ),
                                     tags$br(),
                                     tags$br(),
                                     "The best way to get going with the example
                                     data is to select a dataset (e.g. examle
                                     data 1 ) and press the 'Add Tree button'.
                                     This will display the tree and allow you
                                     to start adjusting the tree visualization
                                     paramters, add annotation, and add a
                                     heatmap.",
                                     tags$br(),
                                     tags$br(),
                                     tags$li("If you want to add annotations
                                     then highlight the tip labels to their most
                                     recent common ancestor,",
                                             tags$a("MRCA", href =
                                                      "https://evolution.berkeley.edu/evolibrary/article/phylogenetics_02"),
                                             ", and press the 'Add Annotations'
                                             button. Repeat until you are 
                                             satisfied.")),
                              tags$li("If you want to remove previously placed
                              annotations, just press the 'Remove Annotations'
                              button, this will step your annotations back one
                              by one by removing the last placed annotation. For
                              the 'Remove Annotations' there is no need for
                              highlighting."),
                              tags$br(),
                              tags$li("Adding a heatmap is only a possibility 
                              for example data 1 and 3 and will automatically
                                     load/unload if you press the 'Add Heatmap/
                                      Remove Heatmap' buttons. You may need
                                      to adjust the position of the heatmap
                                      relative to the spacing of the plot."),
                              tags$br(),
                              tags$br(),
                              tags$p((tags$h5(tags$strong("Data set 1"))),"This
                              data set will automatically be seen when a user
                              presses the 'Add Tree' button. This dataset
                              includes only one column in the metadata for the
                                     heatmap.",
                                     tags$li(tags$em("example tree 1")), "This
                                     is a newick formatted tree with 16 tip
                                     labels.",
                                     tags$li(tags$em("example gene 1")), "This
                                     is a genetic distance matrix imported from
                                     a tsv file with the same corresponding tips
                                     in the example tree 1.",
                                     tags$li(tags$em("example meta 1")),
                                     "This is a meta data file used for
                                     correcting the tip labels and plotting a
                                     heatmap and was imported from a txt file."
                              ),
                              tags$br(),
                              tags$br(),
                              tags$p((tags$h5(tags$strong("Data set 2"))), "This
                              data set will automatically be seen when a user
                              presses the 'Add Tree' button. This dataset does
                              not have metadata for adding the heatmap but does
                              do automatic tip label correction. ", 
                                     tags$li(tags$em("example tree 2")), "This
                                     is a newick formatted tree with 19 tip
                                     labels ",
                                     tags$li(tags$em("example gene 2")), "This
                                     is a genetic distance matrix imported from
                                     a tsv file with the same corresponding tips
                                     in the example tree 2.",
                                     tags$li(tags$em("example meta 2")), "This
                                     is a meta data file used for correcting the
                                     tip labels which contains two columns and
                                     imported from a csv file."),
                              tags$br(),
                              tags$br(),
                              tags$p((tags$h5(tags$strong("Data set 3"))), "This
                              data set will not automatically be seen when a
                              user presses the 'Add Tree' button because the
                              user needs to change the spacing on the plot; we
                              suggest 0.09. The metadata has two columns that 
                              can be plotted with the heatmap button. One of the
                              columns plotted has missing values (NA). If you
                              press 'Add Heatmap' make sure to adjust the
                              position of the heatmap to be less than the
                              spacing on the plot. We suggest this value to be
                              0.02. Both of these options are located in the
                                     alter tree parameters box.",
                                     tags$li(tags$em("example tree 3")), "This
                                     is a newick formatted tree with 15 tip
                                     labels generated by RAxML.",
                                     tags$li(tags$em("example gene 3")), "This
                                     is a genetic distance matrix imported from
                                     a tsv file with the same corresponding tips
                                     in the example tree 3.",
                                     tags$li(tags$em("example meta 3")), "This
                                     is a meta data file used for correcting the
                                     tip labels, has two columns for adding two
                                     columns of heatmap, and imported from a tsv
                                     file.")
                    )))
  )
}

#' aboutExample Server Function
#'
#' @noRd
mod_aboutExample_server <- function(input, output, session) {
  ns <- session$ns
}
