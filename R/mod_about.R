#' about UI Function
#'
#' @title mod_about_ui mod_about_server
#' 
#' @description A shiny Module. This module generates the landing page for the
#' application and provides a link to the github repo where users may file
#' an issue.
#'
#' @rdname mod_about
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @importFrom shiny NS tagList
mod_about_ui <- function(id) {
  ns <- NS(id)
  tagList(
    fluidRow(column(12, offset = 0,
                    mainPanel(h1(strong("Getting started with Tinsel")),
                              tags$br(),
                              p("Tinsel at its' most basic level is a graphical
                              viewer of newick phylogenetic trees and as a
                              program for producing publication-ready figures.",
                                "The", em("power"),"of Tinsel comes with combining
                              a genetic distance matrix for annotating a tree
                              for epidemological outbreak analyses.", "A genetic
                              distance matrix contains the number of snp 
                              differences for all pairwise comparisons. Note 
                              that the data you upload is only held within the
                              application while on the site (i.e. no data is 
                              stored.)"),
                              tags$strong("Please click on the 'Data Upload' 
                                          pane to upload your files."),
                              tags$li(em("Phylogenetic Tree"), "- required; a",
                                      a("newick",
                                        href =
                                          "https://en.wikipedia.org/wiki/
                                        Newick_format"
                                      ),
                                      "generated tree"),
                              tags$br(),
                              tags$li(em("Genetic Distance Data"),
                                      "- optional for use with the annotation
                                      function.",
                                      tags$br(), "A tsv/txt/csv file of
                                      SNP differences. See image below for 
                                      example"),
                              tags$br(),
                              tags$img(src = "www/geneDistanceExample.PNG",
                                       height = "50%", width = "50%"),
                              tags$br(),
                              tags$li(em("Meta Data"), "- optional for easy
                                      correction of tip labels;",
                                      tags$br(), "a tsv/txt/csv file - requires
                                      column headers of Display.labels and
                                      Tip.labels. See image Below for 
                                      example."),
                              tags$br(),
                              tags$img(src = "www/metaDataExample.PNG",
                                       height = "50%", width = "50%"),
                              tags$br(), 
                              tags$li(tags$strong("Once the phylogenetic tree is
                              uploaded you can -")),
                              tags$br(),
                              "Alter visualization parameters",
                              tags$br(),
                              tags$img(src = "www/treeWAlignedTips.PNG",
                                       height = "50%", width = "50%"),
                              tags$br(), 
                              strong("Once the genetic distance file is
                                     uploaded you can -"),
                              "add annotation to the visual representation of
                              the tree",
                              tags$br(),
                              tags$img(src = "www/treeWAnno.PNG",
                                       height = "50%", width = "50%"),
                              tags$br(), 
                              
                              tags$br(),
                              strong("When you are happy with the way your tree
                                     looks-"),
                              tags$br(),
                              "scroll to the bottom; enter your name and press
                              Download",
                              tags$br(),
                              tags$br(),
                              "If you have any problems, please file an",
                              a("issue", href =
                                  "https://github.com/jennahamlin/Tinsel/issues"
                              )
                    )
    )
    )
  )
}

#' about Server Function
#'
#' @rdname mod_about
mod_about_server <- function(input, output, session) {
  ns <- session$ns
}
