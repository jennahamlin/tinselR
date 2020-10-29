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
                    mainPanel(h1(strong("Getting started with tinselR")),
                              tags$br(),
                              p("tinselR  (pronounced tinsel-er) at its' most 
                              basic level is a graphical viewer of newick
                              phylogenetic trees and as a program for producing
                                publication-ready figures.", "The", em("power"),
                                "of tinselR comes with combining a genetic
                                distance matrix for annotating a tree for
                                epidemological outbreak analyses.", "A genetic
                                distance matrix contains the number of snp
                                differences for all pairwise comparisons. Note
                                that the data you upload is only held within
                                the application while on the site
                                (i.e. no data is stored.) When you are
                                happy with the way your tree looks scroll to the
                                bottom and press Download"),
                              tags$p("If you have any problems, please file an",
                                     a("issue", href =
                                         "https://github.com/jennahamlin/tinselR/issues"
                                     )),
                              tags$br(),
                              tags$h4(strong("Data Upload")),
                              tags$p("Please click on the 'Data Upload' pane
                              to upload your files. Alternatively, take a look
                              at the Example Data to test out the application.
                              A brief description about the example data is
                              provided in the About the Example Data pane."), 
                              tags$br(),
                              tags$p("There is only one differences between the
                              'Example Data' and the 'Data Upload' tabs: the 
                              example data tab has pre-loaded data, while the
                              data upload tab is where you, the user, can upload
                              your own files (i.e. tree, genetic distance, and
                              metadata). Both the 'Example Data' and 'Data 
                              Upload' tabs provide error messages as a user 
                              either uploads or selects all three files. Types
                              of file checks that occur inform the user of the
                              correctly selected delimiter, if the tips are
                              concordant across all three files, or if there
                              is a column to use the add heatmap functionality. 
                              This file checking happens independently of if
                              the user wants the information. By that, we mean
                              a user can ignore the file check messages if they
                              do not need them and proceed using the 
                              application. Beyond that the application is
                              exactly the same between user and example data,
                              thus why we highly encourage users to play with
                              the example data first just to familiarize
                              yourself with the application. Below we describe
                              the three example data file sets and what can be
                              done within the application  to alter your tree
                              image. "),
                              tags$br(),
                              tags$h4(strong("Data files to upload")),
                              tags$br(),
                              tags$li(em("1. Phylogenetic Tree"), "- required; a",
                                      a("newick",
                                        href =
                                          "https://en.wikipedia.org/wiki/
                                        Newick_format"), "generated tree"),
                              tags$br(),
                              tags$div(tags$li(em("2. Genetic Distance Data"),
                                               "- optional for use with the
                                               annotation function.",
                                               tags$br(), "A tsv/txt/csv file of
                                      SNP differences. See image below for
                                               example."),
                                       tags$br(),
                                       tags$img(src =
                                                  "www/geneDistanceExample.PNG",
                                                height = "50%", width = "50%")),
                              tags$br(),
                              tags$div(tags$li(em("3. Meta Data"), "- optional for
                              easy correction of tip labels;",
                                               tags$br(),
                                               "a tsv/txt/csv file - requires
                                               column headers of Display.labels
                                               and Tip.labels. See image below
                                               for example."),
                                       tags$br(),
                                       tags$img(src = "www/metaDataExample.PNG",
                                                height = "50%", width = "50%")),
                              tags$br(),
                              tags$h4(strong("Ways to alter your tree image")),
                              tags$br(),
                              tags$div(tags$li(tags$em("Once the phylogenetic
                              tree is uploaded you can -")),
                                       "Alter visualization parameters",
                                       tags$br(),
                                       tags$img(src =
                                                  "www/treeWAlignedTips.PNG",
                                                height = "50%", width = "50%")),
                              tags$br(),
                              tags$div(tags$li(tags$em("Once the genetic
                              distance file is uploaded you can -")), "Add
                              annotation to the visual representation of the
                              tree. If you want to add annotations then
                              highlight the tip labels to their most recent
                                       common ancestor,", tags$a("MRCA", href =
                                                                   "https://evolution.berkeley.edu/evolibrary/article/phylogenetics_02"), "
                                              , and press the 'Add Annotations'
                                              button. Repeat until you are
                                       satisfied.",
                                       tags$br(),
                                       tags$img(src = "www/treeWAnno.PNG",
                                                height = "50%", width = "50%")),
                              tags$br(),
                              tags$div(tags$li(tags$em("If column for heatmap
                                                       included in meta data
                                                       file you  can -")), "add
                                       a heatmap with or without annotations",
                                       tags$br(),
                                       tags$img(src =
                                                  "www/treeWAnnoAndHeatmap.PNG",
                                                height = "50%", width = "50%")),
                              tags$br(),
                              tags$div(tags$li(tags$em("When you are happy
                              with the image, you can download it.")),
                                       tags$br(),
                                       tags$img(src = "www/Slide7.PNG",
                                                height = "50%", width = "50%")), 
                              tags$br()
                                    
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
