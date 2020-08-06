#' about UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd 
#'
#' @importFrom shiny NS tagList 
mod_about_ui <- function(id){
  ns <- NS(id)
  tagList(
    fluidRow(column(12,mainPanel(h1("Getting started with Tinsel"),
              br(),
              p("Tinsel at its' most basic level is a graphical viewer of newick phylogenetic trees and 
    as a program for producing publication-ready figures. The",em("power"),"of Tinsel comes
    with combining a genetic distance matrix for annotating a tree for epidemological outbreak
    analyses.A genetic distance matrix contains the number of snp differences for all pairwise 
                comparisons."), 
              br(), 
              strong("Please click on the 'Data Upload' pane to upload your files."),
              br(), 
    tags$li(em("Phylogenetic Tree"),"- required; a", a("newick", href="https://en.wikipedia.org/wiki/Newick_format"), "generated tree"),
    tags$li(em("Genetic Distance Data"), "- optional for use with the annotation function; a tsv/txt/csv file of SNP differences"),
    tags$li(em("Meta Data"), "- optional for easy correction of tip labels  ; a tsv/txt/csv file of SNP differences"),
    br(), 
    em("Once the phylogenetic tree is uploaded you can") 
    )))
  )
}

#' about Server Function
#'
#' @noRd 
mod_about_server <- function(input, output, session){
  ns <- session$ns
  
}

## To be copied in the UI
# mod_about_ui("about_ui_1")

## To be copied in the server
# callModule(mod_about_server, "about_ui_1")

