#' @import shiny
app_ui <- function() {
  tagList(
    # Leave this function for adding external resources
    golem_add_external_resources(),
    # List the first level UI elements here 
    navbarPage("Tinsel",
               tabPanel("Load Data",
                        sidebarPanel(mod_dataInput_ui("dataInput_ui_meta", "User META data (.csv, .tsv, .txt format)"), helpText("Can add help text here"),
                                     # Horizontal line ----
                                     tags$hr(style="border-color: black;"),
                                     mod_dataInput_ui("dataInput_ui_gene", "User GENETIC data (.csv, .tsv, .txt format)"),
                                     # Horizontal line ----
                                     tags$hr(style="border-color: black;"),
                                     mod_treeInput_ui("treeInput_ui_1")),
                        mainPanel(
                          tabsetPanel(
                            tabPanel("Meta Data",
                                     tableOutput("metacontents")),
                            tabPanel("Genetic Data",
                                     tableOutput("genecontents")),
                            tabPanel("Phylogenetic Tree",
                                     plotOutput("tree"),
                                     checkboxInput("aligntiplabels", "Align tip labels", FALSE),
                                     checkboxInput("shownodelabels", "Show node labels", FALSE),
                                     numericInput("edgewidth", "Edge width", value=1, min=1)
                                     
                            )))),
               tabPanel("Getting Started",
                        fluidRow(column(12,
                                        includeMarkdown("gettingStarted.md")
                        )
                        )
               )
    )
  )
}


#' @import shiny
golem_add_external_resources <- function(){
  
  addResourcePath(
    'www', system.file('app/www', package = 'Tinsel')
  )
  
  tags$head(
    golem::activate_js(),
    golem::favicon()
    # Add here all the external resources
    # If you have a custom.css in the inst/app/www
    # Or for example, you can add shinyalert::useShinyalert() here
    #tags$link(rel="stylesheet", type="text/css", href="www/custom.css")
  )
}
