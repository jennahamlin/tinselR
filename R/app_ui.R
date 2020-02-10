#' @import shiny
app_ui <- function() {
  tagList(
    # Leave this function for adding external resources
    golem_add_external_resources(),
    # List the first level UI elements here 
    fluidPage(
      h1("Tinsel"),
      sidebarPanel(
        mod_uploadTree_ui("uploadTree_ui_1"),
        mod_paramsTree_ui("paramsTree_ui_1"),
        mod_dataInput_ui("dataInput_ui_meta",
                         tags$div("User META data",
                                  tags$br(),
                                  "(.csv, .tsv, or .txt file format)")),
        mod_dataInput_ui("dataInput_ui_gene",
                         tags$div("User GENETIC data",
                                  tags$br(),
                                  "(.csv, .tsv, or .txt file format)"))),
      mainPanel(mod_displayTree_ui("displayTree_ui_1"),
                mod_displayTable_ui("displayTable_ui_1", "Meta Data"),
                mod_displayTable_ui("displayTable_ui_2", "Genetic Data")
                )
    )
  )
}
    
  #   # List the first level UI elements here 
  #   navbarPage(
  #     "Tinsel",
  #     tabPanel(
  #       "Load Data",
  #       sidebarPanel(
  #       mod_treeInput_ui("treeInput_ui_1",
  #                         tags$div(
  #                          "Import a newick phylogenetic tree") ),
  #         tags$hr(style="border-color: black;"),
  #         mod_dataInput_ui(
  #           "dataInput_ui_meta", 
  #           tags$div(
  #             "User META data", 
  #             tags$br(), 
  #             "(.csv, .tsv, or .txt file format)"
  #           )
  #         ), 
  #         helpText("Can add help text here"),
  #         # Horizontal line ----
  #         tags$hr(style="border-color: black;"),
  #         mod_dataInput_ui(
  #           "dataInput_ui_gene", 
  #           tags$div(
  #             "User GENETIC data", 
  #             tags$br(), 
  #             "(.csv, .tsv, or .txt file format)"
  #           )
  #         )
  #       ),
  #       
  #       mainPanel(
  #         tabsetPanel(
  #           mod_treeDisplay_ui("treeDisplay_ui_1", "Phylogenetic Tree"),
  #           mod_displayTable_ui("displayTable_ui_1", "Meta Data"),
  #           mod_displayTable_ui("displayTable_ui_2", "Genetic Data")
  #         )
  #       )
  #     ),
  #     tabPanel("Getting Started",
  #              
  #              fluidRow(column(12,
  #                              includeMarkdown("gettingStarted.md")
  #              )
  #              )
  #     )
  #   )
   #)
#}

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
