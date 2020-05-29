#' @import shiny
app_ui <- function() {
  tagList(
    # Leave this function for adding external resources
    golem_add_external_resources(),
    # List the first level UI elements here 
    fluidPage(
      navbarPage("Tinsel - a tree visulization and annotation tool",
                 tabPanel("ReadMe",fluidRow(column(12, includeMarkdown("gettingStarted.md")))),
                 tabPanel("Phylogenetic Tree",
                          sidebarLayout(
                            sidebarPanel(
                              #mod_errorCheck_ui("errorCheck_ui_1"),
                              mod_uploadData_ui("uploadData_ui_1"),
                              mod_paramsTree_ui("paramsTree_ui_1"),
                              mod_downloadImage_ui("downloadImage_ui_1"),
                              width = 4),
                            mainPanel(
                              mod_displayTree_ui("displayTree_ui_1"),
                              mod_cladeAnnotator_ui("cladeAnnotator_ui_1")
                            )
                          )))
                 #,
      #            tabPanel("Genetic Distance",
      #                     sidebarLayout(
      #                       sidebarPanel(
      #                         mod_dataInput_ui("dataInput_ui_gene",
      #                                          tags$div("User GENETIC data",
      #                                                   tags$br(),
      #                                                   "(.csv, .tsv, or .txt file format)")),width = 3),
      #                       mainPanel(
      #                         mod_displayTable_ui("displayTable_ui_2"))))
      # )
      # ,
      # tabPanel("Metadata",
      #          sidebarLayout(
      #            sidebarPanel(
      #              mod_dataInput_ui("dataInput_ui_meta",
      #                               tags$div("User META data",
      #                                        tags$br(),
      #                                        "(.csv, .tsv, or .txt file format)")),width = 3),
      #            mainPanel(
      #              mod_displayTable_ui("displayTable_ui_1")))))
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
