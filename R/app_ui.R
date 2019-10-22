#' @import shiny
app_ui <- function() {
  tagList(
    # Leave this function for adding external resources
    golem_add_external_resources(),
    # List the first level UI elements here 
    fluidPage(
      sidebarLayout(
        sidebarPanel(
          mod_csvFileInput_ui("csvFileInput_ui_1", label = "Metadata CSV file")
        ),
        mainPanel(
          dataTableOutput("table")
        )
      ),
      sidebarLayout(
        sidebarPanel(
          mod_csvFileInput_ui("csvFileInput_ui_1", label = "Genetic Distance CSV file")
        ),
        mainPanel(
          dataTableOutput("table"))
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
