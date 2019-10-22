#' @import shiny
app_ui <- function() {
  tagList(
    # Leave this function for adding external resources
    golem_add_external_resources(),
    # List the first level UI elements here 
    fluidPage(
      sidebarLayout(
        sidebarPanel(
          mod_csvFileInput_ui("csvFileInput_ui_meta", label = "Metadata File")
        ),
        mainPanel(
          dataTableOutput("metatable")
        )
      ),
      sidebarLayout(
        sidebarPanel(
          mod_csvFileInput_ui("csvFileInput_ui_genetic", label = "Genetic Distance File")
        ),
        mainPanel(
          dataTableOutput("genetable"))
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
