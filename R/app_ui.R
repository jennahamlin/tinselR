#' @import shiny
app_ui <- function() {
  tagList(
    #::themeSelector(),  # <--- Add this somewhere in the UI to test out themes
    # Leave this function for adding external resources
    golem_add_external_resources(),
    # List the first level UI elements here 
    fluidPage(
      theme = shinythemes::shinytheme('yeti'),
      navbarPage("Tinsel - a tree visulization and annotation tool",
                 tabPanel("ReadMe",fluidRow(column(12, wellPanel( includeHTML("About.Rhtml"))))), # wellPanel adds a gray box
                 tabPanel("Data Upload",
                          sidebarLayout(
                            sidebarPanel(
                              mod_uploadData_ui("uploadData_ui_1"),
                              mod_paramsTree_ui("paramsTree_ui_1"),
                              mod_downloadImage_ui("downloadImage_ui_1"),
                              width = 3),
                            mainPanel(
                              mod_displayTree_ui("displayTree_ui_1"),
                              mod_cladeAnnotator_ui("cladeAnnotator_ui_1")
                            )
                          )),
                 
                 tabPanel("Tinsel pratice with example data",
                          sidebarLayout(
                            sidebarPanel(
                              mod_exampleData_ui("exampleData_ui_1"),
                              mod_paramsTree_ui("paramsTree_ui_1_exmaple"), 
                              width = 3),
                            mainPanel(
                              mod_displayTree_ui("displayTree_ui_1_example"),
                              mod_cladeAnnotator_ui("cladeAnnotator_ui_1_example"))))
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
    #,
    #attempting to use a different style sheet - but it appears to be unsucessful
    #tags$link(rel="stylesheet", type="text/css", href="/Users/jennahamlin/Desktop/Tinsel/inst/app/www/bootswatch.litera.css")
    # Add here all the external resources
    # If you have a custom.css in the inst/app/www
    # Or for example, you can add shinyalert::useShinyalert() here
    #tags$link(rel="stylesheet", type="text/css", href="www/custom.css")
  )
}
