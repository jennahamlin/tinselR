#' @import shiny
#' @importFrom shinythemes shinytheme
app_ui <- function() {
  tagList(
    #::themeSelector(),  # <--- Add this somewhere in the UI to test out themes
    # Leave this function for adding external resources
    golem_add_external_resources(),
    # List the first level UI elements here
    fluidPage(
      theme = shinythemes::shinytheme("yeti"),
      navbarPage("tinselR - a tree visulization and annotation tool",
                 tabPanel("About tinselR", mod_about_ui("about_ui_1")),
                 tabPanel("About Example Data",
                       mod_aboutExample_ui("aboutExample_ui_1")),
                 tabPanel("About Tree Parameters",
                          mod_aboutTreeParams_ui("aboutTreeParams_ui_1")),
                 tabPanel("Example Data",
                          sidebarLayout(
                            sidebarPanel(
                              mod_exampleData_ui("exampleData_ui_1"),
                              mod_pushButtons_ui("pushButtons_ui_example"),
                              mod_relaunchApp_ui("relaunchApp_ui_example"),
                              width = 3),
                            mainPanel(
                              mod_tipCheck_ui(
                                "tipCheck_ui_example"),
                              mod_paramsTree_ui("paramsTree_ui_example"),
                              mod_displayTree_ui("displayTree_ui_example"),
                              mod_cladeAnnotator_ui(
                                "cladeAnnotator_ui_example"),
                              br(),
                              br(),
                              br(),
                              mod_downloadImage_ui("downloadImage_ui_example")
                            ))),
                 tabPanel("Upload Data",
                          sidebarLayout(
                            sidebarPanel(
                              mod_uploadData_ui("uploadData_ui_1"),
                              br(),
                              mod_pushButtons_ui(
                                "pushButtons_ui_data"),
                              mod_relaunchApp_ui(
                                "relaunchApp_ui_data"),
                              width = 3),
                            mainPanel(
                              mod_tipCheck_ui(
                                "tipCheck_ui_data"),
                              mod_paramsTree_ui(
                                "paramsTree_ui_data"),
                              mod_displayTree_ui(
                                "displayTree_ui_data"),
                              mod_cladeAnnotator_ui(
                                "cladeAnnotator_ui_data"),
                              br(),
                              br(),
                              br(),
                              mod_downloadImage_ui("downloadImage_ui_data")
                            ))),
                 tabPanel("FAQ", mod_FAQ_ui("FAQ_ui_1"))
      )
    )
  )
}

#' @import shiny
golem_add_external_resources <- function() {

  addResourcePath(
    "www", system.file("app/www", package = "tinselR")
  )

  tags$head(
    golem::activate_js(),
    golem::favicon(),
    # Add here all the external resources
    # If you have a custom.css in the inst/app/www
    # Or for example, you can add shinyalert::useShinyalert() here
    tags$link(rel = "stylesheet", type = "text/css", href = "www/custom.css")
  )
}
