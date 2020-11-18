#' FAQ UI Function
#'
#' @title mod_about_ui mod_about_server
#'
#' @description A shiny Module. This module generates the FAQ page for the
#' application, which provides users with commmon tips for issues.
#'
#' @rdname mod_FAQ
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @importFrom shiny NS tagList

mod_FAQ_ui <- function(id){
  ns <- NS(id)
  tagList(
    fluidRow(column(12, offset = 0,
                    mainPanel(h2(strong("Frequently Asked Questions:")),
                              tags$br(),
                              tags$strong("Below is a list of questions you
                                          may have when using the application
                                          with your own data."),
                              tags$br())))
 
  )
}
    
#' FAQ Server Function
#'
#' @noRd 
mod_FAQ_server <- function(input, output, session){
  ns <- session$ns
 
}
    
## To be copied in the UI
# mod_FAQ_ui("FAQ_ui_1")
    
## To be copied in the server
# callModule(mod_FAQ_server, "FAQ_ui_1")
 
