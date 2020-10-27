#' relaunchApp UI Function
#'
#' @title mod_relaunchApp_ui mod_relaunchApp_server
#'
#' @description A shiny Module. This module will relaunch the application
#' and clear out any existing data sets. 
#'
#' @rdname mod_relaunchApp
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @importFrom shiny NS tagList
mod_relaunchApp_ui <- function(id) {
  ns <- NS(id)
  tagList(
    actionButton(ns("reload_session"), HTML("Relaunch App"),
                 style = "color: #fff; background-color:#9c461e ;
                 border-color: #9c461e; width: 200px;", icon("refresh")))
}

#' relaunchApp Server Function
#'
#'@rdname mod_relaunchApp
mod_relaunchApp_server <- function(input, output, session) {
  ns <- session$ns

  #this will reload the session and clear existing info -
  #good if you want to start TOTALLY new
  observeEvent(input$reload_session, {
    session$reload()
  })
}
