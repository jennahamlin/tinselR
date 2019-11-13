# Module UI

#' @title   mod_dataInput_ui and mod_dataInput_server
#' @description  A shiny Module.
#'
#' @param id shiny id
#' @param input internal
#' @param output internal
#' @param session internal
#'
#' @rdname mod_dataInput
#'
#' @keywords internal
#' @export 
#' @importFrom shiny NS tagList 
mod_dataInput_ui <- function(id, label) {
  # Create a namespace function using the provided id
  ns <- NS(id)
  
  tagList(
    
    # Input: Select a file ----
    fileInput(ns("file"), label,
              multiple = FALSE,
              accept = c("text/csv",
                         "text/comma-separated-values,text/plain",
                         ".csv")),

        # Input: Select separator ----
    radioButtons(ns("sep"), "Separator",
                 choices = c(Comma = ",",
                             Tab = "\t"),
                 selected = "\t"),
    
    radioButtons("disp", "Display",
                 choices = c(Head = "head",
                             All = "all")))
}

# Module Server

#' @rdname mod_dataInput
#' @export
#' @keywords internal

mod_dataInput_server <- function(input, output, session) {
  
  # The selected file, if any
  userFile <- reactive({
    # If no file is selected, don't do anything
    req(input$file)
  })    
  
  df <- reactive({
    utils::read.table(userFile()$datapath,
                      header = FALSE,
                      sep = input$sep,
                      row.names = NULL,
                      skip = 1,
                      stringsAsFactors = FALSE)
    
  })
}


## To be copied in the UI
# mod_dataInput_ui("dataInput_ui_1")

## To be copied in the server
# callModule(mod_dataInput_server, "dataInput_ui_1")

