# Module UI

#' @title   mod_dataInput_ui and mod_dataInput_server
#' @description  A shiny Module.
#'
#' @importFrom magrittr %>%
#' @name %>%
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
    fileInput(ns("id"), 
              label,     #label here is specified and is called in the app_ui with the tags$div section 
              multiple = FALSE,     #does not all multiple files to be uploaded
              accept = c("text/csv",     #accept - this bypasses the  need to do validation as in the web brower only the files with these extensions are selectable
                         "text/comma-separated-values,text/plain",
                         ".csv",
                         ".tsv")),
    
    # Input: Select separator ----
    radioButtons(ns("sep"), "Separator",
                 choices = c(Comma = ",",
                             Tab = "\t"),
                 selected = "\t"),
    
    # Input: Display top 10 rows of file or all ----
    radioButtons(ns("disp"), "Display",
                 choices = c(Head = "head",
                             All = "all"),
                 
                 selected = "head")
  )
}

# Module Server

#' @rdname mod_dataInput
#' @export
#' @keywords internal

mod_dataInput_server <- function(input, output, session) {
  
  #reactive expression that until a file is uploaded, the below message is displayed
  userFile <- reactive({
    validate(need(input$id !="", "Please import a data file"))
    input$id
  })    
  
  #now uses readr package and read_delim function; as this makes it easier to work with. 
  dataFile <- reactive({
    readr::read_delim(userFile()$datapath,
                      delim = input$sep,
                      trim_ws = T, 
                      skip_empty_rows = T,
                      col_names = T)
  })
  
  headfile <- reactive({
    if(input$disp == "head") {
      return(head(dataFile()))
    }
    else {
      return(dataFile())
    }
  })
  
}

## To be copied in the UI
# mod_dataInput_ui("dataInput_ui_1")

## To be copied in the server
# callModule(mod_dataInput_server, "dataInput_ui_1")

