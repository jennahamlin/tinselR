# Module UI
  
#' @title   mod_csvFileInput_ui and mod_csvFileInput_server
#' @description  A shiny Module.
#'
#' @param id shiny id
#' @param input internal
#' @param output internal
#' @param session internal
#'
#' @rdname mod_csvFileInput
#'
#' @keywords internal
#' @export 
#' @importFrom shiny NS tagList 
mod_csvFileInput_ui <- function(id, label = "CSV file") {
  # Create a namespace function using the provided id
  ns <- NS(id)
  
  tagList(
    fileInput(ns("file"), label),
    checkboxInput(ns("heading"), "Has heading")
   )
}
    
# Module Server
    
#' @rdname mod_csvFileInput
#' @export
#' @keywords internal
    
mod_csvFileInput_server <- function(input, output, session, stringsAsFactors) {
  ns <- session$ns
  
  # The selected file, if any
  userFile <- reactive({
    # If no file is selected, don't do anything
    validate(need(input$file, message = FALSE))
    input$file
  })
  
  # The user's data, parsed into a data frame
  dataframe <- reactive({
    read.csv(userFile()$datapath,
             header = input$heading,
             quote = input$quote,
             stringsAsFactors = stringsAsFactors)
  })
  
  # We can run observers in here if we want to
  observe({
    msg <- sprintf("File %s was uploaded", userFile()$name)
    cat(msg, "\n")
  })
  
  # Return the reactive that yields the data frame
  return(dataframe)
}



## To be copied in the UI
# mod_csvFileInput_ui("csvFileInput_ui_1")
    
## To be copied in the server
# callModule(mod_csvFileInput_server, "csvFileInput_ui_1")
 
