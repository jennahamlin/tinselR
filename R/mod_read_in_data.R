# Module UI

#' @title   mod_read_in_data_ui and mod_read_in_data_server
#' @description  A shiny Module.
#'
#' @param id shiny id
#' @param input internal
#' @param output internal
#' @param session internal
#'
#' @rdname mod_read_in_data
#'
#' @keywords internal
#' @export 
#' @importFrom shiny NS tagList 
#Modules UI function should be suffixed with input, output, or ui
mod_read_in_data_ui <- function(id, label = " upload your meta data CSV file"){
  # Create a namespace function using the provided id
  ns <- NS(id)
  tagList(
    fileInput(ns("file"), label)
  )
}

# Module Server

#' @rdname mod_read_in_data
#' @export
#' @keywords internal

mod_read_in_data_server <-   function(input, output, session, stringsAsFactors) {
  # The selected file, if any
  userFile <- reactive({
    # If no file is selected, don't do anything
    validate(need(input$file, message = FALSE))
    input$file
  })
  
  # The user's data, parsed into a data frame
  dataframe <- reactive({
    read.csv(userFile()$datapath,
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

## To be copied in the server

