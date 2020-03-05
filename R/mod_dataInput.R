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
    #accept - this bypasses the  need to do validation as in the web brower only the files with these extensions are selectable
    #label here is specified and is called in the app_ui with the tags$div section 
    #does not all multiple files to be uploaded
    fileInput(ns("id"), label, 
              multiple = FALSE,
              accept = c("text/csv", 
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
  
  ##the below reactive expression allow one to use the full_join method to combine a tree and genetic distance matrix
  ##can this be more elegently written?
  #reactive expression to get the header names, this is necessary for when reading in a square matrix and the first column is a -, which
  #makes read.table skip giving that column a column header
  col.names <- reactive({
    scan(text = readLines(userFile()$datapath, 1), what = "", quiet = TRUE)})
  
  #Filter(function(x)....) is to remove any excess columns at the end of a file. Confirmed with tsv file
  #The rest is just reading in the file based on the reactive userFile expression. 
  dataFile <- reactive({
    Filter(function(x)!all(is.na(x)),  utils::read.table(userFile()$datapath,
                                                         sep = input$sep,
                                                         header = T,
                                                         stringsAsFactors = FALSE,
                                                         row.names = NULL))
  })
  
  #work around to combine the col.names and datafile reactives. This outputs a dataframe with the column headers repeated. 
  bind <- reactive({rbind(col.names(), dataFile())})
  
  #this final step takes the firs row of column names and moves it up and removes the first row. This gives correct column headers without skipping a column. 
  #as of today this does not work with the meta data file; need to fix this
  dataFileCleaned <- reactive({bind() %>% purrr::set_names(bind()[1,])%>% dplyr::distinct()})

  headfile <- reactive({
    if(input$disp == "head") {
      return(head(dataFileCleaned()))
    }
    else {
      return(dataFileCleaned())
    }
  })
  
}

## To be copied in the UI
# mod_dataInput_ui("dataInput_ui_1")

## To be copied in the server
# callModule(mod_dataInput_server, "dataInput_ui_1")

