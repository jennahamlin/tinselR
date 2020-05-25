# Module UI

#' @title   mod_dataInput_ui and mod_dataInput_server
#' @description  A shiny Module. This modules allows one to upload csv/tsv/txt files and is combined with the displayTable module. 
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
    fileInput(ns("geneFile"), 
              label,     #label here is specified and is called in the app_ui with the tags$div section 
              multiple = FALSE,     #does not all multiple files to be uploaded
              accept = c("text/csv",     #accept - this bypasses the  need to do validation as in the web brower only the files with these extensions are selectable
                         "text/comma-separated-values,text/plain",
                         ".csv",
                         ".tsv")),
    
    # Input: Select separator ----
    radioButtons(ns("geneSep"), "Separator for genetic data",
                 choices = c(Comma = ",",
                             Tab = "\t"),
                 selected = "\t"),
    
    # fileInput(ns("metaFile2"), "Choose meta File",
    #           multiple = FALSE,
    #           accept = c("text/csv",
    #                      "text/comma-separated-values,text/plain",
    #                      ".csv")),
    # 
    # # Input: Select separator ----
    # radioButtons(ns("metaSep2"), "Separator meta data",
    #              choices = c(Comma = ",",
    #                          Tab = "\t"),
    #              selected = "\t")
  )
}

# Module Server

#' @rdname mod_dataInput
#' @export
#' @keywords internal

mod_dataInput_server <- function(input, output, session, metaFileOut, metaSepOut) {
  
  #reactive expression that until a file is uploaded, the below message is displayed
  userFile <- reactive({
    validate(need(input$geneFile !="", "Please import a data file"))
    input$geneFile
  })    
  
   dataFile <- reactive({ 
     if (is.null(metaFileOut()$datapath)) {
       geneFileUncorrected <- readr::read_delim(userFile()$datapath,
                                 delim = input$geneSep,
                                 trim_ws = T,
                                 skip_empty_rows = T,
                                 col_names = T)

    } 
    else {
    
      metaFileComb <- readr::read_delim(metaFileOut()$datapath,
                        delim = metaSepOut(),
                        trim_ws = T,
                        skip_empty_rows = T,
                        col_names = T)
      
       geneFileCorrected <-  readr::read_delim(userFile()$datapath,
                                               delim = input$geneSep,
                                               trim_ws = T,
                                               skip_empty_rows = T,
                                               col_names = T)
      
      colnames(geneFileCorrected)[2:ncol(geneFileCorrected)] = metaFileComb$Display.labels[which(metaFileComb$Tip.labels %in% colnames(geneFileCorrected)[2:ncol(geneFileCorrected)])]
       
       geneFileCorrected$. = metaFileComb$Display.labels[which(metaFileComb$Tip.labels %in% geneFileCorrected$.)]
       
       return(geneFileCorrected)
    }
  })
  
  return(dataFile)
  
  
}

## To be copied in the UI
# mod_dataInput_ui("dataInput_ui_1")

## To be copied in the server
# callModule(mod_dataInput_server, "dataInput_ui_1")

