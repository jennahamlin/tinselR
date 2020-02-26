# Module UI

#' @title   mod_downloadImage_ui and mod_downloadImage_server
#' @description  A shiny Module.
#'
#' @param id shiny id
#' @param input internal
#' @param output internal
#' @param session internal
#'
#' @rdname mod_downloadImage
#'
#' @keywords internal
#' @export 
#' @importFrom shiny NS tagList 
mod_downloadImage_ui <- function(id, label = "Download tree Image"){
  ns <- NS(id)
  tagList(
    selectInput(ns("device"), label = "Type", choices = c("png", "pdf", "tiff")),
    numericInput(ns("width"), "Width of Image (inches)", value = 6),
    numericInput(ns("height"), "Height of Images (inches)", value = 8),
    downloadButton(ns("download"), label),
    tableOutput(ns("selectedIndivs"))
    
  )
}

# Module Server

#' @rdname mod_downloadImage
#' @export
#' @keywords internal

mod_downloadImage_server <- function(input, output, session, treeFile){
  ns <- session$ns
  
  output$download <- downloadHandler(
    filename = function() {
      paste("data", Sys.Date(), input$device, sep = ".")
    },
    content = function(filename) {
      ggplot2::ggsave(filename = filename,treeFile(), device = input$device, width = input$width, height = input$height)
    })
  
}

## To be copied in the UI
# mod_downloadImage_ui("downloadImage_ui_1")

## To be copied in the server
# callModule(mod_downloadImage_server, "downloadImage_ui_1")

