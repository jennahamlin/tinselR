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
mod_downloadImage_ui <- function(id, label){
  ns <- NS(id)
  tagList(
    selectInput(ns("device"), label = "Type", choices = c("png", "pdf", "tiff")),
    numericInput(ns("width"), "Width of Image (inches)", value = 6),
    numericInput(ns("height"), "Height of Images (inches)", value = 8),
    downloadButton(ns("download"))
    #,
    #tableOutput(ns("selectedIndivs"))
    
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
      paste("treePlot", '.', Sys.Date(), '.', input$device, sep = '')}, #as is this does not include end of file designation (i.e. .pdf, when)
    
    content = function(file) {
      ggplot2::ggsave(file, treeFile(), width = input$width, height = input$height)}
  )
}

## To be copied in the UI
# mod_downloadImage_ui("downloadImage_ui_1")

## To be copied in the server
# callModule(mod_downloadImage_server, "downloadImage_ui_1")

