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
mod_downloadImage_ui <- function(id){
  ns <- NS(id)
  tagList(
    selectInput(ns("type"), label = "Type", choices = c("png", "pdf", "tiff", "svg")),
    downloadButton(ns("download"), "Download")
  )
}

# Module Server

#' @rdname mod_downloadImage
#' @export
#' @keywords internal

mod_downloadImage_server <- function(input, output, session, plot){
  ns <- session$ns
  
  output$download <- downloadHandler(
    filename = function(){
      paste0("plot.", input$type)
    },
    content = function(file){
      switch(
        input$type,
        png = png(file),
        pdf = pdf(file),
        tiff = tiff(file),
        svg = svg(file)
      )
      print(plot())
      dev.off()
    }
  )
  
}

## To be copied in the UI
# mod_downloadImage_ui("downloadImage_ui_1")

## To be copied in the server
# callModule(mod_downloadImage_server, "downloadImage_ui_1")

