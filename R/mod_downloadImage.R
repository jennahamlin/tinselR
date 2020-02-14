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
    downloadButton(ns("download"), label)
  )
}

# Module Server

#' @rdname mod_downloadImage
#' @export
#' @keywords internal

mod_downloadImage_server <- function(input, output, session, treeFile, 
                                     filename ){
  ns <- session$ns
  
  output$download <- downloadHandler(
    filename = function() {
      paste("data_", Sys.Date())
    },
    content = function(file) {
      ggtree::ggsave(file,treeFile(), device = input$device)
    }
  )
}

## To be copied in the UI
# mod_downloadImage_ui("downloadImage_ui_1")

## To be copied in the server
# callModule(mod_downloadImage_server, "downloadImage_ui_1")

