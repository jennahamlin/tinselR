# Module UI

#' @title   mod_downloadImage_ui and mod_downloadImage_server
#' @description  A shiny Module. This module allows one to download the adjusted tree image. Current download formats include png, pdf, and tiff.
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
    
    tags$table(width ="100%",
               tags$th("Download options", colspan="3", style="font-size:20px; color:#afafae")),
    tags$hr(style="border-color: black;"),
    column(selectInput(ns("fileType"), label = "Type", choices = c( "pdf", "png", "tiff")), width = 3),
    
    column(numericInput(ns("width"), "Width of Image (inches)", value = 6), width = 3),
    column(numericInput(ns("height"), "Height of Images (inches)", value = 8), width = 3),
    
    column(
    shinyjs::useShinyjs(),
    textInput(ns("text"), "User Id", "", placeholder = "please enter your name or user id to download"),
    shinyjs::hidden(downloadButton(ns("downloadPlot"))), width = 3)
  )
}

# Module Server

#' @rdname mod_downloadImage
#' @export
#' @keywords internal

mod_downloadImage_server <- function(input, output, session, treePlotOut){
  ns <- session$ns
  
  output$downloadPlot <- downloadHandler(
    filename = function() {
      paste("data-", Sys.Date(), ".", input$fileType,  sep="")
    },
    content = function(file) {
      ggplot2::ggsave(file, plot = treePlotOut(), width = input$width, height = input$height, device = input$fileType)
    }
  )
  
  #these essentially serves as a count for number of downloads
  observeEvent(input$text, {
    if (input$text == "")
      shinyjs::hide("downloadPlot")
    else
      shinyjs::show("downloadPlot")
    cat(input$text, file = "tinselUserCount.txt", sep = "\n", append = T) 

  })
}

## To be copied in the UI
# mod_downloadImage_ui("downloadImage_ui_1")

## To be copied in the server
# callModule(mod_downloadImage_server, "downloadImage_ui_1")

