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
               tags$th("Download Options", colspan="3", style="font-size:20px; color:#444444")),
    tags$hr(style="border-color: #99b6d8;"),
    column(selectInput(ns("fileType"), label =  tags$b("File Type", style="color:#afafae"), choices = c( "pdf", "png", "tiff")), width = 2),
    
    column(numericInput(ns("width"),  tags$b("Width (inches)", style="color:#afafae"), value = 6), width = 2),
    column(numericInput(ns("height"),  tags$b("Height (inches)", style="color:#afafae"), value = 8), width = 2),
    
    column(
      shinyjs::useShinyjs(),
      textInput(ns("text"), tags$b("User Id or Name", style="color:#afafae"), "", placeholder = "please enter info to download"), width = 2),
    column(
      shinyjs::hidden(downloadButton(ns("downloadPlot"))), width = 2)
  )    
}

# Module Server

#' @rdname mod_downloadImage
#' @export
#' @keywords internal

mod_downloadImage_server <- function(input, output, session, treeOut){
  ns <- session$ns
  
  output$downloadPlot <- downloadHandler(
    filename = function() {
      paste("data-", Sys.Date(), ".", input$fileType,  sep="")
    },
    content = function(file) {
      ggplot2::ggsave(file, plot = treeOut(), width = input$width, height = input$height, device = input$fileType)
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

