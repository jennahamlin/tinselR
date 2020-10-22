#' downloadImage UI Function
#'
#' @title   mod_downloadImage_ui and mod_downloadImage_server
#'
#' @description  A shiny Module. This module allows one to download the
#' adjusted tree image. Current download formats include png, pdf, and tiff.
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
mod_downloadImage_ui <- function(id) {
  ns <- NS(id)
  tagList(

    tags$table(width = "100%",
               tags$th("Download Options", colspan = "3",
                       style = "font-size:20px; color:#444444")),
    tags$hr(style = "border-color: #99b6d8;"),
    column(selectInput(ns("file_type"), label =  tags$b("File Type",
                                                       style = "color:#afafae"),
                       choices = c("pdf", "png", "tiff")), width = 2),

    column(numericInput(ns("width"),  tags$b("Width (inches)",
                                             style = "color:#afafae"),
                        value = 6), width = 2),
    column(numericInput(ns("height"),  tags$b("Height (inches)",
                                              style = "color:#afafae"),
                        value = 8), width = 2),
    column(
      shinyjs::useShinyjs(),
      textInput(ns("text"), tags$b("User Id or Name", style = "color:#afafae"),
                "", placeholder = "please enter info to download"), width = 2),
    column(
      shinyjs::hidden(downloadButton(ns("download_plot"))), width = 2)
  )
}

#' downloadImage Server Funciton
#'
#' @rdname mod_downloadImage
#' @export
#' @keywords internal

mod_downloadImage_server <- function(input, output, session, treeOut) {
  ns <- session$ns

  output$download_plot <- downloadHandler(
    filename = function() {
      paste("data-", Sys.Date(), ".", input$file_type,  sep= "")
    },
    content = function(file) {
      ggplot2::ggsave(file, plot = treeOut(), width = input$width,
                      height = input$height, device = input$file_type)
    }
  )

  #these essentially serves as a count for number of downloads
  observeEvent(input$text, {
    if (input$text == "") {
      shinyjs::hide("download_plot")
    } else {
      shinyjs::show("download_plot")
    cat(input$text, file = "tinselUserCount.txt", sep = "\n", append = TRUE)
    }
  })
}
