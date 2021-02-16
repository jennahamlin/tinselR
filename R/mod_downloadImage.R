#' downloadImage Function
#'
#' @title   mod_downloadImage_ui and mod_downloadImage_server
#'
#' @description  A shiny Module. This module allows user to download the
#' adjusted tree image. Current download formats include png, pdf, and tiff.
#'
#' @rdname mod_downloadImage
#' 
#' @param id shiny id
#' @param input internal
#' @param output internal
#' @param session internal
#' @param treeout from cladeAnnotator module; mostly like class of ggtree, gg, 
#' gglot2 if user combined genetic data with tree data; if not then class phylo
#'
#'
#' @keywords internal
#' @export
#' @importFrom shiny NS tagList
#' @importFrom ggplot2 ggsave
#' @importFrom shinyjs hide
#' @importFrom shinyjs show
mod_downloadImage_ui <- function(id) {
  ns <- NS(id)
  tagList(
    
    tags$table(width = "100%",
               tags$th("Download Options", colspan = "3",
                       style = "font-size:20px; color:#444444")),
    tags$hr(style = "border-color: #99b6d8;"),
    
    column(numericInput(ns("width"),  tags$b("Width (inches)",
                                             style = "color:#afafae"),
                        value = 6), width = 2),
    column(numericInput(ns("height"),  tags$b("Height (inches)",
                                              style = "color:#afafae"),
                        value = 8), width = 2),
    column(selectInput(ns("file_type"), label =  tags$b("File Type",
                                                        style =
                                                          "color:#afafae"),
                       choices = c("pdf", "png", "tiff")), width = 2),
    column(downloadButton(ns("downloadPlot"), "Download Plot"), width = 2)
    
    #uncomment the below out for cdc server for this to act as a rough
    #approximate of user count.
    #column(shinyjs::useShinyjs(),
    # textInput(ns("text"), tags$b("User Id or Name", style = "color:#afafae"),
    #           "", placeholder = "please enter info to download"), width = 2),
    # column(shinyjs::hidden(
    #     downloadButton(ns("downloadPlot")), width = 2))
  )
}

#' downloadImage Server Function
#'
#' @rdname mod_downloadImage
#' @export
#' @keywords internal

mod_downloadImage_server <- function(input, output, session, tree_out) {
  ns <- session$ns

  #inputs
  #tree_out -mostly like class of ggtree, gg, gglot2 if user combined genetic 
  #data with tree data; if not then class phylo 
  
  output$downloadPlot <- downloadHandler(
    filename = function() {
      paste("data-",  Sys.Date(), ".", input$file_type,  sep = "")},
    content = function(file) {
      shiny::withProgress(message = 'Downloading the image',
                          value = 0, {
                            incProgress(0.1, detail = 'processing...')
                            ggplot2::ggsave(file, plot = tree_out(), 
                                            width = input$width,
                                            height = input$height, 
                                            device = input$file_type)
                          } )
    }
  )
  
  #the file 'tinseUserCount.txt' serves as an approximation for user count
  #based on the number of downloads. Please note this is a very rough
  #approximation. If someone does not want to include this; then comment
  #the below code out (L81 - 89) and make sure to remove the line
  #`shinyjs::hidden` in the UI above. Once those things are done, then the
  #download button is automatically displayed.
  # observeEvent(input$text, {
  #   if (input$text == "") {
  #     shinyjs::hide("downloadPlot")
  #   } else {
  #     shinyjs::show("downloadPlot")
  #   cat(input$text, file = "tinselUserCount.txt", sep = "\n", append = TRUE)
  #   }
  # })
}
