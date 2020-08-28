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

    downloadButton(ns("downloadPlot"), "Download the plot"))
  
    # selectInput(ns("fileType"), label = "Type", choices = c("png", "pdf", "tiff")),
    # numericInput(ns("width"), "Width of Image (inches)", value = 6),
    # numericInput(ns("height"), "Height of Images (inches)", value = 8),
    # shinyjs::useShinyjs(),
    # textInput(ns("text"), "User Id", "", placeholder = "please enter your user id to download"),
    # shinyjs::hidden(downloadButton(ns("download")))
    # 
  
}

# Module Server

#' @rdname mod_downloadImage
#' @export
#' @keywords internal

mod_downloadImage_server <- function(input, output, session, treePlotOut){
  ns <- session$ns

  output$downloadPlot <- downloadHandler(
    filename = function() {
      paste("data-", Sys.Date(), ".png", sep="")
    },
    content = function(file) {
      ggplot2::ggsave(file, plot = treePlotOut(), device = "png")
    }
  )
  
  # observe({input$download
  #   
  #     ggplot2::ggsave(filename = "treePlot",path = tempdir(), plot = treeWLayers(), width = input$width, height = input$height, device = input$fileType)
  # })
  #   
  # output$download <- downloadHandler(
  #   
  #   filename = function(){
  #         paste("treePlot", '.', Sys.Date(), '.', input$fileType, sep = '')} ,
  # 
  #   content = function(file){
  #     file.copy(paste(tempdir(), "/", filename= 
  #                       paste("treePlot", '.', Sys.Date(), '.', input$fileType, sep = ''), sep = ""), file)
  #     
  #   }
  #   
  # )     
  
  
  # observe({input$download
  # 
  #   #ggplot2::ggsave(filename = "treePlot", treeWLayers(), path = tempdir(), width = input$width, height = input$height, device = "png")
  # 
  #   ggplot2::ggsave(filename = paste("treePlot", '.', Sys.Date(), '.', input$fileType, sep = ''),
  #                                     path = tempdir(), plot = treeWLayers(), width = input$width, height = input$height, device = input$fileType)
  # 
  # 
  #   zip::zipr(zipfile = paste(tempdir(), "/", "treePlot.zip", sep = ""),
  #             files = paste(tempdir(), "/", "treePlot", '.', Sys.Date(), '.', input$fileType, sep = ''))
  #               
  #               #paste(tempdir(), "/", "treePlot", sep = ""))
  # })
  # 
  # output$download <- downloadHandler(
  # 
  #   filename = "treePlot.zip",
  # 
  #   content = function(file){
  # 
  #     file.copy(paste(tempdir(), "/", "treePlot.zip", sep = ""), file)
  # 
  #   }
  # 
  # )
  # 
  # observeEvent(input$text, {
  #   if (input$text == "") 
  #     shinyjs::hide("download")
  #   else
  #     shinyjs::show("download")
  #   utils::write.table(input$text, file = "tinselUsers.txt", append = T)
  #   
  # })


  
  # output$download <- downloadHandler(
  #   filename = function() {
  #     paste("treePlot", '.', Sys.Date(), '.', input$fileType, sep = '')}, #as is this does not include end of file designation (i.e. .pdf, when)
  #   
  #   content = function(file) {
  #     ggplot2::ggsave(file, treeWLayers(), path = tempdir(), width = input$width, height = input$height)}
  # )
}

## To be copied in the UI
# mod_downloadImage_ui("downloadImage_ui_1")

## To be copied in the server
# callModule(mod_downloadImage_server, "downloadImage_ui_1")

