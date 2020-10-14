#' addMatrix UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd 
#'
#' @importFrom shiny NS tagList 
mod_addMatrix_ui <- function(id){
  ns <- NS(id)
  tagList(
    plotOutput(ns("treeDisplay"))
  )
}
    
#' addMatrix Server Function
#'
#' @noRd 
mod_addMatrix_server <- function(input, output, session, addMatrix, makeTreeOut, metaFileOut, metaSep){
  ns <- session$ns
 
  
  
  mFileConversion <- function(impMeta, metSep){
    mFile <- fileCheck(fileUp = impMeta, fileType = metSep, fileSep = metSep)
    
    print(mFile$Display.labels)
    
    meta2 <-mFile %>%
        #tibble::remove_rownames() %>%
        tibble::column_to_rownames(var = "Display.labels") 
    #%>%
    #    dplyr::select(SourceSite)  #this can be user input
        
    
    print(meta2)
  }
  
  mFileOut <- reactive({mFileConversion( impMeta = metaFileOut(),
                                         metSep = metaSep())
    })
  

  
  observeEvent(addMatrix(), {
    output$treeDisplay <- renderPlot({
      #print(makeTreeOut()$data$label)
      ggtree::gheatmap(makeTreeOut(), mFileOut(), offset = 0.009, width = 0.02) 
      #+
      #ggplot2::scale_fill_manual(breaks = c("Stool", "Other", "Environmental", "Urine"),
      #                           values=c("steelblue", "firebrick", "darkgreen", "brown"))
      })
  })
}
    
## To be copied in the UI
# mod_addMatrix_ui("addMatrix_ui_1")
    
## To be copied in the server
# callModule(mod_addMatrix_server, "addMatrix_ui_1")
 
