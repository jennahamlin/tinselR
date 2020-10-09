#' tipCheck UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd 
#'
#' @importFrom shiny NS tagList 
mod_tipCheck_ui <- function(id){
  ns <- NS(id)
  tagList(
    actionButton(ns("fileTesting"),  "Confirm Files Match ",
                 style="color: #fff; background-color: #d1ad5b; border-color: #d1ad5b; width: 200px;", icon("equals")),
    uiOutput(ns('fileChecking'))
  )
}

#' tipCheck Server Function
#'
#' @noRd 
mod_tipCheck_server <- function(input, output, session, metaFileOut, metaSep, geneFileOut, geneSep){
  ns <- session$ns
  
  output$fileChecking <- renderUI({
    ns <- session$ns
    if(input$fileTesting == T){
      sanity(
        impMeta = metaFileOut(),
        metSep = metaSep(),
        impGene = geneFileOut(),
        genSep = geneSep()
      )
    }  
  })
  
  # Check imported data files for tip label agreement. Abort with instructions to fix if disagreement found.
  sanity <- function(impMeta, metSep, impGene, genSep) {
    
    mFile <- readData(filePath = impMeta$datapath, sep = metSep)
    mFileTips <- mFile %>% dplyr::pull(1) %>% sort
    print(mFileTips)
    
    gFile <- readData(filePath = impGene$datapath, sep = genSep)
    gFileTips <- gFile %>% dplyr::pull(1) %>% sort
    print(gFileTips)
    
    # Check for required column names if meta data file
    if("Tip.labels" %in% colnames(mFile) != TRUE) {
      return(HTML('Your metadata file does not contain the correct column headers. Please correct and try again.'))
    } else if("Display.labels" %in% colnames(mFile) != TRUE) {
      return(HTML('Your metadata file does not contain the correct column headers. Please correct and try again'))
    } else {
      HTML("ok this loaded the files; got tips and checked for headers")}
  }
  
}

## To be copied in the UI
# mod_tipCheck_ui("tipCheck_ui_1")

## To be copied in the server
# callModule(mod_tipCheck_server, "tipCheck_ui_1")

