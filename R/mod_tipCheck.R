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
    htmlOutput(ns('fileChecking')),
    
    tags$hr(style="border-color: #99b6d8;"),
    tags$table(width ="100%",
               tags$th("Tree Display", colspan="3", style="font-size:20px; color:#7ab567;")),
    tags$hr(style="border-color: #99b6d8;")
  )
}

#' tipCheck Server Function
#'
#' @noRd 
mod_tipCheck_server <- function(input, output, session, metaFileOut, metaSep, geneFileOut, geneSep, treeFileOutTips){
  ns <- session$ns
  
  output$fileChecking <- renderUI({
    ns <- session$ns
    if(input$fileTesting == T){
      sanity(
        impMeta = metaFileOut(),
        metSep = metaSep(),
        impGene = geneFileOut(),
        genSep = geneSep(), 
        impTree = treeFileOutTips()
      )
    }  
  })
  
  # Check imported data files for tip label agreement. Abort with instructions to fix if disagreement found.
  sanity <- function(impMeta, metSep, impGene, genSep, impTree) {
    
    mFile <- fileCheck(fileUp = impMeta, fileType = metSep, fileSep = metSep)
    mFileTips <- mFile %>% dplyr::pull(1) %>% sort
    print(mFileTips)
    
    gFile <- fileCheck(fileUp = impGene, fileType = genSep, fileSep = genSep)
    gFileTips <- gFile %>% dplyr::pull(1) %>% sort
    print(gFileTips)
    
    tFile <- treeio::read.newick(file = impTree$datapath)
    tFileTips <- sort(tFile$tip.label)
    
    # Check for required column names if meta data file
    if("Tip.labels" %in% colnames(mFile) != TRUE) {
      return('Your metadata file does not contain the correct column headers. Please correct and try again.')
    } else if("Display.labels" %in% colnames(mFile) != TRUE) {
      return('Your metadata file does not contain the correct column headers. Please correct and try again')
    } 
    
    # Check for the same number of tips
    if(length(tFileTips) != length(gFileTips) |
       length(tFileTips) != length(mFileTips) |
       length(gFileTips) != length(mFileTips)) {
      return(HTML(paste(
        "The number of tip labels in your input files are unequal, please correct.", 
        "No. of labels in tree file:", 
        as.character(length(tFileTips)),
        "No. of labels in distance file:",
        as.character(length(gFileTips)),
        "No. of labels in meta data file:",
        as.character(length(mFileTips)),
        sep = "<br/>")))
      } else {
      HTML("All three files pass checks and contain the same tip labels! - Go ahead and visualize")}
  }
  
}

## To be copied in the UI
# mod_tipCheck_ui("tipCheck_ui_1")

## To be copied in the server
# callModule(mod_tipCheck_server, "tipCheck_ui_1")

