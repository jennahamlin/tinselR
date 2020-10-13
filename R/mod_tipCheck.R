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
    tags$table(width ="100%",
               tags$th("File Check Output", colspan="3", style="font-size:20px; color:#7ab567;")),
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
    if (is.null(treeFileOutTips())) { 
      return(HTML('<span style="color:gray">Please upload a tree file</span>'))
    } else if (is.null(geneFileOut())){
      return(HTML('<span style="color:gray">Please upload a genetic distance file</span>'))
    } else if (is.null(metaFileOut())){
      return(HTML('<span style="color:gray">Please upload a meta data file</span>'))
      } else {
        sanity(
          impTree = treeFileOutTips(),
          impGene = geneFileOut(),
          genSep = geneSep(),
          impMeta = metaFileOut(),
          metSep = metaSep()
        )
      } 
  })
  
  # Check imported data files for tip label agreement. Abort with instructions to fix if disagreement found.
  sanity <- function(impMeta, metSep, impGene, genSep, impTree) {
    
    mFile <- fileCheck(fileUp = impMeta, fileType = metSep, fileSep = metSep)
    mFileTips <- mFile %>% dplyr::pull(1) %>% sort
    #print(mFileTips)
    
    gFile <- fileCheck(fileUp = impGene, fileType = genSep, fileSep = genSep)
    gFileTips <- gFile %>% dplyr::pull(1) %>% sort
    #print(gFileTips)
    
    tFile <- treeio::read.newick(file = impTree$datapath)
    tFileTips <- sort(tFile$tip.label)
    
    # Check for required column names if meta data file
    if("Tip.labels" %in% colnames(mFile) != TRUE) {
      return(HTML('<span style="color:gray">Your metadata file does not contain the correct column headers. Please correct and try again.</span>'))
    } else if("Display.labels" %in% colnames(mFile) != TRUE) {
      return(HTML('<span style="color:gray">Your metadata file does not contain the correct column headers. Please correct and try again.</span>'))
    } 
    
    # Check for the same number of tips
    if(length(tFileTips) != length(gFileTips) |
       length(tFileTips) != length(mFileTips) |
       length(gFileTips) != length(mFileTips)) {
      return(HTML(paste(
        '<span style="color:gray">The number of tip labels in your input files are unequal, please correct.</span>', 
        '<span style="color:gray">No. of labels in tree file:</span>', 
        as.character(length(tFileTips)),
        '<span style="color:gray">No. of labels in distance file:</span>',
        as.character(length(gFileTips)),
        '<span style="color:gray">No. of labels in meta data file:</span>',
        as.character(length(mFileTips)),
        sep = "<br/>")))
    } else {
      return(HTML('<span style="color:gray">All three files pass checks and contain the same tip labels!</span>'))}
  }
  
}

## To be copied in the UI
# mod_tipCheck_ui("tipCheck_ui_1")

## To be copied in the server
# callModule(mod_tipCheck_server, "tipCheck_ui_1")

