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
      return(HTML('Your metadata file does not contain the correct column headers. Please correct and try again.'))
    } else if("Display.labels" %in% colnames(mFile) != TRUE) {
      return(HTML('Your metadata file does not contain the correct column headers. Please correct and try again'))
    } 
    # Check for the same number of tips
    if(length(tFileTips) != length(gFileTips) |
       length(tFileTips) != length(mFileTips) |
       length(gFileTips) != length(mFileTips)) {
      return(paste("No. of labels in tree file:",
                   as.character(length(tFileTips)),
                   "No. of labels in distance file:",
                   as.character(length(gFileTips)),
                   "No. of labels in meta data file:",
                   as.character(length(mFileTips)),
                   '<span style="color:red"><font size=4>The number of tip labels given in your input files are unequal. Please correct and try again.</font></span>'))}
    
    if(setequal(tFileTips, gFileTips) != TRUE) {
      return(paste('<font size=4>Labels',
                   paste(setdiff(gFileTips, tFileTips), collapse = ", "),
                   "are present in your distance file but not your tree file.</font>",
                   '<font size=4>Labels',
                   paste(setdiff(tFileTips, gFileTips), collapse = ", "),
                   "are present in your tree file but not your distance file.</font>",
                   '<span style="color:red"><font size=4>Please correct the label differences in your input files and try again.</font></span>'))
    } else if(setequal(tFileTips, mFileTips) != TRUE) {
      return(paste('<font size=4>Labels',
                   paste(setdiff(mFileTips, tFileTips), sep = ", "),
                   "are present in your metadata file but not your tree file.</font>",
                   '<font size=4>Labels',
                   paste(setdiff(tFileTips, mFileTips), sep = ", "),
                   "are present in your tree file but not your metadata file.</font>",
                   '<span style="color:red"><font size=4>Please correct the label differences in your input files and try again.</font></span>'))
    } else if(setequal(gFileTips, mFileTips) != TRUE) {
      return(paste('<font size=4>Labels',
                   paste(setdiff(mFileTips, gFileTips), sep = ", "),
                   "are present in your metadata file but not your distance file.</font>",
                   '<font size=4>Labels',
                   paste(setdiff(gFileTips, mFileTips), sep = ", "),
                   "are present in your distance file but not your metadata file</font>",
                   '<span style="color:red"><font size=4>Please correct the label differences in your input files and try again.</font></span>'))
    } 
    else {
      HTML("ok this loaded the files; got tips and checked for headers")}
  }
  
}

## To be copied in the UI
# mod_tipCheck_ui("tipCheck_ui_1")

## To be copied in the server
# callModule(mod_tipCheck_server, "tipCheck_ui_1")

