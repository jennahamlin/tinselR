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
    
    #uiOutput(ns('fileChecking'))
    htmlOutput(ns('fileChecking'))
    
    #htmlOutput(ns("text"))
 
  )
}
    
#' tipCheck Server Function
#'
#' @noRd 
mod_tipCheck_server <- function(input, output, session, metaFileOut, geneFileTipCheckOut, treeFileOut){
  ns <- session$ns
  
  # Check imported data files for tip label agreement. Abort with instructions to fix if disagreement found.
  sanity <- function(impTree, impMeta, impGene ) {
    #tree <- treeio::read.newick(file = impTree) #this is the imported tree - impTree
    treeTips <- sort(impTree$tip.label)
    
    #gFile <- readData(impGene, genSep) #fileIn is either tsv, txt, or csv of genetic distance and sep is a reactive selected by user and is the variable of impGene
    gFileTips <- impGene %>% dplyr::pull(1) %>% sort
    
    #mFile <- readData(impMeta, metSep)
    mFileTips <- impMeta %>% dplyr::pull(1) %>% sort
    
    # Check for required column names if meta data file 
    if("Tip.labels" %in% colnames(impMeta) != TRUE) { 
      return('<span style="color:red"><font size=4>Your metadata file does not contain the column Tip.labels. Please correct and try again.</font></span>')
    } else if("Display.labels" %in% colnames(impMeta) != TRUE) {
      return('<span style="color:red"><font size=4>Your metadata file does not contain the column Display.labels. Please correct and try again.</font></span>')
    } 
    
    # Check for the same number of tips
    if(length(treeTips) != length(gFileTips) | 
       length(treeTips) != length(mFileTips) | 
       length(gFileTips) != length(mFileTips)) {
      return(paste("No. of labels in tree file:", 
                   as.character(length(treeTips)),
                   "No. of labels in distance file:", 
                   as.character(length(gFileTips)), 
                   "No. of labels in meta data file:", 
                   as.character(length(mFileTips)), 
                   '<span style="color:red"><font size=4>The number of tip labels given in your input files are unequal. Please correct and try again.</font></span>'))}
    
    if(setequal(treeTips, gFileTips) != TRUE) {
      return(paste('<font size=4>Labels', 
                   paste(setdiff(gFileTips, treeTips), collapse = ", "), 
                   "are present in your distance file but not your tree file.</font>",
                   '<font size=4>Labels', 
                   paste(setdiff(treeTips, gFileTips), collapse = ", "), 
                   "are present in your tree file but not your distance file.</font>",
                   '<span style="color:red"><font size=4>Please correct the label differences in your input files and try again.</font></span>'))
    } else if(setequal(treeTips, mFileTips) != TRUE) {
      return(paste('<font size=4>Labels', 
                   paste(setdiff(mFileTips, treeTips), sep = ", "), 
                   "are present in your metadata file but not your tree file.</font>",
                   '<font size=4>Labels',
                   paste(setdiff(treeTips, mFileTips), sep = ", "), 
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
    } else {
      return(NULL)
    }
  }
  
  fileTest <- eventReactive(input$fileTesting, {

    if(is.null(metaFileOut())) {
      return('<span style="color:red"><font size=4>Please select a metadata file before continuing.</font></span>')
    } else if(is.null(geneFileTipCheckOut())) {
      return('<span style="color:red"><font size=4>Please select a distance file before continuing.</font></span>')
    } else if(is.null(treeFileOut())) {
      return('<span style="color:red"><font size=4>Please select a Newick tree file before continuing.</font></span>')
    } else {
      print("Line 95")
      sanity(impTree = treeFileOut(),
             impGene = geneFileTipCheckOut(),
             impMeta = metaFileOut())
    }
    return(fileTest)
    
    })

  
  # output$text <- renderUI({
  #   ns <- session$ns
  #   HTML(paste(c("banana","raccoon","duck","grapefruit")))
  # })
  
  output$fileChecking <- renderUI({
    ns <- session$ns
    if(is.null(fileTest())) {
      HTML('<span style="color:green"><font size=4><strong>No errors were detected in the input files. Please continue with Tinsel by clicking the Update Plot button at the bottom of the Decorate Your Tree section below.</strong></font></span>')
    } else {
      HTML(fileTest())
    }
  })
  
}
    
## To be copied in the UI
# mod_tipCheck_ui("tipCheck_ui_1")
    
## To be copied in the server
# callModule(mod_tipCheck_server, "tipCheck_ui_1")
 
