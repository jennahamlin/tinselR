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
mod_tipCheck_server <- function(input, output, session, metaFileOut, geneFileTipCheckOut, metaSep, geneSep){
  ns <- session$ns
  
  output$fileChecking <- renderUI({
    ns <- session$ns
    if(input$fileTesting == T){
      if(is.null(metaFileOut())) {
        return(HTML('<span style="color:red"><font size=4>Please select a meta file before continuing.</font></span>'))
      } 
      # else if(is.null(geneFileTipCheckOut())) {
      #   return(HTML('<span style="color:red"><font size=4>Please select a distance file before continuing.</font></span>'))
      # }
      else if ((!is.null(metaFileOut()) )) { # & (!is.null(geneFileTipCheckOut()))
        mSanity(
          impMeta = metaFileOut(),
          metSep = metaSep()
          #,
          #genSep = geneSep()
          #, 
          #impGene = geneFileTipCheckOut()
        )
        
        #HTML('<span style="color:green"><font size=4><strong>No errors were detected in the input files. Please continue with Tinsel by clicking the Update Plot button at the bottom of the Decorate Your Tree section below.</strong></font></span>')
      } else {
        return(NULL)
      }
    }  
  })
  
  
  
  # Check imported data files for tip label agreement. Abort with instructions to fix if disagreement found.
  mSanity <- function(impMeta, metSep, impGene, genSep) {
    
    # #tree <- treeio::read.newick(file = impTree) #this is the imported tree - impTree
    # treeIn <- impTree
    # treeIntTips <- treeIn$tip.label
    # #treeTips <- sort(treeIn$tip.label)
    
    mFile <- readData(filePath = impMeta$datapath, sep = metSep)
    #print(mFile)
    mFileTips <- mFile %>% dplyr::pull(1) %>% sort
    print(mFileTips)
  
    # Check for required column names if meta data file
    if("Tip.labels" %in% colnames(mFile) != TRUE) {
      return(HTML('<span style="color:red"><font size=4>Your metadata file does not contain the column Tip.labels. Please correct and try again.</font></span>'))
    } else if("Display.labels" %in% colnames(mFile) != TRUE) {
      return(HTML('<span style="color:red"><font size=4>Your metadata file does not contain the column Display.labels. Please correct and try again.</font></span>'))
    }
    
    # gFile <- readData(filePath = impGene$datapath, sep = genSep)
    # gFileTips <- gFile %>% dplyr::pull(1) %>% sort
    # print(gFileTips)
    
    
    
    
    
    
    # # Check for the same number of tips
    # if(length(treeTips) != length(gFileTips) | 
    #    length(treeTips) != length(mFileTips) | 
    #    length(gFileTips) != length(mFileTips)) {
    #   return(paste("No. of labels in tree file:", 
    #                as.character(length(treeTips)),
    #                "No. of labels in distance file:", 
    #                as.character(length(gFileTips)), 
    #                "No. of labels in meta data file:", 
    #                as.character(length(mFileTips)), 
    #                '<span style="color:red"><font size=4>The number of tip labels given in your input files are unequal. Please correct and try again.</font></span>'))}
    # 
    # if(setequal(treeTips, gFileTips) != TRUE) {
    #   return(paste('<font size=4>Labels', 
    #                paste(setdiff(gFileTips, treeTips), collapse = ", "), 
    #                "are present in your distance file but not your tree file.</font>",
    #                '<font size=4>Labels', 
    #                paste(setdiff(treeTips, gFileTips), collapse = ", "), 
    #                "are present in your tree file but not your distance file.</font>",
    #                '<span style="color:red"><font size=4>Please correct the label differences in your input files and try again.</font></span>'))
    # } else if(setequal(treeTips, mFileTips) != TRUE) {
    #   return(paste('<font size=4>Labels', 
    #                paste(setdiff(mFileTips, treeTips), sep = ", "), 
    #                "are present in your metadata file but not your tree file.</font>",
    #                '<font size=4>Labels',
    #                paste(setdiff(treeTips, mFileTips), sep = ", "), 
    #                "are present in your tree file but not your metadata file.</font>",
    #                '<span style="color:red"><font size=4>Please correct the label differences in your input files and try again.</font></span>'))
    # } else if(setequal(gFileTips, mFileTips) != TRUE) {
    #   return(paste('<font size=4>Labels', 
    #                paste(setdiff(mFileTips, gFileTips), sep = ", "), 
    #                "are present in your metadata file but not your distance file.</font>",
    #                '<font size=4>Labels', 
    #                paste(setdiff(gFileTips, mFileTips), sep = ", "), 
    #                "are present in your distance file but not your metadata file</font>",
    #                '<span style="color:red"><font size=4>Please correct the label differences in your input files and try again.</font></span>'))
    # } else {
    #return(NULL)
    HTML("ok")
    # }
  }

  # fileTest <- eventReactive(input$fileTesting, {
  #   
  #   # if(is.null(treeFileOut())) {
  #   #   return('<span style="color:red"><font size=4>Please select a metadata file before continuing.</font></span>')
  #   # } else if(is.null(metaFileOut())) {
  #   #   return('<span style="color:red"><font size=4>Please select a distance file before continuing.</font></span>')
  #   # } else if(is.null(geneFileTipCheckOut())) {
  #   #   return('<span style="color:red"><font size=4>Please select a Newick tree file before continuing.</font></span>')
  #   # } else {
  #   print("line 39")
  #   sanity(
  #     #impTree = treeFileOut()
  #          # ,
  #          impGene = geneFileTipCheckOut(), 
  #          genSep = geneSep()
  #          #,
  #          # impMeta = metaFileOut()
  #   )
  #   # }
  #   return(fileTest)
  #   
  # })
  
}

## To be copied in the UI
# mod_tipCheck_ui("tipCheck_ui_1")

## To be copied in the server
# callModule(mod_tipCheck_server, "tipCheck_ui_1")

