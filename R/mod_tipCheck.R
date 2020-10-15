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
               tags$th("File Check Output", colspan="3", style="font-size:20px; color:#444444;")),
    htmlOutput(ns('fileChecking')), # this output info about if tip labels are not concordant between all three files
    htmlOutput(ns('fileChecking2')), # this one will tell user if they can add a matrix to the tree 
    
    tags$hr(style="border-color: #99b6d8;"),
    tags$table(width ="100%",
               tags$th("Tree Display", colspan="3", style="font-size:20px; color:#444444;")),
    tags$hr(style="border-color: #99b6d8;")
  )
}

#' tipCheck Server Function
#'
#' @noRd 
mod_tipCheck_server <- function(input, output, session, metaFileOut, metaSep, geneFileOut, geneSep, treeFileOutTips){
  ns <- session$ns
  
  #this will render the output from the sanity function found in the golem_utils_server.R file
  #and takes in 5 reactive files - tree, genetic distance, genetic distance seperator, meta data, and meta data seperartor
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
  
  mFileConversion <- function(impMeta, metSep){
    mFile <- fileCheck(fileUp = impMeta, fileType = metSep, fileSep = metSep)
    
    meta2 <-mFile %>%
      tibble::column_to_rownames(var = "Display.labels")%>% 
      dplyr::select(-Tip.labels) #do not include the column of 'ugly' tip labels
  }
  
  
  
  notColumns <- function (file){
    colFile<- ncol(file)
    
    print(colFile)
    
    if(colFile < 1 ){
      return("Looks like there is not a column for matrix plotting")
    } else {
      return("Can add a matrix")
    }
  }
  
  
  output$fileChecking2 <- renderUI({
    ns <- session$ns
    if (is.null(metaFileOut())) { 
      return(NULL)
    } else {
  #  mFileOut <- reactive({
    #mFileConversion(impMeta = metaFileOut(),metSep = metaSep() ) 
    mFile <- mFileConversion(impMeta = metaFileOut(), metSep = metaSep() ) 
    validate(notColumns(file = mFile))}
  })
#  })
  
  
}

## To be copied in the UI
# mod_tipCheck_ui("tipCheck_ui_1")

## To be copied in the server
# callModule(mod_tipCheck_server, "tipCheck_ui_1")

