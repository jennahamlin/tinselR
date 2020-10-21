#' tipCheck UI Function
#'
#' @title mod_tipCheck_ui mod_tipCheck_server
#' 
#' @description A shiny Module. This module generates the landing page for the 
#' application and provides a link to the github repo where users may file
#' an issue
#'
#' @rdname mod_tipCheck
#'
#' @param id,input,output,session internal
#' @param tFileOut imported tree for tip checking
#' @param gFileOut imported genetic distance file for tip checking
#' @param metaFileOut imported meta file for confirmation that file is uploaded
#' @param mFileOut imported meta data file for tip checking
#'
#' @importFrom shiny NS tagList 
mod_tipCheck_ui <- function(id){
  ns <- NS(id)
  tagList(
    
    tags$table(width ="100%",
               tags$th("File Check Output", colspan="3",
                       style="font-size:20px; color:#444444;")),
    
    # this output info about if tip labels are not concordant between all 
    #three files
    htmlOutput(ns('fileChecking')), 
    
    # this one will tell user if they can add a matrix to the tree 
    uiOutput(ns('fileChecking2')), 
    
    tags$hr(style="border-color: #99b6d8;")
  )
  
}

#' tipCheck Server Function
#'
#' @rdname mod_tipCheck
#' 
mod_tipCheck_server <- function(input, output, session, mFileOut,
                                metaFileOut, gFileOut, tFileOut){
  ns <- session$ns
  
  tFileOut <- NULL
  
  #this will render the output from the sanity function found in the 
  #golem_utils_server.R file and takes in 5 reactive files - 
  #tree, genetic distance, genetic distance separator, meta data, and meta data 
  #separator
  
  output$fileChecking <- renderUI({
    ns <- session$ns
    
    if (is.null(tFileOut())) { 
      return(HTML('<span style="color:gray">Please upload a tree file</span>'))
    } else if (is.null(gFileOut())){
      return(HTML(
        '<span style="color:gray">Please upload a genetic distance file</span>')
      )
    } else if (is.null(metaFileOut())){
      return(HTML(
        '<span style="color:gray">Please upload a meta data file</span>')
      )
    } else {
      sanity(
        tFile = tFileOut(),
        gFile = gFileOut(), 
        mFile = mFileOut()
      )
    } 
  })
  
  #displays number of columns that are available for adding a matrix to the tree
  output$fileChecking2 <- renderUI({
    ns <- session$ns
    if (is.null(mFileOut() )) {
      return(NULL)
    } else {
      validate(notColumns(  mFileConversion(mFileOut() ) ) )
    }
  })
  
}

## To be copied in the UI
# mod_tipCheck_ui("tipCheck_ui_1")

## To be copied in the server
# callModule(mod_tipCheck_server, "tipCheck_ui_1")

