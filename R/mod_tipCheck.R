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
    uiOutput(ns('fileChecking2')), # this one will tell user if they can add a matrix to the tree 
    tags$hr(style="border-color: #99b6d8;")
  )
}

#' tipCheck Server Function
#'
#' @noRd 
mod_tipCheck_server <- function(input, output, session, mFileOut, metaFileOut, gFileOut, tFileOut){
  ns <- session$ns
  
  #this will render the output from the sanity function found in the golem_utils_server.R file
  #and takes in 5 reactive files - tree, genetic distance, genetic distance seperator, meta data, and meta data seperartor
  output$fileChecking <- renderUI({
    ns <- session$ns
    if (is.null(tFileOut())) { 
      return(HTML('<span style="color:gray">Please upload a tree file</span>'))
    } else if (is.null(gFileOut())){
      return(HTML('<span style="color:gray">Please upload a genetic distance file</span>'))
    } else if (is.null(metaFileOut())){
      return(HTML('<span style="color:gray">Please upload a meta data file</span>'))
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

