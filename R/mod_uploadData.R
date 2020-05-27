#' uploadData UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd 
#'
#' @importFrom shiny NS tagList 
mod_uploadData_ui <- function(id){
  ns <- NS(id)
  tagList(
    
    fileInput(ns("treeFile"), label ="1. Upload a newick file, please"),
    
    # Input: Select a file ----
    fileInput(ns("geneFile"), 
              label = "2. Upload a genetic distance file ",     #label here is specified and is called in the app_ui with the tags$div section 
              multiple = FALSE,                                 #does not all multiple files to be uploaded
              accept = c("text/csv",                            #accept - this bypasses the  need to do validation as in the web brower only the files with these extensions are selectable
                         "text/comma-separated-values,text/plain",
                         ".csv",
                         ".tsv")),
    
    # Input: Select separator ----
    radioButtons(ns("geneSep"), "Separator for genetic data",
                 choices = c(Comma = ",",
                             Tab = "\t"),
                 selected = "\t"),
    
    # Input: Select a file ----
    fileInput(ns("metaFile"), 
              label= "3. Upload an optional meta data file",     #label here is specified and is called in the app_ui with the tags$div section 
              multiple = FALSE,                                  #does not all multiple files to be uploaded
              accept = c("text/csv",                             #accept - this bypasses theneed to do validation as in the web brower only the files with these extensions are selectable
                         "text/comma-separated-values,text/plain",
                         ".csv",
                         ".tsv")),
    
    # Input: Select separator ----
    radioButtons(ns("metaSep"), "Separator for meta data",
                 choices = c(Comma = ",",
                             Tab = "\t"),
                 selected = "\t")
  )
}


#' uploadData Server Function
#'
#' @noRd 
mod_uploadData_server <- function(input, output, session){
  ns <- session$ns
  
  # #function to read data in either genetic distance matrix or meta data file 
  readData<-function(filepath, sep)
  {readr::read_delim(filepath,
                     sep,
                     trim_ws = T,
                     skip_empty_rows = T,
                     col_names = T,
                     col_types = readr::cols(.default = readr::col_character())
  )
    }

  # 
  # readData <- function(filepath, sep){
  #   read.delim(filepath,
  #              sep,
  #              strip.white = T,
  #              blank.lines.skip=T,
  #              header = F,
  #              stringsAsFactors = F)
  # }
    
  #reactive expression that until a file is uploaded, the below message is displayed
  geneFileUp <- reactive({
    validate(need(input$geneFile !="", "Please import a genetic distance file to use the clade annotator"))
    input$geneFile
  }) 
  
  # geneFileUpRename <- reactive({
  #  g <-geneFileUp()
  #  names(g)<-g[1,]
  # })
  # 
  #reactive expression that holds the meta data file
  metaFileUp <- reactive({
   input$metaFile
  })

  # metaFileUpRename <-reactive({
  #   m <- metaFileUp()
  #   names(m)<-m[1,]
  #   return(m)
  #   })
  # 
  # metaFileUpRename2 <- reactive({
  #   m2 <-metaFileUpRename()
  #   m2 <- m2 %>% dplyr::slice(-1)
  #   return(m2)
  # })
  
  # metaFileUpRename2<-reactive({
  #   m2<-metaFileUpRename()
  #   m3 <-dplyr::slice(m2, -1)
  #   return(m3)
  # })
  #  
  #reactive expression that uploads the newick tree and allows the optional upload of meta data to correct tree tip labels 
  treeFileUp <- reactive({
    
    validate(need(input$treeFile !="", "Please import newick tree file"))
    req(input$treeFile)
    
    if (is.null(metaFileUp()$datapath)) {
      treeio::read.newick(input$treeFile$datapath)
    } else {
      
      dataFile <- readData(filepath=metaFileUp()$datapath,
                           sep =  input$metaSep)
      
      treeio::read.newick(input$treeFile$datapath)%>%
        phylotools::sub.taxa.label(., as.data.frame(dataFile))
    }
  })
  
  geneFileCorOrUn <- reactive({ 
    if (is.null(metaFileUp()$datapath)) {
      
      geneFileUncorrected <- readData(filepath = geneFileUp()$datapath,
                                      sep =  input$geneSep)
    } 
    else {
      
      metaFileComb <- readData(filepath = metaFileUp()$datapath,
                               sep =  input$metaSep)
      
      geneFileCorrected <- readData(filepath = geneFileUp()$datapath,
                                    sep =  input$geneSep)
      
      colnames(geneFileCorrected)[2:ncol(geneFileCorrected)] = metaFileComb$Display.labels[which(metaFileComb$Tip.labels %in% colnames(geneFileCorrected)[2:ncol(geneFileCorrected)])]
      
      geneFileCorrected$. = metaFileComb$Display.labels[which(metaFileComb$Tip.labels %in% geneFileCorrected$.)]
      
      return(geneFileCorrected)
    }
  })
  
  #return these reactive objects to be used in tree display module 
  return(
    list(
      treeFileOut = reactive(treeFileUp()),
      geneFileCorOrUnOut = reactive(geneFileCorOrUn())
    ))
}

## To be copied in the UI
# mod_uploadData_ui("uploadData_ui_1")

## To be copied in the server
# callModule(mod_uploadData_server, "uploadData_ui_1")

