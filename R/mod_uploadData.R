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
    
    
    # #this function does not work yet, error is "as.character: cannot coerce type 'closure' to vector of type 'character'
    # fileUpload<-function(id, label){
    #   fileInput(ns(id),
    #             label = label,
    #            multiple = FALSE,                                 #does not all multiple files to be uploaded
    #           accept = c("text/csv",                            #accept - this bypasses the  need to do validation as in the web brower only the files with these extensions are selectable
    #                     "text/comma-separated-values,text/plain",
    #                    ".csv",
    #                   ".tsv"))
    # },
    # 
    # 
    # fileUpload(id = "geneFile", label = "label"),


    
    fileInput(ns("treeFile"), label ="1. Upload a newick file, please"),

    # Input: Select a file ----
    fileInput(ns("geneFile"),
              label = "2. Upload a genetic distance file ",     #label here is specified and is called in the app_ui with the tags$div section
              multiple = FALSE,                                 #does not all multiple files to be uploaded
              accept = c("text/csv",                            #accept - this bypasses the  need to do validation as in the web brower only the files with these extensions are selectable
                         "text/comma-separated-values,text/plain",
                         ".csv",
                         ".tsv")),
    
    # inputSeparator <- function(id, label){
    #   radioButtons(ns(id), 
    #                label,  
    #                choices = c(Comma = ",",
    #                            Tab = "\t"),
    #                selected = "\t")
    # },
    # 
    # inputSeparator("geneSep", label = "Separator for genetic distance file"),
    
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

  
  # geneFileUpRename <- reactive({
  #  g <-geneFileUp()
  #  names(g)<-g[1,]
  # })
  # 
  # 
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
  
  # #function to read data in either genetic distance matrix or meta data file 
  # readData<-function(filepath, sep)
  # {readr::read_delim(filepath,
  #                    sep,
  #                    trim_ws = T,
  #                    skip_empty_rows = T,
  #                    col_names = T,
  #                    col_types = readr::cols(.default = readr::col_character())
  # )
  # }
  
  #reactive expression that until a file is uploaded, the below message is displayed if attempting to use the clade annotator button
  geneFileUp <- reactive({
    validate(need(input$geneFile !="", "Please import a genetic distance file to use the clade annotator"))
    input$geneFile
  })
  
  #reactive which holds the input selected for delimiter for gene file - this will check for correctly selected delimitor
  geneFileType <- eventReactive(input$geneSep, {
    if(input$geneSep == "\t")
    {
      return("\t")
    }
    else (input$geneSep == ",")
    {
      return(",")
    }
  })
  
  #reactive expression that holds the meta data file, as this is optional not necessary to validate with informative text display
  metaFileUp <- reactive({
    input$metaFile
  })
  
  #reactive which holds the input selected for delimiter for meta file
  metaFileType <- eventReactive(input$metaSep, {
    if(input$metaSep == "\t")
    {
      return("\t")
    }
    else (input$metaSep == ",")
    {
      return(",")
    }
  })
  
  #reactive expression that uploads the newick tree and allows the optional upload of meta data to correct tree tip labels
  #this also performs a check to see if the correct delimitor is selected before reading in the file and provides users with error output
  treeFileUp <- reactive({
    
    validate(need(input$treeFile !="", "Please import newick tree file"))
    req(input$treeFile)
    
    if (is.null(metaFileUp()$datapath)) { #if no meta file still upload the tree
      treeio::read.newick(input$treeFile$datapath)
    } 
    else { #if metafile do an error check and then correct tip labels using phylotools sub.taxa.label function
      
      metaFileSeperate <- fileCheck(FileUp = metaFileUp(), FileType = metaFileType(), FileSep = input$metaSep) 
      
      treeio::read.newick(input$treeFile$datapath)%>%
        phylotools::sub.taxa.label(., as.data.frame(metaFileSeperate))
    }
  })
  
  #reactive expression that uploads the genetic distance file and allows the optional upload of meta data to correct tree tip labels
  #this also performs a check to see if the correct delimitor is selected before reading in the files and provides users with error output
  
  geneFileCorOrUn <- reactive({ 
    if (is.null(metaFileUp()$datapath)) { #if no meta file, error check delimitor choosen for genetic distance file uploaded to be able to use clade annotator function
      
      geneFileUncorrected <- fileCheck(FileUp = geneFileUp(), FileType = geneFileType(), FileSep = input$geneSep)
    } 
    
    else { #if meta file uploaded do an error check, then do an error check for genetic distance and then correct the distance file to match meta file tip labels
      
      metaFileComb <- fileCheck(FileUp = metaFileUp(), FileType = metaFileType(), FileSep = input$metaSep) 
      
      geneFileCorrected <- fileCheck(FileUp = geneFileUp(), FileType = geneFileType(), FileSep = input$geneSep)
      
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

