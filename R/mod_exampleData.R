#' exampleData UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd 
#'
#' @importFrom shiny NS tagList 
mod_exampleData_ui <- function(id){
  ns <- NS(id)
  tagList(
    tagList(
      
      #upload tree file 
      selectInput(ns("exTreeFile"), label ="1. Select example newick file", 
                  choices = c("example Tree 1", "example Tree 2"))
      
     
      
      
      # selectInput(ns("exTreeFile"), label ="1. Select example newick file", 
      #           choices = c( system.file("extdata", "1504MLEXH-1.dnd", package = "Tinsel"), "1509MNJJP-1_RAxML_bipartitions")), 
       
      # #upload genetic distance file using a function 
      # selectInput(ns("exGeneFile"), label = "2. Selected assocaited genetic distance file", 
      #             choices =c("1504MLEXH-1_geneticMatrix.tsv", "1509MNJJP-1_geneticMatrix.tsv")),
      # 
      # div(style = "margin-top:-1em", #this decreases the distance between the two buttons (fileupload and inputseparator)
      #     #specify the type of separator for the genetic distance file uploaded
      #     inputSeparator(ns("exGeneSep"), fileLabel = "Separator for genetic distance file")),
      # 
      # selectInput(ns("exMetaFile"), label = "3. Select associated (optional) meta data file",
      #             choices = c("1504MLEXH-1_metaData.csv", "1509MNJJP-1_metaData.tsv")),
      # 
      # div(style = "margin-top:-1em", 
      #     inputSeparator(ns("exMetaSep"), fileLabel = "Separator for optional meta data file")),
      # tags$hr(style="border-color: black;")
      
    )
  )
}

#' exampleData Server Function
#'
#' @noRd 
mod_exampleData_server <- function(input, output, session){
  ns <- session$ns
  
  ##############
  ### TREES ###
  ##############
  
  ## read in tree based on selected example tree
  
  here::set_here()
  
  exTreeFileUp <- eventReactive(input$exTreeFile, {
    if(input$exTreeFile == "example Tree 1"){
      exTreeFileUp <-  treeio::read.newick(here::here("inst", "extdata", "1504MLEXH-1.dnd"))
    }
    else if (input$exTreeFile == "example Tree 2") {
      exTreeFileUp <-  treeio::read.newick(here::here("inst", "extdata", "1509MNJJP-1_RAxML_bipartitions"))}
    }
  )
  
  return(
       list(
         extreeFileOut = reactive(exTreeFileUp())))
  
  
  
  
  
  
  
  # exTreeFileUp <- eventReactive(input$exTreeFile == "example Tree 1", {
  #     exTreeFileUp <-  treeio::read.newick(here("extdata", "inst/extdata/1509MNJJP-1_RAxML_bipartitions"))
  #   })
  #   
    # else if (input$exTreeFile == "example Tree 2") {
    #   exTreeFileUp <-  treeio::read.newick(here("extdata", "inst/extdata/1504MLEXH-1.dnd"))  }})

  
  
  
  # #reactive expression that until a file is uploaded, the below message is displayed if attempting to use the clade annotator button
  # exGeneFileUp <- reactive({
  #   validate(need(input$exGeneFile !="", "Please import a genetic distance file to use the clade annotator"))
  #   input$exGeneFile
  # })
  # 
  # #reactive which holds the input selected for delimiter for gene file using fileType function - this will check for correctly selected delimitor
  # exGeneFileType <- eventReactive(input$exGeneSep, {
  #   fileType(input$exGeneSep)
  # })
  # 
  # #reactive expression that holds the meta data file, as this is optional not necessary to validate with informative text display
  # exMetaFileUp <- reactive({
  #   input$exMetaFile
  # })
  # 
  # #reactive which holds the input selected for delimiter for meta file
  # exMetaFileType <- eventReactive(input$exMetaSep, {
  #   fileType(input$exMetaSep)
  # })
  # 
  # # #reactive expression that uploads the newick tree and allows the optional upload of meta data to correct tree tip labels
  # # #this also performs a check to see if the correct delimitor is selected before reading in the file and provides users with error output
  # # exTreeFileUp <- reactive({
  # #   
  # #   validate(need(input$exTreeFile !="", "Please import newick tree file"))
  # #   req(input$exTreeFile)
  # #   
  # #   if (is.null(exMetaFileUp()$datapath)) { #if no meta file still upload the tree
  # #     treeio::read.newick(input$exTreeFile$datapath)
  # #   } 
  # #   else { #if metafile do an error check and then correct tip labels using phylotools sub.taxa.label function
  # #     
  # #     exMetaFileSeperate <- fileCheck(fileUp = exMetaFileUp(), fileType = exMetaFileType(), fileSep = input$exMetaSep) 
  # #     
  # #     treeio::read.newick(input$exTreeFile$datapath)%>%
  # #       phylotools::sub.taxa.label(., as.data.frame(exMetaFileSeperate)) #this line converts tip labels to pretty labels based on user upload of meta data file
  # #   }
  # # })
  # 
  # #reactive expression that uploads the genetic distance file and allows the optional upload of meta data to correct tree tip labels
  # #this also performs a check to see if the correct delimitor is selected before reading in the files and provides users with error output
  # 
  # exGeneFileCorOrUn <- reactive({ 
  #   if (is.null(exMetaFileUp()$datapath)) { #if no meta file, error check delimitor choosen for genetic distance file uploaded to be able to use clade annotator function
  #     
  #     exGeneFileUncorrected <- fileCheck(fileUp = exGeneFileUp(), fileType = exGeneFileType(), fileSep = input$exGeneSep)
  #   } 
  #   
  #   else { #if meta file uploaded do an error check, then do an error check for genetic distance and then correct the distance file to match meta file tip labels
  #     
  #     . <- NULL
  #     
  #     exMetaFileComb <- fileCheck(fileUp = exMetaFileUp(), fileType = exMetaFileType(), fileSep = input$exMetaSep) 
  #     
  #     exGeneFileCorrected <- fileCheck(fileUp = exGeneFileUp(), fileType = exGeneFileType(), fileSep = input$exGeneSep)
  #     
  #     colnames(exGeneFileCorrected)[2:ncol(exGeneFileCorrected)] = exMetaFileComb$Display.labels[which(exMetaFileComb$Tip.labels %in% colnames(exGeneFileCorrected)[2:ncol(exGeneFileCorrected)])]
  #     
  #     exGeneFileCorrected$. = exMetaFileComb$Display.labels[which(exMetaFileComb$Tip.labels %in% exGeneFileCorrected$.)]
  #     
  #     return(exGeneFileCorrected)
  #   }
  # })
  # 
  # #return these reactive objects to be used in tree display module 
  # return(
  #   list(
  #     treeFileOut = reactive(exTreeFileUp()),
  #     geneFileCorOrUnOut = reactive(exGeneFileCorOrUn())
  #   ))
  
}

## To be copied in the UI
# mod_exampleData_ui("exampleData_ui_1")

## To be copied in the server
# callModule(mod_exampleData_server, "exampleData_ui_1")

