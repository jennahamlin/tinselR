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
                  choices = c("example Tree 1", "example Tree 2")),
      
      #upload genetic distance file using a function
      selectInput(ns("exGeneFile"), label = "2. Selected assocaited genetic distance file",
                  choices =c("", "example Genetic Distance 1", "example Genetic Distance 2")),

      # div(style = "margin-top:-1em", #this decreases the distance between the two buttons (fileupload and inputseparator)
      #     #specify the type of separator for the genetic distance file uploaded
      #     inputSeparator(ns("exGeneSep"), fileLabel = "Separator for genetic distance file")),
      # 
       selectInput(ns("exMetaFile"), label = "3. Select associated (optional) meta data file",
                   choices = c("",  "example Meta Data 1", "example Meta Data 2")),
       
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
  
  #reactive expression that until a file is uploaded, the below message is displayed if attempting to use the clade annotator button
  exGeneFileUp <- reactive({
    if(input$exGeneFile == "example Genetic Distance 1"){
      Tinsel::gene1}
    else if (input$exGeneFile == "example Genetic Distance 2") {
      Tinsel::gene2}
  })

  exMetaFileUp <- eventReactive(input$exMetaFile, {
    if(input$exMetaFile == "example Meta Data 1"){
      Tinsel::meta1}
    else if (input$exMetaFile == "example Meta Data 2") {
      Tinsel::meta2}
  })
  
  
  ##############
  ### TREES ###
  ##############
  
  ## load in tree based on selected example tree
  exTreeFileUp <- eventReactive(input$exTreeFile, {
    if(input$exTreeFile == "example Tree 1"){
      Tinsel::tree1
    }
    else if (input$exTreeFile == "example Tree 2") {
      Tinsel::tree2}}
  )
  

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
  #reactive expression that uploads the newick tree and allows the optional upload of meta data to correct tree tip labels
  #this also performs a check to see if the correct delimitor is selected before reading in the file and provides users with error output
  exTreeFileUp2 <- reactive({

    #validate(need(input$exTreeFile !="", "Please import newick tree file"))
    #req(input$exTreeFile)

    if (is.null(exMetaFileUp())) { #if no meta file still upload the tree
      exTreeFileUp()
      #treeio::read.newick(input$exTreeFile$datapath)
    }
    else { #if metafile do an error check and then correct tip labels using phylotools sub.taxa.label function

      #exMetaFileSeperate <- fileCheck(fileUp = exMetaFileUp(), fileType = exMetaFileType(), fileSep = input$exMetaSep)

      #treeio::read.newick(input$exTreeFile$datapath)%>%
      exTreeFileUp()%>% 
        phylotools::sub.taxa.label(., as.data.frame(exMetaFileUp())) #this line converts tip labels to pretty labels based on user upload of meta data file
    }
  })
  
  return(
    list(
      extreeFileOut = reactive(exTreeFileUp2())))
  
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

