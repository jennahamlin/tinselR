# Module UI

#' @title   mod_exampleData_ui and mod_exampleData_server
#' @description  A shiny Module. This module sources the pre-loaded example data (e.g. tree (x2), genetic distance file (x2), and meta data file (x2))
#'
#' @param id shiny id
#' @param input internal
#' @param output internal
#' @param session internal
#'
#' @rdname mod_exampleData
#'
#' @keywords internal
#' @export 
#' @importFrom shiny NS tagList 
mod_exampleData_ui <- function(id){
  ns <- NS(id)

  tagList(
    tags$table(width ="100%",
               tags$th("Upload Files", colspan="3", style="font-size:20px; color:#7ab567")),
    tags$hr(style="border-color: #99b6d8;"),
    
    #select tree file that is sourced using the UI function in app_ui.R file
    selectInput(ns("exTreeFile"), label = tags$b("1. Select example newick file", style="color:#afafae"), 
                choices = c("", "example Tree 1", "example Tree 2")),
    
    #select genetic distance file 
    selectInput(ns("exGeneFile"), label = tags$b("2. Selected assocaited genetic distance file", style="color:#afafae"),
                choices =c("", "example Genetic Distance 1", "example Genetic Distance 2")),
    
    #select meta data file 
    selectInput(ns("exMetaFile"), label = tags$b("3. Select associated (optional) meta data file", style="color:#afafae"),
                choices = c("",  "example Meta Data 1", "example Meta Data 2")),
    
    #add horizontal line to seperate tree viz parameters
    tags$hr(style="border-color: #99b6d8;")
      
    )

}

# Module Server

#' @rdname mod_exampleData
#' @export
#' @keywords internal
mod_exampleData_server <- function(input, output, session){
  ns <- session$ns
  
  
  ####################
  ### EXAMPLE DATA ###
  ####################
  
  #reactive expressions that loads and allows to user to select from two different datasets (e.g tree1, gene1, meta1)
  #example data are made via reading in the data, usethis::use_data(), and finally documenting the data
  #tree2 <- treeio::read.newick("C:/Users/ptx4/Desktop/Tinsel/inst/extdata/1509MNJJP-1_RAxML_bipartitions")
  #usethis::use_data(tree2, tree2)
  #no need to provide option to select input delimiter as this is done when reading in data to use with usethis::use_data()
  
  ###############
  ### GENETIC ###
  ###############
  
  exGeneFileIn <- reactive({
    if(input$exGeneFile == "example Genetic Distance 1"){
      Tinsel::gene1}
    else if (input$exGeneFile == "example Genetic Distance 2") {
      Tinsel::gene2}
  })

  ############
  ### META ###
  ############
  
  exMetaFileIn <- eventReactive(input$exMetaFile, {
    if(input$exMetaFile == "example Meta Data 1"){
      Tinsel::meta1}
    else if (input$exMetaFile == "example Meta Data 2") {
      Tinsel::meta2}
  })
  
  ##############
  ### TREES ###
  ##############
  
  exTreeFileIn <- eventReactive(input$exTreeFile, {
    if(input$exTreeFile == "example Tree 1"){
      Tinsel::tree1}
    else if (input$exTreeFile == "example Tree 2") {
      Tinsel::tree2}
    })
 
  #reactive expression that uploads the newick tree and allows the optional upload of meta data to correct tree tip labels
  exTreeFileUp <- reactive({

    validate(need(input$exTreeFile !="", "Please import newick tree file"))
    req(exTreeFileIn())

    if (is.null(exMetaFileIn())) { #if no meta file still upload the tree
      exTreeFileIn()
    }
    else { #if metafile correct tip labels using phylotools sub.taxa.label function

      exTreeFileIn()%>%
        phylotools::sub.taxa.label(., as.data.frame(exMetaFileIn())) #this line converts tip labels to pretty labels based on user upload of meta data file
    }
  })
  
  #reactive expression that uploads the genetic distance file and allows the optional upload of meta data to correct tree tip labels
  exGeneFileCorOrUn <- reactive({
    if (is.null(exMetaFileIn())) { #if no meta file, error check delimitor choosen for genetic distance file uploaded to be able to use clade annotator function
      exGeneFileIn()
    }

    else { #if meta file uploaded correct the distance file to match meta file tip labels
      . <- NULL
      exMetaFileComb <- exMetaFileIn()
      exGeneFileCorrected <- exGeneFileIn()
      colnames(exGeneFileCorrected)[2:ncol(exGeneFileCorrected)] = exMetaFileComb$Display.labels[which(exMetaFileComb$Tip.labels %in% colnames(exGeneFileCorrected)[2:ncol(exGeneFileCorrected)])]
      exGeneFileCorrected$. = exMetaFileComb$Display.labels[which(exMetaFileComb$Tip.labels %in% exGeneFileCorrected$.)]
      return(exGeneFileCorrected)
    }
  })


  #additional manipulation of genetic distance matrix for ultimately getting the mean number of SNPs 
  #geneObjectOut is a function that is applied to another function (toThreeColumns) for the reactive exGeneFileCorOrU
  exGeneObject <-reactive({
    label <- NULL
    geneObjectOut(toThreeColumns(exGeneFileCorOrUn()))
  })
 
  #return these reactive objects to be used in tree display module
  return(
    list(
      extreeFileOut = reactive(exTreeFileUp()), #for data display
      exGeneObjectOutForS4 = reactive(geneObjectOut()), #for data display
      exGeneObjectForSNP = reactive(exGeneObject()) #for clade annotator
    ))
}

## To be copied in the UI
# mod_exampleData_ui("exampleData_ui_1")

## To be copied in the server
# callModule(mod_exampleData_server, "exampleData_ui_1")

