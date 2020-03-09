# Module UI

#' @title   mod_combineTandG_ui and mod_combineTandG_server
#' @description  A shiny Module.
#'
#' @importFrom magrittr %>%
#' @name %>%
#' @param id shiny id
#' @param input internal
#' @param output internal
#' @param session internal
#'
#' @rdname mod_combineTandG
#'
#' @keywords internal
#' @export 
#' @importFrom shiny NS tagList 
mod_combineTandG_ui <- function(id){
  ns <- NS(id)
  tagList(
    verbatimTextOutput(ns("combinedTandG"))
  )
}

# Module Server

#' @rdname mod_combineTandG
#' @export
#' @keywords internal

mod_combineTandG_server <- function(input, output, session, treeFile, dataFileCleaned){
  ns <- session$ns
  
  treeObject<-reactive({treeio::as.treedata(treeFile())}) #this is an S4 object
  
  treeTibble <- reactive({tibble::as_tibble(treeObject())}) #convert to tibble to access 
  
  dataFileUpdate <- reactive({dplyr::rename(dataFileCleaned(), label = 1)}) #change column header to label in order to use full_join
  
  tibbleTandG<-reactive({dplyr::full_join(treeTibble(), dataFileUpdate(), by = "label")})
  
  tibbleTandG2 <-reactive({tibbleTandG()[complete.cases(tibbleTandG()),]})
  
  tibbleTandG3 <-reactive({replace(tibbleTandG2(), tibbleTandG2()=="-",0) })
  
  tibbleTandG4 <- reactive({
    tibbleTandG3() %>% 
      dplyr::select(-c(parent, node, branch.length))%>% 
      tidyr::pivot_longer(-label)
    
  })
  
  output$combinedTandG <- renderPrint({
    tibbleTandG4()
  })
  
  #tibbleTandGSelected <- reactive({dplyr::filter(tibbleTandG(), unlist(dataWithSelection()$tip.label))})
  }

## To be copied in the UI
# mod_combineTandG_ui("combineTandG_ui_1")

## To be copied in the server
# callModule(mod_combineTandG_server, "combineTandG_ui_1")
