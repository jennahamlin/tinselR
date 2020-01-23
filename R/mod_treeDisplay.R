# Module UI
  
#' @title   mod_treeDisplay_ui and mod_treeDisplay_server
#' @description  A shiny Module.
#'
#' @param id shiny id
#' @param input internal
#' @param output internal
#' @param session internal
#'
#' @rdname mod_treeDisplay
#'
#' @keywords internal
#' @export 
#' @importFrom shiny NS tagList 
mod_treeDisplay_ui <- function(id, name){
  # id refers to the module ('treeDisplay_ui_1'); name refers to the name of the tabpanel ('Phylogenetic tree')
  ns <- NS(id)
  tabPanel(
<<<<<<< HEAD
    name, 
    
        plotOutput(ns("tree"))
  )
=======
    name, plotOutput(ns("tree")) 
    )                            
>>>>>>> 30e71c60263888ebdad7ec6bf92800be6dc53b7d
}
    
# Module Server
    
#' @rdname mod_treeDisplay
#' @export
#' @keywords internal
    
mod_treeDisplay_server <- function(input, output, session, file){
  ns <- session$ns

    output$tree<-renderPlot({
      ape::plot.phylo(file())
      })
    
}
    
## To be copied in the UI
# mod_treeDisplay_ui("treeDisplay_ui_1")
    
## To be copied in the server
# callModule(mod_treeDisplay_server, "treeDisplay_ui_1")
 
