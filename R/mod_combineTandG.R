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
    plotOutput(ns("treeDisplay"), brush =ns("plot_brush")),
    tableOutput(ns("selected")),
    tableOutput(ns("selectedIndivs")) #this displays the brushed tips
    
  )
}

# Module Server

#' @rdname mod_combineTandG
#' @export
#' @keywords internal

mod_combineTandG_server <- function(input, output, session, make_tree){
  ns <- session$ns
  
  output$treeDisplay <- renderPlot({
    make_tree()
  })
  
  dataWithSelection <- reactive({
    brushedPoints(make_tree()$data, input$plot_brush)
  })
  
  output$selected<-renderTable(dataWithSelection())
  
  
  gandT <-reactive({
    dataWithSelection()%>%
      dplyr::mutate_if(is.numeric,as.character, is.factor, as.character) %>%
      na.omit() %>%
      dplyr::select(-c(parent, node, branch.length, isTip, x, y, branch, angle))%>%
      tidyr::pivot_longer(-label)%>%
      dplyr::filter()
  })
  
  output$selectedIndivs <- renderTable({ #renderTable makes a table of values - can this be accessed 
    gandT()
    #ifelse(dataWithSelection()$isTip == TRUE, dataWithSelection()$label, "") 
  })
  
}

## To be copied in the UI
# mod_combineTandG_ui("combineTandG_ui_1")

## To be copied in the server
# callModule(mod_combineTandG_server, "combineTandG_ui_1")
