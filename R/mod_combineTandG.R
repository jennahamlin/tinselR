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
  
  #makes the tree plot, uses output from the displayTree module 
  output$treeDisplay <- renderPlot({
    make_tree()
  })
  
  #reactive that holds the brushed points on a plot
  dataWithSelection <- reactive({
    brushedPoints(make_tree()$data, input$plot_brush)
  })
  
  #displays the output from brushed points - makeplot is of class "ggtree" "gg" and "ggplot"
  output$selected<-renderTable(dataWithSelection())
  
  #converts the brushed points data into a long data table - displays all possible combinations 
  gandT <-reactive({
    dataWithSelection()%>%
      dplyr::mutate_if(is.numeric,as.character, is.factor, as.character) %>%
      na.omit() %>%
      dplyr::select(-c(parent, node, branch.length, isTip, x, y, branch, angle))%>%
      tidyr::pivot_longer(-label)
  })
  
  gandTreduced <- reactive({
    gandT()%>%
      dplyr::filter(label[1] & name[2])
  })


  output$selectedIndivs <- renderTable({ #renderTable makes a table of values - can this be accessed 
    gandT()
    #gandTreduced()
    #ifelse(dataWithSelection()$isTip == TRUE, dataWithSelection()$label, "") 
  })
  
}

## To be copied in the UI
# mod_combineTandG_ui("combineTandG_ui_1")

## To be copied in the server
# callModule(mod_combineTandG_server, "combineTandG_ui_1")
