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
    tableOutput(ns("selectedIndivs")),
    tableOutput(ns("selectedIndivsSNPs")) #this displays the brushed tips
  )
}

# Module Server

#' @rdname mod_combineTandG
#' @export
#' @keywords internal

mod_combineTandG_server <- function(input, output, session){
  ns <- session$ns
 
  # #displays the output from brushed points - makeplot is of class "ggtree" "gg" and "ggplot"
  # output$selectedIndivs<-renderTable(
  #   ifelse(dataWithSelection2()$isTip == TRUE, dataWithSelection2()$label, "")
  #   #,
  #   #caption="Above are the individuals that you selected. Would you like to annotate?"
  # )
  # 
  # #converts the brushed points data into a long data table - displays all possible combinations
  # gandT <-reactive({
  #   dataWithSelection2()%>%
  #     na.omit() %>%
  #     dplyr::select(-c(parent, node, branch.length, isTip, x, y, branch, angle))%>%
  #     tidyr::pivot_longer(-label)
  # })
  # 
  # gandTreduced <- reactive({
  #   gandT()%>%
  #     dplyr::filter(label == dataWithSelection2()$label[1] & name == dataWithSelection2()$label[2])%>%
  #     dplyr::pull(value)
  # })
  # 
  # output$selectedIndivsSNPs <- renderTable({ #renderTable makes a table of values
  #   gandTreduced()
  # })
  # 
  # #return(dataWithSelection)
  # return(make_tree)
  
}

## To be copied in the UI
# mod_combineTandG_ui("combineTandG_ui_1")

## To be copied in the server
# callModule(mod_combineTandG_server, "combineTandG_ui_1")
