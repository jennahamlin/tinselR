# Module UI

#' @title   mod_displayTree_ui and mod_displayTree_server
#' @description  A shiny Module.
#'
#' @param id shiny id
#' @param input internal
#' @param output internal
#' @param session internal
#'
#' @rdname mod_displayTree
#'
#' @keywords internal
#' @export 
#' @importFrom shiny NS tagList 
mod_displayTree_ui <- function(id){
  ns <- NS(id)
  tagList(
    plotOutput(ns("treeDisplay"), brush =ns("plot_brush"))
    , #this displays the tree and allows one to brush tips
    tableOutput(ns("selectedIndivs")) #this displays the brushed tips
  )
}

# Module Server

#' @rdname mod_displayTree
#' @export
#' @keywords internal

mod_displayTree_server <- function(input, output, session, 
                                   treeFile,dataFile, treeformat, align, font, numscale, node){
  ns <- session$ns
  
   treeObject<-reactive({
       tibble::as_tibble(treeFile()) #convert to tibble to access and join tree and genetic distance matrix
   }) 
   
   combTandG <- reactive({
     dplyr::rename(dataFile(), label = 1)%>%  #change column1, row1 to the id of label 
       replace(., .=="-", 0) #replace - with a 0 within the file
   })
   
   gandTS4 <- reactive({
     dplyr::full_join(treeObject(), combTandG(), by = "label")%>% #join the treeobjectt and updated genetic distance file by label
       treeio::as.treedata() #convert to s4 object
   })
   
  make_tree <- reactive({
    ggtree::ggtree(gandTS4(), layout = treeformat())+ #major plotting reactive
      ggtree::geom_tiplab(align = align(), fontface = font(), family="Helvetica") + 
      ggtree::geom_treescale(width = numscale())+
      ggtree::geom_text2(ggplot2::aes(label=label, subset=!is.na(as.numeric(label)) & label >node()), nudge_x = 0.0002)
  })
  
  output$treeDisplay <- renderPlot({
    make_tree()
  })
  
   dataWithSelection <- reactive({
     brushedPoints(make_tree()$data, input$plot_brush)
   })
  
  output$selectedIndivs <- renderTable({ #renderTable makes a table of values - can this be accessed 
    ifelse(dataWithSelection()$isTip == TRUE, dataWithSelection()$label, "") 
  })
  
  # gandT <-reactive({
  #   make_tree()$data %>%
  #     dplyr::mutate_if(is.numeric,as.character, is.factor, as.character) %>%
  #     na.omit() %>%
  #     dplyr::select(-c(parent, node, branch.length, isTip, x, y, branch, angle))%>%
  #     tidyr::pivot_longer(-label)
  # })
  
  return(make_tree)
}

## To be copied in the UI
# mod_displayTree_ui("displayTree_ui_1")

## To be copied in the server
# callModule(mod_displayTree_server, "displayTree_ui_1"