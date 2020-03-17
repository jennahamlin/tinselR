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
                                   treeFile,dataFileCleaned, treeformat, align, font, numscale, node){
  ns <- session$ns
  
  #convert phylogenetic tree to tibble to join tree and genetic distance matrix
  treeObject<-reactive({
    tibble::as_tibble(treeFile()) 
  }) 
  
  #change column1, row1 to the id of label and replace - with a 0 within the file
  combTandG <- reactive({
    dplyr::rename(dataFileCleaned(), label = 1)%>%  
      replace(., .=="-", 0) 
  })
  
  #join the treeobject and updated genetic distance file by label and convert to s4 object
  gandTS4 <- reactive({
    dplyr::full_join(treeObject(), combTandG(), by = "label")%>% 
      treeio::as.treedata() 
  })
  
  #major plotting reactive using an S4 object
  make_tree <- reactive({
    ggtree::ggtree(gandTS4(), layout = treeformat())+
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
  
  gandT <-reactive({
    dataWithSelection()%>%
      dplyr::mutate_if(is.numeric,as.character, is.factor, as.character) %>%
      na.omit() %>%
      dplyr::select(-c(parent, node, branch.length, isTip, x, y, branch, angle))%>%
      tidyr::pivot_longer(-label)
  })
  
  output$selectedIndivs <- renderTable({ #renderTable makes a table of values - can this be accessed 
    gandT()
    #ifelse(dataWithSelection()$isTip == TRUE, dataWithSelection()$label, "") 
  })
  
  return(make_tree)
}

## To be copied in the UI
# mod_displayTree_ui("displayTree_ui_1")

## To be copied in the server
# callModule(mod_displayTree_server, "displayTree_ui_1"