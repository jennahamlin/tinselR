<<<<<<< HEAD
# Module UI

#' @title   mod_displayTree_ui and mod_displayTree_server
#' @description  A shiny Module. This module combines the tree with genetic distance as an S4 object that allows tree plotting and accessing the combined data. 
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
  )
  
}

# Module Server

#' @rdname mod_displayTree
#' @export
#' @keywords internal

mod_displayTree_server <- function(input, output, session, 
                                   treeFile, dataFile, treeformat, lim, align, font, numscale, node){
  ns <- session$ns
  
  #convert phylogenetic tree to tibble to join tree and genetic distance matrix
  treeObject<-reactive({
    tibble::as_tibble(treeFile()) 
  }) 
  
  #change column1, row1 to the id of label and replace - with a 0 within the file
  combTandG <- reactive({
    dplyr::rename(dataFile(), label = 1)%>%  
      replace(., .=="-", 0) 
  })
  
  #join the treeobject and updated genetic distance file by label and convert to s4 object
  gandTS4 <- reactive({
    dplyr::full_join(treeObject(), combTandG(), by = "label")%>% 
      treeio::as.treedata() 
  })
  
  #major plotting reactive using an S4 object called above (gandTS4)
  make_tree <- reactive({
    ggtree::ggtree(gandTS4(), layout = treeformat())+
      ggplot2::xlim(NA, lim())+
      ggtree::geom_tiplab(align = align(), fontface = font(), family="Helvetica") + 
      ggtree::geom_treescale(width = numscale())+
      ggtree::geom_text2(ggplot2::aes(label=label, subset=!is.na(as.numeric(label)) & label >node()), nudge_x = 0.0002)
  })
  
  return(make_tree)
}

## To be copied in the UI
# mod_displayTree_ui("displayTree_ui_1")

## To be copied in the server
=======
# Module UI

#' @title   mod_displayTree_ui and mod_displayTree_server
#' @description  A shiny Module. This module combines the tree with genetic distance as an S4 object that allows tree plotting and accessing the combined data. 
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
  )
  
}

# Module Server

#' @rdname mod_displayTree
#' @export
#' @keywords internal

mod_displayTree_server <- function(input, output, session, 
                                   treeFile, dataFile, treeformat, lim, align, font, numscale, node){
  ns <- session$ns
  
  #convert phylogenetic tree to tibble to join tree and genetic distance matrix
  treeObject<-reactive({
    tibble::as_tibble(treeFile()) 
  }) 
  
  #change column1, row1 to the id of label and replace - with a 0 within the file
  combTandG <- reactive({
    dplyr::rename(dataFile(), label = 1)%>%  
      replace(., .=="-", 0) 
  })
  
  #join the treeobject and updated genetic distance file by label and convert to s4 object
  gandTS4 <- reactive({
    dplyr::full_join(treeObject(), combTandG(), by = "label")%>% 
      treeio::as.treedata() 
  })
  
  #major plotting reactive using an S4 object called above (gandTS4)
  make_tree <- reactive({
    ggtree::ggtree(gandTS4(), layout = treeformat())+
      ggplot2::xlim(NA, lim())+
      ggtree::geom_tiplab(align = align(), fontface = font(), family="Helvetica") + 
      ggtree::geom_treescale(width = numscale())+
      ggtree::geom_text2(ggplot2::aes(label=label, subset=!is.na(as.numeric(label)) & label >node()), nudge_x = 0.0002)
  })
  
  return(make_tree)
  
  # observeEvent(input$add_tree, {
  #   output$treeDisplay <- renderPlot({
  #     make_tree()
  #   })
  # }
  # )
  
}

## To be copied in the UI
# mod_displayTree_ui("displayTree_ui_1")

## To be copied in the server
>>>>>>> 13d0bd1d1bbae398d0b41cb91af7f1338170f51f
# callModule(mod_displayTree_server, "displayTree_ui_1"