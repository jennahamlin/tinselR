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

mod_displayTree_server <- function(input, output, session, mFileOut, 
                                   treeFileOut, geneObjectOutForS4, align,
                                   treeformat, font,  numscale, node, lim, bootPos, midP, matOff){
  ns <- session$ns
  
  #midpoint root the tree based on reactive value, if not just display the tree
  midTree <- reactive({
    if(midP() == TRUE) {
      return(phytools::midpoint.root(treeFileOut()))
    }
    else {
      return(treeFileOut())
    }
  })
  
  #convert phylogenetic tree (midpoint rooted or not) to tibble to join tree and genetic distance matrix
  treeObject<-reactive({
    tibble::as_tibble(midTree())
  })
  
  #join the treeobject and updated genetic distance file by label and convert to s4 object
  gandTS4 <- reactive({
    combineGandT(treeObject(), geneObjectOutForS4())
  })
  
  
  mFile <- reactive({
    if(!is.null(mFileOut())){
      mFileConversion(mFile = mFileOut() )
      print("Line 58 Display Tree")
      print( mFileConversion(mFile = mFileOut() ))
    } else {
      #skip
    }
  })
  
  ########additional reactive tree parameters to possibly include
  #these could be parameters to increase/decrease how far tree fills
  #to the margins 
  #ggplot2::theme(plot.margin=ggplot2::margin(10,10,10,10))
  
  ## displayTree server function. In theory this function should be able to be moved over to the golem_utils_server.R script
  # and updating the function to take all of the reactive values but I have been unsuccessful at doing that and
  # get a strange error `Warning: Error in modifyList: is.list(val) is not TRUE` so leaving here for now
  treePlot <- function(inputFile){
    label <- NULL
    print("L 74 Display Tree")
    if(is.null(mFileOut())){
      print("L 76 display Tree")
    ggtree::ggtree(inputFile, layout = treeformat())+
      ggplot2::xlim(NA, lim())+
      ggtree::geom_tiplab(align = align(), fontface = font(), family="Helvetica", size = 3)+
      ggtree::geom_treescale(width = numscale(), x = 0.005, y = -3 )+
      ggtree::geom_text2(ggplot2::aes(label=label, subset = !is.na(as.numeric(label)) & as.numeric(label) > node()), nudge_x = bootPos()) 
    } else {
      ggtree::ggtree(inputFile, layout = treeformat())+
        ggplot2::xlim(NA, lim())+
        ggtree::geom_tiplab(align = align(), fontface = font(), family="Helvetica", size = 3)+
        ggtree::geom_treescale(width = numscale(), x = 0.005, y = -3 )+
        ggtree::geom_text2(ggplot2::aes(label=label, subset = !is.na(as.numeric(label)) & as.numeric(label) > node()), nudge_x = bootPos())+
        ggtree::gheatmap(inputFile,
                         mFile(),
                         offset = matOff(),
                         width = 0.2,
                         colnames_angle = 45,
                         colnames_offset_y = -1,
                         hjust = 0.5)
      }
    } 

  
  #major plotting reactive using an S4 object called above (gandTS4) or the base midTree reactive made
  #from import of treeFileOut and the  Upload data module 
  makeTree <- reactive({
    
    if(is.null(input$gandTS4)){ # this disconnects the need for genetic distance file to be uploaded for the tree viz to happen
      treePlot(midTree())
      #what the call should look like if treePlot function was in the golem_utils_server.R file
      #treePlot(inputFile = midTree(), align = align(), layout = treeformat(), fontface = font(), width = numscale(), node = node(), limit = lim(), nudge_x = bootPos())
      
    } 
    else{
      treePlot(gandTS4())
      #what the call should look like if treePlot function was in the golem_utils_server.R file
      #treePlot(inputFile = gandTS4(), align = align(), layout = treeformat(), fontface = font(), width = numscale(), node = node(), limit = lim(), nudge_x = bootPos())
    }
  })
  
  #return display tree reactive to be used in cladeAnnotator module 
  return(
    list(
      makeTreeOut = reactive(makeTree())
    ))
}

## To be copied in the UI
# mod_displayTree_ui("displayTree_ui_1")

## To be copied in the server
# callModule(mod_displayTree_server, "displayTree_ui_1"