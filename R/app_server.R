#' @import shiny
app_server <- function(input, output,session) {
  # List the first level callModules here
  
  ####################
  #### USERS DATA ####
  ####################
  
  #uplodad the data (tree, genetic distance and meta data) and call that module
  #output by dataDisplay$
  dataDisplay <- callModule(mod_uploadData_server, "uploadData_ui_1")
  
  #reactive which stores the buttons a user can push
  buttons <- callModule(mod_pushButtons_server, "pushButtons_ui_data")
  
  #outputs directly a user message if the file tip labels are all the same for 
  #the three files and tells user if they can use matrix button
  tipCheckOut <- callModule(mod_tipCheck_server, "tipCheck_ui_1",
                            dataDisplay$metaFileOut, dataDisplay$mFileOut,
                            dataDisplay$geneFileOut, dataDisplay$gFileOut, 
                            dataDisplay$tFileOut)
  
  #module which holds the tree viz parameters and referenced with params$
  params <- callModule(mod_paramsTree_server, "paramsTree_ui_data")
  
  #module which will relaunch the entire application
  callModule(mod_relaunchApp_server, "relaunchApp_ui_data")
  
  #displays the tree and uses the params as input to change tree viz
  plot <- callModule(mod_displayTree_server, "displayTree_ui_data",  
                     dataDisplay$treeFileOut, dataDisplay$geneObjectOutForSNP, 
                     params$align, params$treeformat, params$font, 
                     params$numscale, params$node, params$lim, params$bootPos,
                     params$midP)
  

    
  #annotates tree with incorporated tree viz parameters and allows user to
  #add visual matrix
  treeWLayers <- callModule(mod_cladeAnnotator_server, "cladeAnnotator_ui_data",
                            dataDisplay$mFileMatOut, plot$makeTreeOut,
                            buttons$addTree, buttons$addAnno,
                            buttons$removeAnno, buttons$addMatrix, 
                            buttons$removeMatrix, dataDisplay$geneObjectForSNP,
                            params$labelOff, params$labColor, params$matOff)
  
  #allows tree with annotation and viz parameters to be donwloaded
  callModule(mod_downloadImage_server, "downloadImage_ui_1", treeWLayers)
  
  ####################
  ### EXAMPLE DATA ###
  ####################
  
  #repeated for preloaded data; no need to download so that module is not
  #included below plodad the data (tree, genetic, and meta) and call that module
  #dataDisplay. This is the only module that is re-written, the rest of the same
  #used in the above section and are called the same as above.
  
  exampleData <- callModule(mod_exampleData_server, "exampleData_ui_1")

  exampleParams <- callModule(mod_paramsTree_server, "paramsTree_ui_example")

  exampleButtons <- callModule(mod_pushButtons_server, "pushButtons_ui_example")

  callModule(mod_relaunchApp_server, "relaunchApp_ui_example")

  examplePlot <- callModule(mod_displayTree_server, "displayTree_ui_example",
                            exampleData$extreeFileOut,
                            exampleData$exGeneFileCorOrUnOut,
                            exampleParams$align, exampleParams$treeformat,
                            exampleParams$font, exampleParams$numscale,
                            exampleParams$node, exampleParams$lim,
                            exampleParams$bootPos, exampleParams$midP)

  callModule(mod_cladeAnnotator_server, "cladeAnnotator_ui_example", 
             exampleData$exMetaFileOut, examplePlot$makeTreeOut,
             exampleButtons$addTree, exampleButtons$addAnno, 
             exampleButtons$removeAnno, exampleButtons$addMatrix, 
             exampleButtons$removeMatrix, 
             exampleData$exGeneObjectForSNP, exampleParams$labelOff,
             exampleParams$labColor, exampleParams$matOff)
  
}
