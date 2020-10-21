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
                            dataDisplay$meta_file_out, dataDisplay$m_file_out,
                            dataDisplay$gene_file_out, dataDisplay$g_file_out, 
                            dataDisplay$t_file_out)
  
  #module which holds the tree viz parameters and referenced with params$
  params <- callModule(mod_paramsTree_server, "paramsTree_ui_data")
  
  #module which will relaunch the entire application
  callModule(mod_relaunchApp_server, "relaunchApp_ui_data")
  
  #displays the tree and uses the params as input to change tree viz
  plot <- callModule(mod_displayTree_server, "displayTree_ui_data",  
                     dataDisplay$tree_file_out, dataDisplay$geneObjectOutForSNP, 
                     params$align, params$tree_format, params$font, 
                     params$num_scale, params$node, params$lim, params$boot_pos,
                     params$mid_p)
  

    
  #annotates tree with incorporated tree viz parameters and allows user to
  #add visual matrix
  treeWLayers <- callModule(mod_cladeAnnotator_server, "cladeAnnotator_ui_data",
                            dataDisplay$mFileMatOut, plot$makeTreeOut,
                            buttons$add_tree, buttons$add_anno,
                            buttons$remove_anno, buttons$add_matrix, 
                            buttons$remove_matrix, dataDisplay$geneObjectForSNP,
                            params$label_off, params$lab_color, params$mat_off)
  
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
                            exampleParams$align, exampleParams$tree_format,
                            exampleParams$font, exampleParams$num_scale,
                            exampleParams$node, exampleParams$lim,
                            exampleParams$boot_pos, exampleParams$mid_p)

  callModule(mod_cladeAnnotator_server, "cladeAnnotator_ui_example", 
             exampleData$exMetaFileOut, examplePlot$makeTreeOut,
             exampleButtons$add_tree, exampleButtons$add_anno, 
             exampleButtons$remove_anno, exampleButtons$add_matrix, 
             exampleButtons$remove_matrix, 
             exampleData$exGeneObjectForSNP, exampleParams$label_off,
             exampleParams$lab_color, exampleParams$mat_off)
  
}
