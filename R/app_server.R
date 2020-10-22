#' @import shiny
app_server <- function(input, output, session) {
  # List the first level callModules here

  ####################
  #### USERS DATA ####
  ####################

  #uplodad the data (tree, genetic distance and meta data) and call that module
  data_display <- callModule(mod_uploadData_server, "uploadData_ui_1")

  #reactive which stores the buttons a user can push
  buttons <- callModule(mod_pushButtons_server, "pushButtons_ui_data")

  #outputs directly a user message if the file tip labels are all the same for 
  #the three files and tells user if they can use matrix button
  callModule(mod_tipCheck_server, "tipCheck_ui_1", data_display$meta_file_out, 
             data_display$m_file_out, data_display$gene_file_out,
             data_display$g_file_out, data_display$t_file_out)

  #module which holds the tree viz parameters and referenced with params$
  params <- callModule(mod_paramsTree_server, "paramsTree_ui_data")

  #module which will relaunch the entire application
  callModule(mod_relaunchApp_server, "relaunchApp_ui_data")
  
  #displays the tree and uses the params as input to change tree viz
  plot <- callModule(mod_displayTree_server, "displayTree_ui_data",  
                     data_display$tree_file_out,
                     data_display$geneObjectOutForSNP, params$align,
                     params$tree_format, params$font, params$num_scale,
                     params$node, params$lim, params$boot_pos, params$mid_p)

  #annotates tree with incorporated tree viz parameters and allows user to
  #add visual matrix
  treeWLayers <- callModule(mod_cladeAnnotator_server, "cladeAnnotator_ui_data",
                            data_display$mFileMatOut, plot$make_tree_out,
                            buttons$add_tree, buttons$add_anno,
                            buttons$remove_anno, buttons$add_matrix, 
                            buttons$remove_matrix, 
                            data_display$geneObjectForSNP, params$label_off, 
                            params$lab_color, params$mat_off)
  
  #allows tree with annotation and viz parameters to be donwloaded
  callModule(mod_downloadImage_server, "downloadImage_ui_1", treeWLayers)

  ####################
  ### EXAMPLE DATA ###
  ####################

  #repeated for preloaded data; no need to download so that module is not
  #included below plodad the data (tree, genetic, and meta) and call that module
  #data_display. This is the only module that is re-written, the rest of the 
  #same used in the above section and are called the same as above.

  example_data <- callModule(mod_exampleData_server, "exampleData_ui_1")

  example_params <- callModule(mod_paramsTree_server, "paramsTree_ui_example")

  example_buttons <- callModule(mod_pushButtons_server, "pushButtons_ui_example")

  callModule(mod_relaunchApp_server, "relaunchApp_ui_example")

  example_plot <- callModule(mod_displayTree_server, "displayTree_ui_example",
                            example_data   $extreeFileOut,
                            example_data   $exGeneFileCorOrUnOut,
                            example_params$align, example_params$tree_format,
                            example_params$font, example_params$num_scale,
                            example_params$node, example_params$lim,
                            example_params$boot_pos, example_params$mid_p)

  callModule(mod_cladeAnnotator_server, "cladeAnnotator_ui_example", 
             example_data  $exMetaFileOut, example_plot$make_tree_out,
             example_buttons$add_tree, example_buttons$add_anno, 
             example_buttons$remove_anno, example_buttons$add_matrix, 
             example_buttons$remove_matrix, 
             example_data  $exGeneObjectForSNP, example_params$label_off,
             example_params$lab_color, example_params$mat_off)
  
}
