#' exampleData UI function
#'
#' @title   mod_exampleData_ui and mod_exampleData_server
#' @description  A shiny Module. This module sources the pre-loaded
#' example data (e.g. tree (x3), genetic distance file (x3), and meta data file
#'(x3)).
#'
#' @param id shiny id
#' @param input internal
#' @param output internal
#' @param session internal
#'
#' @rdname mod_exampleData
#'
#' @keywords internal
#' @export
#' @importFrom shiny NS tagList
#' @importFrom tibble column_to_rownames
#' @importFrom tidyr drop_na
#' @importFrom tidyr pivot_longer
mod_exampleData_ui <- function(id) {
  ns <- NS(id)
  
  tagList(
    tags$table(width = "100%",
               tags$th("Upload Files", colspan = "3",
                       style = "font-size:20px; color:#444444")),
    tags$hr(style = "border-color: #99b6d8;"),
    
    selectInput(ns("example_data_set"), label = "Example Data", choices = c("example data 1",
                                                    "example data 2", 
                                                    "example data 3")),
    
    #select tree file that is sourced using the UI function in app_ui.R file
    selectInput(ns("ex_tree_file"),
                label = tags$b("1. Select example newick file",
                               style = "color:#afafae"),
                choices = c("", "example Tree 1", "example Tree 2",
                            "example Tree 3")),
    
    #select genetic distance file
    selectInput(ns("ex_gene_file"), label = tags$b("2. Selected assocaited
                                                 genetic distance file",
                                                   style = "color:#afafae"),
                choices = c("", "example Genetic Distance 1",
                            "example Genetic Distance 2",
                            "example Genetic Distance 3")),
    
    #select meta data file
    selectInput(ns("ex_meta_file"), label = tags$b("3. Select associated
                                                 (optional) meta data file",
                                                   style = "color:#afafae"),
                choices = c("", "example Meta Data 1", "example Meta Data 2",
                            "example Meta Data 3")),
    
    #add horizontal line to separate tree viz parameters
    tags$hr(style = "border-color: #99b6d8;")
  )
}

#' exampleData Server function
#'
#' @rdname mod_exampleData
#' @export
#' @keywords internal
mod_exampleData_server <- function(input, output, session, n_values) {
  ns <- session$ns
  
  ####################
  ### EXAMPLE DATA ###
  ####################
  
  #reactive expressions that loads and allows to user to select from three
  #different datasets (e.g tree1, gene1, meta1). Example data are made via
  #reading in the data, usethis::use_data(), and finally documenting the data
  #like so - usethis::use_data(tree2, tree2)
  #no need to provide option to select input delimiter as this is done when
  #reading in data to use with usethis::use_data()
  
  # Initialize a reactive value and set to zero (count) and an empty
  #list for tip vector input
  files <- reactiveValues()
  observe({
    files[["tree"]]   <- c()
    files[["gene"]] <- c()
    files[["meta"]] <- c()
  })
  
  
  example_data_meta <- reactive({
    if (input$example_data_set == "example data 1") {
      files[["meta"]] <- tinselR::meta1
    } else if (input$example_data_set == "example data 2") {
      files[["meta"]] <- tinselR::meta2
    } else if (input$example_data_set == "example data 3") {
      files[["meta"]] <- tinselR::meta3
    }
    
  })
  
  example_data_gene <- reactive({
    if (input$example_data_set == "example data 1") {
      files[["gene"]] <- tinselR::gene1
    } else if (input$example_data_set == "example data 2") {
      files[["gene"]] <- tinselR::gene2
    } else if (input$example_data_set == "example data 3") {
      files[["gene"]] <- tinselR::gene3
    }
    
  })
  
  example_data_tree <- reactive({
    if (input$example_data_set == "example data 1") {
      files[["tree"]] <- tinselR::tree1
    } else if (input$example_data_set == "example data 2") {
      files[["tree"]] <- tinselR::tree2
    } else if (input$example_data_set == "example data 3") {
      files[["tree"]] <- tinselR::tree3
    }
    
  })
  
  # ############
  # ### META ###
  # ############
  # 
  # ex_meta_file_in <- reactive({
  #   switch(input$ex_meta_file, 
  #          "example Meta Data 1" = tinselR::meta1,
  #          "example Meta Data 2" = tinselR::meta2,
  #          "example Meta Data 3" = tinselR::meta3)
  # })
  
  #reactive that allows for the matrix visualization on phylogenetic trees
  #specific to example data set 3
  ex_meta <- reactive({
    if (!is.null(example_data_meta())) {
      m_file_conversion(example_data_meta())
    } else {
      return(NULL)
    }
  })
  
  ###############
  ### GENETIC ###
  ###############
  
  # ex_gene_file_in <- reactive({
  #   switch(input$ex_gene_file, 
  #          "example Genetic Distance 1" = tinselR::gene1,
  #          "example Genetic Distance 2" = tinselR::gene2,
  #          "example Genetic Distance 3" = tinselR::gene3)
  # })

  ##############
  ### TREES ###
  ##############
  
  # example_data_tree <- reactive({
  #   switch(input$ex_tree_file, 
  #          "example Tree 1" = tinselR::tree1,
  #          "example Tree 2" = tinselR::tree2,
  #          "example Tree 3" = tinselR::tree3)
  # })
  # 
  #reactive expression that uploads the newick tree and allows the optional
  #upload of meta data to correct tree tip labels
  ex_tree_file_up <- reactive({
    #str(example_data_meta())
    . <- NULL 
    
    #validate(need(input$ex_tree_file != "", "Please select newick tree file"))
    req(example_data_tree())
    
    if (is.null(example_data_meta())) { #if no meta file still upload the tree
      example_data_tree()
    } else {
      example_data_tree() %>%
        #this line converts tip labels to pretty labels based on user upload
        phylotools::sub.taxa.label(., as.data.frame(example_data_meta()))
    }
  })
  
  #reactive expression that uploads the genetic distance file and allows the
  #optional upload of meta data to correct tree tip labels
  ex_gene_file_cor_or_un <- reactive({
    #if no meta file, uploaded to be able to use clade annotator function
    if (is.null(example_data_meta())) {
      req(example_data_gene())
      
      #if meta file uploaded, correct the distance file to match meta tip labels
    } else { 
      . <- NULL
      ex_meta_file_comb <- example_data_meta()
      ex_gene_file_corrected <- example_data_gene()
      colnames(ex_gene_file_corrected)[2:ncol(ex_gene_file_corrected)] <-
        ex_meta_file_comb$Display.labels[which(
          ex_meta_file_comb$Tip.labels %in% colnames(ex_gene_file_corrected)
          [2:ncol(
            ex_gene_file_corrected)])]
      ex_gene_file_corrected$. <-
        ex_meta_file_comb$Display.labels[which(ex_meta_file_comb$Tip.labels
                                               %in% ex_gene_file_corrected$.)]
      return(ex_gene_file_corrected)
    }
  })
  
  
  # print("L 82 example")
  # print(n_values())
  # if (n_values() > 0) {
  #   print("L 85")
  #   validate(need(TRUE), "Please reload")
  # } else 
  #additional manipulation of genetic distance matrix for ultimately getting
  #the mean number of SNPs. geneObjectOut is a function that is applied to
  #another function (replaceHwithZeros) for the reactive exGeneFileCorOrU
  ex_gene_object <- reactive({
    label <- NULL
    g <- gene_object_out(replace_column_header(ex_gene_file_cor_or_un()))
    return(g)
  })
  
  
  ###################################
  #### exampleData server output ####
  ###################################
  
  #return these reactive objects to be used in tree display module
  return(
    list(
      #for adding on a heatmap; sent to cladeAnnotator module
      exMetaFileOut = reactive(ex_meta()),

      #checks for file and sends user message; sent to tipCheck
      ex_m_file_out = reactive(example_data_meta()),

      #checks for file and sends user message; sent to tipCheck
      ex_g_file_out = reactive(example_data_gene()),

      #checks for file and sends user message; sent to tipCheck
      #and for data display module
      extreeFileOut = reactive(ex_tree_file_up()),

      ##for clade annotator
      exGeneObjectForSNP = reactive(ex_gene_object())
    ))
}
