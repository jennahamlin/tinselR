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
               tags$th("Choose an example data set", colspan = "3",
                       style = "font-size:20px; color:#444444")),
    tags$hr(style = "border-color: #99b6d8;"),
    
    selectInput(ns("example_data_set"), label = "Example Data", choices = 
                  c("example data 1", "example data 2", "example data 3")),
    
    tags$em("Exmaple data set 2 - adjust the spacing to plot to equal 0.09."),
    
    #add horizontal line to separate tree viz parameters
    tags$hr(style = "border-color: #99b6d8;")
  )
}

#' exampleData Server function
#'
#' @rdname mod_exampleData
#' @export
#' @keywords internal
mod_exampleData_server <- function(input, output, session) {
  ns <- session$ns
  
  ####################
  ### EXAMPLE DATA ###
  ####################
  
  #reactive expressions that loads and allows to user to select from three
  #different datasets already combined (e.g tree1, gene1, meta1). 
  #Example data are made via reading in the data, usethis::use_data(), and
  #finally documenting the data like so - usethis::use_data(tree2, tree2)
  #no need to provide option to select input delimiter as this is done when
  #reading in data to use with usethis::use_data()
  
  #Below concordant files are combined and displayed to user as example data 1, 
  #example data 2, and example data 3. For example, tree1, gene1, and meta 1 all
  #become example data 1. This was a suggestion from code review. However, I 
  #think this divergence hides some of the things a user can learn when 
  #testing out the application. The user does not see that you don't have to
  #include all three files through the absence of the 3 drop down menus now
  #only being one drop down menu. 
  
  # Initialize a reactive values for the three different file types
  files <- reactiveValues()
  observe({
    files[["tree"]]   <- c()
    files[["gene"]] <- c()
    files[["meta"]] <- c()
  })
  
  # ############
  # ### META ###
  # ############
  
  #reactive which allows the three different data sets to be switched by
  #changing the reactive value of files[["meta"]]
  example_data_meta <- reactive({
    if (input$example_data_set == "example data 1") {
      files[["meta"]] <- tinselR::meta1
    } else if (input$example_data_set == "example data 2") {
      files[["meta"]] <- tinselR::meta2
    } else if (input$example_data_set == "example data 3") {
      files[["meta"]] <- tinselR::meta3
    }
  })
  
  # ###############
  # ### GENETIC ###
  # ###############
  
  #reactive which allows the three different data sets to be switched by
  # changing the reactive value of files[["gene"]]
  example_data_gene <- reactive({
    if (input$example_data_set == "example data 1") {
      files[["gene"]] <- tinselR::gene1
    } else if (input$example_data_set == "example data 2") {
      files[["gene"]] <- tinselR::gene2
    } else if (input$example_data_set == "example data 3") {
      files[["gene"]] <- tinselR::gene3
    }
  })
  
  # ##############
  # ### TREES ###
  # ##############
  
  #reactive which allows the three different data sets to be switched by
  #changing the reactive value of files[["tree"]]
  example_data_tree <- reactive({
    if (input$example_data_set == "example data 1") {
      files[["tree"]] <- tinselR::tree1
    } else if (input$example_data_set == "example data 2") {
      files[["tree"]] <- tinselR::tree2
    } else if (input$example_data_set == "example data 3") {
      files[["tree"]] <- tinselR::tree3
    }
  })
  
  #reactive that allows for the matrix visualization on phylogenetic trees
  #specific to example data set 2 and 3 by performing the function 
  #m_file_conversion on the meta data file
  ex_meta <- reactive({
    if (!is.null(example_data_meta())) {
      m_file_conversion(example_data_meta())
    } else {
      return(NULL)
    }
  })
  
  #reactive expression that reads in the newick tree and allows the optional
  #use of the meta data to correct tree tip labels. Because all three files
  #are included at the same time, this is one place that example data and user
  #data diverge because in the user data, we provide the option for the user to
  #not include the meta data file.
  ex_tree_file_up <- reactive({
    . <- NULL 
    
    #validate(need(input$ex_tree_file != "", "Please select newick tree file"))
    req(example_data_tree())
    
    example_data_tree() %>%
      #this line converts tip labels to pretty labels based on user upload
      phylotools::sub.taxa.label(., as.data.frame(example_data_meta()))
  })
  
  #reactive expression that reads in the genetic distance file and allows the
  #meta data to correct tree tip labels found in the genetic distance file. 
  #Because all three files are included at the same time, this is one place
  #that example data and user data diverge because in the user data, we provide
  #the option for the user to not include the meta data file.
  ex_gene_file_cor_or_un <- reactive({
    . <- NULL
    
    #require this file
    req(example_data_gene())
    
    #assigned meta data to new variable name
    ex_meta_file_comb <- example_data_meta()
    
    #assigne genetic distance file to new variable name
    ex_gene_file_corrected <- example_data_gene()
    
    #the next lines essentially map the preferred tip lab display in the
    #meta data file to that in the genetic distance file, which has the long
    #tip display names so essentially replacing the long tip labels with
    #whateveris included in the meta data file.
    colnames(ex_gene_file_corrected)[2:ncol(ex_gene_file_corrected)] <-
      ex_meta_file_comb$Display.labels[which(
        ex_meta_file_comb$Tip.labels %in% colnames(ex_gene_file_corrected)
        [2:ncol(
          ex_gene_file_corrected)])]
    ex_gene_file_corrected$. <-
      ex_meta_file_comb$Display.labels[which(ex_meta_file_comb$Tip.labels
                                             %in% ex_gene_file_corrected$.)]
    return(ex_gene_file_corrected)
    
  })
  
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
