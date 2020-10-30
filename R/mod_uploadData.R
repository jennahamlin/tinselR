#' uploadData UI Function
#'
#' @title   mod_uploadData_ui and mod_uploadData_server
#' @description  A shiny Module. This module allows the user to upload three
#'  different types of files and does file checking to confirm the correct
#'  delimiter is selected. The output from this module is sent to three
#'  different modules (tipCheck, displayTree, and cladeAnnotator).
#'
#'
#' @param id shiny id
#' @param input internal
#' @param output internal
#' @param session internal
#'
#' @rdname mod_uploadData
#'
#' @keywords internal
#' @export
#' @importFrom shiny NS tagList
#' @importFrom readr read_delim
#' @importFrom treeio read.newick
#' @importFrom phylotools sub.taxa.label
#' @importFrom tidyr drop_na
#' @importFrom tidyr pivot_longer
mod_uploadData_ui <- function(id) {
  ns <- NS(id)
  tagList(
    
    tags$table(width = "100%",
               tags$th("Upload Files", colspan = "3",
                       style = "font-size:20px; color:#444444;")),
    tags$hr(style = "border-color: #99b6d8;"),
    
    #upload for the tree file
    fileInput(ns("tree_file"), label = tags$b("1. Upload a newick file",
                                              style = "color:#afafae")),
    
    #upload genetic distance file using a UI function, which has two inputs:
    #file_id and file_label. The file_upload function is located in the 
    #golem_utils_ui.R file 
    file_upload(ns("gene_file"), file_label =
                  tags$b("2. Upload a genetic distance file",
                         style = "color:#afafae")),
    
    #this decreases the distance between the two buttons (upload and separator)
    div(style = "margin-top:-2em",
        
        #specify the type of separator for the genetic distance file uploaded. 
        #input_separator is an ui function with two inputs: file_id and file_label
        #the function is located in the golem_utils_ui.R file
        input_separator(ns("gene_sep"), file_label = tags$em(
          "Separator for genetic distance file", style = "color:#afafae"))),
    
    file_upload(ns("meta_file"), file_label = tags$b(
      "3. Upload an optional meta data file", style = "color:#afafae")),
    
    div(style = "margin-top:-2em",
        input_separator(ns("meta_sep"), file_label = tags$em(
          "Separator for optional meta data file", style = "color:#afafae"))
    ),
    tags$hr(style = "border-color: #99b6d8;")
  )
}

#' uploadData Server Function
#'
#' @rdname mod_uploadData
#' @export
#' @keywords internal
mod_uploadData_server <- function(input, output, session) {
  ns <- session$ns
  
  ############
  ### META ###
  ############
  
  #1. reactive expression that holds meta data file and sends tipCheck message
  meta_file_up <- reactive({
    input$meta_file
  })
  
  #2. to perform check for correctly selected delimiter using file_type function
  # which is located at the end of this module. 
  meta_file_type <- eventReactive(input$meta_sep, {
    file_type(input$meta_sep)
  })
  
  #3. read in the file using file up and file type functions; both located at 
  #end of this module.
  meta_file <- reactive({
    file_check(file_up = meta_file_up(), file_type = meta_file_type(),
               file_sep = meta_file_type())
  })
  
  #this performs file conversion for the meta file if there is matrix data,
  #and is a reactive that is ultimately sent to the cladeAnnotator
  m_file_mat <- reactive({
    if (!is.null(meta_file_up())) { #if not; then will complain w/button push
      m_file_conversion(m_file = meta_file())
    }
  })
  
  ###############
  ### GENETIC ###
  ###############
  
  #1. reactive expression that holds the genetic distance file
  gene_file_up <- reactive({
    input$gene_file
  })
  
  #2. to perform check for correctly selected delimiter using file_type function
  gene_file_type <- eventReactive(input$gene_sep, {
    file_type(input$gene_sep)
  })
  
  #3. read in the file using file up and file type reactive
  gene_file <- reactive({
    file_check(file_up = gene_file_up(),
               file_type = gene_file_type(),
               file_sep = gene_file_type())
  })
  
  ##############
  ### TREES ###
  ##############
  
  #reactive expression that uploads the newick tree and allows the optional
  #upload of meta data to correct tree tip labels
  tree_file_up  <- reactive({
    
    . <- NULL #this refers to the file that is passed through
    
    validate(need(input$tree_file != "", "Please import newick tree file"))
    req(input$tree_file)
    
    if (is.null(meta_file_up()$datapath)) { #if no meta  still upload the tree
      treeio::read.newick(input$tree_file$datapath)
    } else {
      meta_file_seperate <- meta_file() #pass in the file read above
      
      treeio::read.newick(input$tree_file$datapath) %>%
        phylotools::sub.taxa.label(., as.data.frame(meta_file_seperate))
      #line converts tip labels to pretty labels based on user meta upload
    }
  })
  
  #reactive expression that uploads the genetic distance file and allows the
  #optional upload of meta data to correct tip labels
  gene_file_cor_or_un <- reactive({
    if (is.null(meta_file_up()$datapath)) {
      gene_file_uncorrected <- gene_file()
    }
    
    else { #if meta file uploaded then correct tip labels of distance matrix
      
      meta_file_comb <- meta_file()
      
      #rename column to center; necessary for next step.
      gene_file_corrected <- gene_file() %>% dplyr::rename(center = 1)
      
      #the next lines essentially map the preferred tip lab display in the
      #meta data file to that in the genetic distance file, which has the long
      #tip display names so essentially replacing the long tip labels with
      #whatever the user prefers and is included in the meta data file.
      
      colnames(gene_file_corrected)[2:ncol(gene_file_corrected)] <-
        meta_file_comb$Display.labels[which(meta_file_comb$Tip.labels %in%
                                              colnames(gene_file_corrected)
                                            [2:ncol(gene_file_corrected)])]
      
      gene_file_corrected$center <-
        meta_file_comb$Display.labels[which(meta_file_comb$Tip.labels
                                            %in% gene_file_corrected$center)]
      return(gene_file_corrected)
    }
  })
  
  #additional manipulation of genetic distance matrix for ultimately
  #getting the mean number of SNPs for either the corrected or uncorrected file;
  #uses two functions located in goloem_utils_server.R file and has a
  #description of those functions within.
  gene_object <- reactive({
    label <- NULL
    #if (is.null(meta_file_up()$datapath)) {
      gene_object_out(replace_column_header(
        gene_file_in = gene_file_cor_or_un())) 
    #} else {
    #  validate(need(input$meta_file !="", "Please relaunch the app."))
    #}
  })
  
  #####################################
  #### uploadData server functions ####
  #####################################
  
  #function to read in the data using readr::read_delim
  #filePath is the path to the location of the file you want to read in
  #sep is the specified delimiter, probably either a tab ("\t") or comma (",")
  #the other bits here help with reading in the file: trim whitespace, skip
  #empty row, column names, and how to read in the data; default is
  #set at column as characters
  read_data <- function(file_path, sep) {
    readr::read_delim(file_path,
                      sep,
                      trim_ws = TRUE,
                      skip_empty_rows = TRUE,
                      col_names = TRUE,
                      col_types =
                        readr::cols(.default = readr::col_character()))
  }
  
  #function which maps the type of file uploaded based on user selection. For
  #example, in_var could be input$gene_sep
  file_type <- function(in_var) {
    if (in_var == "\t") {
      return("\t")
    } else if (in_var == ",") {
      return(",")
    }
  }
  
  #function to confirm the type of file uploaded, matches the selected type
  #this uses the fill uploaded (file_up), the type of file delimited selected
  #(file_type - either a csv or tsv), and the file separate from input$sep,
  #which the user specifies on the interface -so this is ultimately a reactive
  file_check <- function(file_up, file_type, file_sep) {
    my_file <- req(file_up$datapath)
    my_lines <- readLines(con = my_file, n = 3)
    file_chk <- validate(
      need(
        length(strsplit(my_lines[2],
                        file_type)[[1]]) ==
          length(strsplit(my_lines[3], file_type)[[1]]),
        paste(
          "Error: the delimiter chosen does not match the file type uploaded:",
          file_up[1], sep = "")),
      need(
        length(strsplit(my_lines[2], file_type)[[1]]) > 1,
        paste(
          "Error: the delimiter chosen does not match the file type uploaded:",
          file_up[1], sep = "")))
    if (is.null(file_chk) == TRUE) {
      file_name <- read_data(file_path = file_up$datapath, sep = file_sep)
    } else {
      return(file_chk)
    }
  }
  
  ##################################
  #### uploadData server output ####
  ##################################
  
  #return these reactive objects to be used in particular modules
  return(
    list(
      #for adding on a heatmap; sent to cladeAnnotator module
      mFileMatOut = reactive(m_file_mat()),
      
      #checks for file and sends user message; sent to tipCheck
      meta_file_out = reactive(meta_file_up()),
      
      #used for sanity (concordant check between files) and mFileConversion
      #(convert for heatmap) functions
      m_file_out = reactive(meta_file()),
      
      #checks for file and sends user message; sent to tipCheck
      gene_file_out = reactive(gene_file_up()),
      
      #used for sanity (concordant check between files); sent to tipCheck
      g_file_out = reactive(gene_file()),
      
      #for clade annotator to get snp differences and calculate the mean
      geneObjectForSNP = reactive(gene_object()),
      
      #holds tree with or without converted tip labels; send to displayTree
      tree_file_out = reactive(tree_file_up()),
      
      #require tree file for concordant tip checking; send to tipCheck
      #holds tree does not read it in.
      t_file_out = reactive({
        req(input$tree_file)
        treeio::read.newick(input$tree_file$datapath)
      })
    ))
}
