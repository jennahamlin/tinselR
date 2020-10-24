#' tipCheck UI Function
#'
#' @title mod_tipCheck_ui mod_tipCheck_server
#'
#' @description A shiny Module. This module generates the landing page for the
#' application and provides a link to the github repo where users may file
#' an issue
#'
#' @rdname mod_tipCheck
#'
#' @param id,input,output,session internal
#' @param t_file_out imported tree for tip checking
#' @param g_file_out imported genetic distance file for tip checking
#' @param gene_file_out imported gene file for confirming that file is uploaded
#' @param meta_file_out imported meta file for confirming that file is uploaded
#' @param m_file_out imported meta data file for tip checking
#'
#' @importFrom shiny NS tagList
mod_tipCheck_ui <- function(id) {
  ns <- NS(id)
  tagList(
    
    tags$table(width = "100%",
               tags$th("File Check Output", colspan = "3",
                       style = "font-size:20px; color:#444444;")),
    
    # this outputs info about if tip labels are not concordant between all
    #three files
    htmlOutput(ns("file_checking")),
    
    # this one will tell user if they can add a matrix to the tree
    uiOutput(ns("file_checking_mat")),
    
    tags$hr(style = "border-color: #99b6d8;")
  )
}

#' tipCheck Server Function
#'
#' @rdname mod_tipCheck
#'
mod_tipCheck_server <- function(input, output, session, meta_file_out,
                                m_file_out, gene_file_out, g_file_out,
                                t_file_out) {
  ns <- session$ns
  
  #this will render the output from the sanity function found in the
  #golem_utils_server.R file and takes in 5 reactive files -
  #tree, genetic distance, genetic distance file, meta data, and meta data file
  
  output$file_checking <- renderUI({
    ns <- session$ns
    
    if (is.null(t_file_out())) {
      return(HTML('<span style="color:gray">Please upload a tree file</span>'))
    } else if (is.null(gene_file_out())) {
      return(HTML(
        '<span style="color:gray">Please upload a genetic distance file</span>')
      )
    } else if (is.null(meta_file_out())) {
      return(HTML(
        '<span style="color:gray">Please upload a meta data file</span>')
      )
    } else {
      sanity(
        t_file = t_file_out(),
        g_file = g_file_out(),
        m_file = m_file_out()
      )
    }
  })
  
  #displays number of columns that are available for adding a matrix to the
  #tree via a message to the user
  output$file_checking_mat <- renderUI({
    ns <- session$ns
    if (is.null(m_file_out())) {
      return(NULL)
    } else {
      validate(not_columns(m_file_conversion(m_file_out())))
    }
  })
  
  ######################################
  ###### tipCheck server function ######
  ######################################
  
  ## tipCheck server function
  #Function to check imported data files for tip label agreement. If no tip label
  #agreement, tells user what is problematic; this can include if number of tips
  #does not match when all three files are uploaded. The input paramters include
  #tree file, genetic file, and meta file
  sanity <- function(m_file, g_file, t_file) {
    
    #meta data get tips
    m_file_tips <- m_file %>% dplyr::pull(1) %>% sort
    
    #genetic data get tips
    g_file_tips <- g_file %>% dplyr::pull(1) %>% sort
    
    #tree file get tips
    t_file_tips <- sort(t_file$tip.label)
    
    # Check for required column names in meta data file
    if ("Tip.labels" %in% colnames(m_file) != TRUE) {
      return(HTML('<span style = "color:gray">Your metadata file does not contain
                the correct column headers. Please correct and try again.
                </span>'))
    } else if ("Display.labels" %in% colnames(m_file) != TRUE) {
      return(HTML('<span style = "color:gray">Your metadata file does not contain
                the correct column headers. Please correct and try again.
                </span>'))
    }
    
    # Check for the same number of tips for all three files
    if (length(t_file_tips) != length(g_file_tips) |
        length(t_file_tips) != length(m_file_tips) |
        length(g_file_tips) != length(m_file_tips)) {
      return(HTML(paste(
        '<span style = "color:gray">The number of tip labels in your input files
      are unequal, please correct.</span>',
        '<span style = "color:gray">No. of labels in tree file:</span>',
        length(t_file_tips),
        '<span style="color:gray">No. of labels in distance file:</span>',
        length(g_file_tips),
        '<span style = "color:gray">No. of labels in meta data file:</span>',
        length(m_file_tips),
        sep = "<br/>")))
    } else {
      return(HTML('<span style = "color:gray">All three files pass checks and
                contain the same tip labels!</span>'))}
  }

  #get the number of columns of the meta data file. Here columns should be 1 or
  #more after transformation of meta data. Tell user how many columns there are
  #as the output here allows the user to use the add heatmap/matrix button.
  not_columns <- function(file) {
    col_n_file <- ncol(file)
    
    if (col_n_file < 1) {
      return("And looks like there is not a column for matrix plotting")
    } else {
      return(
        paste("And looks like the number of columns for matrix plotting is: ",
              col_n_file))
    }
  }
  
}
