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
#' @importFrom tibble column_to_rownames
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
}
