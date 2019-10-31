#' @import shiny
app_ui <- function() {
  tagList(
    # Leave this function for adding external resources
    golem_add_external_resources(),
    # List the first level UI elements here 
    fluidPage(
      
      # Sidebar layout with input and output definitions ----
      sidebarLayout(
        
        # Sidebar panel for inputs ----
        sidebarPanel(
          mod_dataInput_ui("dataInput_ui_meta", "User META data (.csv, .tsv, .txt format)")
        ),
        
        # Main panel for displaying outputs ----
        mainPanel(
          
          # Output: Data file ----
          tableOutput("metacontents")
          
        )
      ),
      # Sidebar layout with input and output definitions ----
      sidebarLayout(
        
        # Sidebar panel for inputs ----
        sidebarPanel(
          mod_dataInput_ui("dataInput_ui_gene", "User GENETIC data (.csv, .tsv, .txt format)")
        ),
        
        # Main panel for displaying outputs ----
        mainPanel(
          
          # Output: Data file ----
          tableOutput("genecontents")
          
        )
      ),
      # Sidebar layout with input and output definitions ----
      sidebarLayout(
        
        # Sidebar panel for inputs ----
        sidebarPanel(
          mod_treeInput_ui("treeInput_ui_1")
        ),
        
        mainPanel (
          plotOutput("tree")
        )
      )
    )
  )
}


#' @import shiny
golem_add_external_resources <- function(){
  
  addResourcePath(
    'www', system.file('app/www', package = 'Tinsel')
  )
  
  tags$head(
    golem::activate_js(),
    golem::favicon()
    # Add here all the external resources
    # If you have a custom.css in the inst/app/www
    # Or for example, you can add shinyalert::useShinyalert() here
    #tags$link(rel="stylesheet", type="text/css", href="www/custom.css")
  )
}
