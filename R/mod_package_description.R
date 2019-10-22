# Module UI
  
#' @title   mod_package_description_ui and mod_package_description_server
#' @description  A shiny Module.
#'
#' @param id shiny id
#' @param input internal
#' @param output internal
#' @param session internal
#'
#' @rdname mod_package_description
#'
#' @keywords internal
#' @export 
#' @importFrom shiny NS tagList 
mod_package_description_ui <- function(id){
  ns <- NS(id)
  withTags({p(b('Tinsel:'), 
              'A Phylogenetic Tool')}
  )
}
    
# Module Server
    
#' @rdname mod_package_description
#' @export
#' @keywords internal
    
mod_package_description_server <- function(input, output, session){
  ns <- session$ns
  
}
    
## To be copied in the UI
# 
    
## To be copied in the server
# 
