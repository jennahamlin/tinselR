# Module UI
  
#' @title   mod_cladeFindr_ui and mod_cladeFindr_server
#' @description  A shiny Module.
#'
#' @param id shiny id
#' @param input internal
#' @param output internal
#' @param session internal
#'
#' @rdname mod_cladeFindr
#'
#' @keywords internal
#' @export 
#' @importFrom shiny NS tagList 
mod_cladeFindr_ui <- function(id){
  ns <- NS(id)
  tagList(
  
  )
}
    
# Module Server
    
#' @rdname mod_cladeFindr
#' @export
#' @keywords internal
    
mod_cladeFindr_server <- function(input, output, session){
  ns <- session$ns
}
    
## To be copied in the UI
# mod_cladeFindr_ui("cladeFindr_ui_1")
    
## To be copied in the server
# callModule(mod_cladeFindr_server, "cladeFindr_ui_1")
 
