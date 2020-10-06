#' Run the Shiny Application
#' 
#' @param ... other arguments 
#'
#' @export
#' @importFrom shiny shinyApp
#' @importFrom golem with_golem_options
run_app <- function(
  onStart = NULL,
  options = list(),
  enableBookmarking=NULL,
  ...
  ) {
  with_golem_options(
    app = shinyApp(
      ui = app_ui,
      server = app_server,
      onStart = onStart,
      options = options,
      enableBookmarking = "url"), 
    golem_opts = list(...)
  )
}
