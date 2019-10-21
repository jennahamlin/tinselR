#' @import shiny
app_server <- function(input, output,session) {
  # List the first level callModules here
  callModule(mod_package_description_server, "package_description_ui_1")
  callModule(mod_read_in_data_server, "read_in_data_ui_1",
                         stringsAsFactors = FALSE)
  }


