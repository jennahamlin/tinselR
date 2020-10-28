#' aboutTreeParams UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd 
#'
#' @importFrom shiny NS tagList 
mod_aboutTreeParams_ui <- function(id){
  ns <- NS(id)
  tagList(
    fluidRow(column(12, offset = 0,
                    mainPanel(h2(strong("Description of Tree Parameters")),
                              tags$br(),
                              tags$strong("Below we define the tree parameters
                                          that a user may adjust. Please
                                          note that these parameters are 
                                          what is available for adjustment
                                          in the package ggtree or ggplot2."),
                              tags$br(),
                              tags$br(),
                              tags$li(tags$em("Add spacing to plot -"), "This
                              will adjust the plot spacing for the tree image
                              essentially allocating more space to the plot.
                              Can be useful to adjust when adding annotations or 
                              heatmap."),
                              tags$br(),
                              tags$li(tags$em("Align the tips -"), "This will 
                                      allow for tip labels to either be aligned
                                      to the right of the tree or not."),
                              tags$br(),
                              tags$li(tags$em("Annotation Label Color -"), "This
                                      will change the color of the annotation 
                                      label to either 'red', 'blue', 
                                      'black', or 'grey'"),
                              tags$br(),
                              tags$li(tags$em("Bootstrap Positions -"), "This
                                      will move over the bootstrap values to
                                      the left or right of the intial position."
                                      ),
                              tags$br(),
                              tags$li(tags$em("Font format -"), "This will allow
                              the font to be changed from 'bold', 'italic', or
                              'bold and italic'"),
                              tags$br(),
                              tags$li(tags$em("Heatmap Color Options -"), "This
                              will allow the color of the heatmap to be changed, 
                              using the viridis color options. Options include
                              'A - magma', 'B - inferno', 'C - plasma',
                              'D - viridis', or 'E - cividis'."),
                              tags$br(),
                              tags$li(tags$em("Midpoint Root -"), "This will
                                      either allow the tree to be midpoint
                                      rooted or not."),
                              tags$br(),
                              tags$li(tags$em("Min. Value of Bootstrap -"), "
                                      This is the minimum value which should be
                                      displayed for bootstraps"),
                              tags$br(),
                              tags$li(tags$em("Move All Annotations -"), "This 
                                      will move all annotations over. Can be 
                                      useful if spacing has been adjusted for 
                                      the plot."),
                              tags$br(),
                              tags$li(tags$em("Move Heatmap -"), "Adjust the 
                                      position of the heatmap, this value needs
                                      to be less than the 'Add spacing to plot'
                                      parameter for the heatmap to be seen."),
                              tags$br(),
                              tags$li(tags$em("Size of Scale Bar -"), "This will
                                      convert the size of the scale bar that 
                                      is displayed."),
                              tags$br(),
                              tags$li(tags$em("Tree Layout -"), "This provides
                                      the user the option for the tree layout 
                                      to be either 'rectangular', 'slanted', or
                                      'circular'."),         
                                      
                              
                              
                              
                              
                              
                              ))), 
                          
                                     
  )
}
    
#' aboutTreeParams Server Function
#'
#' @noRd 
mod_aboutTreeParams_server <- function(input, output, session){
  ns <- session$ns
 
}
