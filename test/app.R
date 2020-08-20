#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)

# Define UI for application that draws a histogram
ui <- fluidPage(

    # Application title
    titlePanel("Old Faithful Geyser Data"),

    # Sidebar with a slider input for number of bins 
    sidebarLayout(
        sidebarPanel(
            #upload tree file 
            selectInput("exTreeFile", label ="1. Select example newick file", 
                        choices = c("example Tree 1", "example Tree 2")),
            actionButton("add_tree","Visualize Tree")
        ),

        # Show a plot of the generated distribution
        mainPanel(
            plotOutput("treeDisplay")
            
        )
    )
)

# Define server logic required to draw a histogram
server <- function(input, output) {

    ##############
    ### TREES ###
    ##############
    
    ## read in tree based on selected example tree
    
    exTreeFileUp <- eventReactive(input$add_tree, {
        if(input$exTreeFile == "example Tree 1"){
        exTreeFileUp <-  treeio::read.newick(here::here("inst/extdata", "/1509MNJJP-1_RAxML_bipartitions"))
    }
     else if (input$exTreeFile == "example Tree 2") {
       exTreeFileUp <-  treeio::read.newick(here::here("inst/extdata", "/1504MLEXH-1.dnd"))}
        }
    )
    
    print(here::here("inst/extdata"))
    
    make_tree <- reactive({
        ggtree::ggtree(exTreeFileUp())+
            ggtree::geom_tiplab()+
            ggplot2::xlim(NA, 0.02)})
    
    
    #displays the tree plot, uses output from the displayTree module 
    observeEvent(input$add_tree, {output$treeDisplay <- renderPlot({
        make_tree()})
    })
    
}

# Run the application 
shinyApp(ui = ui, server = server)
