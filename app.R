#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    https://shiny.posit.co/
#


library(shiny)
library(tidyverse)

superstore <- read_excel("data/us_superstore.xls")


#=================== UI ===================
ui <- fluidPage(

    # Application title
    titlePanel("Superstore Data"),

    sidebarLayout(
        sidebarPanel(
            sliderInput("bins",
                        "Number of bins:",
                        min = 1,
                        max = 50,
                        value = 30
                        )
        ),

        mainPanel(
          tabsetPanel(
            id = "main_tabs",
            
              tabPanel("About",
                h3("Purpose"),
                p("words"),
                br(),
                h3("Data and Source"),
                p("words"),
                br(),
                h3("Functionality"),
                p("purpose of the sidebar and each tab")
              ),
            
            tabPanel("Data Download", 
                     p("display the data")
                     ),
            
            tabPanel("Data Exploration", 
                     p("EDA work"),
                     
                     plotOutput("distPlot")
                     )
          )
           
        )
    )
)

#================= Server =================
server <- function(input, output) {

    output$distPlot <- renderPlot({
        # generate bins based on input$bins from ui.R
        x    <- faithful[, 2]
        bins <- seq(min(x), max(x), length.out = input$bins + 1)

        # draw the histogram with the specified number of bins
        hist(x, breaks = bins, col = 'darkgray', border = 'white',
             xlab = 'Waiting time to next eruption (in mins)',
             main = 'Histogram of waiting times')
    })
}

# Run the application 
shinyApp(ui = ui, server = server)
