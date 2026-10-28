# ===================== Needed Libs =====================
library(shiny)
library(tidyverse)
library(ggplot2)

source("helpers.R")

# ========================== UI =========================
ui <- fluidPage(
  # Application title
  titlePanel("Superstore Data"),
  sidebarLayout(
    sidebarPanel(
      h4("Subset the Data"),
      radioButtons(
        "region",
        "Region:",
        choices = c("All", levels(superstore$region)),
        selected = "All"
      ),
      radioButtons(
        "segment",
        "Customer Segment:",
        choices = c("All", levels(superstore$segment)),
        selected = "All"
      ),
      radioButtons(
        "category",
        "Product Category:",
        choices = c("All", levels(superstore$category)),
        selected = "All"
      ),
      sliderInput(
        "bins",
        "Number of bins:",
        min = 1,
        max = 50,
        value = 25
      ),
      sliderInput(
        "rank_value",
        "Top Categories shown:",
        min = 2,
        max = 17,
        value = 25
      ),
      
      actionButton("apply", "Apply")
    ),
    mainPanel(tabsetPanel(
      id = "main_tabs",
      tabPanel(
        "About",
        h3("Purpose"),
        p("words"),
        br(),
        h3("Data and Source"),
        p("words"),
        br(),
        h3("Functionality"),
        p("purpose of the sidebar and each tab")
      ),
      tabPanel("Data Download", p("display the data")),
      tabPanel(
        "Data Exploration",
        p("EDA work"),
        plotOutput("histogram"),
        plotOutput("barGraph"),
        plotOutput("bumpPlot")
      )
    ))
  )
)

# ======================= Server ========================
server <- function(input, output) {
  filtered <- eventReactive(input$apply,
    {
      list(
        data = superstore |>
          apply_filter("region", input$region) |>
          apply_filter("segment", input$segment) |>
          apply_filter("category", input$category),
        bins = input$bins,
        category = input$category
      )
      
    },
    ignoreInit = TRUE
  )
  # ----------------------- Numerical Summaries -----------------------
  
  
  # ----------------------- Plots -----------------------
  
  #output$pieChart <- renderPlot({
    
  #})
  
  
  output$histogram <- renderPlot({
    req(filtered())
    data <- filtered()$data

    validate(
      need(nrow(data) > 0, "No data available for these selections."),
      need(
        any(data$sales > 0, na.rm = TRUE),
        "No positive sales values available."
      )
    )
    
    plot_sales_histogram(filtered()$data, filtered()$bins)

  })

  output$barGraph <- renderPlot({
    req(filtered())
    data <- filtered()$data

    validate(need(nrow(data) > 0, "No data available for this filter."))
    
    plot_profit_by_subcategory(filtered()$data, filtered()$category)
    
  })
  
  #output$boxPlot <- renderPlot({
    
  #})

  # output$facetWrap <- renderPlot({
  #   
  # })
  # output$heatmap <- renderPlot({
  #   
  # })
  
  output$bumpPlot <- renderPlot({
    req(filtered())
    plot_profit_ranking(filtered()$data)
  })}

# Run the application
shinyApp(ui = ui, server = server)