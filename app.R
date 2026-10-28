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

      # ------------------------ About ------------------------

      tabPanel(
        "About",
        h3("Purpose"),
        p(
          "The purpose of this app is to help users understand online sales, in an
          easy and interactive manner, by using data from a US retail superstore.",
          "You can narrow the dataset easily using filters, download that subset, or even create
          specific summary statistics and visuals as a report for that subset.",
          "By allowing users to customize their analysis, this app makes it easier to",
          "identify trends, compare product categories, and explore factors associated with",
          "profitability, helping users gain insights into the store's overall business performance"
        ),
        br(),
        h3("Data and Source"),
        p(
          "The data are order-line records from a superstore. Each row is one",
          "product on one order, with details about the customer, the location,",
          "the product, and the sale (sales, quantity, discount, and profit).",
          p(
            "More information about the data: ",
            tags$a(
              href = "https://www.kaggle.com/datasets/juhi1994/superstore",
              "Superstore dataset on Kaggle", target = "_blank"
            )
          ),
          br(),
          h3("Functionality"),
          tags$li(
            strong("Sidebar: "),
            "Choose a region, segment, and product category, and optionally",
            "pick up to two numeric variables to filter with a range slider.",
            "Nothing changes until you press 'Apply Subset'."
          ),
          tags$li(
            strong("Data Download: "),
            "View the current subset as a table and save it as a CSV file."
          ),
          tags$li(
            strong("Data Exploration: "),
            "Use the 'Categorical Summaries' sub-tab for contingency tables",
            "and bar charts, and the 'Numeric Summaries' sub-tab for summary",
            "statistics and plots (boxplots, histograms, scatter plots, and",
            "heatmaps) of the current subset."
          )
        ),

        # ------------------------ Download ------------------------

        tabPanel("Data Download", p("display the data")),

        # ------------------------ EDA ------------------------

        tabPanel(
          "Data Exploration",
          p("EDA work"),
          plotOutput("histogram"),
          plotOutput("barGraph"),
          plotOutput("bumpPlot")
        )
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

  # output$pieChart <- renderPlot({

  # })


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

  # output$boxPlot <- renderPlot({

  # })

  # output$facetWrap <- renderPlot({
  #
  # })
  # output$heatmap <- renderPlot({
  #
  # })

  output$bumpPlot <- renderPlot({
    req(filtered())
    plot_profit_ranking(filtered()$data)
  })
}

# Run the application
shinyApp(ui = ui, server = server)