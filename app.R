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
    
    # ------------- Global sidebar: subsetting only -------------
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
      actionButton("apply", "Apply")
    ),
    
    
    mainPanel(
      tabsetPanel(
        id = "main_tabs",
        
        # ------------------------ About ------------------------
        tabPanel(
          "About",
          h3("Purpose"),
          p("Explore the Superstore dataset using interactive visualizations and summary statistics."),
          
          br(),
          h3("Data and Source"),
          p("The dataset contains information about orders, products, sales, discounts, profits, and shipping."),
          p(
            "More information about the dataset: ",
            tags$a(
              href = "https://www.kaggle.com/datasets/juhi1994/superstore/data",
              "Superstore Dataset on Kaggle",
              target = "_blank"
            )
          ),
          
          br(),
          h3("Functionality"),
          tags$ul(
            tags$li("Filter the dataset by region, customer segment, and product category."),
            tags$li("Explore categorical and numerical variables through plots."),
            tags$li("Compare product profitability and rankings over time."),
            tags$li("Download and examine the dataset.")
          )
        ),
        
        # ------------------------ Data Download ------------------------
        tabPanel(
          "Data Download",
          p("Display and download the data here.")
        ),
        
        # ------------------------ Data Exploration ------------------------
        tabPanel(
          "Data Exploration",
          sidebarLayout(
            
            # ---- Data Exploration sidebar: shows only the option for the selected plot ----
            sidebarPanel(
              width = 3,
              
              # Pie Chart
              conditionalPanel(
                condition = "input.eda_tabs == 'Categorical Summaries' &&
                             input.cat_tabs == 'Pie Chart'",
                h4("Plot Options"),
                selectInput(
                  inputId = "pieVar",
                  label = "Select categorical variable for pie chart:",
                  choices = cat_var,
                  selected = "category"
                )
              ),
              
              # Bump Chart
              conditionalPanel(
                condition = "input.eda_tabs == 'Categorical Summaries' &&
                             input.cat_tabs == 'Bump Chart'",
                h4("Plot Options"),
                sliderInput(
                  inputId = "rank_value",
                  label = "Number of sub-categories to display:",
                  min = 1,
                  max = nlevels(superstore$sub_category),
                  value = nlevels(superstore$sub_category),
                  step = 1
                )
              ),
              
              # Sales Histogram
              conditionalPanel(
                condition = "input.eda_tabs == 'Numerical Summaries' &&
                             input.num_tabs == 'Sales Histogram'",
                h4("Plot Options"),
                sliderInput(
                  "bins",
                  "Number of bins:",
                  min = 1,
                  max = 50,
                  value = 25
                )
              ),
              
              # Plots with no options
              conditionalPanel(
                condition = "(input.eda_tabs == 'Categorical Summaries' &&
                              input.cat_tabs == 'Profit by Sub-Category') ||
                             (input.eda_tabs == 'Numerical Summaries' &&
                              input.num_tabs != 'Sales Histogram')",
                helpText("No extra options for this plot.")
              )
            ),
            
            mainPanel(
              width = 9,
              tabsetPanel(
                id = "eda_tabs",
                
                # ------------------------ Categorical Summaries ------------------------
                tabPanel(
                  "Categorical Summaries",
                  tabsetPanel(
                    id = "cat_tabs",
                    tabPanel(
                      "Pie Chart",
                      plotOutput("pieChart")
                    ),
                    tabPanel(
                      "Profit by Sub-Category",
                      plotOutput("barGraph")
                    ),
                    tabPanel(
                      "Bump Chart",
                      plotOutput("bumpPlot")
                    )
                  )
                ),
                
                # ------------------------ Numerical Summaries ------------------------
                tabPanel(
                  "Numerical Summaries",
                  tabsetPanel(
                    id = "num_tabs",
                    tabPanel(
                      "Sales Histogram",
                      plotOutput("histogram")
                    ),
                    tabPanel(
                      "Discount Boxplot",
                      plotOutput("boxPlot")
                    ),
                    tabPanel(
                      "Discount-Profit Heatmap",
                      plotOutput("heatmap")
                    ),
                    tabPanel(
                      "Shipping Duration",
                      plotOutput("facetWrap")
                    )
                  )
                )
              )
            )
          )
        )
      )
    )
  )
  
)

# ======================= Server ========================
server <- function(input, output, session) {
    filtered <- eventReactive(input$apply,
                            {
                              list(
                                data = superstore |>
                                  apply_filter("region", input$region) |>
                                  apply_filter("segment", input$segment) |>
                                  apply_filter("category", input$category),
                                category = input$category
                              )
                            },
                            ignoreNULL = FALSE
  )
  
  observeEvent(filtered(), {
    n_sub <- max(n_distinct(filtered()$data$sub_category), 1)
    
    updateSliderInput(
      session,
      "rank_value",
      min = 1,
      max = n_sub,
      value = n_sub
    )
  })
  
  # ===================== Categorical Plots =====================
  
  output$pieChart <- renderPlot({
    req(filtered())
    validate(
      need(nrow(filtered()$data) > 0, "No data available for these selections.")
    )
    
    plot_category_pie(
      filtered()$data,
      input$pieVar
    )
  })
  
  output$barGraph <- renderPlot({
    req(filtered())
    validate(
      need(nrow(filtered()$data) > 0, "No data available for these selections.")
    )
    
    plot_profit_by_subcategory(
      filtered()$data,
      filtered()$category
    )
  })
  
  
  output$bumpPlot <- renderPlot({
    req(filtered())
    
    data <- filtered()$data
    
    subcategories <- data |>
      distinct(sub_category) |>
      filter(!is.na(sub_category)) |>
      pull(sub_category)
    
    n_subcategories <- length(subcategories)
    
    if (n_subcategories == 0) {
      plot.new()
      text(
        0.5, 0.5,
        "No sub-categories to display!\nExpand the data set so more can be visible.",
        cex = 1.2
      )
      return()
    }
    
    if (n_subcategories == 1) {
      plot.new()
      text(
        0.5, 0.5,
        paste0(
          "Only one sub-category (", subcategories[1],
          ")!\nExpand the data set so more can be visible."
        ),
        cex = 1.2
      )
      return()
    }
    
    plot_profit_ranking(
      data,
      n_top = min(input$rank_value, n_subcategories)
    )
  })
  
  # ====================== Numerical Plots ======================
  
  output$histogram <- renderPlot({
    req(filtered())
    validate(
      need(nrow(filtered()$data) > 0, "No data available for these selections."),
      need(
        any(filtered()$data$sales > 0, na.rm = TRUE),
        "No positive sales values available."
      )
    )
    
    plot_sales_histogram(
      filtered()$data,
      bins = input$bins
    )
  })
  
  output$boxPlot <- renderPlot({
    req(filtered())
    validate(
      need(nrow(filtered()$data) > 0, "No data available for these selections.")
    )
    
    plot_discount_boxplot(filtered()$data)
  })
  
  output$heatmap <- renderPlot({
    req(filtered())
    validate(
      need(nrow(filtered()$data) > 0, "No data available for these selections.")
    )
    
    plot_discount_profit_heatmap(filtered()$data)
  })
  
  output$facetWrap <- renderPlot({
    req(filtered())
    validate(
      need(nrow(filtered()$data) > 0, "No data available for these selections.")
    )
    
    plot_shipping_time(filtered()$data)
  })
}


# ============================== Run the application ==============================
shinyApp(ui = ui, server = server)