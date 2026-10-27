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
        plotOutput("distPlot"),
        plotOutput("barGraph")
      )
    ))
  )
)

# ================= Server =================
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

# ----------------------- histogram -----------------------
  output$distPlot <- renderPlot({
    req(filtered())
    data <- filtered()$data

    validate(
      need(nrow(data) > 0, "No data available for these selections."),
      need(
        any(data$sales > 0, na.rm = TRUE),
        "No positive sales values available."
      )
    )

    ggplot(filter(data, sales > 0), aes(x = sales)) +
      geom_histogram(
        bins = filtered()$bins,
        fill = "steelblue",
        color = "white"
      ) +
      scale_x_log10() +
      labs(title = "Distribution of Order Sales", x = "Log(Sales) (log $)", y = "Number of Records") +
      theme_minimal(base_size = 13)
  })

# ----------------------- bar graph -----------------------
  output$barGraph <- renderPlot({
    req(filtered())
    data <- filtered()$data

    validate(need(nrow(data) > 0, "No data available for this filter."))

    profit_by_subcategory <- data |>
      group_by(category, sub_category) |>
      summarise(
        total_profit = sum(profit, na.rm = TRUE),
        .groups = "drop"
      )

    # If all categories, color by category. Else profit = green / loss = red

    if (filtered()$category == "All") {
      ggplot(
        profit_by_subcategory,
        aes(
          x = reorder(sub_category, total_profit),
          y = total_profit,
          fill = category
        )
      ) +
        geom_col() +
        geom_hline(yintercept = 0, color = "gray40") +
        coord_flip() +
        labs(title = "Total Profit by Product Sub-Category", x = NULL, y = "Total Profit ($)") +
        theme_minimal()
    } else {
      ggplot(
        profit_by_subcategory,
        aes(
          x = reorder(sub_category, total_profit),
          y = total_profit,
          fill = total_profit > 0
        )
      ) +
        geom_col() +
        geom_hline(yintercept = 0, color = "gray40") +
        coord_flip() +
        scale_fill_manual(
          values = c(
            "TRUE" = "seagreen3",
            "FALSE" = "indianred2"
          ),
          guide = "none"
        ) +
        labs(title = "Total Profit by Product Sub-Category", x = NULL, y = "Total Profit ($)") +
        theme_minimal()
    }
  })
}

# Run the application
shinyApp(ui = ui, server = server)