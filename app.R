# ===================== Needed Libs =====================
library(shiny)
library(tidyverse)
library(ggplot2)
library(DT)
library(shinycssloaders)

source("helpers.R")

# ===================== UI helpers =====================

# Conditions that show a sidebar option only for the selected plot/table
cat_condition <- function(tab) {
  paste0("input.eda_tabs == 'Categorical Summaries' && input.cat_tabs == '", tab, "'")
}

num_condition <- function(tab) {
  paste0("input.eda_tabs == 'Numerical Summaries' && input.num_tabs == '", tab, "'")
}

# Optional variables get a "None" choice
none_cat <- c("None" = "none", cat_var)
none_num <- c("None" = "none", num_var)

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
      hr(),
      h4("Filter by Numeric Variables"),
      selectInput(
        "num_filter1",
        "First numeric variable:",
        choices = none_num,
        selected = "none"
      ),
      uiOutput("slider1"),
      selectInput(
        "num_filter2",
        "Second numeric variable:",
        choices = none_num,
        selected = "none"
      ),
      uiOutput("slider2"),
      hr(),
      actionButton("apply", "Apply")
    ),
    mainPanel(
      tabsetPanel(
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
          br(),
          downloadButton("download_data", "Download Data (.csv)"),
          br(), br(),
          textOutput("n_rows"),
          br(),
          withSpinner(DT::dataTableOutput("data_table"))
        ),

        # ------------------------ Data Exploration ------------------------
        tabPanel(
          "Data Exploration",
          sidebarLayout(
            # ---- Data Exploration sidebar: shows only the options for the selected plot ----
            sidebarPanel(
              width = 3,

              # ---------- Categorical Summaries ----------

              # Pie Chart
              conditionalPanel(
                condition = cat_condition("Pie Chart"),
                h4("Plot Options"),
                selectInput(
                  "pieVar",
                  "Categorical variable:",
                  choices = cat_var,
                  selected = "category"
                )
              ),

              # Bar Chart
              conditionalPanel(
                condition = cat_condition("Bar Chart"),
                h4("Plot Options"),
                selectInput(
                  "bar_num",
                  "Numeric variable:",
                  choices = num_var,
                  selected = "profit"
                ),
                selectInput(
                  "bar_group",
                  "Group by:",
                  choices = cat_var,
                  selected = "sub_category"
                ),
                selectInput(
                  "bar_fill",
                  "Color by (optional):",
                  choices = none_cat,
                  selected = "category"
                ),
                radioButtons(
                  "bar_stat",
                  "Summary:",
                  choices = c("Total" = "sum", "Average" = "mean"),
                  selected = "sum"
                )
              ),

              # Bump Chart (slider max is updated by the server to match the subset)
              conditionalPanel(
                condition = cat_condition("Bump Chart"),
                h4("Plot Options"),
                selectInput(
                  "rank_var",
                  "Rank:",
                  choices = rank_var_choices,
                  selected = "sub_category"
                ),
                selectInput(
                  "rank_metric",
                  "Ranked by:",
                  choices = metric_choices,
                  selected = "profit"
                ),
                sliderInput(
                  "rank_value",
                  "Number of groups to display:",
                  min = 1,
                  max = nlevels(superstore$sub_category),
                  value = nlevels(superstore$sub_category),
                  step = 1
                )
              ),

              # Contingency Table
              conditionalPanel(
                condition = cat_condition("Contingency Table"),
                h4("Table Options"),
                selectInput(
                  "tab_var1",
                  "Variable 1:",
                  choices = cat_var,
                  selected = "category"
                ),
                selectInput(
                  "tab_var2",
                  "Variable 2 (makes a two-way table):",
                  choices = none_cat,
                  selected = "none"
                )
              ),

              # ---------- Numerical Summaries ----------

              # Histogram
              conditionalPanel(
                condition = num_condition("Histogram"),
                h4("Plot Options"),
                selectInput(
                  "hist_num",
                  "Numeric variable:",
                  choices = num_var,
                  selected = "sales"
                ),
                selectInput(
                  "hist_fill",
                  "Color by (optional):",
                  choices = none_cat,
                  selected = "none"
                ),
                selectInput(
                  "hist_facet",
                  "Facet by (optional):",
                  choices = none_cat,
                  selected = "none"
                ),
                checkboxInput(
                  "hist_log",
                  "Log scale (drops values <= 0)",
                  value = TRUE
                ),
                sliderInput(
                  "bins",
                  "Number of bins:",
                  min = 1,
                  max = 50,
                  value = 25
                )
              ),

              # Boxplot
              conditionalPanel(
                condition = num_condition("Boxplot"),
                h4("Plot Options"),
                selectInput(
                  "box_num",
                  "Numeric variable:",
                  choices = num_var,
                  selected = "discount"
                ),
                selectInput(
                  "box_cat",
                  "Compare across:",
                  choices = cat_var,
                  selected = "category"
                ),
                selectInput(
                  "box_fill",
                  "Color by (optional):",
                  choices = none_cat,
                  selected = "category"
                ),
                selectInput(
                  "box_facet",
                  "Facet by (optional):",
                  choices = none_cat,
                  selected = "none"
                )
              ),

              # Heatmap
              conditionalPanel(
                condition = num_condition("Heatmap"),
                h4("Plot Options"),
                selectInput(
                  "heat_x",
                  "Columns (x-axis):",
                  choices = cat_var,
                  selected = "discount_level"
                ),
                selectInput(
                  "heat_y",
                  "Rows (y-axis):",
                  choices = cat_var,
                  selected = "sub_category"
                ),
                selectInput(
                  "heat_num",
                  "Numeric variable to average:",
                  choices = num_var,
                  selected = "profit"
                )
              ),

              # Shipping Duration
              conditionalPanel(
                condition = num_condition("Shipping Duration"),
                h4("Plot Options"),
                selectInput(
                  "ship_facet",
                  "Facet by:",
                  choices = cat_var,
                  selected = "ship_mode"
                ),
                selectInput(
                  "ship_fill",
                  "Color by (optional):",
                  choices = none_cat,
                  selected = "none"
                )
              ),

              # Summary Statistics
              conditionalPanel(
                condition = num_condition("Summary Statistics"),
                h4("Table Options"),
                selectInput(
                  "sum_num",
                  "Numeric variable:",
                  choices = num_var,
                  selected = "sales"
                ),
                selectInput(
                  "sum_cat",
                  "Summarize across levels of:",
                  choices = cat_var,
                  selected = "category"
                )
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
                      withSpinner(plotOutput("pieChart"))
                    ),
                    tabPanel(
                      "Bar Chart",
                      withSpinner(plotOutput("barGraph"))
                    ),
                    tabPanel(
                      "Bump Chart",
                      withSpinner(plotOutput("bumpPlot"))
                    ),
                    tabPanel(
                      "Contingency Table",
                      withSpinner(tableOutput("contingencyTable"))
                    )
                  )
                ),

                # ------------------------ Numerical Summaries ------------------------
                tabPanel(
                  "Numerical Summaries",
                  tabsetPanel(
                    id = "num_tabs",
                    tabPanel(
                      "Histogram",
                      withSpinner(plotOutput("histogram"))
                    ),
                    tabPanel(
                      "Boxplot",
                      withSpinner(plotOutput("boxPlot"))
                    ),
                    tabPanel(
                      "Heatmap",
                      withSpinner(plotOutput("heatmap"))
                    ),
                    tabPanel(
                      "Shipping Duration",
                      withSpinner(plotOutput("facetWrap"))
                    ),
                    tabPanel(
                      "Summary Statistics",
                      withSpinner(tableOutput("summaryTable"))
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
  # ignoreNULL = FALSE: start with the full data, then only update on "Apply"
  filtered <- eventReactive(input$apply,
    {
      superstore |>
        apply_filter("region", input$region) |>
        apply_filter("segment", input$segment) |>
        apply_filter("category", input$category) |>
        apply_range_filter(
          input$num_filter1,
          input[[paste0("slider1_", input$num_filter1)]]
        ) |>
        apply_range_filter(
          input$num_filter2,
          input[[paste0("slider2_", input$num_filter2)]]
        )
    },
    ignoreNULL = FALSE
  )

  # The subset, with one shared message if nothing matches the selections
  plot_data <- reactive({
    dat <- filtered()
    validate(
      need(nrow(dat) > 0, "No data available for these selections.")
    )
    dat
  })

  # Keep the bump chart slider in sync with the current subset and the
  # variable being ranked: its max (and default) is the number of groups
  observe({
    req(input$rank_var)
    n_groups <- max(n_distinct(filtered()[[input$rank_var]]), 1)

    updateSliderInput(
      session,
      "rank_value",
      min = 1,
      max = n_groups,
      value = n_groups
    )
  })

  # ================= Dynamic sliders for the numeric filters =================
  # The slider id includes the variable name, so a slider that has not been
  # re-drawn yet can never be applied to the wrong variable.

  output$slider1 <- renderUI({
    req(input$num_filter1 != "none")
    make_range_slider(
      paste0("slider1_", input$num_filter1),
      input$num_filter1,
      superstore
    )
  })

  output$slider2 <- renderUI({
    req(input$num_filter2 != "none")
    make_range_slider(
      paste0("slider2_", input$num_filter2),
      input$num_filter2,
      superstore
    )
  })

  # ====================== Data Download ======================

  output$n_rows <- renderText({
    paste("Rows in current subset:", format(nrow(filtered()), big.mark = ","))
  })

  output$data_table <- DT::renderDataTable(
    {
      filtered()
    },
    options = list(pageLength = 10, scrollX = TRUE)
  )

  output$download_data <- downloadHandler(
    filename = function() {
      paste0("superstore_subset_", Sys.Date(), ".csv")
    },
    content = function(file) {
      write_csv(filtered(), file)
    }
  )

  # ===================== Categorical Plots =====================

  output$pieChart <- renderPlot({
    plot_category_pie(
      plot_data(),
      input$pieVar
    )
  })

  output$barGraph <- renderPlot({
    plot_summary_bar(
      plot_data(),
      num_var = input$bar_num,
      group_var = input$bar_group,
      fill_var = input$bar_fill,
      stat = input$bar_stat
    )
  })

  output$bumpPlot <- renderPlot({
    dat <- plot_data()
    n_groups <- n_distinct(dat[[input$rank_var]])

    validate(
      need(
        n_groups >= 2,
        paste0(
          "Only ", n_groups, " level of ", pretty_name(input$rank_var),
          " in this subset, so there is nothing to rank.\n",
          "Expand the data set so more can be visible."
        )
      )
    )

    plot_ranking(
      dat,
      rank_var = input$rank_var,
      metric = input$rank_metric,
      n_top = min(input$rank_value, n_groups)
    )
  })

  output$contingencyTable <- renderTable({
    dat <- plot_data()

    if (input$tab_var2 == "none") {
      one_way_contingency(dat, input$tab_var1)
    } else {
      validate(
        need(
          input$tab_var1 != input$tab_var2,
          "Please choose two different variables for a two-way table."
        )
      )
      two_way_contingency(dat, input$tab_var1, input$tab_var2)
    }
  })

  # ====================== Numerical Plots ======================

  output$histogram <- renderPlot({
    dat <- plot_data()

    validate(
      need(
        !input$hist_log || any(dat[[input$hist_num]] > 0, na.rm = TRUE),
        "No positive values to show on a log scale. Uncheck the log scale option."
      )
    )

    plot_histogram(
      dat,
      num_var = input$hist_num,
      bins = input$bins,
      fill_var = input$hist_fill,
      facet_var = input$hist_facet,
      log_scale = input$hist_log
    )
  })

  output$boxPlot <- renderPlot({
    plot_numeric_boxplot(
      plot_data(),
      num_var = input$box_num,
      cat_var = input$box_cat,
      fill_var = input$box_fill,
      facet_var = input$box_facet
    )
  })

  output$heatmap <- renderPlot({
    validate(
      need(
        input$heat_x != input$heat_y,
        "Please choose two different variables for the heatmap."
      )
    )

    plot_heatmap(
      plot_data(),
      x_var = input$heat_x,
      y_var = input$heat_y,
      num_var = input$heat_num
    )
  })

  output$facetWrap <- renderPlot({
    plot_shipping_time(
      plot_data(),
      facet_var = input$ship_facet,
      fill_var = input$ship_fill
    )
  })

  output$summaryTable <- renderTable({
    grouped_numerical_summary(
      plot_data(),
      num_var = input$sum_num,
      cat_var = input$sum_cat
    ) |>
      mutate(across(where(is.numeric), ~ round(.x, 2)))
  })
}


# ============================== Run the application ==============================
shinyApp(ui = ui, server = server)