library(tidyverse)
library(readxl)
library(janitor)
library(ggplot2)
library(ggbump)

# ========================== Data info ==========================

# glimpse(superstore)

# Row ID                 int                     #Segment                chr (factor)
# Order ID               chr                     #Ship Date              dttm
# Order Date             dttm                    #Ship Mode              chr (factor)

# Customer ID            chr
# Customer Name          chr

# Country                chr (removed)           #Product ID             chr
# City                   chr                     #Category               chr (factor)
# State                  chr                     #Sub-Category           chr (factor)
# Postal Code            int                     #Product Name           chr
# Region                 chr (factor)

# Sales                  dbl                     #Discount               dbl
# Quantity               int                     #Profit                 dbl


# ======================== Clean up data ========================

superstore <- read_excel("data/us_superstore.xls")

superstore <- superstore |>
  select(-Country) |>
  clean_names() |>
  mutate(
    row_id = as.integer(row_id),
    order_id = as.character(order_id), # e.g. CA-2016-152156
    customer_id = as.character(customer_id), # e.g. CG-12520 (First Initial Last initial)
    product_id = as.character(product_id), # e.g. FUR-BO-10001798 (Category, )
    postal_code = as.integer(postal_code),
    order_date = as.Date(order_date),
    ship_date = as.Date(ship_date),
    ship_mode = factor(ship_mode),
    segment = factor(segment),
    region = factor(region),
    category = factor(category),
    sub_category = factor(sub_category),
    sales = as.double(sales),
    quantity = as.integer(quantity),
    discount = as.double(discount),
    profit = as.double(profit)
  )

cat_var <- c("ship_mode", "segment", "region", "category", "sub_category")

num_var <- c("sales", "quantity", "discount", "profit")

# ======================== Helper Functions ========================

apply_filter <- function(data, var, level) {
  if (level == "All") {
    return(data)
  }

  data |>
    filter(.data[[var]] == level)
}

# ========================= Static Graphs ==========================

# ----------------------- Contingency Tables -----------------------

one_way_contingency <- function(data, cat_var) {
  data |>
    count(.data[[cat_var]], name = "Count", .drop = FALSE) |>
    arrange(desc(Count))
}


two_way_contingency <- function(data, row_var, col_var) {
  data |>
    count(
      .data[[row_var]],
      .data[[col_var]],
      name = "Count",
      .drop = FALSE
    ) |>
    pivot_wider(
      names_from = all_of(col_var),
      values_from = Count,
      values_fill = 0
    )
}

# ----------------------- Numerical Summaries ----------------------

grouped_numerical_summary <- function(data, num_var, cat_var) {
  data |>
    group_by(.data[[cat_var]]) |>
    summarise(
      n = sum(!is.na(.data[[num_var]])),
      mean = mean(.data[[num_var]], na.rm = TRUE),
      median = median(.data[[num_var]], na.rm = TRUE),
      sd = sd(.data[[num_var]], na.rm = TRUE),
      min = min(.data[[num_var]], na.rm = TRUE),
      max = max(.data[[num_var]], na.rm = TRUE),
      .groups = "drop"
    ) |>
    mutate(
      across(
        c(mean, median, sd, min, max),
        ~ ifelse(n == 0, NA_real_, .x)
      )
    )
}

# --------------------------- 0. Pie Chart ----------------------------

plot_category_pie <- function(data, selected_var = "category") {
  
  pie_data <- data |>
    count(.data[[selected_var]], name = "count") |>
    filter(count > 0) |>
    mutate(
      percentage = count / sum(count),
      label = paste0(
        round(percentage * 100, 1),
        "%"
      )
    )
  
  ggplot(
    pie_data,
    aes(
      x = "",
      y = count,
      fill = .data[[selected_var]]
    )
  ) +
    geom_col(width = 1, color = "white") +
    coord_polar(theta = "y") +
    geom_text(
      aes(label = label),
      position = position_stack(vjust = 0.5),
      size = 4
    ) +
    labs(
      title = paste(
        "Distribution of",
        stringr::str_to_title(
          stringr::str_replace_all(selected_var, "_", " ")
        )
      ),
      x = NULL,
      y = NULL,
      fill = stringr::str_to_title(
        stringr::str_replace_all(selected_var, "_", " ")
      )
    ) +
    theme_void() +
    theme(
      plot.title = element_text(
        hjust = 0.5,
        face = "bold"
      )
    )
}

# --------------------------- 1.Histogram ----------------------------

plot_sales_histogram <- function(data, bins = 25) {
  data |>
    filter(sales > 0) |>
    ggplot(aes(x = sales)) +
    geom_histogram(
      bins = bins,
      fill = "blue",
      color = "white"
    ) +
    scale_x_log10() +
    labs(
      title = "Distribution of Order Sales",
      x = "log(Sales) in $",
      y = "Number of Records"
    ) +
    theme_minimal()
}


# --------------------------- 2.Bar Chart ----------------------------

# all (fill = category) vs one category (fill = loss or profit)

plot_profit_by_subcategory <- function(data, selected_category = "All") {
  profit_by_subcategory <- data |>
    group_by(category, sub_category) |>
    summarise(
      total_profit = sum(profit, na.rm = TRUE),
      .groups = "drop"
    )
  
  if (selected_category == "All") {
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
      labs(
        title = "Total Profit by Product Sub-Category",
        x = NULL,
        y = "Total Profit ($)",
        fill = "Category"
      ) +
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
          "TRUE" = "green",
          "FALSE" = "red"
        ),
        guide = "none"
      ) +
      labs(
        title = "Total Profit by Product Sub-Category",
        x = NULL,
        y = "Total Profit ($)"
      ) +
      theme_minimal()
  }
}

# ---------------------------- 3.Box Plot ----------------------------

plot_discount_boxplot <- function(data) {
  ggplot(
    data,
    aes(x = category, y = discount, fill = category)
  ) +
    geom_boxplot(outlier.alpha = 0.3) +
    labs(
      title = "Discount Distribution by Product Category",
      x = "Product Category",
      y = "Discount Rate",
      fill = "Category"
    ) +
    scale_y_continuous(
      labels = scales::label_percent()
    ) +
    theme_minimal(base_size = 12) +
    theme(
      legend.position = "none"
    )
}
# ------------------------- 4.Faceted Plot ---------------------------

plot_shipping_time <- function(data) {
  shipping_data <- data |>
    mutate(
      shipping_days = as.numeric(ship_date - order_date)
    ) |>
    filter(!is.na(shipping_days), shipping_days >= 0)
  
  ggplot(shipping_data, aes(x = shipping_days)) +
    geom_histogram(
      binwidth = 1,
      fill = "steelblue",
      color = "white"
    ) +
    facet_wrap(~ship_mode) +
    labs(
      title = "Shipping Duration by Shipping Mode",
      x = "Shipping time (Shipment date - Order date)",
      y = "Number of Order Lines"
    ) +
    theme_minimal(base_size = 12)
}

# ---------------------------- 5. Bump Chart ----------------------------

plot_profit_ranking <- function(data) {
  
  ranking_data <- data |>
    mutate(year = lubridate::year(order_date)) |>
    group_by(year, sub_category) |>
    summarise(
      total_profit = sum(profit, na.rm = TRUE),
      .groups = "drop"
    ) |>
    group_by(year) |>
    mutate(rank = min_rank(desc(total_profit))) |>
    ungroup()
  
  # let user pick number of categories to show
  top_subcategories <- ranking_data |>
    group_by(sub_category) |>
    summarise(
      overall_profit = sum(total_profit),
      .groups = "drop"
    ) |>
    slice_max(overall_profit, n = 17) |>
    pull(sub_category)
  
  ranking_data |>
    filter(sub_category %in% top_subcategories) |>
    ggplot(aes(x = year, y = rank, color = sub_category,
               group = sub_category)) +
    ggbump::geom_bump(linewidth = 1.2) +
    geom_point(size = 2.5) +
    scale_y_reverse(breaks = 1:17) +
    scale_x_continuous(breaks = sort(unique(ranking_data$year))) +
    labs(
      title = "Product Subcategory Ranked by Profit Over Time",
      x = "Year",
      y = "Profit Rank (1 = Highest)",
      color = "Sub-Category"
    ) +
    theme_minimal(base_size = 13)
}

# ---------------------------- 6.Heatmap ----------------------------
# profits vs discount (does high discounts lead to high profits? sweetspot?)

plot_discount_profit_heatmap <- function(data) {
  
  heatmap_data <- data |>
    mutate(
      discount_level = cut(
        discount,
        breaks = seq(0, 0.9, by = 0.1),
        include.lowest = TRUE,
        right = TRUE
      )
    ) |>
    group_by(sub_category, discount_level) |>
    summarise(
      avg_profit = mean(profit, na.rm = TRUE),
      n = n(),
      .groups = "drop"
    )
  
  ggplot(
    heatmap_data,
    aes(
      x = discount_level,
      y = sub_category,
      fill = avg_profit
    )
  ) +
    geom_tile(color = "white", linewidth = 0.4) +
    scale_fill_gradient2(
      low = "red",
      mid = "grey90",
      high = "green",
      midpoint = 0,
      name = "Average Profit ($)"
    ) +
    labs(
      title = "Discount Levels and Average Profit",
      subtitle = "Red = average loss; green = average profit",
      x = "Discount Level",
      y = "Product Sub-Category"
    ) +
    theme_minimal(base_size = 12) +
    theme(
      axis.text.x = element_text(angle = 45, hjust = 1),
      panel.grid = element_blank()
    )
}