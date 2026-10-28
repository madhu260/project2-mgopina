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
    profit = as.double(profit),
    
    # ---- derived variables (so they can be chosen in the app) ----
    shipping_days = as.numeric(ship_date - order_date),
    order_year = factor(lubridate::year(order_date)),
    discount_level = cut(
      discount,
      breaks = seq(0, 0.9, by = 0.1),
      include.lowest = TRUE,
      right = TRUE
    )
  )

# Variables the user can pick in the app.
# Names are the labels shown in the dropdowns; values are the column names.
cat_var <- c(
  "Ship Mode" = "ship_mode",
  "Segment" = "segment",
  "Region" = "region",
  "Category" = "category",
  "Sub-Category" = "sub_category",
  "Order Year" = "order_year",
  "Discount Level" = "discount_level"
)

num_var <- c(
  "Sales" = "sales",
  "Quantity" = "quantity",
  "Discount" = "discount",
  "Profit" = "profit",
  "Shipping Days" = "shipping_days"
)

# Subsets of the choices used by the bump chart
rank_var_choices <- cat_var[c("Sub-Category", "Category", "Region", "Segment", "Ship Mode")]
metric_choices <- num_var[c("Profit", "Sales", "Quantity")]

# ======================== Helper Functions ========================

apply_filter <- function(data, var, level) {
  if (level == "All") {
    return(data)
  }
  
  data |>
    filter(.data[[var]] == level)
}

# "sub_category" -> "Sub Category"
pretty_name <- function(x) {
  stringr::str_to_title(stringr::str_replace_all(x, "_", " "))
}

# Add facets only when the user picked a facet variable
add_facet <- function(plot, facet_var) {
  if (facet_var == "none") {
    return(plot)
  }
  
  plot + facet_wrap(as.formula(paste("~", facet_var)))
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
      title = paste("Distribution of", pretty_name(selected_var)),
      x = NULL,
      y = NULL,
      fill = pretty_name(selected_var)
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

plot_histogram <- function(data, num_var, bins = 25, fill_var = "none",
                           facet_var = "none", log_scale = FALSE) {
  
  if (log_scale) {
    data <- data |>
      filter(.data[[num_var]] > 0)
  }
  
  p <- ggplot(data, aes(x = .data[[num_var]]))
  
  if (fill_var == "none") {
    p <- p +
      geom_histogram(bins = bins, fill = "blue", color = "white")
  } else {
    p <- p +
      geom_histogram(aes(fill = .data[[fill_var]]), bins = bins, color = "white") +
      labs(fill = pretty_name(fill_var))
  }
  
  if (log_scale) {
    p <- p + scale_x_log10()
  }
  
  p <- add_facet(p, facet_var)
  
  p +
    labs(
      title = paste("Distribution of", pretty_name(num_var)),
      x = if (log_scale) paste(pretty_name(num_var), "(log scale)") else pretty_name(num_var),
      y = "Number of Records"
    ) +
    theme_minimal()
}


# --------------------------- 2.Bar Chart ----------------------------

# Total (or average) of a numeric variable for each level of a categorical
# variable, optionally split by a second categorical variable (fill).
# With profit and no fill variable, bars are green (profit) or red (loss).

plot_summary_bar <- function(data, num_var, group_var, fill_var = "none",
                             stat = "sum") {
  
  summary_fn <- if (stat == "mean") mean else sum
  stat_label <- if (stat == "mean") "Average" else "Total"
  group_vars <- unique(c(group_var, if (fill_var != "none") fill_var))
  
  bar_data <- data |>
    group_by(across(all_of(group_vars))) |>
    summarise(
      value = summary_fn(.data[[num_var]], na.rm = TRUE),
      .groups = "drop"
    )
  
  p <- ggplot(
    bar_data,
    aes(
      x = forcats::fct_reorder(.data[[group_var]], value, .fun = summary_fn),
      y = value
    )
  )
  
  if (fill_var != "none") {
    p <- p +
      geom_col(aes(fill = .data[[fill_var]])) +
      labs(fill = pretty_name(fill_var))
  } else if (num_var == "profit") {
    p <- p +
      geom_col(aes(fill = value > 0)) +
      scale_fill_manual(
        values = c("TRUE" = "green", "FALSE" = "red"),
        guide = "none"
      )
  } else {
    p <- p + geom_col(fill = "steelblue")
  }
  
  p +
    geom_hline(yintercept = 0, color = "gray40") +
    coord_flip() +
    labs(
      title = paste(stat_label, pretty_name(num_var), "by", pretty_name(group_var)),
      x = NULL,
      y = paste(stat_label, pretty_name(num_var))
    ) +
    theme_minimal()
}

# ---------------------------- 3.Box Plot ----------------------------

plot_numeric_boxplot <- function(data, num_var, cat_var, fill_var = "none",
                                 facet_var = "none") {
  
  p <- ggplot(data, aes(x = .data[[cat_var]], y = .data[[num_var]]))
  
  if (fill_var == "none") {
    p <- p +
      geom_boxplot(fill = "steelblue", outlier.alpha = 0.3)
  } else {
    p <- p +
      geom_boxplot(aes(fill = .data[[fill_var]]), outlier.alpha = 0.3) +
      labs(fill = pretty_name(fill_var))
  }
  
  # discount is a proportion, so show it as a percent
  if (num_var == "discount") {
    p <- p + scale_y_continuous(labels = scales::label_percent())
  }
  
  p <- add_facet(p, facet_var)
  
  p <- p +
    labs(
      title = paste(pretty_name(num_var), "by", pretty_name(cat_var)),
      x = pretty_name(cat_var),
      y = pretty_name(num_var)
    ) +
    theme_minimal(base_size = 12) +
    theme(axis.text.x = element_text(angle = 45, hjust = 1))
  
  # the legend would just repeat the x-axis
  if (fill_var == cat_var) {
    p <- p + theme(legend.position = "none")
  }
  
  p
}

# ------------------------- 4.Faceted Plot ---------------------------

plot_shipping_time <- function(data, facet_var = "ship_mode", fill_var = "none") {
  
  shipping_data <- data |>
    filter(!is.na(shipping_days), shipping_days >= 0)
  
  p <- ggplot(shipping_data, aes(x = shipping_days))
  
  if (fill_var == "none") {
    p <- p +
      geom_histogram(binwidth = 1, fill = "steelblue", color = "white")
  } else {
    p <- p +
      geom_histogram(aes(fill = .data[[fill_var]]), binwidth = 1, color = "white") +
      labs(fill = pretty_name(fill_var))
  }
  
  p <- add_facet(p, facet_var)
  
  p +
    labs(
      title = paste("Shipping Duration by", pretty_name(facet_var)),
      x = "Shipping time (Shipment date - Order date)",
      y = "Number of Order Lines"
    ) +
    theme_minimal(base_size = 12)
}

# ---------------------------- 5. Bump Chart ----------------------------

# Ranks the levels of a categorical variable (rank_var) by a numeric metric
# for each year, and shows the n_top levels with the highest overall total.

plot_ranking <- function(data, rank_var = "sub_category", metric = "profit",
                         n_top = 10) {
  
  ranking_data <- data |>
    mutate(year = lubridate::year(order_date)) |>
    group_by(year, .data[[rank_var]]) |>
    summarise(
      total = sum(.data[[metric]], na.rm = TRUE),
      .groups = "drop"
    ) |>
    group_by(year) |>
    mutate(rank = min_rank(desc(total))) |>
    ungroup()
  
  top_groups <- ranking_data |>
    group_by(.data[[rank_var]]) |>
    summarise(
      overall = sum(total, na.rm = TRUE),
      .groups = "drop"
    ) |>
    slice_max(order_by = overall, n = n_top, with_ties = FALSE) |>
    pull(1)
  
  ranking_data |>
    filter(.data[[rank_var]] %in% top_groups) |>
    ggplot(aes(x = year, y = rank, color = .data[[rank_var]],
               group = .data[[rank_var]])) +
    ggbump::geom_bump(linewidth = 1.2) +
    geom_point(size = 2.5) +
    scale_y_reverse(breaks = seq_len(max(ranking_data$rank))) +
    scale_x_continuous(breaks = sort(unique(ranking_data$year))) +
    labs(
      title = paste(pretty_name(rank_var), "Ranked by", pretty_name(metric), "Over Time"),
      x = "Year",
      y = paste(pretty_name(metric), "Rank (1 = Highest)"),
      color = pretty_name(rank_var)
    ) +
    theme_minimal(base_size = 13)
}

# ---------------------------- 6.Heatmap ----------------------------
# Average of a numeric variable for every combination of two categorical
# variables (default: does a high discount lead to high profit? sweet spot?)

plot_heatmap <- function(data, x_var = "discount_level", y_var = "sub_category",
                         num_var = "profit") {
  
  heatmap_data <- data |>
    group_by(.data[[x_var]], .data[[y_var]]) |>
    summarise(
      avg_value = mean(.data[[num_var]], na.rm = TRUE),
      .groups = "drop"
    )
  
  p <- ggplot(
    heatmap_data,
    aes(
      x = .data[[x_var]],
      y = .data[[y_var]],
      fill = avg_value
    )
  ) +
    geom_tile(color = "white", linewidth = 0.4)
  
  if (num_var == "profit") {
    # red / green only makes sense for profit, where 0 is the break-even point
    p <- p +
      scale_fill_gradient2(
        low = "red",
        mid = "grey90",
        high = "green",
        midpoint = 0,
        name = "Average Profit ($)"
      )
    subtitle <- "Red = average loss; green = average profit"
  } else {
    p <- p +
      scale_fill_viridis_c(name = paste("Average", pretty_name(num_var)))
    subtitle <- NULL
  }
  
  p +
    labs(
      title = paste("Average", pretty_name(num_var), "by", pretty_name(x_var),
                    "and", pretty_name(y_var)),
      subtitle = subtitle,
      x = pretty_name(x_var),
      y = pretty_name(y_var)
    ) +
    theme_minimal(base_size = 12) +
    theme(
      axis.text.x = element_text(angle = 45, hjust = 1),
      panel.grid = element_blank()
    )
}