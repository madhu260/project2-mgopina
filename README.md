# project2-mgopina

# Superstore Data Explorer

A Shiny app for exploring sales data from a retail superstore.

**Deployed app:** https://mgopinath26.shinyapps.io/superstore-analysis/

## Purpose

The app lets a user explore which regions, customer segments, and products drive sales and profit. The user can narrow the data down, look at it in a table, download it, and build numerical and graphical summaries by choosing which variables are plotted, colored, faceted, or summarized.

## Data

The data come from the [Superstore dataset on Kaggle](https://www.kaggle.com/datasets/juhi1994/superstore/data). Each row is one product on one order, with information about the customer, location, product, and sale (sales, quantity, discount, profit) and shipping. The app adds three variables derived from the original columns: `shipping_days` (ship date minus order date), `order_year`, and `discount_level` (discount grouped into bins of 0.1).

## What the App Does

**Sidebar (subsetting).** The data can be subset by region, customer segment, and product category (each with an "All" option), and by two numeric variables of the user's choice (sales, quantity, discount, profit, or shipping days) using range sliders. 
The data only update when the **Apply** button is pressed.

**About tab.** Describes the app, the data, and how to use each part.

**Data Download tab.** Shows the subsetted data in a table and allows the user to save it as a CSV file.

**Data Exploration tab.** The user chooses which variables are plotted and used for coloring, faceting, or summarizing.

- *Categorical Summaries:* pie chart, bar chart (total or average of a numeric variable), bump chart (rankings over time), and one-way and two-way contingency tables.
- *Numerical Summaries:* histogram, boxplot, heatmap, faceted shipping duration plot, and summary statistics (mean, median, standard deviation, min, and max) across the levels of a categorical variable.

## Files

- `app.R`: the Shiny app (user interface and server)
- `helpers.R`: static file. Reads and cleans the data, and defines the numerical and graphical summaries functions used by app.R
- `data/us_superstore.xls`: data
- `www/`: images used on the About tab
