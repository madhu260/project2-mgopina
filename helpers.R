library(shiny)
library(tidyverse)
library(readxl)
library(janitor)


superstore <- read_excel("data/us_superstore.xls")
glimpse(superstore)

#Row ID                 int                     #Segment                chr (factor)
#Order ID               chr                     #Ship Date              dttm
#Order Date             dttm                    #Ship Mode              chr (factor)

#Customer ID            chr
#Customer Name          chr

#Country                chr (removed)           #Product ID             chr
#City                   chr                     #Category               chr (factor)
#State                  chr                     #Sub-Category           chr (factor)
#Postal Code            int                     #Product Name           chr
#Region                 chr (factor)

#Sales                  dbl                     #Discount               dbl
#Quantity               int                     #Profit                 dbl



superstore <- superstore |>
  select(-Country) |>
  clean_names() |>
  mutate(
    row_id = as.integer(row_id),
    
    order_id = as.character(order_id),                # e.g. CA-2016-152156
    customer_id = as.character(customer_id),          # e.g. CG-12520 (First Initial Last initial)
    product_id = as.character(product_id),            # e.g. FUR-BO-10001798 (Category, )
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

apply_filter <- function(data, var, level) {
  if (level == "All") {
    return(data)
  }
  
  data |>
    filter(.data[[var]] == level)
}

