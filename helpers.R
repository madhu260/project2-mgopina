library(shiny)
library(tidyverse)
library(readxl)


superstore <- read_excel("data/us_superstore.xls")
glimpse(superstore)

#Row ID                 int
#Order ID               chr
#Order Date             dttm
#Ship Date              dttm
#Ship Mode              chr (factor)
#Customer ID            chr
#Customer Name          chr
#Segment                chr (factor?)
#Country                chr (ck unique?)
#City                   chr
#State                  chr
#Postal Code            int
#Region                 chr (factor)
#Product ID             chr
#Category               chr (factor)
#Sub-Category           chr
#Product Name           chr
#Sales                  dbl
#Quantity               int
#Discount               dbl
#Profit                 dbl