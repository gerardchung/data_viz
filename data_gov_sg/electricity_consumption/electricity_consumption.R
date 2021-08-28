# Data Visualization #
# https://data.gov.sg/dataset/average-monthly-household-electricity-consumption-by-dwelling-type

# Read in data csv #### 
rm(list = ls())
library(tidyverse)
library(janitor)
library(ggplot2)
library(ggtext)
library(scales)

# Color theme for all the graphs ######
blue = "#6892C1"
lightblue = "#add8e6" # "#6892C1"
orange = "#ED713F"

data <- read.csv(file = "data_gov_sg/electricity_consumption/average-monthly-household-electricity-consumption-by-dwelling-type/average-monthly-household-electricity-consumption-by-dwelling-type-2005-to-2020.csv")

# Create by year
library(stringr)

data <- 
    data %>% 
    mutate(year = str_extract_all(month, "\\d{4}", simplify = T)) %>% 
    relocate(year, .after = month)


data$year <- str_extract(data$month, pattern = "\\d{4}", simplify = T)



# Average by housing types ####


