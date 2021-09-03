# Data vizualization 
## https://data.gov.sg/dataset/average-retail-prices-of-selected-items-annual
## Average Retail Prices Of Selected Consumer Items, Annual


# LOAD DATASET #######

rm(list = ls())
library(tidyverse)
library(ggplot2)
library(ggtext) # For R markdown codes below

getwd()
## Read in data ====
data <- 
    read_csv(file = "data_gov_sg/retail_prices_consumer_items/average-retail-prices-of-selected-items-annual/average-retail-prices-of-selected-consumer-items-annual.csv",
             col_names = c("year", "food", "value"), 
             skip =1)


# CLEAN DATA #####

## descriptives ====
data$value <- as.numeric(data$value)
data <-
    data %>% 
    filter(!is.na(value))

mean(data$value)
max(data$value) - min(data$value)

## Remove years with missing ====
library(janitor)
tabyl(data$year) # year 2014 onwards has full non-missing data

data <- 
    data %>% 
    filter(year >=2014)

tabyl(data$year) 

## number of food 
unique(data$food)

## select only some food 
data <- 
    data %>% 
    filter(food == "Lean Pork, Chilled (Per Kilogram)" |
               food == "Beef, Chilled (Per Kilogram)" |
               food == "Mutton, Chilled (Per Kilogram)" |
               food == "Whole Chicken, Chilled (Per Kilogram)" )
        
        
        
     #   food == "Bananas (Per Kilogram)" |
     #          food == "Papaya (Per Kilogram)" |
     #          food == "Watermelon (Per Kilogram)" |
     #          food == "Grapes (Per Kilogram)" |
     #          food == "Orange (Each)" |
     #          food == "Apple (Each)" |
     #          food == "Pear (Each)")

#  food == "Premium Thai Rice (Per 5 Kilogram)" |
#  food == "Lean Pork, Chilled (Per Kilogram)" |
#  food == "Beef, Chilled (Per Kilogram)" |
#  food == "Mutton, Chilled (Per Kilogram)" |
#  food == "Whole Chicken, Chilled (Per Kilogram)" )
unique(data$food)

## calculate change 
data_wide <- 
    data %>% 
    filter(year == 2014 | year == 2020 | year == 2019)

data_wide <- 
    data_wide %>% 
    pivot_wider(id_cols = "food",
                names_from = "year",
                values_from = "value")


data_wide <- 
    data_wide %>% 
    mutate(change = (`2020` - `2014`),
           pct = change/`2014`*100,
           change1 = (`2019` - `2014`),
           pct1 = change1/`2014`*100)


#data_wide$food <-gsub("\\([^()]*\\)", "", data_wide$food) # remove anything between paentheses
data_wide$food1 <-str_replace(data_wide$food, pattern = "\\([^()]*\\)", replacement = "") # remove anything between paentheses


# dumbbell
## https://yonicd.github.io/ggalt/index.html
## install.packages("ggalt")
## ## install.packages("showtext")
library(ggalt)
library(showtext)
library(ggtext)

# https://github.com/yixuan/showtext
# http://www.r-graph-gallery.com/custom-fonts-in-R-and-ggplot2.html
font_add_google("Pacifico", "pacifico")
font_add_google("Roboto Condensed")
showtext_opts(dpi = 320)
showtext_auto(enable = TRUE)

p <- ggplot(data_wide, aes(y=food1, x = `2014`, xend = `2020`)) 
    
p + geom_dumbbell(size=3, color="#e3e2e1",
                  colour_x = "#5b8124", 
                  colour_xend = "#bad744",
                  dot_guide=F, 
                  dot_guide_size=0.5) +
    labs(x=NULL, y=NULL, title="ggplot2 geom_dumbbell with dot guide") +
    theme_minimal() +
    theme(panel.grid.major.x=element_line(size=0.05))

p + geom_dumbbell(size=3, color="#bad744",
                  colour_x = "#bad744", 
                  colour_xend = "#5b8124",
                  dot_guide=F, 
                  dot_guide_size=0.5) +
 #   theme(panel.grid.major.x=element_line(size=0.05)) + 
    theme_minimal() +
    scale_x_continuous(labels=scales::dollar_format()) +
    labs(title = "Prices of meat per kilogram",
         subtitle = "Changes in prices from 2014 to 2020",
         x = "Price per KG",
         caption = "Data: data.gov.sg | gerardchung.com | Codes: https://github.com/gerardchung/data_viz/tree/main/data_gov_sg/retail_prices_consumer_items") +
    theme(rect = element_rect(fill = "#ffffff"),
          panel.background = element_rect(fill = "#ffffff", color = "#ffffff"),
          plot.background = element_rect(fill = "#ffffff", color = "#ffffff"),
          axis.text.y = element_text(size = 15,  family = "Roboto Condensed", color = "black"),
         # axis.text.y = element_blank(),
         # axis.text.x = element_text(size = 15,  family = "Roboto Condensed", color = "#595959"),
          axis.title = element_blank(),
          axis.title.x = element_text(size = 15,family = "Roboto Condensed", color = "black"),
          axis.ticks = element_blank(),
          panel.grid.major = element_blank(),
          panel.grid.minor = element_blank(),
          plot.title = element_text(size = 35, family = "pacifico", hjust = -0.8),
          plot.subtitle = element_text(size = 25,  family = "Roboto Condensed", color = "black", hjust = -0.65),
          plot.caption  = element_text(size = 7, family = "Roboto Condensed", color = "#595959"),
          # legend.position = "none"
         ) +
    geom_text(data = data_wide ,
          aes(label = paste0("+","$", sprintf("%0.2f", change))),
           hjust = -.3, vjust = -1.5,
          nudge_x = -0.2,
          #  fontface = "bold",
          family = "Roboto Condensed",
          size = 5) -> plot_final
ggsave("data_gov_sg/retail_prices_consumer_items/plots/meatprices.png", plot = plot_final, type = 'cairo', width = 10, height = 6.5, dpi = 300, units = "in", bg = "#ffffff")    
    
          
