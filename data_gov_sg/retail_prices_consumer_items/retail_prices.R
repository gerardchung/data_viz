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
    filter(food == "Coffee/tea With Milk (Per Cup)" |
               food == "Fishball Noodle (Per Bowl)" |
               food == "Economical Rice (1 Meat & 2 Vegetables) (Per Plate)" |
               food == "Roti Prata (Plain) (Per Piece)" |
               food == "Fried Carrot Cake (Per Plate)" |
               food == "Ice Kachang (Per Bowl)" |
               food == "Mee Rebus (Per Bowl)" |
               food == "Chicken Nasi Briyani (Per Plate)")
               
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
    



# PLOT LINE GRAPHS ##### 

p <- ggplot(data, aes(x = year, y = value, group = food))

p + geom_smooth(method = "lm", se = F)

p <- ggplot(data, aes(x = year, y = value))

p + geom_smooth(method = "loess", se = F) + 
    facet_wrap(vars(food)) + geom_point()
    #facet_grid()

p <- ggplot(data_wide, aes(x = food, y = pct))

## colors =====
red <- "#ee2536" # singapore flag color https://colorswall.com/palette/18520/ 
white <- "#ffffff"


p + 
geom_col(show.legend = FALSE) +
    # facet_wrap(~id, scales = "free_y") + 
   # facet_wrap(~id) + 
    #  geom_text(
  #  geom_text(data = sentiment_focus %>% filter(id == "Home (1998)"),
  #            aes(label = paste(sentiment)),
  #            vjust = +2, 
  #            #nudge_x = -0.2,
  #            fontface = "bold",
  #            family = "Roboto Condensed",
  #            size = 5) + 
    scale_fill_manual(values = c("#459DE0", red, "orange")) + 
    theme_minimal(base_family = "Roboto Condensed") +
    labs(title = "COMPARING POSITIVE FEELINGS ACROSS THREE NDP SONGS",
         subtitle = "Proportion of feelings anticipation, joy, & trust",
         caption = "gerardchung.com | Codes: https://github.com/gerardchung/ndp2021") +
    theme(rect = element_rect(fill = "#ffffff"),
          panel.background = element_rect(fill = "#ffffff", color = "#ffffff"),
          plot.background = element_rect(fill = "#ffffff", color = "#ffffff"),
          # axis.text.y = element_text(size = 15, hjust = 1, family = "Roboto Condensed", color = "black"),
          axis.text.y = element_blank(),
          axis.text.x = element_blank(),
          axis.title = element_blank(),
          axis.ticks = element_blank(),
          panel.grid.major = element_blank(),
          panel.grid.minor = element_blank(),
          plot.title = ggtext::element_markdown(size = 32, face="bold"),
          plot.subtitle = ggtext::element_markdown(size = 28, face="bold"),
          plot.caption  = element_text(size = 10, hjust = 1, family = "Roboto Condensed", color = "#595959"),
          legend.position = "none",
          strip.text.x = element_text(
              size = 15, face = "bold.italic", family = "Roboto Condensed"
          ))


# Slope graphs 
p <- ggplot(data, aes(x = year, y = value, group = food))
p + 
  geom_line(aes(color = food), size = 1, alpha = 1) +
  geom_point(aes(color = food), size = 5, alpha = .8) +
#  scale_fill_manual(values = c("#459DE0", red, "orange")) + 
  theme_minimal(base_family = "Roboto Condensed") +
  labs(title = "Prices of common hawker food",
       subtitle = "Average from 2014 to 2020",
       caption = "gerardchung.com | Codes: https://github.com/gerardchung/ndp2021") +
  theme(rect = element_rect(fill = "#ffffff"),
        panel.background = element_rect(fill = "#ffffff", color = "#ffffff"),
        plot.background = element_rect(fill = "#ffffff", color = "#ffffff"),
        axis.text.y = element_text(size = 15, hjust = .1, family = "Roboto Condensed", color = "black"),
       # axis.text.y = element_blank(),
       # axis.text.y = element_text(size = 20, family = "Roboto Condensed", color = "black"),
        axis.text.x = element_blank(),
        axis.title = element_blank(),
        axis.ticks = element_blank(),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        plot.title = ggtext::element_markdown(size = 32, face="bold"),
        plot.subtitle = ggtext::element_markdown(size = 28, face="bold"),
        plot.caption  = element_text(size = 10, hjust = 1, family = "Roboto Condensed", color = "#595959"),
       # legend.position = "none",
        strip.text.x = element_text(
          size = 15, face = "bold.italic", family = "Roboto Condensed"
        )) +
  scale_y_continuous(labels = scales::dollar_format()) +
  ggrepel::geom_text_repel(data = data %>% filter(year == "2020"),
                           aes(label = paste(value)),
                           hjust = .3, #hjust = -1.3, 
                           nudge_x = +.2,
                           #fontface = "bold",
                           family = "Roboto Condensed",
                           size = 4)


  
  
    geom_text(data = data %>% filter(year == "2020"),
            aes(label = paste(value)),
            vjust = +2, 
            nudge_x = 0.,
          #  fontface = "bold",
            family = "Roboto Condensed",
            size = 5) +


  


  ggrepel::geom_text_repel(data = data %>% filter(year == "2014"),
                           aes(label = paste(value)),
                           vjust = -0.8, hjust = -1.3, 
                           #nudge_x = +0.9,
                           fontface = "bold",
                           family = "Roboto Condensed",
                           size = 4)

  
  
  
  ggrepel::geom_text_repel(data = data %>% filter(year == "2014"),
            aes(label = paste(value)),
            vjust = -0.8, hjust = -1.3, 
            #nudge_x = +0.9,
            fontface = "bold",
            family = "Roboto Condensed",
            size = 4)
  
ggrepel::geom_text_repel(data = sentiment_wrdcount_bysong %>% filter(id == "9"),
                         aes(label = paste("Home (1998)")),
                         vjust = +2, 
                         direction = "y",
                         min.segment.length = 0,
                         family = "Roboto Condensed",
                         size = 4) + 
  



