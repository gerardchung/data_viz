# Tidytuesday
## https://github.com/rfordatascience/tidytuesday/blob/master/data/2021/2021-10-12/readme.md 

rm(list=ls())
library(tidyverse)
library(dplyr)
library(janitor)
library(ggplot2)

# Get the Data

# Read in with tidytuesdayR package 
# Install from CRAN via: install.packages("tidytuesdayR")
# This loads the readme and all the datasets for the week of interest

# Either ISO-8601 date or year/week works!
#
#tuesdata <- tidytuesdayR::tt_load('2021-10-12')
#tuesdata <- tidytuesdayR::tt_load(2021, week = 42)
#
#consumption <- tuesdata$`fish-and-seafood-consumption-per-capita`

# Or read in the data manually

#farmed <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2021/2021-10#-12/aquaculture-farmed-fish-production.csv')
#captured_vs_farmed <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data#/2021/2021-10-12/capture-fisheries-vs-aquaculture.csv')
#
#captured <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2021/2021#-10-12/capture-fishery-production.csv')

consumption <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2021/2021-10-12/fish-and-seafood-consumption-per-capita.csv')

#stock <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2021/2021-10#-12/fish-stocks-within-sustainable-levels.csv')
#fishery <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2021/2021#-10-12/global-fishery-catch-by-sector.csv')
#production <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2021#/2021-10-12/seafood-and-fish-production-thousand-tonnes.csv')

# No singapore
data <- consumption 

data <- data %>% 
    rename("consumption" = "Fish, Seafood- Food supply quantity (kg/capita/yr) (FAO, 2020)")

rm(consumption)
#str_view_all(data$Entity, regex(pattern = "Singapore", ignore_case = T), match = T)
#str_view_all(data$Entity, regex(pattern = "malaysia", ignore_case = T), match = T)

# Remove those with NA values in Code becos these are region and not countries 
data <- data %>% filter(!is.na(Code))

# Check values of each var
unique(data$Entity)
unique(data$Year)
tabyl(data$Year)
unique(data$Code)

# Keep 2017, 2007, 1997, 1987, 1977 
#data %>% 
#    ggplot(aes(x = Year, y = consumption)) + 
#    geom_point() +
#    geom_smooth()    

names(data)
data <- 
    data %>% 
   # filter(Year == 2016 | Year == 2006 | Year == 1996 | Year == 1986 | Year == 1976 | Year == 1966) %>% 
    filter(Year == 2016 | Year == 2006 | Year == 1996 | Year == 1986 | Year == 1976) %>% 
    select(!Code)


# Annotations data ####

## create the specific labels for higghest consumptions pumpkins in 1976, 1996, 2006, 2016
glimpse(data)
data_highest <- 
    data %>% 
    group_by(Year) %>% 
    slice_max(consumption, n =4)

data_highest <-
    data_highest  %>% 
    filter(Year == 2016 | Entity == "Maldives") %>% 
    mutate(label = paste(Entity,",",consumption,"kg") %>% 
               str_wrap(width = 30))
    

# Plot ####

library(ggtext)
library(scales)
library(showtext)

#font_add_google("roboto condensed")
#font_add_google("poppins")
font_add_google("Dosis", "Dosis")
showtext_auto()

font <- "Dosis"
label_font <- "Dosis"

bcolor <- "#7EC8E3"

fontcolor <- "#000000"

theme_set(theme_classic(base_size = 30, base_family = font))


p <- ggplot(data = data, 
            mapping = aes(x = Year, y = consumption) )

p + geom_point(color = "#FB4570", position = position_jitter(seed = 42)) + ##FF7518
    scale_y_continuous(label = number_format(big.mark = ","),
                       expand = c(0,0),
                       limits = c(0, 180),
                       breaks = seq(0, 160, 20)) +
    scale_x_continuous(breaks = seq(1976, 2016, 10)) +
    coord_cartesian(clip = "off") +
    labs(title = "<b>How much fish do people eat?</b>",
         x = "",
         y = "Kilograms Per Person",
    caption = "<b>GERARDCHUNG.COM </b>  | <b>Data:</b> ourworldindata.org") +
    theme(
        axis.ticks.x = element_blank(),
        panel.grid.minor = element_blank(),
        panel.grid.major = element_blank(),
        
        panel.background = element_rect(fill = bcolor, color = NA),
        plot.background = element_rect(fill = bcolor, color = NA),
        
        axis.title = element_text(size = 50, color = fontcolor),
        axis.text = element_text(size = 50, color = fontcolor),
        axis.ticks = element_line(color = fontcolor),
        
        axis.line = element_line(color = bcolor),
        
        axis.line.x = element_blank(), 
        
        strip.text = element_text(size = 15, color = fontcolor, hjust = 0),
        
        legend.text = element_text(size = 15, color = fontcolor),
        legend.title = element_text(size = 15, color = fontcolor),
        
        plot.title.position = "plot",
        plot.title = element_markdown(size = 80, color = fontcolor),
        
        plot.subtitle = element_markdown(size = 15, color = fontcolor),
        
        plot.caption.position = "plot",
        plot.caption = element_markdown(size = 30, color = fontcolor),
        
        plot.margin = margin(t = 10, r = 10, b = 10, l = 10)) +
    
    # Annotations
    
#    ## 1966
#    geom_text(data = data_highest %>% 
#                  filter(Year == 1966),
#              mapping = aes(x = Year,
#                            y = consumption,
#                            label = label),
#              color = fontcolor,
#              family = label_font,
#              hjust = 0,
#              vjust = 0,
#              position = position_nudge(x = -2.99,
#                                        y = 10),
#              size = 8,
#              lineheight = 0.3) +
#    annotate("curve",
#             x = 1962, xend = 1962.9,
#             y = 72.8, yend = 78,
#             curvature = -0.2, arrow = arrow(length = unit(2, "mm")),
#             color = fontcolor, size = 0.5) + 
    
    # 1976
    
    geom_text(data = data_highest %>% 
                  filter(Year == 1976),
              mapping = aes(x = Year,
                            y = consumption,
                            label = label),
              color = fontcolor,
              family = label_font,
              hjust = 0,
              vjust = 0,
              position = position_nudge(x = -3.99,
                                        y = 10),
              size = 15,
              lineheight = 0.3) +
    annotate("curve",
             x = 1977, xend = 1975.9,
             y = 105.8, yend = 112,
             curvature = -.2, arrow = arrow(length = unit(2, "mm")),
             color = fontcolor, size = 0.5) +

    # 1986

    geom_text(data = data_highest %>% 
              filter(Year == 1986),
          mapping = aes(x = Year,
                        y = consumption,
                        label = label),
          color = fontcolor,
          family = label_font,
          hjust = 0,
          vjust = 0,
          position = position_nudge(x = -3.99,
                                    y = 10),
          size = 15,
          lineheight = 0.3) +
    annotate("curve",
             x = 1985.5, xend = 1986,
             y = 125.8, yend = 131.3,
             curvature = +0.2, arrow = arrow(length = unit(2, "mm")),
             color = fontcolor, size = 0.5) +

    # 1996
    
    geom_text(data = data_highest %>% 
                  filter(Year == 1996),
              mapping = aes(x = Year,
                            y = consumption,
                            label = label),
              color = fontcolor,
              family = label_font,
              hjust = 0,
              vjust = 0,
              position = position_nudge(x = -3.99,
                                        y = 10),
              size = 15,
              lineheight = 0.3) +
    annotate("curve",
             x = 1994.8, xend = 1995.8,
             y = 155, yend = 162,
             curvature = +0.1, arrow = arrow(length = unit(2, "mm")),
             color = fontcolor, size = 0.5) +
    
    # 2006
    
    geom_text(data = data_highest %>% 
                  filter(Year == 2006),
              mapping = aes(x = Year,
                            y = consumption,
                            label = label),
              color = fontcolor,
              family = label_font,
              hjust = 0,
              vjust = 0,
              position = position_nudge(x = -3.99,
                                        y = 10),
              size = 15,
              lineheight = 0.3) +
    annotate("curve",
             x = 2003.8, xend = 2003.5,
             y = 111, yend = 116,
             curvature = -0.1, arrow = arrow(length = unit(2, "mm")),
             color = fontcolor, size = 0.5) +
    
    # 2016 - Maldives
    
    geom_text(data = data_highest %>% 
                  filter(Year == 2016 & Entity == "Maldives"),
              mapping = aes(x = Year,
                            y = consumption,
                            label = label),
              color = fontcolor,
              family = label_font,
              hjust = 0,
              vjust = 0,
              position = position_nudge(x = -2.99,
                                        y = 3),
              size = 15,
              lineheight = 0.3) +
    annotate("curve",
             x = 2003.8, xend = 2003.5,
             y = 111, yend = 116,
             curvature = -0.1, arrow = arrow(length = unit(2, "mm")),
             color = fontcolor, size = 0.5) +
    
    # 2016 - Iceland
    
    geom_text(data = data_highest %>% 
                  filter(Year == 2016 & Entity == "Iceland"),
              mapping = aes(x = Year,
                            y = consumption,
                            label = label),
              color = fontcolor,
              family = label_font,
              hjust = 0,
              vjust = 0,
              position = position_nudge(x = -2.99,
                                        y = 3),
              size = 10,
              lineheight = 0.3) +
    annotate("curve",
             x = 2003.8, xend = 2003.5,
             y = 111, yend = 116,
             curvature = -0.1, arrow = arrow(length = unit(2, "mm")),
             color = fontcolor, size = 0.5) +
    
    # 2016 - Kiribati
    
    geom_text(data = data_highest %>% 
                  filter(Year == 2016 & Entity == "Kiribati"),
              mapping = aes(x = Year,
                            y = consumption,
                            label = label),
              color = fontcolor,
              family = label_font,
              hjust = 0,
              vjust = 0,
              position = position_nudge(x = -0.99,
                                        y = 3),
              size = 10,
              lineheight = 0.3) +
    annotate("curve",
             x = 2003.8, xend = 2003.5,
             y = 111, yend = 116,
             curvature = -0.1, arrow = arrow(length = unit(2, "mm")),
             color = fontcolor, size = 0.5) +
    
    # 2016 - Hong Kong 
    
    geom_text(data = data_highest %>% 
                  filter(Year == 2016 & Entity == "Hong Kong"),
              mapping = aes(x = Year,
                            y = consumption,
                            label = label),
              color = fontcolor,
              family = label_font,
              hjust = 0,
              vjust = 0,
              position = position_nudge(x = -4.99,
                                        y = -5),
              size = 10,
              lineheight = 0.3) +
    annotate("curve",
             x = 2003.8, xend = 2003.5,
             y = 111, yend = 116,
             curvature = -0.1, arrow = arrow(length = unit(2, "mm")),
             color = fontcolor, size = 0.5)

getwd()
ggsave("tidytuesday/global_fishing/fish_consumption.png",
       plot = last_plot(),
       device = "png",
       width = 10,
       height = 7,
       type = "cairo")


    



