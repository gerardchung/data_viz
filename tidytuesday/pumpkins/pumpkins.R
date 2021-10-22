# Tidytuesday 
## https://github.com/rfordatascience/tidytuesday/blob/master/data/2021/2021-10-19/readme.md 
## Pumpkins

# Get the Data

# Read in with tidytuesdayR package 
# Install from CRAN via: install.packages("tidytuesdayR")
# This loads the readme and all the datasets for the week of interest

# Either ISO-8601 date or year/week works!
#
#tuesdata <- tidytuesdayR::tt_load('2021-10-19')
#tuesdata <- tidytuesdayR::tt_load(2021, week = 43)
#
#pumpkins <- tuesdata$pumpkins

# Or read in the data manually
rm(list = ls())

library(tidyverse)
library(extrafont)
library(ggtext)
library(scales)



pumpkins <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2021/2021-10-19/pumpkins.csv')

pumpkins <- 
    pumpkins %>% 
    filter(!str_detect(place, "Entries")) %>%
    separate(col = id,
             into = c("year", "type"), 
             sep = "-") %>%
    mutate(type = case_when(
        type == "F" ~ "Field Pumpkin",
        type == "P" ~ "Giant Pumpkin",
        type == "S" ~ "Giant Squash",
        type == "W" ~ "Giant Watermelon",
        type == "L" ~ "Long Gourd",
        type == "T" ~ "Tomato",
        TRUE ~ "NA"
    )) %>%
    mutate(place_num = parse_number(place),
           weight_lbs = parse_number(weight_lbs),
           ott = parse_number(ott),
           year = parse_number(year))

# Annotations data
## create the specific labels for large pumpkins in 2014,16, 20, and 21 - smart!
giant_pumpkins <- pumpkins %>%
    filter(type == "Giant Pumpkin") %>%
    filter((year == 2014 & place_num == 1) |
               (year == 2016 & place_num == 1) |
               (year == 2021 & place_num == 1) |
               (year == 2020 & place_num == 1)) %>%
    separate(col = grower_name,
             into = c("last", "first"),
             sep = ", ") %>%
    mutate(grower_name = paste(first, last),
           weight_kg = weight_lbs*0.453592,
           weight_kg_lab = number(weight_kg,  big.mark = ","),
           country = ifelse(country == "United Kingdom", 
                            "the United Kingdom", country),
           label = paste("In", year, grower_name, "from", country, 
                         "grew a pumpkin weighing", weight_kg_lab, "kilogram") %>%
               str_wrap(width = 30)) %>%
    select(year, weight_kg, label)

#### Formatting ####

# Special font
library(showtext)
#font_add_google("roboto condensed")
font_add_google("poppins")
showtext_auto()
font <- "poppins"

label_font <- "poppins"

bcolor <- "#FFFFFF"

fontcolor <- "#000000"

theme_set(theme_classic(base_size = 15, base_family = font))

pumpkins <-
    pumpkins %>% 
    mutate(weight_kg = weight_lbs*0.453592)

#### Plot ####


# Base Plot
ggplot(data = pumpkins %>%
           filter(type == "Giant Pumpkin"),
       mapping = aes(x = year,
                     y = weight_kg)) +
    geom_point(color = "#FF7518",
               position = position_jitter(seed = 42)
               ) +
    scale_y_continuous(label = number_format(big.mark = ","),
                       expand = c(0,0),
                       limits = c(0, 1300),
                       breaks = seq(0, 1300, 100)) +
    scale_x_continuous(breaks = seq(2013, 2021, 1)) +
    coord_cartesian(clip = "off") +
    labs(title = "<b>The Great Giant Pumpkin Weigh-Off.</b>",
         subtitle = "<br>",
         x = "",
         y = "Kilograms",
         caption = "<b>GERARDCHUNG.COM </b>  | <b>Data:</b> BigPumpkins.com | <b>Credits:</b> Jenn Schilling") +
    theme(
          axis.ticks.x = element_blank(),
          panel.grid.minor = element_blank(),
          panel.grid.major = element_blank(),
          
          panel.background = element_rect(fill = bcolor, color = NA),
          plot.background = element_rect(fill = bcolor, color = NA),
          
          axis.title = element_text(size = 30, color = fontcolor),
          axis.text = element_text(size = 30, color = fontcolor),
          axis.ticks = element_line(color = fontcolor),
          
          axis.line = element_line(color = bcolor),
          
          axis.line.x = element_blank(), 
          
          strip.text = element_text(size = 15, color = fontcolor, hjust = 0),
          
          legend.text = element_text(size = 15, color = fontcolor),
          legend.title = element_text(size = 15, color = fontcolor),
          
          plot.title.position = "plot",
          plot.title = element_markdown(size = 55, color = fontcolor),
          
          plot.subtitle = element_markdown(size = 15, color = fontcolor),
          
          plot.caption.position = "plot",
          plot.caption = element_markdown(size = 20, color = fontcolor),
          
          plot.margin = margin(t = 10, r = 10, b = 10, l = 10)) +
    
    # Annotations
    geom_text(data = giant_pumpkins %>% 
                  filter(year == 2014),
              mapping = aes(x = year,
                            y = weight_kg,
                            label = label),
              color = fontcolor,
              family = label_font,
              hjust = 0,
              vjust = 0,
              position = position_nudge(x = -0.5,
                                        y = 100),
              size = 8,
              lineheight = 0.3) +
    annotate("curve",
             x = 2013.7, xend = 2014.2,
             y = 1140, yend = 1060,
             curvature = 0.2, arrow = arrow(length = unit(2, "mm")),
             color = fontcolor, size = 0.5) +   
    geom_text(data = giant_pumpkins %>% 
                  filter(year == 2016),
              mapping = aes(x = year,
                            y = weight_kg,
                            label = label),
              color = fontcolor,
              family = label_font,
              hjust = 0,
              vjust = 0,
              position = position_nudge(x = 0.2,
                                        y = 100),
              size = 8,
              lineheight = 0.3) +
    annotate("curve",
             x = 2016.5, xend = 2016.27,
             y = 1280, yend = 1200,
             curvature = -0.2, arrow = arrow(length = unit(2, "mm")),
             color = fontcolor, size = 0.5)  +   
    geom_text(data = giant_pumpkins %>% 
                  filter(year == 2021),
              mapping = aes(x = year,
                            y = weight_kg,
                            label = label),
              color = fontcolor,
              family = label_font,
              hjust = 1.3,
              vjust = -0.8,
           #   position = position_nudge(x = -1,
           #                             y = 200),
              size = 8,
              lineheight = 0.3) +
    annotate("curve",
             x = 2020.3, xend = 2021,
             y = 1270, yend = 1225,
             curvature = 0.2, arrow = arrow(length = unit(1, "mm")),
             color = fontcolor, size = 0.5)  
# Save

getwd()
ggsave("tidytuesday/pumpkins/giantpumpkins.png",
       plot = last_plot(),
       device = "png",
       width = 10,
       height = 7,
       type = "cairo")



