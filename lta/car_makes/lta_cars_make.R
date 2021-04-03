# Data viz  
# Data from LTA Singapore #####
    # Description: The original dataset was on the annual motor vehicle 
    # population in Singapore (year 2005-2020). The dataset you are loading 
    # is reduced to only year 2020. 
    # Source: https://datamall.lta.gov.sg/ under “Annual Motor Vehicle 
    # Population by Vehicle Type”

rm(list = ls())
library(tidyverse)
library(ggplot2)
library(janitor)
library(readxl)
library(ggtext)
library(patchwork)

## READ IN DATA ######
data <- readxl::read_xls(path = "lta/car_makes/MVP01-6_Cars_by_make.xls")
glimpse(data)
summary(data)

# view makes that are missing
missing <- data[is.na(data$number),]
data <- data %>% 
    filter(!is.na(number))

## TOP  makes in 2020 ########
rank <- 
    data %>% 
    filter(year == 2020) %>% 
    arrange(desc(number)) # TOYOTA, HONDA, and MERCEDES BENZ, BW are top 4 cars in 2020

## CREATE TOTAL AND PERCENTAGE #####
make <- 
    data %>% 
    group_by(year) %>% 
    mutate(total = sum(number)) %>% 
    filter(make == "TOYOTA" | make == "HONDA" | make == "B.M.W." | make == "MERCEDES BENZ") %>% 
    mutate(pct = number/total) %>% 
    ungroup()

## PLOT #######


### Color theme for all the graphs =====
blue = "#6892C1"
lightblue = "#add8e6" # "#6892C1"
orange = "#ED713F"
red = "#F53446"

color_point <- c( blue, orange, lightblue, red)
color_line <- "#7C878EFF"

### Plot 1 =======
plot1 <- 
    make %>% 
    ggplot(aes(x = number, y = factor(year))) 
 
   
plot1 + 
    geom_point(aes(color = make), size = 6, alpha = .8) +
    geom_line(aes(group = year), size = 2, alpha = .5, color = color_line) +
    scale_color_manual(values = color_point) +
    scale_x_continuous(breaks = seq(from = 0, to = 175000, by = 25000), limits = c(0,175000)) +
    labs(title = "Number of cars by make",
         y = "",
         x = "") +
    guides(color = FALSE, size = FALSE) + 
    theme_classic(base_family = "Roboto Condensed") +
    theme(
        rect = element_rect(fill = "#F5F5F5"),
        panel.background = element_rect(fill = "#F5F5F5", color = "#F5F5F5"),
        plot.background = element_rect(fill = "#F5F5F5", color = "#F5F5F5"),
        plot.title = ggtext::element_markdown(size = 14),
        plot.title.position = "plot",
        plot.subtitle = element_markdown(family = "sans", size = 10, lineheight = 1.2),
        panel.grid.minor=element_blank(),
        panel.grid.major=element_blank(),
        strip.text = element_markdown(size = 12, face = "bold"),
        strip.background = element_blank(),
        axis.line.x = element_line(size = 0.5, colour = "gray20"),
        axis.text.x = element_text(size = 9, color = "gray20"),
        axis.line.y = element_blank(),
        axis.ticks.y = element_blank(),
        axis.text.y = element_text(size = 12, color = "gray20") 
    ) -> p_left
  
### Plot 2 =======
plot2 <- 
    make %>% 
    ggplot(aes(x = pct, y = factor(year)))   

plot2 + 
    geom_point(aes(color = make), size = 6, alpha = .8) +
    geom_line(aes(group = year), size = 2, alpha = .5, color = color_line) +
    scale_color_manual(values = color_point) +
    scale_x_continuous(labels = scales::label_percent(accuracy = 1), 
                       breaks = seq(from = 0, to = .3, by = .05), 
                       limits = c(0,.3)) +
    labs(title = "Percentage of cars by make",
         y = "",
         x = "") +
    guides(color = FALSE, size = FALSE) + 
    theme_classic(base_family = "Roboto Condensed") + 
    theme(
        rect = element_rect(fill = "#F5F5F5"),
        panel.background = element_rect(fill = "#F5F5F5", color = "#F5F5F5"),
        plot.background = element_rect(fill = "#F5F5F5", color = "#F5F5F5"),
        plot.title = ggtext::element_markdown(size = 14),
        plot.title.position = "plot",
        plot.subtitle = element_markdown(family = "sans", size = 10, lineheight = 1.2),
        panel.grid.minor=element_blank(),
        panel.grid.major=element_blank(),
        strip.text = element_markdown(size = 12, face = "bold"),
        strip.background = element_blank(),
        axis.line.x = element_line(size = 0.5, colour = "gray20"),
        axis.text.x = element_text(size = 9, color = "gray20"),
        axis.line.y = element_blank(),
        axis.ticks.y = element_blank(),
        axis.text.y = element_text(size = 12, color = "gray20") 
    ) -> p_right

## COMBINE PLOTS #######

plot <- p_left + p_right

blue = "#6892C1"
lightblue = "#add8e6" # "#6892C1"
orange = "#ED713F"
red = "#F53446"



## ADD TITLE AND OTHERS #####

plot_final <- plot + 
    plot_annotation(
        title = "Annual car population in Singapore for top four car makes (2005-2020)",
        subtitle = "In 2020, the four **most** popular car makes were <b><span style = 'color: #F53446;'>**Toyota**</span></b>, <b><span style = 'color: #ED713F;'>**Honda**</span></b>, <b><span style = 'color: #add8e6;'>**Mercedes**</span></b>, and <b><span style = 'color: #6892C1;'>**B.M.W.**</span></b>. <br>Since 2013, <b><span style = 'color: #add8e6;'>**Mercedes**</span></b> has gained ground over <b><span style = 'color: #6892C1;'>**B.M.W.**</span></b> Nonetheless, <b><span style = 'color: #F53446;'>**Toyota**</span></b> remains the most popular car make <br>since 2005 though its share of the market has been shrinking from 2006. In other words, you still see more<br> <b><span style = 'color: #F53446;'>**Toyota**</span></b> cars on the road. But it is ***increasingly less*** likely that the next car you see will be a <b><span style = 'color: #F53446;'>**Toyota**</span>!</b>" , 
        caption = "@GerardChung | gerardchung.com | Data: Land Transport Authority SG", 
        theme = theme(plot.title = element_markdown(size = 20, family = "Roboto Condensed"),
                      plot.subtitle = element_markdown(family = "Roboto Condensed", size = 12, lineheight = 1.5),
                      plot.caption = element_markdown(colour = color_line))
    ) +
    theme(rect = element_rect(fill = "#F5F5F5"),
          panel.background = element_rect(fill = "#F5F5F5", color = "#F5F5F5"),
          plot.background = element_rect(fill = "#F5F5F5", color = "#F5F5F5"))

#FINAL PLOT ####
plot_final

# SAVE PLOT ######

ggsave("lta/car_makes/LTA_car_make.png", plot = plot_final, type = 'cairo', width = 8, height = 6.5, dpi = 300, units = "in", bg = "#F5F5F5")
