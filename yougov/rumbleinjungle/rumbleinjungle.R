# Data vizualization 
# https://today.yougov.com/topics/lifestyle/articles-reports/2021/05/13/lions-and-tigers-and-bears-what-animal-would-win-f 
# Rumble in the jungle 



# LOAD DATASET #######

rm(list = ls())
library(tidyverse)
library(ggplot2)
library(ggtext) # For R markdown codes below
library(readxl)

data <- 
    read_xlsx(path = "yougov/rumbleinjungle/rumbleinjungle.xlsx", 
              sheet = "data")




# PIVOT LONGER DATA #####

data_long <- 
    data %>% 
    pivot_longer(!animal, names_to = "sex", values_to = "pct")

# CREATE A AVERAGE PCT ACROSS SEX #####
data_long <- 
    data_long %>% 
    group_by(animal) %>% 
    mutate(avr_pct = mean(pct)) %>% 
    ungroup()

# PLOT #######

## Color theme for all the graphs =====
blue = "#6892C1"
lightblue = "#add8e6" # "#6892C1"
orange = "#ED713F"
#red = "#F53446"
red = "#cc0000"
#color_point <- c(red, blue)
#color_line <- "#7C878EFF"

## Plot
p <- 
    data_long %>% 
    ggplot(data = data_long, mapping = aes(x = pct, y = reorder(factor(animal), avr_pct)))

p + 
    geom_line() +
    geom_point(aes(color = sex), size = 4, alpha = 1) +
    scale_color_manual(values=c(red,blue)) + 
    labs(title = "Humans versus Animals",
         subtitle = "Which of the following animals do you (<b><span style = 'color: #cc0000;'>**female**</span></b>, <b><span style = 'color: #6892C1;'>**male**</span></b>) think you could beat in a fight if you were unarmed?",
         y = "",
         x = "",
         caption = "@GerardChung | gerardchung.com | Data: YouGovAmerica's 'Rumble in the Jungle'") +
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
        axis.text.y = element_text(size = 12, color = "gray20")) +
    ggrepel::geom_text_repel(data = data_long %>% filter(animal == "rat"), 
                             aes(label = paste0(sprintf("%0.0f", pct),"%")),  # paste0(Importance)
                             hjust = "left",
                             fontface = "plain",
                             family = "Roboto Condensed",
                             size = 3,
                             nudge_x = -.9,
                             direction = "y")

ggsave(last_plot(),
       file = "yougov/rumbleinjungle/rumbleinjungle.png",
       type = "cairo")
