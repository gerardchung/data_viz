# Data vid on highest edu qualifications #####

    # Source: data.gov.sg
    # Read meta data file for more info on dataset

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

data <- read.csv(file = "data_gov_sg/gender_sex/resident-labour-force-aged-15-years-and-over-by-highest-qualification-attained-and-sex.csv")

glimpse(data)

# calculate the total workforce for each year ########
data <- 
    data %>% 
    group_by(year, sex) %>% 
    mutate(total_sex = sum(labour_force))

# calculate the total workforce for each year for each gender #######
data <- 
    data %>% 
    group_by(year) %>% 
    mutate(total = sum(labour_force)) %>% 
    mutate(prop_sex = labour_force/total_sex)

p <- ggplot(data, aes(x = year, y = prop_sex, color = sex))
p + geom_point(size = .7,) +
    geom_line() +
    facet_wrap(~ highest_qualification) + 
    theme_minimal(base_family = "Roboto Condensed") + 
    scale_colour_manual(values = c(orange, blue))
extrafont::fonts()

# Separate datasets - extract primary and degree #######

unique(data$highest_qualification)
degree <- 
    data %>% 
    filter(highest_qualification == "degree") 

primary <- 
    data %>% 
    filter(highest_qualification == "primary and below") 

# Plot 1 - Degree #####

col_point <- c(orange, blue)
col_line <- "#7C878EFF"

degree %>% 
    ggplot(aes(x = prop_sex  , y = as.factor(year))) + 
    geom_line(aes(group = year), size = 2, alpha = .5, color = col_line) + 
    geom_point(aes(color = sex), size = 4, alpha = .7) +
    scale_color_manual(values = col_point, name = "") + 
    scale_x_continuous(labels = scales::label_percent(), limits = c(0,0.40)) + 
    labs(title = "Degree holders as a percentage of male/female population",
         x = "",
         y = "") +
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
        axis.text.x = element_text(size = 12, color = "gray20"),
        axis.line.y = element_blank(),
        axis.ticks.y = element_blank(),
        axis.text.y = element_text(size = 12, color = "gray20") 
    ) -> degree

primary %>% 
    ggplot(aes(x = prop_sex  , y = as.factor(year))) + 
    geom_line(aes(group = year), size = 2, alpha = .5, color = col_line) + 
    geom_point(aes(color = sex), size = 4, alpha = .7) +
    scale_color_manual(values = col_point, name = "") + 
    scale_x_continuous(labels = scales::label_percent(), limits = c(0,0.40)) + 
    labs(title = "Primary school education as a percentage of male/female population",
         x = "",
         y = "") +
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
        axis.text.x = element_text(size = 12, color = "gray20"),
        axis.line.y = element_blank(),
        axis.ticks.y = element_blank(),
        axis.text.y = element_text(size = 12, color = "gray20") 
    ) 




