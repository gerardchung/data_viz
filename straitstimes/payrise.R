# Remake ST graph > 22nd Nov 

rm(list = ls())
pacman::p_load("tidyverse", "ggplot2", "readxl", "janitor")


## READ IN DATA ######
getwd()
data <- readxl::read_xlsx(path = "straitstimes/payrise.xlsx")
glimpse(data)
summary(data)

data <-
    data %>% 
    pivot_longer(cols = starts_with("year"), names_to = "year")

data$industry <- factor(data$industry,
                        labels = c("All",
                                   "Banking & finance",
                                   "Technology",
                                   "Logistics",
                                   "Consumer goods",
                                   "Life sciences",
                                   "Aerospace",
                                   "Real estate",
                                   "Chemicals",
                                   "Lifestyle retail")
                        )

library("stringr")

data$year <-    str_replace(data$year, pattern = "year", replacement = "")
data$year <- factor(data$year)

# plot
library(ggtext)
library(scales)
library(showtext)

#font_add_google("roboto condensed")
#font_add_google("poppins")
font_add_google("Dosis", "Dosis")
font_add_google("Lato", "Lato")
font_add_google("Noto Sans", "Noto Sans")

showtext_auto()

font <- "Dosis"
font <- "Lato"
font <- "Noto Sans"
label_font <- "Dosis"
label_font <- "Lato"
label_font <- "Noto Sans"

bcolor <- "#7EC8E3"

fontcolor <- "#000000"

theme_set(theme_classic(base_size = 30, base_family = font))

data %>% 
    ggplot(mapping = aes(x = year, y = value, group = industry)) + 
    geom_point(color = "#FB4570", size = 4) + 
    geom_line(size =1.5, alpha = .7) +
    facet_wrap(vars(industry), nrow = 2, ncol = 5) +
    labs(title = "Actual/budgeted salary increase (%) by industry",
         x = "",
         y = "%",
         caption = "<b>GERARDCHUNG.COM </b>  | <b>Data:</b> ST 22nd Nov") +
    theme(
        axis.ticks.x = element_blank(),
        axis.ticks.y = element_blank(),
        panel.grid.minor = element_blank(),
        panel.grid.major = element_blank(),

       # panel.background = element_rect(fill = bcolor, color = NA),
       # plot.background = element_rect(fill = bcolor, color = NA),
        
        axis.title = element_text(size = 28, color = fontcolor),
        axis.text.x = element_text(size = 13, color = fontcolor),
        axis.text.y = element_blank(),
        axis.ticks = element_line(color = fontcolor),
        
        #axis.line = element_line(color = bcolor),
        
        axis.line.x = element_blank(), 
        axis.line.y = element_blank(), 
        
        strip.text = element_text(size = 15, color = fontcolor, hjust = 0),
        
        legend.text = element_text(size = 15, color = fontcolor),
        legend.title = element_text(size = 15, color = fontcolor),
        
       # plot.title.position = "plot",
        plot.title = element_text(size = 28, color = fontcolor, hjust = 0, face = "bold"),
        
      #  plot.subtitle = element_markdown(size = 15, color = fontcolor),
        
        plot.caption.position = "plot",
        plot.caption = element_markdown(size = 20, color = fontcolor),
        
        ) +
   geom_text(data = data,
                             aes(label = value),
                             color = fontcolor,
                             family = label_font,
                             nudge_x = -.45,
                             nudge_y = -.08,
                             size = 3.5) 

    
    
    ggrepel::geom_text_repel(data = data,
                             aes(label = value),
                             color = fontcolor,
                             family = label_font,
                             nudge_x = -.3,
                             nudge_y = -.05,
                             size = 3.5) 


    
\