# Data Viz 
# Data: DOS Singapore
## https://www.singstat.gov.sg/modules/infographics/population
## Downlink: https://www.tablebuilder.singstat.gov.sg/publicfacing/downloadMultiple.action?id=132 

rm(list= ls())
library(readxl)
library(tidyr)
library(ggplot2)
library(dplyr)
library(ggtext)
library(cowplot)
options(scipen = 999)

# Load data
############
dos <- read_xlsx(path = "dos/hse_hld_child/data/dos_hsehold_child.xlsx", 
                 sheet = "chd_hsehld")

# Reshape to long
####################
doslong <- 
    dos %>%
    pivot_longer(!Variables, names_to = "year", values_to = "num_hsehold")

# Clean vars
############

# Make numeric 
doslong$year <- as.numeric(doslong$year)
table(doslong$year, exclude = F)

# Remove the , for thousands
doslong$num_hsehold <- stringr::str_replace_all(doslong$num_hsehold, ",", "")
doslong$num_hsehold <- as.numeric(doslong$num_hsehold)

# Factor vars
table(doslong$Variables) 
doslong$Variables <- factor(doslong$Variables, levels = c("Resident Households", 
                                                          "Couple-based With Children", 
                                                          "Couple-based Without Children",
                                                          "Living Alone",
                                                          "Lone Parent",
                                                          "Others"),
                                                labels = c("Resident Households",
                                                           "Couple (children)",
                                                           "Couple (no children)",
                                                           "Living Alone",
                                                           "Lone Parent",
                                                           "Others"))
                                                          
doslong$year <- factor(doslong$year)

# Colors 
##########

blue <- "#4e79a7"
ltblue <- "#76b7b2"
red <- "#e15759"
orange <- "#f28e2b"
green <- "59a14f"
grey <- "#bab0bc"


# Slope graph
#############

# Remove total households
pct <- 
    doslong %>% 
    filter(Variables != "Resident Households")

# Create percentage 
pct <- 
    pct %>% 
    group_by(year) %>% 
    mutate(prop_hsehold = num_hsehold/sum(num_hsehold)) %>% 
    mutate(pct_hsehold = prop_hsehold*100) %>% 
    ungroup()

pct$pct_hsehold <- as.numeric(format(pct$pct_hsehold, digits = 0))

# Focus on four years 1990, 2000, 2010, 2018
pct1 <- 
    pct %>% 
    filter(year == 1990 | year == 2000| year == 2010 | year == 2019) %>% 
    filter(Variables != "Others")


# Plot slope
    # https://ibecav.github.io/slopegraph/
q <- ggplot(pct1, mapping = aes(x = year, y = pct_hsehold, group = Variables))
library(ggrepel)
q + geom_line(aes(color = Variables, alpha = 1), size =2) +
    geom_point(aes(color = Variables,  alpha = 1) , size = 4) + 
  labs(title = "",
       subtitle = "Change in % of households: <span style = 'color: #4e79a7;'>**Couple-based (children)**</span>, <span style = 'color: #76b7b2;'>**Lone Parent**</span>, <span style = 'color: #e15759;'>**Living alone**</span>, or <span style = 'color: #f28e2b;'>**Couple-based (no children)**</span>.") +

    # Move x axis to top
    scale_x_discrete(position = "top") + 
  theme_minimal(base_family = "Roboto Condensed") +    # Remove the legend
    theme(legend.position = "none") + 
    # Remove the panel border
    theme(panel.border = element_blank()) + 
    # Remove just about everything from the y axis
    theme(axis.title.y = element_blank()) + 
    theme(axis.text.y = element_blank()) + 
    theme(panel.grid.major.y = element_blank()) +
    theme(panel.grid.minor.y = element_blank()) + 
    # Remove a few things from the x axis and increase font size
    theme(axis.title.x = element_blank()) + 
    theme(panel.grid.major.x = element_blank()) + 
    theme(axis.text.x.top      = element_text(size=12)) +
    # Remove x & y tick marks
    theme(axis.ticks = element_blank()) +
    theme(plot.subtitle = element_markdown(hjust = 2)) +

    geom_text_repel(data = pct1 %>% filter(year == 1990), 
                    aes(label = paste0( pct_hsehold,"%")),
                    hjust = "left",
                    fontface = "bold",
                    family = "Roboto Condensed",
                    size = 3.5,
                    nudge_x = -.05,
                    direction = "y") + 
  geom_text_repel(data = pct1 %>% filter(year == 2000), 
                  aes(label = paste0(pct_hsehold,"%")),
                  hjust = "left",
                  fontface = "bold",
                  family = "Roboto Condensed",
                  size = 3,
                  nudge_x = 0.05,
                  direction = "y") + 
  
  geom_text_repel(data = pct1 %>% filter(year == 2010), 
                  aes(label = paste0(pct_hsehold,"%")),
                  hjust = "left",
                  fontface = "bold",
                  family = "Roboto Condensed",
                  size = 3,
                  nudge_x = 0.05,
                  direction = "y") + 
                        
    geom_text_repel(data = pct1 %>% filter(year == 2019), 
                    aes(label = paste0(Variables,"-", pct_hsehold,"%")),
                    hjust = "right",
                    fontface = "bold",
                    family = "Roboto Condensed",
                    size = 3.5,
                    nudge_x = 2,
                    direction = "y")  +
    scale_color_manual(values=c(blue, orange,red, ltblue)) -> final_1


# Waffle
##########
    # https://rud.is/rpubs/building-waffle-charts.html

# Normal waffle
library(waffle)
library(hrbrthemes)

waf <- 
    doslong %>% 
    filter(year == 2019) %>% 
    filter(Variables != "Resident Households") 

w <- ggplot(data =waf, aes(fill = Variables, values = num_hsehold))

w + geom_waffle(n_rows = 10, size = .10, color = "white", flip =T,
                make_proportional = T) +
  #  expand_limits(x=c(0,0), y=c(0,0)) + 
    coord_equal() + 

    labs(fill = NULL, colour = NULL) +
   # theme_ipsum_rc(grid="") +
   # theme_enhance_waffle() +
    ggthemes::scale_fill_tableau(name=NULL) +
    labs(title = "",
    subtitle =  "In 2019, 47% of total households were <span style = 'color: #4e79a7;'>**households with children**</span><br>",
    caption = "www.gerardchung.com | Codes: https://github.com/gerardchung/ | Source: Department of Statistics SG",
    x = "Year",
    y = "%") + 
   # theme(plot.subtitle = element_markdown()) + 
    guides(fill = guide_legend(reverse = TRUE)) +
  theme_minimal(base_family = "Roboto Condensed") +
#  theme(panel.grid = element_blank(), axis.ticks.y = element_line()) +
#  theme(legend.position = "none") + 
  theme(plot.subtitle = element_markdown(hjust = .2)) +
  theme(axis.title.y = element_blank()) + 
  theme(axis.text.y = element_blank()) + 
  theme(panel.grid.major.y = element_blank()) +
  theme(panel.grid.minor.y = element_blank()) +
  theme(axis.title.x = element_blank()) + 
  theme(axis.text.x = element_blank()) + 
  theme(panel.grid.major.x = element_blank()) +
  theme(panel.grid.minor.x = element_blank()) +
  theme(plot.caption = element_text(hjust = -3, size = 8)) -> final_2



# Combine graphs
##################


# title: https://wilkelab.org/cowplot/articles/plot_grid.html 
title <- ggdraw() + 
  draw_label(
    "Trends in household types from 1990-2019",
    fontface = 'bold',
    fontfamily = "Roboto Condensed",
    size = 14,
    x = 0,
    hjust = 0,
  ) +
  theme(
    # add margin on the left of the drawing canvas,
    # so title is aligned with left edge of first plot
    plot.margin = margin(0, 0, 0, 30)
  )

subtitle <- ggdraw() + 
  draw_label(
    "Households with children is on a decreasing trend but remains the majority. Policies and social services\nneed to plan for increasing needs of households with no children and those living alone\n",
    #  fontface = 'bold',
    fontfamily = "Roboto Condensed",
    size = 10,
    x = 0,
    hjust = 0
  ) +
  theme(
    # add margin on the left of the drawing canvas,
    # so title is aligned with left edge of first plot
    plot.margin = margin(0, 0, 0, 30)
  )


plot_column <- plot_grid(final_1, final_2, ncol = 1, hjust = -3)
plot_grid(title, subtitle,
          plot_column, 
          ncol = 1,
          axis = c("l", "b"),
          rel_heights = c(.01, .001, .1)) # c(0.1, .05, 1))

  
  # https://wilkelab.org/cowplot/articles/aligning_plots.html


