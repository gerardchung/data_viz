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
dos <- read_xlsx(path = "dos/data/dos_hsehold_child.xlsx", 
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

glimpse(doslong)

blue <- "#4e79a7"
ltblue <- "#76b7b2"
red <- "#e15759"
orange <- "#f28e2b"
green <- "59a14f"
grey <- "#bab0bc"


# Spagetti plot 
##################
doslong <- 
    doslong %>% 
    mutate(Variables2 = Variables)

p <- ggplot(data = doslong, mapping = aes(x=year, y = num_hsehold))

# With grayed out lines
## https://www.data-to-viz.com/caveat/spaghetti.html 
p + geom_line(data = doslong %>% select(-Variables), aes(group = Variables2), color = "grey", size = .5, alpha = .5 ) +
    geom_line(aes(group=Variables), color = "#6892C1", size = 1.2) +
    facet_wrap(~Variables) + 
    theme_minimal() +
    theme(
        legend.position="none",
        plot.title = element_text(size=14),
       # panel.grid = element_blank()
    ) +
    ggtitle("Change in number of households from 1990-2019")
 
library(gghighlight)
p + geom_line(aes(color = Variables, alpha = .5), color = "grey", size = .5) +
    geom_line(aes(group=Variables), color = "#6892C1", size = 1.2) +
    gghighlight(label_key = F) +
    facet_wrap(~Variables)

p + geom_line(aes(group = Variables), color = "#6892C1", size = 1.2) +
    facet_wrap(~Variables)


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
       subtitle = "Percentages of households that are <span style = 'color: #4e79a7;'>**Couple-based (has children)**</span>, <span style = 'color: #76b7b2;'>**Lone Parent**</span>, <span style = 'color: #e15759;'>**Living alone**</span>, or<br><span style = 'color: #f28e2b;'>**Couple-based (no children)**</span>.<br><br>") +

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
    theme(plot.subtitle = element_markdown()) +
    
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
                    nudge_x = .7,
                    direction = "y")  +
    scale_color_manual(values=c(blue, orange,red, ltblue)) -> final_1


#q2 + geom_line(aes(color = Variables, alpha = 1), size =2) + 
#  scale_color_manual(values=c(orange,red, ltblue)) + 
#  geom_point(aes(color = Variables,  alpha = 1) , size = 4) + 
#  labs(
#    subtitle = "Changes in the percentage of households that are <span style = 'color: #76b7b2;'>**Lone Parent**</span>, <br><span #style = 'color: #f28e2b;'>**Couple-based (no children)**</span>, or <span style = 'color: #e15759;'>**Living alone**</span><br>") +
#  # Move x axis to top
#  scale_x_discrete(position = "top") + 
#  theme_minimal(base_family = "Roboto Condensed") +
#  # Remove the legend
#  theme(legend.position = "none") + 
#  # Remove the panel border
#  theme(panel.border = element_blank()) + 
#  # Remove just about everything from the y axis
#  theme(axis.title.y = element_blank()) + 
#  theme(axis.text.y = element_blank()) + 
#  theme(panel.grid.major.y = element_blank()) +
#  theme(panel.grid.minor.y = element_blank()) + 
#  # Remove a few things from the x axis and increase font size
#  theme(axis.title.x = element_blank()) + 
#  theme(panel.grid.major.x = element_blank()) + 
#  theme(axis.text.x.top      = element_text(size=12)) +
#  # Remove x & y tick marks
#  theme(axis.ticks = element_blank()) +
#  theme(plot.subtitle = element_markdown()) +
#  
#  geom_text_repel(data = pct2 %>% filter(year == 1990), 
#                  aes(label = paste0(Variables,"-", pct_hsehold,"%")),
#                  hjust = "left",
#                  fontface = "bold",
#                  family = "Roboto Condensed",
#                  size = 3,
#                  nudge_x = -.6,
#                  direction = "y") + 
#  geom_text_repel(data = pct2 %>% filter(year == 2000), 
#                  aes(label = paste0(pct_hsehold,"%")),
#                  hjust = "left",
#                  fontface = "bold",
#                  family = "Roboto Condensed",
#                  size = 3,
#                  nudge_x = 0.05,
#                  direction = "y") + 
#  geom_text_repel(data = pct2 %>% filter(year == 2010), 
#                  aes(label = paste0(pct_hsehold,"%")),
#                  hjust = "left",
#                  fontface = "bold",
#                  family = "Roboto Condensed",
#                  size = 3,
#                  nudge_x = 0.05,
#                  direction = "y") + 
#  
#  geom_text_repel(data = pct2 %>% filter(year == 2019), 
#                  aes(label = paste0( pct_hsehold,"%")),
#                  hjust = "right",
#                  fontface = "bold",
#                  family = "Roboto Condensed",
#                  size = 3,
#                  nudge_x = .2,
#                  direction = "y") -> final_1


    
## Focus on four years 1990, 2000, 2010, 2018 & exclude Couple(with children) & Others
## =============================================================================
#
## Focus on four years 1990, 2000, 2010, 2018 & exclude children & others
#pct2 <- 
#    pct %>% 
#    filter(year == 1990 | year == 2000| year == 2010 | year == 2019) %>% 
#    filter(Variables != "Couple (children)" & Variables != "Others")
#
#levels(pct2$Variables)
#pct2$Variables <- factor(pct2$Variables, c(
#                                           "Couple (no children)", 
#                                           "Living Alone", 
                                           "Lone Parent"))

## Plot slope
## https://ibecav.github.io/slopegraph/
## colors: https://jrnold.github.io/ggthemes/reference/tableau_color_pal.html 
#q2 <- ggplot(pct2, mapping = aes(x = year, y = pct_hsehold, group = Variables))
#library(ggrepel)
#
#
#
#levels(pct2$Variables)
#
#
#q2 + geom_line(aes(color = Variables, alpha = 1), size =2) + 
#    scale_color_manual(values=c(orange,red, ltblue)) + 
#    geom_point(aes(color = Variables,  alpha = 1) , size = 4) + 
#    labs(title = "Percentage of total households",
#         subtitle = "<span style = 'color: #76b7b2;'>**Lone Parent**</span>, <span style = 'color: #f28e2b;'>**Couple-based (no #children)**</span>, and <span style = 'color: #e15759;'>**Living alone**</span> households from 2000 to 2020<br>") +
#    # Move x axis to top
#    scale_x_discrete(position = "top") + 
#    theme_bw() +
#    # Remove the legend
#    theme(legend.position = "none") + 
#    # Remove the panel border
#    theme(panel.border = element_blank()) + 
#    # Remove just about everything from the y axis
#    theme(axis.title.y = element_blank()) + 
#    theme(axis.text.y = element_blank()) + 
#    theme(panel.grid.major.y = element_blank()) +
#    theme(panel.grid.minor.y = element_blank()) + 
#    # Remove a few things from the x axis and increase font size
#    theme(axis.title.x = element_blank()) + 
#    theme(panel.grid.major.x = element_blank()) + 
#    theme(axis.text.x.top      = element_text(size=12)) +
#    # Remove x & y tick marks
#    theme(axis.ticks = element_blank()) +
#    theme(plot.subtitle = element_markdown()) +
#    
#    geom_text_repel(data = pct2 %>% filter(year == 1990), 
#                    aes(label = paste0(Variables,"-", pct_hsehold,"%")),
#                    hjust = "left",
#                    fontface = "bold",
#                    size = 3,
#                    nudge_x = -.6,
#                    direction = "y") + 
#    
#    geom_text_repel(data = pct2 %>% filter(year == 2019), 
#                    aes(label = paste0( pct_hsehold,"%")),
#                    hjust = "right",
#                    fontface = "bold",
#                    size = 3,
#                    nudge_x = .5,
#                    direction = "y") 
#
#
#levels(pct2$Variables)


## Focus on four years 1990, 2000, 2010, 2018 & Only on Couple(with children)
## =============================================================================
#
## Focus on four years 1990, 2000, 2010, 2018 & Only on Couple(with children)
#pct3 <- 
#    pct %>% 
#    filter(year == 1990 | year == 2000| year == 2010 | year == 2019) %>% 
#    filter(Variables == "Couple (children)")
#
#
## Plot slope
## https://ibecav.github.io/slopegraph/
#q3 <- ggplot(pct3, mapping = aes(x = year, y = pct_hsehold, group = Variables))
#library(ggrepel)
#q3 + geom_line(aes(color = Variables, alpha = 1), size =2) +
#    geom_point(aes(color = Variables,  alpha = 1) , size = 4) + 
#    labs(title = "Percentage of total households",
#         subtitle = "1990-2019") + 
#    # Move x axis to top
#    scale_x_discrete(position = "top") + 
#    theme_bw() +
#    # Remove the legend
#    theme(legend.position = "none") + 
#    # Remove the panel border
#    theme(panel.border = element_blank()) + 
#    # Remove just about everything from the y axis
#    theme(axis.title.y = element_blank()) + 
#    theme(axis.text.y = element_blank()) + 
#    theme(panel.grid.major.y = element_blank()) +
#    theme(panel.grid.minor.y = element_blank()) + 
#    # Remove a few things from the x axis and increase font size
#    theme(axis.title.x = element_blank()) + 
#    theme(panel.grid.major.x = element_blank()) + 
#    theme(axis.text.x.top      = element_text(size=12)) +
#    # Remove x & y tick marks
#    
#    geom_text_repel(data = pct3 %>% filter(year == 1990), 
#                    aes(label = paste0(Variables,"-", pct_hsehold,"%")),
#                    hjust = "left",
#                    fontface = "bold",
#                    size = 3,
#                    nudge_x = -.6,
#                    direction = "y") + 
#    
#    geom_text_repel(data = pct3 %>% filter(year == 2019), 
#                    aes(label = paste0(Variables,"-", pct_hsehold,"%")),
#                    hjust = "right",
#                    fontface = "bold",
#                    size = 3,
#                    nudge_x = .5,
#                    direction = "y")  +
#    scale_color_manual(values=blue) 
#
#
#
#
#


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
    x = "Year",
    y = "%") + 
    theme(plot.subtitle = element_markdown()) + 
    guides(fill = guide_legend(reverse = TRUE)) +
  theme_minimal(base_family = "Roboto Condensed") +
#  theme(panel.grid = element_blank(), axis.ticks.y = element_line()) +
#  theme(legend.position = "none") + 
  theme(plot.subtitle = element_markdown()) +
  theme(axis.title.y = element_blank()) + 
  theme(axis.text.y = element_blank()) + 
  theme(panel.grid.major.y = element_blank()) +
  theme(panel.grid.minor.y = element_blank()) +
  theme(axis.title.x = element_blank()) + 
  theme(axis.text.x = element_blank()) + 
  theme(panel.grid.major.x = element_blank()) +
  theme(panel.grid.minor.x = element_blank()) -> final_2

# title: https://wilkelab.org/cowplot/articles/plot_grid.html 
title <- ggdraw() + 
  draw_label(
    "Trends in household types from 1990-2019",
    fontface = 'bold',
    fontfamily = "Roboto Condensed",
    x = 0,
    hjust = 0
  ) +
  theme(
    # add margin on the left of the drawing canvas,
    # so title is aligned with left edge of first plot
    plot.margin = margin(0, 0, 0, 7)
  )

subtitle <- ggdraw() + 
  draw_label(
    "Households with children is on a decreasing trend but remains the majority. Policies and social services\nneed to plan for increasing needs of households with no children and those living alone\n",
    #  fontface = 'bold',
    fontfamily = "Roboto Condensed",
    size = 12,
    x = 0,
    hjust = 0
  ) +
  theme(
    # add margin on the left of the drawing canvas,
    # so title is aligned with left edge of first plot
    plot.margin = margin(0, 0, 0, 7)
  )


plot_column <- plot_grid(final_1, final_2, ncol = 1)
plot_grid(title, subtitle,
          plot_column,
          ncol = 1,
          rel_heights = c(0.1, .0001, 1))  # c(0.1, .05, 1))

  
#
#
#
## Waffle with facet_wrap with all levels
#
#
#waf1 <- doslong %>%
#    filter(year == "1990" | year == "2000" | year == "2010" | year == "2019") %>% 
#    filter(Variables != "Resident Households" & Variables != "Others") 
#
#waf1 <- waf1 %>% 
#    mutate(Variables3 = Variables) 
#
#waf1$Variables3 <- factor(waf1$Variables3, levels = c("Couple (children)", 
#                                                      "Couple (no children)",
#                                                      "Living Alone" ,
#                                                      "Lone Parent"))
#
#janitor::tabyl(waf1$Variables3)
#
#
#w2 <- ggplot(data = waf1, aes(fill = Variables3, values = num_hsehold)) 
#
#w2 + geom_waffle(n_rows = 5, size = .25, color = "white", flip = T , make_proportional = T) + 
#    facet_wrap(~year, nrow = 1, strip.position = "bottom") + 
#    scale_x_discrete() +
#    scale_y_continuous(labels = function(x) x * 10, # make this multiplyer the same as n_rows
#                       expand = c(0,0)) +
#    coord_equal() +
#    labs(title = "Percentage of Households with Children",
#         subtitle = "1990-2019",
#         x = "Year",
#         y = "# of Households") + 
#    theme_minimal(base_family = "Roboto Condensed") +
#    theme(panel.grid = element_blank(), axis.ticks.y = element_line()) +
#    guides(fill = guide_legend(reverse = TRUE)) +  
#    scale_fill_manual(values=c(blue, orange,red, ltblue)) 
#
## Waffle with facet_wrap only Couple with children
#
#
#waf1 <- doslong %>%
#        filter(year == "1990" | year == "2000" | year == "2010" | year == "2019") %>% 
#        group_by(year) %>% 
#        mutate(total = sum(num_hsehold)) %>% 
#        ungroup() %>% 
#        mutate(num_other = total - num_hsehold) 
#       # 
#  
#waf_waffle <- 
#  waf1 %>% 
#  select(-total) %>% 
#  filter(Variables == "Couple (children)")
#  
#waf1 <- waf1 %>% 
#        mutate(Variables3 = Variables) 
#
#waf1$Variables3 <- recode(waf1$Variables3, "Couple (children)" = "Couple (children)", 
#                                            .default = "Others")
#
#waf1$Variables3 <- factor(waf1$Variables3, levels = c("Couple (children)", 
#                                                      "Others"))
#        
#janitor::tabyl(waf1$Variables3)
#janitor::tabyl(waf1$Variables2)
#
#
#w2 <- ggplot(data = waf1, aes(fill = Variables3, values = num_hsehold)) 
#
#w2 + geom_waffle(n_rows = 10, size = .25, color = "white", flip = T , make_proportional = T) + 
#     facet_wrap(~year, nrow = 1, strip.position = "bottom") + 
#     scale_x_discrete() +
#    # scale_y_continuous(# labels = function(x) x * 10, # make this multiplyer the same as n_rows
#                      #  expand = c(0,0)) +
#     coord_equal() +
#     labs(title = "Percentage of Households with Children",
#          subtitle = "1990-2019",
#          x = "Year",
#          y = "%") + 
#    theme_minimal(base_family = "Roboto Condensed") +
#    theme(panel.grid = element_blank(), axis.ticks.y = element_line()) +
#    theme(legend.position = "none") + 
#    guides(fill = guide_legend(reverse = TRUE)) +  
#    scale_fill_manual(values=c(blue, grey)) 
#    
#     
#   # ggthemes::scale_fill_tableau(name=NULL, direction = 1) +
#    
#    
#levels(waf1$Variables3)   
#waf1$Variables3 <- factor(waf1$Variables3, levels = c("Couple (children)",
#                                                      "Couple (no children)" ,
#                                                      "Living Alone/Lone parent/Others"))
#    


# Combine plots using cowplot package

#q2 + geom_line(aes(color = Variables, alpha = 1), size =2) + 
#    scale_color_manual(values=c(orange,red, ltblue)) + 
#    geom_point(aes(color = Variables,  alpha = 1) , size = 4) + 
#    labs(subtitle = "Changes in the percentage of households that are <span style = 'color: #76b7b2;'>**Lone Parent**</span>, <span #style = 'color: #f28e2b;'>**Couple-based (no children)**</span>, or <span style = 'color: #e15759;'>**Living alone**</span><br>") +
#    # Move x axis to top
#    scale_x_discrete(position = "top") + 
#    theme_minimal(base_family = "Roboto Condensed") +
#    # Remove the legend
#    theme(legend.position = "none") + 
#    # Remove the panel border
#    theme(panel.border = element_blank()) + 
#    # Remove just about everything from the y axis
#    theme(axis.title.y = element_blank()) + 
#    theme(axis.text.y = element_blank()) + 
#    theme(panel.grid.major.y = element_blank()) +
#    theme(panel.grid.minor.y = element_blank()) + 
#    # Remove a few things from the x axis and increase font size
#    theme(axis.title.x = element_blank()) + 
#    theme(panel.grid.major.x = element_blank()) + 
#    theme(axis.text.x.top      = element_text(size=12)) +
#    # Remove x & y tick marks
#    theme(axis.ticks = element_blank()) +
#    theme(plot.subtitle = element_markdown()) +
#    
#    geom_text_repel(data = pct2 %>% filter(year == 1990), 
#                    aes(label = paste0(Variables,"-", pct_hsehold,"%")),
#                    hjust = "left",
#                    fontface = "bold",
#                    family = "Roboto Condensed",
#                    size = 3,
#                    nudge_x = -.6,
#                    direction = "y") + 
#    geom_text_repel(data = pct2 %>% filter(year == 2000), 
#                    aes(label = paste0(pct_hsehold,"%")),
#                    hjust = "left",
#                    fontface = "bold",
#                    family = "Roboto Condensed",
#                    size = 3,
#                    nudge_x = 0.05,
#                    direction = "y") + 
#    geom_text_repel(data = pct2 %>% filter(year == 2010), 
#                    aes(label = paste0(pct_hsehold,"%")),
#                    hjust = "left",
#                    fontface = "bold",
#                    family = "Roboto Condensed",
#                    size = 3,
#                    nudge_x = 0.05,
#                    direction = "y") + 
#    
#    geom_text_repel(data = pct2 %>% filter(year == 2019), 
#                    aes(label = paste0( pct_hsehold,"%")),
#                    hjust = "right",
#                    fontface = "bold",
#                    family = "Roboto Condensed",
#                    size = 3,
#                    nudge_x = .2,
#                    direction = "y") -> final_1
#
#
#
#w2 + geom_waffle(n_rows = 10, size = .25, color = "white", flip = T , make_proportional = T) + 
#    facet_wrap(~year, nrow = 1, strip.position = "bottom") + 
#    scale_x_discrete() +
#    scale_y_continuous(labels = function(x) x * 10, # make this multiplyer the same as n_rows
#                       expand = c(0,0)) +
#    coord_equal() +
#    labs(
#   subtitle = "Percentage of total households that are <span style = 'color: #4e79a7;'>**households with children**</span><br>",
#         x = "Year",
#         y = "%") + 
#    theme_minimal(base_family = "Roboto Condensed") +
#    theme(panel.grid = element_blank(), axis.ticks.y = element_line()) +
#    theme(legend.position = "none") + 
#    guides(fill = guide_legend(reverse = TRUE)) +  
#    scale_fill_manual(values=c(blue, grey)) +
#    theme(plot.subtitle = element_markdown())  -> final_2
#
#q3 + geom_line(aes(color = Variables, alpha = 1), size =2) +
#    geom_point(aes(color = Variables,  alpha = 1) , size = 4) + 
#    labs(subtitle = "Changes in the percentage of households are <span style = 'color: #4e79a7;'>**households with children**</span#>?<br>") + 
#    # Move x axis to top
#    scale_x_discrete(position = "top") + 
#    theme_bw() +
#    # Remove the legend
#    theme(legend.position = "none") + 
#    # Remove the panel border
#    theme(panel.border = element_blank()) + 
#    # Remove just about everything from the y axis
#    theme(axis.title.y = element_blank()) + 
#    theme(axis.text.y = element_blank()) + 
#    theme(panel.grid.major.y = element_blank()) +
#    theme(panel.grid.minor.y = element_blank()) + 
#    # Remove a few things from the x axis and increase font size
#    theme(axis.title.x = element_blank()) + 
#    theme(panel.grid.major.x = element_blank()) + 
#    theme(axis.text.x.top      = element_text(size=12)) +
#    # Remove x & y tick marks
#    
#    geom_text_repel(data = pct3 %>% filter(year == 1990), 
#                    aes(label = paste0(Variables,"-", pct_hsehold,"%")),
#                    hjust = "left",
#                    fontface = "bold",
#                    size = 3,
#                    nudge_x = -.5,
#                    direction = "y") + 
#    geom_text_repel(data = pct3 %>% filter(year == 2000), 
#                    aes(label = paste0(pct_hsehold,"%")),
#                    hjust = "left",
#                    fontface = "bold",
#                    size = 3,
#                    nudge_x = -.05,
#                    direction = "y") + 
#    geom_text_repel(data = pct3 %>% filter(year == 2010), 
#                    aes(label = paste0( pct_hsehold,"%")),
#                    hjust = "left",
#                    fontface = "bold",
#                    size = 3,
#                    nudge_x = -.05,
#                    direction = "y") + 
#    
#    geom_text_repel(data = pct3 %>% filter(year == 2019), 
#                    aes(label = paste0(pct_hsehold,"%")),
#                    hjust = "right",
#                    fontface = "bold",
#                    size = 3,
#                    nudge_x = .05,
#                    direction = "y")  +
#    scale_color_manual(values=blue) +
#    theme(plot.subtitle = element_markdown())




