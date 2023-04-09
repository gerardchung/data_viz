# easter dates


rm(list=ls())

pacman::p_load(tidyverse, ggplot2, janitor, lubridate)
getwd()
data <- readxl::read_excel("easter_dates/easter500.xls")

glimpse(data)


data <- data %>% filter(Year <= 2023)

data %>% group_by(Month) %>% tally() %>% ungroup()



data <- 
    data %>% 
    mutate(date = str_c("2000", Month, Day, sep="-")) %>%  # give an abitrary year 
    mutate(date = ymd(date))

data <-
    data %>% 
    mutate(Month = month(date,label = T), Day = day(date))

data %>% group_by(Month, Day) %>% tally() %>% arrange(desc(n))



md <- 
    data %>% 
    group_by(Month, Day) %>% 
    tally() %>% 
    arrange(desc(n)) 

md <- md %>% mutate(date = str_c(Month, Day, sep="-")) %>% mutate(date = factor(date))

md <- 
    md %>% 
    mutate(highlight_date = ifelse(date == 'Apr-16', T, F))


md %>% 
    ggplot(aes(x = n, y = fct_reorder(date,n))) +
  #  geom_bar(aes(x = n, y = fct_reorder(date,n)),  stat='identity') +
    geom_col(aes(fill = highlight_date)) +
    geom_text(
        aes(label = paste(date)),
        hjust = 1, 
        nudge_x = -0.2,
        fontface = "bold",
        family = "Roboto Condensed",
        size = 7
    ) +
    theme_minimal(base_family = "Roboto Condensed") +
    scale_fill_manual(values = c('#808080', '#ee2536')) +
    labs(title = "MONTHS OF COUNTRIES' NATIONAL DAYS",
         caption = "Source: https://www.worldatlas.com/articles/list-of-independence-days-by-country.html | gerardchung.com | Codes: https://github.com/gerardchung/ndp2021"  
    ) +
    theme(    rect = element_rect(fill = "#ffffff"),
              panel.background = element_rect(fill = "#ffffff", color = "#ffffff"),
              plot.background = element_rect(fill = "#ffffff", color = "#ffffff"),
              axis.text.x = element_text(size = 30, hjust = 1, family = "Roboto Condensed", color = "black"),
              axis.text.y = element_blank(),
              axis.title = element_blank(),
              axis.ticks = element_blank(),
              panel.grid.major = element_blank(),
              panel.grid.minor = element_blank(),
              plot.title = ggtext::element_markdown(size = 32, face="bold"),
              plot.caption  = element_text(size = 10, hjust = 1, family = "Roboto Condensed", color = "#595959"),
              legend.position = "none"
    )
    
    


data %>% 
    ggplot(aes(x = date, y = sample(seq_along(date)))) +
    geom_point() +
    geom_jitter() 



    scale_x_date(date_labels = "%b %d")
