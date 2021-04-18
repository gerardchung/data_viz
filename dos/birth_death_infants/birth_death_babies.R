# Data Viz 9th April 2020 #####
## DOS Singapore ====== 
## Link: https://www.singstat.gov.sg/find-data/search-by-theme/population/births-and-fertility/latest-data 


library(dplyr)
library(tidyr)
library(ggplot2)
library(janitor)
library(readxl)
library(stringr)

library(waffle)
library(patchwork)
library(showtext)
library(ggtext)

rm(list= ls())

# Data for Births  ######
data1 <- read_xlsx(path = "dos/birth_death_infants/outputFile 5.xlsx", 
                  sheet = "T6", 
                  range = "A6:BC13"
                )


## Select decades =====
data1 <- 
    data1 %>%
    select("Variables", "1970":"2019") 


## Pivot long =====
unique(data1$Variables)

data1_long <- 
    data1 %>% 
    pivot_longer(`1970`:`2019`,
                 names_to = "year") %>% 
    filter(Variables != "Total Live-births By Birth Order")

### Remove "," from the values and convert into numerical ====
data1_long$value <- as.numeric(str_remove_all(data1_long$value, pattern = ","))

### Variable to indicate decades ====
data1_long$year1 <- as.numeric(str_extract_all(data1_long$year, regex("..."), simplify = T))
glimpse(data1_long)

data1_long <- 
    data1_long %>%
    mutate(decade = case_when( year1 == 197 ~ 1970,
                               year1 == 198 ~ 1980,
                               year1 == 199 ~ 1990,
                               year1 == 200 ~ 2000,
                               year1 == 201 ~ 2010)) 

data1_long$decade <- factor(data1_long$decade)

#str_view_all(data1_long$year, regex("..."))


## Recode Variables to four levels: 1st Live-birth, 2nd Live-birth, 3rd-4th birth, 5th or more =====
unique(data1_long$Variables)
data1_long <- 
    data1_long %>% 
    mutate(num_child = case_when(Variables == "1st Live-birth" ~ "1st or 2nd child",
                                 Variables == "2nd Live-birth" ~ "1st or 2nd child",
                                 Variables == "3rd Live-birth" ~ "3rd or 4th child",
                                 Variables == "4th Live-birth" ~ "3rd or 4th child",
                                TRUE ~ "5th child or more")) %>% 
    group_by(year,num_child) %>% 
    mutate(value1 = sum(value)) %>% 
    ungroup()

data1_long1 <- 
    data1_long %>% 
    filter(Variables != "2nd Live-birth") %>% 
    filter(Variables != "4th Live-birth") %>% 
    filter(Variables != "6th Live-birth & Over") 

## Sum values1 to get total number for each decade =====
data1_long_plot <- 
    data1_long1 %>% 
    group_by(decade, num_child) %>% 
    summarise(num_births = sum(value1)) %>% 
    ungroup()


## Convert variables to appropriate types =====

data1_long_plot$num_child <- factor(data1_long_plot$num_child, 
                               levels = c("1st or 2nd child", "3rd or 4th child",  "5th child or more"))



# Data for Deaths ######

data2 <- read_xlsx(path = "dos/birth_death_infants/outputFile 6.xlsx", 
                       sheet = "T5", 
                       range = "A6:ABE21"
    )
## Select for 1970 to 2019 ====

data2 <- 
    data2 %>% 
    select(Variables, starts_with(c("197","198","199","200","201"))) %>% 
    filter(Variables == "Total Infant Deaths By Ethnic Group")
glimpse(data2)

## Convert to numeric ====

data2[,2:601] <- lapply(data2[,2:601],as.numeric)
glimpse(data2)


## Create values for each year by summing up months for each year ====
data2 <- 
    data2 %>% 
    mutate(`1970` = rowSums(across(starts_with("1970")))) %>%
    mutate(`1971` = rowSums(across(starts_with("1971")))) %>%
    mutate(`1972` = rowSums(across(starts_with("1972")))) %>%
    mutate(`1973` = rowSums(across(starts_with("1973")))) %>%
    mutate(`1974` = rowSums(across(starts_with("1974")))) %>%
    mutate(`1975` = rowSums(across(starts_with("1975")))) %>%
    mutate(`1976` = rowSums(across(starts_with("1976")))) %>%
    mutate(`1977` = rowSums(across(starts_with("1977")))) %>%
    mutate(`1978` = rowSums(across(starts_with("1978")))) %>%
    mutate(`1979` = rowSums(across(starts_with("1979")))) %>%
    
    mutate(`1980` = rowSums(across(starts_with("1980")))) %>%
    mutate(`1981` = rowSums(across(starts_with("1981")))) %>%
    mutate(`1982` = rowSums(across(starts_with("1982")))) %>%
    mutate(`1983` = rowSums(across(starts_with("1983")))) %>%
    mutate(`1984` = rowSums(across(starts_with("1984")))) %>%
    mutate(`1985` = rowSums(across(starts_with("1985")))) %>%
    mutate(`1986` = rowSums(across(starts_with("1986")))) %>%
    mutate(`1987` = rowSums(across(starts_with("1987")))) %>%
    mutate(`1988` = rowSums(across(starts_with("1988")))) %>%
    mutate(`1989` = rowSums(across(starts_with("1989")))) %>%
    
    mutate(`1990` = rowSums(across(starts_with("1990")))) %>%
    mutate(`1991` = rowSums(across(starts_with("1991")))) %>%
    mutate(`1992` = rowSums(across(starts_with("1992")))) %>%
    mutate(`1993` = rowSums(across(starts_with("1993")))) %>%
    mutate(`1994` = rowSums(across(starts_with("1994")))) %>%
    mutate(`1995` = rowSums(across(starts_with("1995")))) %>%
    mutate(`1996` = rowSums(across(starts_with("1996")))) %>%
    mutate(`1997` = rowSums(across(starts_with("1997")))) %>%
    mutate(`1998` = rowSums(across(starts_with("1998")))) %>%
    mutate(`1999` = rowSums(across(starts_with("1999")))) %>%
    
    mutate(`2000` = rowSums(across(starts_with("2000")))) %>%
    mutate(`2001` = rowSums(across(starts_with("2001")))) %>%
    mutate(`2002` = rowSums(across(starts_with("2002")))) %>%
    mutate(`2003` = rowSums(across(starts_with("2003")))) %>%
    mutate(`2004` = rowSums(across(starts_with("2004")))) %>%
    mutate(`2005` = rowSums(across(starts_with("2005")))) %>%
    mutate(`2006` = rowSums(across(starts_with("2006")))) %>%
    mutate(`2007` = rowSums(across(starts_with("2007")))) %>%
    mutate(`2008` = rowSums(across(starts_with("2008")))) %>%
    mutate(`2009` = rowSums(across(starts_with("2009")))) %>%
    
    mutate(`2010` = rowSums(across(starts_with("2010")))) %>%
    mutate(`2011` = rowSums(across(starts_with("2011")))) %>%
    mutate(`2012` = rowSums(across(starts_with("2012")))) %>%
    mutate(`2013` = rowSums(across(starts_with("2013")))) %>%
    mutate(`2014` = rowSums(across(starts_with("2014")))) %>%
    mutate(`2015` = rowSums(across(starts_with("2015")))) %>%
    mutate(`2016` = rowSums(across(starts_with("2016")))) %>%
    mutate(`2017` = rowSums(across(starts_with("2017")))) %>%
    mutate(`2018` = rowSums(across(starts_with("2018")))) %>%
    mutate(`2019` = rowSums(across(starts_with("2019")))) 

glimpse(data2)

## Select only the years vars =====
data2 <-
    data2 %>% 
    select(Variables, c(602:651))
   
## pivot longer
data2_long <- 
    data2 %>% 
    pivot_longer(`1970`:`2019`,
                 names_to = "year")

## Change variables to appropriate types ======
data2_long$year <- factor(data2_long$year)

## Create a decade var to indicate dcades  ==== 
data2_long$year1 <- as.numeric(str_extract_all(data2_long$year, regex("..."), simplify = T))
glimpse(data2_long)
unique(data2_long$year1)

data2_long <- 
    data2_long %>%
    mutate(decade = case_when( year1 == 197 ~ 1970,
                               year1 == 198 ~ 1980,
                               year1 == 199 ~ 1990,
                               year1 == 200 ~ 2000,
                               year1 == 201 ~ 2010)) 

data2_long$decade <- factor(data2_long$decade)

## Merge both datasets =====
data2_long <- 
    data2_long %>% 
    select(-Variables,-year1) %>%
    rename(num_deaths = value) 
  
rm(data1, data1_long, data1_long1, data2)  
 
## To create a row for each year ====

data2_long <- 
  data2_long %>% 
  mutate(num_deaths2 = ceiling(num_deaths/10)) %>% 
  mutate(point = purrr::map(num_deaths2, function(x){seq(1,x,1)})) %>%
  unnest(cols = point)

## To extract the kast digit from the value ======
data2_long$year_last <- str_sub(data2_long$year, -1)



#data_combined <- full_join(x = data1_long_plot,
#                           y = data2_long, 
#                           by = c("decade"))


# Plot for deaths: line ######

ggplot(data = data2_long, aes(x = year_last, y = point)) +
  geom_point(color = "#F2385A", size = 0.5) +
  scale_y_continuous(trans = "reverse") +
  facet_wrap(~decade, nrow = 1) +
  theme_void() +
  theme(plot.background = element_rect(fill = "#1A2E40", color = "NA"),
        strip.text = element_blank()) -> death_plt



# Plot for birth: Waffle plot for data on births ######

ggplot(data = data1_long_plot, aes(values = (num_births/2000), fill = num_child)) +
    geom_waffle(n_rows = 10, size = 0.2, color = "#1A2E40", flip = TRUE, make_proportional = F) +
    scale_fill_manual(
        name = NULL,
        values = c("#51718C","#6892C1", "#88A5BF",  "#3d7bbf"),
        labels = c("1st or 2nd child", "3rd or 4th child",  "5th child or more")
    ) +
    coord_equal()+
    facet_wrap(~decade, nrow = 1, strip.position = "bottom", labeller = labeller(decade = function(string){paste0(string,"s")})) +
    guides(fill = guide_legend(label.position = "top",
                               title.hjust = 0.5,
                               keywidth = unit(.5, "line"),
                               keyheight = unit(.5, "line"),
                               nrow = 1
    )
    ) +
    theme_void()+
    theme(plot.background = element_rect(fill = "#1A2E40", color = NA),
          strip.text.x = element_text(family = "Roboto Condensed", face = "bold", size = 12, color = "#D8E6F2"),
          plot.margin = margin(20,0,300,0),
          panel.spacing = unit(0, "mm"),
          legend.position = c(0.175, 0.97),
          legend.text = element_text(family = "Roboto Condensed", face = "bold", size = 10, color = "#D8E6F2")) -> birth_plot

# PLOT BOTH GRAPHS TOGETHER ======
  # https://patchwork.data-imaginist.com/reference/inset_element.html

final <- 
  
  birth_plot + inset_element(death_plt, left = 0.17, bottom = 0.05, right = .825, top = 0.49, align_to = "full") + 
  plot_annotation(
    title = "Number of infant births and deaths in Singapore from 1970-2019",
    subtitle = "Each square represents 2000 live births & <span style='color:#F2385A'>each dot</span> represents ten infant deaths.",
    caption = "www.gerardchung.com | Codes: https://github.com/gerardchung/ | Source: Department of Statistics SG",
    theme = theme(
      plot.background = element_rect(fill = "#1A2E40"),
      plot.title = element_text(family = "Roboto Condensed", 
                                size = 18, color = "#D8E6F2", 
                                hjust = 0.5, 
                                face = "bold", 
                                margin = margin(10,0,5,0)),
      plot.subtitle = element_textbox_simple(family = "Roboto Condensed", color = "#D8E6F2", size = 14, halign = 0.5),
      plot.caption = element_text(family = "Roboto Condensed", color = "#D8E6F2", size = 9, hjust = 0.98)      
    )
  ) 


final

