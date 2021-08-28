# Data vizualization 
## https://data.gov.sg/dataset/median-age-of-grooms-by-ethnic-group-annual
## Median Age of Grooms by Ethnic Group, Annual
## https://data.gov.sg/dataset/median-age-of-brides-by-ethnic-group-annual
## Median Age of Brides by Ethnic Group, Annual



# LOAD DATASET #######

rm(list = ls())
library(tidyverse)
library(ggplot2)
library(ggtext) # For R markdown codes below

getwd()

## Read in bride
data_bride <- 
    readr::read_csv(file = "data_gov_sg/median_age_groombride/median-age-of-brides-by-ethnic-group-annual/median-age-of-brides-by-ethnic-group-annual.csv",
                    col_names = c("year", "ethnic", "age_median"), 
                    skip =1)

max(data_bride$age_median)
#install.packages("gtsummary")
library(gtsummary)
data_bride %>% tbl_summary()

## Read in groom
data_groom <- 
    readr::read_csv(file = "data_gov_sg/median_age_groombride/median-age-of-grooms-by-ethnic-group-annual/median-age-of-grooms-by-ethnic-group.csv",
                    col_names = c("year", "ethnic", "age_median"), 
                    skip =1)

max(data_groom$age_median)
#install.packages("gtsummary")
library(gtsummary)
data_groom %>% tbl_summary()




# PREP DATA ######

## MERGE BOTH DATAFRAMES ====
data <- bind_rows(data_bride,data_groom,
                  .id="person")

data$person <- factor(data$person, labels = c("bride","groom"), levels = c(1,2))

## FILTER TO TOTAL ====
data <- 
    data %>% 
    filter(ethnic != "Total" & person == "bride")

# PLOT ####
# https://ggplot2.tidyverse.org/reference/geom_density.html
# https://github.com/pyykkojuha/tidytuesday/blob/main/R/2021_35/tidy_2021_35.R

#p <- ggplot(data, aes(y = age_median , x =year , fill = person ))
#p <- ggplot(data, aes(y = age_median , x =year))
#
#p +   geom_area(fill = "blue", alpha = .4) + geom_line()
#
#p <- ggplot(data_bride, aes(y = age_median , x =year , fill = ethnic, color = ethnic ))
#p +   geom_area(alpha = .4)

#install.packages("ggpubr")
#library(ggpubr)

#install.packages("ggridges")
library(ggridges)


p <- ggplot(data, aes(y = age_median , x =year , height = age_median, fill = person, group = person ))
p + geom_density_line(stat = "identity", size=.5, alpha=0.3) +
    scale_fill_manual(name='', values=c("groom" = "green4", "bride" = "red"))

p <- ggplot(data, aes(y = year , x = age_median , height = age_median, fill = person, group = person ))
p + geom_density_ridges(stat = "identity", size=.5, alpha=0.3) +
    scale_fill_manual(name='', values=c("groom" = "green4", "bride" = "red"))


p <- ggplot(data, aes(x = year, y = ethnic, height = age_median, group = ethnic))
p +   geom_ridgeline(fill = "lightblue")
p + geom_density_ridges(stat = "identity", scale = 1)


p + geom_line()
p + ggdensity(palette = "jco")


d <- data.frame(
    x = rep(1:5, 3),
    y = c(rep(0, 5), rep(1, 5), rep(2, 5)),
    height = c(0, 1, 3, 4, 0, 1, 2, 3, 5, 4, 0, 5, 4, 4, 1)
)

ggplot(d, aes(x, y, height = height, group = y)) + 
    geom_ridgeline(fill = "lightblue")





