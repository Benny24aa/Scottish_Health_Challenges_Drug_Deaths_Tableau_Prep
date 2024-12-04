library(shiny)
library(shinyWidgets)
library(DT)
library(data.table)
library(ggplot2)
library(shinydashboard)
library(readr)
library(dplyr)
library("readxl")
library("RColorBrewer")
library(rsconnect)
library(base)
library(datasets)
library(graphics)
library(grDevices)
library(methods)
library(readr)
library(readxl)
library(RColorBrewer)
library(stats)
library(utils)
library(sf)
library(writexl)
library(base64enc)
library(phsopendata)
library(plotly)
library(crosstalk)
library(lubridate)
library(kableExtra)
library(tidyverse)


scotland_drug_death_numbers <- read.csv("C:/Users/benny/Documents/My Resps/Scottish_Health_Challenges_Drug_Deaths_Tableau_Prep/numbers.csv")

#### Reducing dataframe size to speed up analysis
scotland_drug_death_numbers_final <- scotland_drug_death_numbers %>% 
  select(trend_axis, numerator, measure, upper_confidence_interval, lower_confidence_interval)

#### Preparing a theme for power point graphs 

drug_deaths_line_graph_theme <- theme(axis.title = element_text(colour="#06402b", family = "sans"),
                                      axis.text.x = element_text(size=10, colour = "#06402b"),
                                      axis.text.y = element_text(size=10, colour = "#06402b"),
                                      axis.title.x = element_text(size=14),
                                      axis.title.y = element_text(size=14),
                                      panel.background = element_blank(),
                                      panel.grid.major.y = element_line(colour = "grey"),
                                      panel.grid.major.x = element_blank(),
                                      axis.line.x = element_line(colour="black"),
                                      axis.line.y = element_line(colour="black"))

scotland_drug_rates_plot <- scotland_drug_death_numbers_final %>% 
  ggplot(aes(x = trend_axis, y = measure, color)) +
  geom_line(linewidth = 1.5, color = "#06402b") +
  geom_point(color = '#06402b', size = 2.5)+
  labs(x = 'Year',
       y = 'Age-sex standardised rate per 100000')+
  drug_deaths_line_graph_theme

# save out plot for power point

ggsave("drug_death_rate_graph.png",
       plot = scotland_drug_rates_plot,
       height = 5,
       width = 10,
       dpi = 300)

### Establishing Scottish Death Numerator Date

scotland_drug_death_number_plot <- scotland_drug_death_numbers_final %>% 
  ggplot(aes(x = trend_axis, y = numerator, color)) +
  geom_line(linewidth = 1.5, color = "#06402b") +
  geom_point(color = '#06402b', size = 2.5)+
  labs(x = 'Year',
       y = 'Number of Deaths')+
  drug_deaths_line_graph_theme

# save out plot for power point

ggsave("drug_death_rate_graph.png",
       plot = scotland_drug_rates_plot,
       height = 5,
       width = 10,
       dpi = 300)
