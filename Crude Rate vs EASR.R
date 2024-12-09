### EASR verus Crude Rates

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
library(writexl)
library(base64enc)
library(phsopendata)
library(plotly)
library(crosstalk)
library(lubridate)
library(kableExtra)
library(tidyverse)

scotland_drug_death_numbers <- read.csv("/conf/EIC/DASHBOARD/4 Personal/Ben/R Respos/Scottish_Health_Challenges_Drug_Deaths_Tableau_Prep/numbers.csv")

scotland_drug_death_numbers_final <- scotland_drug_death_numbers %>% 
  select(trend_axis, numerator, measure)

scot_est_pop <- get_resource(res_id = "09ebfefb-33f4-4f6a-8312-2d14e2b02ace")

scot_est_pop_final <- scot_est_pop %>% 
filter(Sex == "All" & CA == "S92000003") %>% 
  select(Year, AllAges)

scotland_drug_death_numbers_final$trend_axis <- as.numeric(scotland_drug_death_numbers_final$trend_axis) 
  
  scotland_drug_death_numbers_final <- scotland_drug_death_numbers_final %>% 
  rename(Year = trend_axis)

overall_merge <- left_join(scotland_drug_death_numbers_final, scot_est_pop_final, by = "Year" )

overall_merge_final <- overall_merge %>% 
  mutate(crude_rate = numerator/AllAges * 100000) %>% 
  mutate(diff_crude_easr = measure - crude_rate)

crude_rates <- overall_merge_final %>% 
  select(Year, crude_rate) %>% 
  mutate(Type = "Crude Rate") %>% 
  rename(Rate = crude_rate)

easr_rates <- overall_merge_final %>% 
  select(Year, measure) %>% 
  mutate(Type = "EASR") %>% 
  rename(Rate = measure)

full_merge <- bind_rows(easr_rates, crude_rates)

drug_admission_line_graph_theme2 <- theme(axis.title = element_text(colour="#06402b", family = "sans"),
                                          axis.text.x = element_text(size=14, colour = "#06402b"),
                                          axis.text.y = element_text(size=14, colour = "#06402b"),
                                          axis.title.x = element_text(size=14),
                                          axis.title.y = element_text(size=14),
                                          panel.background = element_blank(),
                                          panel.grid.major.y = element_line(colour = "grey"),
                                          panel.grid.major.x = element_blank(),
                                          axis.line.x = element_line(colour="black"),
                                          axis.line.y = element_line(colour="black"))

color_palette <- c("EASR" = "#9B4393",
                   "Crude Rate" = "#3F3685")


scotland_drug_death_plot <- full_merge %>% 
  ggplot(aes(x = Year, y = Rate, group = Type, color=Type)) +
  geom_line(linewidth = 1.5) +
  geom_point(size = 2.5)+
  drug_admission_line_graph_theme2

ggsave("scotland easr verus crude graph.png",
       plot = scotland_drug_death_plot,
       height = 7.5,
       width = 18,
       dpi = 300)
