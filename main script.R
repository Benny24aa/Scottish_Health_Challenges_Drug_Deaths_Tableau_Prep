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

#### Reducing dataframe size to speed up analysis
scotland_drug_death_numbers_final <- scotland_drug_death_numbers %>% 
  select(trend_axis, numerator, measure, upper_confidence_interval, lower_confidence_interval)

#### Preparing a theme for power point graphs 

drug_deaths_line_graph_theme <- theme(axis.title = element_text(colour="#06402b", family = "sans"),
                                      axis.text.x = element_text(size=15, colour = "#06402b"),
                                      axis.text.y = element_text(size=15, colour = "#06402b"),
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

hb_rates <- read_csv("/conf/EIC/DASHBOARD/4 Personal/Ben/R Respos/Scottish_Health_Challenges_Drug_Deaths_Tableau_Prep/hb_rates.csv")

hb_rates_final <- hb_rates %>% 
  select(areaname, measure, numerator, trend_axis) %>% 
  filter(trend_axis == "2002") %>% 
  mutate(areaname = gsub("NHS ", "", areaname)) %>% 
  filter(!is.na(numerator)) %>% 
  filter(areaname != "Scotland")





# %>% 
  # filter(areaname == "Dumfries & Galloway" | areaname == "Greater Glasgow & Clyde" | areaname == "Ayrshire & Arran" | areaname == "Lanarkshire" | areaname == "Tayside")

# hb_rates_final$trend_axis <- as.Date(as.character(hb_rates_final$trend_axis),format = "%Y")


drug_deaths_bar_graph_theme <- theme(axis.title = element_text(colour="#06402b", family = "sans"),
                      axis.text.x = element_text(size=8, colour = "black"),
                      axis.text.y = element_text(size=8, colour = "black"),
                      axis.title.x = element_text(size=14),
                      axis.title.y = element_text(size=14),
                      panel.background = element_blank(),
                      panel.grid.major.x = element_line(colour = "grey"),
                      panel.grid.major.y = element_blank(),
                      axis.line.x = element_line(colour="black"),
                      axis.line.y = element_line(colour="black"),
                      plot.margin = margin(1,1,1,1, "cm"))

hb_rates_final_graph <- hb_rates_final %>%
  mutate(areaname = fct_reorder(areaname, desc(numerator))) %>% 
ggplot(aes(x = areaname, y = numerator, fill = areaname)) +
  geom_bar(stat = "identity")+
  drug_deaths_bar_graph_theme + theme(legend.position="none")   + 
  labs(x="Health Board", y="Number of Deaths", fill = "Member")

ggsave("hbs.png",
       plot = hb_rates_final_graph,
       height = 7.5,
       width = 18,
       dpi = 300)


drug_admissions <- get_resource(res_id = "cdd2e229-9955-4d2a-8c9f-d9bc091a602d") ### Loads in data from open data scotland
HB_Lookup <- get_resource(res_id = "652ff726-e676-4a20-abda-435b98dd7bdc") %>% 
  rename(HBR = HB)

drug_admissions_final <- left_join(drug_admissions, HB_Lookup, by = "HBR" )

drug_admissions_scotland <- drug_admissions_final %>% 
  select(NumberOfStays, FinancialYear, HBName ) %>% 
  filter(is.na(HBName)) %>% 
  filter(FinancialYear != "1995/96" & FinancialYear != "1996/97" & FinancialYear != "1997/98" & FinancialYear != "1998/99" & FinancialYear != "1999/00") %>% 
  select(-HBName) %>% 
  group_by(NumberOfStays, FinancialYear) 

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

scotland_drug_admission_plot <- drug_admissions_scotland %>% 
  ggplot(aes(x = FinancialYear, y = NumberOfStays, color, group = 1)) +
  geom_line(linewidth = 1.5, color = "#06402b") +
  geom_point(color = '#06402b', size = 2.5)+
  labs(x = 'Financial Year',
       y = 'Number of Stays')+
  drug_admission_line_graph_theme

ggsave("drug admissions.png",
       plot = scotland_drug_admission_plot,
       height = 7.5,
       width = 18,
       dpi = 300)

drug_admissions_glas <- drug_admissions_final %>% 
  select(EASRStays, FinancialYear, HBName ) %>% 
  filter(HBName == "NHS Greater Glasgow and Clyde") %>% 
  filter(FinancialYear != "1995/96" & FinancialYear != "1996/97" & FinancialYear != "1997/98" & FinancialYear != "1998/99" & FinancialYear != "1999/00") %>%
  group_by(EASRStays, FinancialYear) 

glas_drug_admission_plot <- drug_admissions_glas %>% 
  ggplot(aes(x = FinancialYear, y = EASRStays, color, group = 1)) +
  geom_line(linewidth = 1.5, color = "#06402b") +
  geom_point(color = '#06402b', size = 2.5)+
  labs(x = 'Financial Year',
       y = 'Age-sex standardised rate per 100000',
       title = "Drug Stays EASR Per 100k each Financial Year")+
  drug_admission_line_graph_theme

ggsave("glasgow drug admissions.png",
       plot = glas_drug_admission_plot,
       height = 7.5,
       width = 18,
       dpi = 300)

scotland_drug_death_rates_hbs <- read.csv("/conf/EIC/DASHBOARD/4 Personal/Ben/R Respos/Scottish_Health_Challenges_Drug_Deaths_Tableau_Prep/hb_rates.csv")

glas_scotland_drug_death_rates_hb <- scotland_drug_death_rates_hbs %>% 
  filter(areaname == "NHS Greater Glasgow & Clyde") %>% 
  select(trend_axis, measure) %>% 
  filter(measure != "0")

glas_drug_death_rate_plot <- glas_scotland_drug_death_rates_hb %>% 
  ggplot(aes(x = trend_axis, y = measure, color, group = 1)) +
  geom_line(linewidth = 1.5, color = "#06402b") +
  geom_point(color = '#06402b', size = 2.5)+
  labs(x = 'Year',
       y = 'Age-sex standardised rate per 100000',
       title = "Drug Deaths EASR Per 100k each Year")+
  drug_admission_line_graph_theme2

ggsave("glasgow death rates.png",
       plot = glas_drug_death_rate_plot,
       height = 7.5,
       width = 18,
       dpi = 300)


