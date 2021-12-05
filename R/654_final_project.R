#654 project

#use UG GSS survey data?
#greater satisfaction (based on average rating) -> more likely to attend graduate school at UO?
#higher gpa -> more likely to attend graduate school
#higher utilization of advising has an outcome?
#install.packages("janitor")
library(tidyverse)
library(janitor)
library(here)

initial <- read_csv("C:/Users/rlatimer/Documents/personal/EDLD MS/EDLD 654 DS/
            654-project/data/aac_intakes_outcomes.csv")

initial <- import(here("data", "aac_intakes_outcomes.csv")) %>% 
  clean_names() %>%
  as_tibble()