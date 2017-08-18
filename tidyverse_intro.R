surveys <- read.csv("data/portal_data_joined.csv")
install.packages("tidyverse")
library(tidyverse)

# select the columns plot_id, species-id and weight
## surveys dataframe

select(surveys, plot_id, species_id, weight)

## using filter select rows where year is 1995
filter(surveys, year == 1995)

#PIPES!!!

## This is a PIPE %>%
surveys_sml <- surveys %>%
  filter(year == 1995) %>%
  select(plot_id, species_id, weight) 

surveys_sml <- surveys %>%
  filter(weight < 5) %>%
  select(plot_id, species_id, weight) 

surveys_sml

surveys %>%
  mutate(weight_kg = weight / 1000, 
         weight_kg2 = weight_kg * 2) %>%
  tail


surveys %>%
  filter(!is.na(weight)) %>%
  mutate(weight_kg = weight / 1000, 
         weight_kg2 = weight_kg * 2) %>%
  head


## Challenge

## Challenge
## Create a new data frame from the surveys data 
## that meets the following criteria: contains only the species_id column 
## and a new column called hindfoot_half containing values that are half the hindfoot_lengthvalues. In this hindfoot_half colulues are less than 30mn, there are no NAs and all va.

 challenge1 = surveys %>%
  mutate(hindfoot_half = 0.5 * hindfoot_length) %>%
  select(species_id, hindfoot_half) %>%
  filter(!is.na(hindfoot_half)) %>%
  filter(hindfoot_half < 30) 
 
 head(challenge1)  
  ## it's possible to combine two rows
  # filter(!is.na(hindfoot_half), filter(hindfoot_half < 30)) %>%
  
## group_by and summarize
surveys %>%
  filter(sex == "F" | sex == "M") %>%
 group_by(sex, species_id) %>%
 summarize(mean_weight = mean(weight, na.rm = T),
           min_weight = min(weight))



