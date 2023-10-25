#Assignment 4

##Simulate Data
library(tidyverse)

# Generate sample data
# Sightings of Black Oystercatcher chicks at Santa Cruz beaches
beaches <- c("Cowell's", "Steamer Lane", "Natural Bridges", "Mitchell's", "Main")
# blue, green, black, white, yellow
band_colors <- c("B", "G", "K", "W", "Y") 
# Surveys took place weekly in the summer of 2023
surveys <- seq(as.Date("2023-06-01"), as.Date("2023-08-31"), by = 7)

# Setting the "seed" forces randomized functions (like sample()) to generate
# the same output
set.seed(1538)
# 3 band colors identify a bird. We want 12 birds.
birds <- paste0(
  sample(band_colors, 25, replace = TRUE),
  sample(band_colors, 25, replace = TRUE),
  sample(band_colors, 25, replace = TRUE)
) %>% 
  unique() %>%
  head(12)
bloy_chicks <- tibble(
  # Randomly generate survey data
  beach = sample(beaches, size = 100, replace = TRUE),
  bird = sample(birds, size = 100, replace = TRUE),
  survey = sample(surveys, size = 100, replace = TRUE)
) %>% 
  # Remove duplicates (see ?distinct)
  distinct() %>% 
  # Sort by survey date and location
  arrange(survey, beach)

#Q1: Using set.seed() uses a unique ID for the random sampling of the possible outcomes. Even though 1538 is a random sampling of the scenario,  it is the same across other machines running the same function.

#Q2: The first part of the function selects 3 colors to make a color sequence and pastes them to make a 3-color sequence. With replacement means that it is possible to be used again. 'unique' function only keeps unique sequences of colors to use for identification to avoid redundant patterns. 'head' displays first 12 sequences generated. 

#Q3: nrow(bloy_chicks) = 95 due to removal of identical rows.

#Q4: Goal: 1. where was it seen most often?; 2. if tied, choose earliest; 3. if still tied, choose randomly.

#Find most frequent beach per bird
beach_freq <- bloy_chicks %>% 
  group_by(bird) %>% 
  count(bird, beach) %>% 
  filter(n == max(n)) %>% 
  ungroup()

# Find first date for each bird+beach
beach_early <- bloy_chicks %>% 
  group_by(bird, beach) %>% 
  summarize(earliest = min(survey),
            .groups = "drop")
            
# Join the two conditions and retain most frequent beach, only earliest
hatch_beach <- beach_freq %>% 
  group_by(bird) %>% 
  left_join(beach_early, by = c("bird", "beach")) %>% 
  filter(earliest == min(earliest)) %>% 
  sample_n(1) %>% # Randomly choose 1 row. See ?sample_n
  ungroup()
  
#Write Function
find_hatching_beach <- function(site, date) {
  # Start with a data frame (or tibble) of site and date for *one* bird
  # Use pipes and dplyr functions to find the hatching beach
  bird_observations <- tibble(site, date)
  result <- bird_observations %>%  # use as many pipes and dplyr functions as necessary
    add_count(site) %>% 
    filter(n==max(n)) %>% #need to return all data for matching beaches
    filter(date==min(date)) %>% 
    sample_n(1) %>% 
    select(site)
  # result should end up as a data frame with one row for the hatching beach
  return(result$site) # return the hatching beach
}

# split-apply-combine
bloy_chicks %>% 
  group_by(bird) %>% 
  summarize(find_hatching_beach(site = beach, date = survey))
  
#Q5:site = beach, date = survey

#Q6: Mitchell's for both.