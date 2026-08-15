
# Script for processing eu birds.

# setup -------------------------------------------------------------------

library(traitdata)
library(tidyverse)

data("eubirds")

eubirds_habitat <-
  eubirds %>% 
  janitor::clean_names() %>%  
  select(
    species = scientific_name_std, 
    deciduous_forest:human_settlements
  ) %>% 
  pivot_longer(
    deciduous_forest:human_settlements,
    names_to = "habitat_type"
  ) %>% 
  filter(
    value == 1
  ) %>% 
  summarize(
    habitat = 
      str_flatten(
        habitat_type,
        collapse = "; "
      ),
    .by = species
  )
