

# setup -------------------------------------------------------------------

library(tidyverse)

species_classified_long <- 
  read_csv("data/processed/classification.csv")

# wide form ---------------------------------------------------------------

species_classified_long %>% 
  pivot_wider(
    names_from = source, 
    values_from = classification
  )
