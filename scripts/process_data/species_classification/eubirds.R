
library(here)
library(tidyverse)

# Grab and format Bird Life International list of species and common names:

taxonomy <-
  here(
    "data/raw/for_species_classification", 
    "birdlife_international_all_species.csv"
  ) %>% 
  read_csv() %>% 
  janitor::clean_names() %>% 
  mutate(
    common_name = 
      common_name %>% 
      str_to_lower() %>% 
      str_replace_all("-", "_") %>% 
      str_remove_all("'") %>% 
      str_replace_all(" ", "_"),
    scientific_name,
    .keep = "none"
  )

# Grab eubirds record (Storchova and Horak 2018) from the traitdata package:

eu_birds <- 
  traitdata::eubirds %>% 
  janitor::clean_names() %>%
  as_tibble() %>% 
  select(scientific_name_std, deciduous_forest:human_settlements) %>% 
  
  # Make longform:
  
  pivot_longer(
    cols = deciduous_forest:human_settlements,
    names_to = "classification"
  ) %>% 
  
  # Subset to classes where the bird occurs:
  
  filter(value == 1) %>% 
  
  # Flatten to combined classes where the bird occurs:
  
  summarize(
    classification = 
      str_flatten(classification, collapse = "; "),
    .by = scientific_name_std
  )

# Add common names:
  
eu_birds %>% 
  anti_join(
    taxonomy,
    by = join_by(scientific_name_std == scientific_name)
  )
