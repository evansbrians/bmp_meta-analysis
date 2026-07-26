
# Trying to use BIRDBASE for classification

# setup -------------------------------------------------------------------

library(readxl)
library(tidyverse)

bb_path <- "data/raw/BIRDBASE v2025.1 Sekercioglu et al. Final.xlsx"

# Read the two header rows:

bb_raw <- 
  read_excel(
    "data/raw/BIRDBASE v2025.1 Sekercioglu et al. Final.xlsx", 
    col_names = FALSE
  )

# Combined names:

bb_col_names <-
  str_c(
    
    # Group names:
    
    bb_raw %>% 
      slice(1) %>% 
      unlist(use.names = FALSE) %>%
      tibble(prefix = .) %>%
      fill(prefix, .direction = "down") %>%
      pull(prefix),
    
    # Column names:
    
    bb_raw %>%
      slice(2) %>% 
      unlist(use.names = FALSE),
    sep = "_"
  )

# Combine column names and data:

birdbase_full <-
  bb_raw %>%
  slice(
    3:nrow(.)
  ) %>% 
  set_names(
    bb_col_names
  ) %>% 
  janitor::clean_names()

# Get just relevant columns (out of the 97!):

birdbase_habitat <- 
  birdbase_full %>% 
  select(
    common_name = taxonomy_english_name_bird_life_ioc_clements_avi_list,
    scientific_name = taxonomy_latin_bird_life_ioc_clements_avi_list,
    order = taxonomy_order,
    family = taxonomy_family_ioc_15_1,
    matches("habitat")
  )

