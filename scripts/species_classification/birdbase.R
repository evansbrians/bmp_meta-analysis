
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

birdbase <-
  bb_raw %>%
  slice(
    3:nrow(.)
  ) %>% 
  set_names(
    bb_col_names
  ) %>% 
  janitor::clean_names()

