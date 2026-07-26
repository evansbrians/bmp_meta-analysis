# File for reading in and pre-processing the Google sheets

# setup -------------------------------------------------------------------

library(googlesheets4)
library(meta)
library(tidyverse)

source("scripts/functions.R")

# URL for the effect size table:

sheet_url <- 
  file.path(
    "https://docs.google.com/spreadsheets/d",
    "14SWR7TXIKNvrYGr2_vwx9xBp5LDrDNaYcqZt6pldSoA",
    "edit?gid=1072800016"
  )

# Get effect size tables:

bmp_effects <- 
  sheet_url %>% 
  googlesheets4::sheet_names() %>% 
  set_names() %>% 
  map(
    ~ googlesheets4::read_sheet(
      sheet_url,
      sheet = .x
    ) %>% 
      janitor::clean_names() %>% 
      select(
        !matches("^x[0-9]")
      ) %>% 
      mutate(
        sheet = .x,
        .before = paper
      ) %>% 
      mutate(
        across(
          everything(),
          ~ tolower(.x)
        ),
        
        # Fix BMP naming convention:
        
        bmp =
          bmp %>% 
          str_replace_all(" ", "_") %>% 
          str_replace_all(";_", "; ") %>% 
          str_replace("plant.*(wildflowers|nwsgs)", "plant_nwsg") %>% 
          str_remove("_planting") %>% 
          str_remove("the_use_of_") %>% 
          str_remove(",_including_insecticides_and_rodenticides") %>% 
          str_remove("your_first_cutting_of_") %>% 
          str_remove("fields_") %>% 
          str_replace("_all_", "_")%>% 
          str_replace("set.*areas", "set_aside_adjacent_unmowed"),
        
        # Fix error class:
        
        error_class = 
          case_when(
            str_detect(error_class, "dev") ~ "standard_deviation",
            str_detect(error_class, "^se$|err") ~ "standard_error",
            str_detect(error_class, "^conf") ~ "confidence_intervals",
            .default = error_class
          ),
        
        # Numeric columns should be numeric:
        
        across(
          matches(
            "^xbar|beta|^.*n_?[ec]?$|se(_[ec])?$|sd|[ul]cl|df|value"
          ),
          ~ as.numeric(.x)
        )
      )
  )

# Write to file:

bmp_effects %>% 
  write_rds("data/processed/effect_size_tables.rds")
  
  