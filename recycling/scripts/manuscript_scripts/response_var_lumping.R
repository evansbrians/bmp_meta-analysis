# Purpose: Generate a table of all of the response variables for joining.

# setup -------------------------------------------------------------------

library(tidyverse)

source("scripts/functions.R")

effect_size_tables_google <-  
  read_effect_size_tables()

# generate clean response variable classes --------------------------------

response_vars <- 
  effect_size_tables_google %>% 
  
  # Combine list items, subsetting to unique response classes and variables:
  map_df(
    ~ distinct(
      .x,
      response_class,
      response_var
    )
  ) %>% 
  mutate(
    
    # Define new response variable values:
    
    response_new = 
      response_var %>% 
      case_when(
        
        # Define variables associated with abundance:
        
        str_detect(
          .,
          str_c(
            "abundance",
            "density",
            "(nests|pairs)/ha",
            "crowing",
            "singing males",
            "territories",
            "birds (captured|per count)",
            "nests per patch",
            "detection frequency",
            "number of detections",
            sep = "|"
          )
        ) ~ "abundance",
        
        # Define variables associated with occupancy:
        
        str_detect(., "occupancy|selection|occurr") ~ "occupancy",
        
        # Define variables associated with species diversity:
        
        str_detect(
          .,
          "diversity|species evenness|shannon"
        ) ~ "species_diversity",
        
        # Define variables associated with species richness:
        
        str_detect(., "richness|species per field") ~ "species_richness",
        
        # Define variables associated with population trend:
        
        str_detect(., "population (gro|tre)") ~ "population_trend",
        
        # Define variables associated with nest success:
        
        str_detect(
          ., 
          str_c(
            "dsr",
            "successful n",
            "breeding success",
            "brood red",
            "fledge_rate",
            "nest(ing)?[ _](succ|surv)",
            "nest pred",
            "depred",
            "daily nest",
            "predation (prob|r)",
            "surviving to fledging",
            "daily surv",
            "fledgl?ing success",
            "daily mort",
            "nests predated",
            sep = "|"
          )
        ) ~ "nest_success",
        
        # Define variables associated with fecundity (here defined as n
        # offspring per female):
        
        str_detect(
          .,
          str_c(
            "(eggs|fledglings) per",
            "number of (offs|fledg)",
            "clutch size",
            "chicks per female",
            "productivity",
            sep = "|"
          )
        ) ~ "fecundity",
        
        # Define variables associated with brood parasitism:
        
        str_detect(., "brood[ _]para") ~ "brood_parasitism",
        
        # Define variables associated with survival:
        
        str_detect(
          ., 
          str_c(
            "(male|female|juvenile|non-breeding season) surv",
            "adult breeding season survival",
            "adult mortality",
            sep = "|"
          )
        ) ~ "survival",
        .default = .
      ),
    sign = 
      case_when(
        str_detect(
          response_var,
          str_c(
            "(de)?predat(ion|ed)",
            "parasit(ism|ized)",
            "mortality",
            "failure",
            sep = "|"
          )
        ) ~ -1,
        .default = 1
      )
  ) %>% 
  distinct()

response_vars %>% 
  write_rds("data/proc_2025-10-16/response_vars_lumped.rds")
