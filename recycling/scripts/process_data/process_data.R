# Script for processing data prior to effect size calculation

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

# Get citations_by_bmp_long:

papers <- 
  file.path(
    "https://docs.google.com/spreadsheets/d",
    "1Lf3v8fU0sCCAcJ6Wj1v8GwgogjI4ve3xcMlPzLW0hnU"
  ) %>% 
  read_sheet() %>% 
  mutate(
    key = tolower(key),
    bmp = 
      if_else(
        bmp == "no_bmp", 
        "z-no_bmp",
        bmp
      )
  )

# Get effect size tables:

bmp_effects <- 
  sheet_url %>% 
  googlesheets4::sheet_names() %>% 
  map_df(
    ~ googlesheets4::read_sheet(
      sheet_url,
      sheet = .x
    ) %>% 
      mutate(
        sheet = .x,
        .before = paper
      )
  ) %>% 
  janitor::clean_names() %>% 
  select(
    !matches("^x[0-9]")
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
    
    # Make that which should be numeric, numeric:
    
    across(
      c(
        xbar_e:ucl_c,
        beta:ucl,
        test_stat_value:control_se
      ),
      as.numeric
    )
  )

# Separate rows with multiple bmps and fix response snake_case:

bmp_fixed <- 
  bmp_effects %>% 
  filter(
    !str_detect(bmp, ";")
  ) %>% 
  distinct(bmp) %>% 
  pull() %>% 
  map_df(
    ~ bmp_effects %>% 
      filter(
        str_detect(bmp, .x)
      ) %>% 
      mutate(
        bmp = .x
      )
  ) %>% 
  mutate(
    response_class =
      response_class %>%
      str_replace_all(" ", "_"),
    response_var =
      response_var %>%
      str_replace_all("_", " ")
  )

# Classify response variables

bmp_response_classified <- 
  bmp_fixed %>% 
  filter(
    str_detect(sheet, "mean_diff|beta_categorical")
  ) %>%
  mutate(
    response_metric =
      case_when(
        str_detect(
          response_var,
          "successful nests per breeding pair"
        ) ~ "nest survival",
        str_detect(
          response_var, 
          "successful nests per 100"
        ) ~ "Who thought this was a good response",
        str_detect(
          response_var, 
          "nest density|nests/|nests per|abundance of nests"
        ) ~ "abundance",
        str_detect(
          response_var, 
          "log"
        ) ~ "log abundance",
        str_detect(
          response_var,
          "detect"
        ) ~ "abundance",
        str_detect(
          response_var,
          "hatch-year"
        ) ~ "age demographics",
        str_detect(
          response_var, 
          "dsr|daily survival rate"
        ) ~ "DSR",
        str_detect(
          response_var,
          "clutch|eggs per|chicks"
        ) ~ "clutch size",
        str_detect(
          response_var,
          "fledglings per successful nest|young fledged"
        ) ~ "fledge rate (successful nests)",
        str_detect(
          response_var,
          "fledglings per nest|number of nestlings|fledg.* rate|per female|number of fledglings|productivity"
        ) ~ "fledge rate (unknown nest fate)",
        str_detect(
          response_var,
          "mortality|killed|brood loss|brood reduction|predat"
        ) ~ "mortality",
        str_detect(
          response_var,
          "nest failure"
        ) ~ "nest failure",
        str_detect(
          response_var, 
          "nest survival|[Nn]est.success|success"
        ) ~ "nest survival",
        str_detect(
          response_var, 
          "shannon"
        ) ~ "shannon diversity index",
        str_detect(
          response_var, 
          "diversity"
        ) ~ "diversity",
        str_detect(
          response_var, 
          "evenn?ess"
        ) ~ "evenness",
        str_detect(
          response_var, 
          "species.richness|species per"
        ) ~ "species richness",
        str_detect(
          response_var, 
          "juvenile"
        ) ~ "juvenile survival",
        str_detect(
          response_var, 
          "survival$"
        ) ~ "survival",
        str_detect(
          response_var, 
          "community occupancy"
        ) ~ "community occupancy",
        str_detect(
          response_var, 
          "occupancy|occurrence"
        ) ~ "occupancy",
        str_detect(
          response_var, 
          "abundance|density|number|territor|breeding pair|count"
        ) ~ "abundance",
        str_detect(
          response_var, 
          "parasiti"
        ) ~ "brood parasiti",
        .default = "other"
      )
  )

# standardized mean difference --------------------------------------------

bmp_response_classified %>% 
  filter(sheet == "mean_diff") %>% 
  select(
    paper:ucl_c,
    response_metric
  ) %>% 
  add_sd_to_table() %>% 
  select(
    paper:response_class,
    response_metric,
    treatment:control,
    species,
    xbar_e:sd_e,
    xbar_c:sd_c
  ) %>% 
  write_rds("data/processed/mean_diff_proc.rds")







