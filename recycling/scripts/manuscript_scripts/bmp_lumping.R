
# setup -------------------------------------------------------------------

library(tidyverse)

source("scripts/functions.R")

effect_size_tables_google <-  
  read_effect_size_tables()

# generate clean response variable classes --------------------------------

bmp_lumping <- 
  effect_size_tables_google %>% 

  # Combine list items, subsetting to unique BMPs:
  
  map_df(
    ~ distinct(.x, bmp)
  ) %>% 
  separate_longer_delim(bmp, delim = ";") %>% 
  mutate(
    bmp_new = 
      bmp %>% 
      str_trim() %>%
      str_replace_all("_", " ") %>% 
      str_to_title() %>% 
      str_replace_all("And", "and") %>% 
      str_replace_all("Of", "of") %>%
      str_replace_all("To ", "to ") %>% 
      str_replace_all("  ", " ") %>% 
      case_when(
        str_detect(., "Hay$") ~ "Delay Hay",
        str_detect(., "Plant (Native|Nwsg)") ~ "Plant NWSGs",
        str_detect(., "Pesticides") ~ "Eliminate Pesticides",
        str_detect(., "Cats Ind") ~ "Keep Cats Indoors",
        str_detect(., "Manage (Fields|In)") ~ "Manage in Patches",
        str_detect(., "^Stream") ~ "Stream Exclusion and Buffer Plantings",
        str_detect(., "^Summer") ~ "Rotational Grazing",
        .default = .
      )
  ) %>% 
  distinct() %>% 
  arrange(bmp)

bmp_lumping %>% 
  write_rds("data/proc_2025-10-16/bmp_lumped.rds")
  