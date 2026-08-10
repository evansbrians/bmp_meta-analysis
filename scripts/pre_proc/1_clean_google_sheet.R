# Reformats the Google sheet and saves each tab as individual csv files.

# set-up ------------------------------------------------------------------

library(tidyverse)

# Google sheet url:

url <- 
  str_c(
    "https://docs.google.com/spreadsheets/d/",
    "14SWR7TXIKNvrYGr2_vwx9xBp5LDrDNaYcqZt6pldSoA"
  )

# Read in the google sheets:

analysis_subset_list <-
  googlesheets4::sheet_names(url) %>% 
  set_names() %>% 
  map(
    ~ googlesheets4::read_sheet(
      url,
      sheet = .x
    ) %>% 
      janitor::clean_names()
  )

# Function to clean common names:

fix_common_names <-
  function(.common_name) {
    .common_name %>% 
      str_replace_all("-", " ") %>% 
      str_remove_all("'") %>% 
      str_to_snake()
  }

# clean bmps --------------------------------------------------------------

analysis_subset_list_bmp_edits <-
  analysis_subset_list %>% 
  map(
    ~ pluck(.x) %>% 
      # distinct(.x, bmp) %>% 
      mutate(
        bmp = 
          bmp %>% 
          str_to_lower() %>% 
          str_replace_all("; ", "--") %>% 
          str_replace_all(" ", "_") %>% 
          str_trim() %>% 
          str_replace(
            "plant_native_warm_season_grasses",
            "plant_nwsg"
          ) %>% 
          str_remove("_\\(nwsgs\\)_and_wildflowers") %>% 
          str_replace("nwsgs", "nwsg") %>% 
          str_remove(",_including_insecticides_and_rodenticides") %>% 
          str_replace(
            "_the_use_of_pesticides",
            "_pesticides"
          ) %>% 
          str_replace("manage_fields_in_patches", "manage_in_patches") %>% 
          str_replace("_plantings", "s") %>%  
          str_replace(
            "delay_your_first_cutting_of_hay",
            "delay_hay"
          ) %>% 
          str_replace(
            "keep_all_cats_indoors",
            "keep_cats_indoors"
          ) %>% 
          str_replace("__", "_") %>% 
          str_replace("--", "; ")
      )
  )

# clean species names -----------------------------------------------------

analysis_subset_list_species_edits <- 
  analysis_subset_list_bmp_edits %>% 
  map(
    ~ .x %>% 
      mutate(
        species = 
          species %>% 
          fix_common_names() %>% 
          str_replace("^lapwing", "northern_lapwing") %>% 
          str_replace("^skylark", "eurasian_skylark") %>% 
          str_replace("florida_grasshopper_sparrow", "grasshopper_sparrow") %>% 
          str_replace("yellow_wagtail", "western_yellow_wagtail") %>% 
          str_replace("mc_cowns", "mccowns") %>% 
          str_replace("alpine_whinchat", "whinchat") %>% 
          str_replace("thick_billed_longspur", "mccowns_longspur") %>% 
          str_replace("tengmalms", "boreal") %>% 
          str_replace("plain_titmouse", "oak_titmouse") %>% 
          str_replace("swaintsonts_hawk", "swainsons_hawk") %>% 
          str_replace("western_western_", "western_") %>% 
          str_replace("swainsonts", "swainsons")
      ) %>% 
      filter(
        nchar(species) > 1
      )
  )
  
# write to file -----------------------------------------------------------

analysis_subset_list_species_edits %>% 
  iwalk(
    \ (.table, idx) {
      write_csv(
        .table,
        file.path(
          "data/processed/for_analysis", 
          glue::glue("{idx}.csv")
        )
      )
    }
  )
