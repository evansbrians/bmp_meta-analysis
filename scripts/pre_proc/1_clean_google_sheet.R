# This script:
# - Reformats the Google sheets
# - Cleans grouping variables (e.g., BMPs, species)
# - Saves each tab as an individual csv file in data/processed.

# set-up ------------------------------------------------------------------

library(tidyverse)

source("scripts/functions.R")

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

# Read in the species classification frame:

species_guilds <-
  here::here(
    "data/processed/species_classification",
    "species_classified_analysis_frame.csv"
  ) %>% 
  read_csv() %>% 
  select(
    species, 
    analysis_class,
    species_include = include
  )

# clean bmps --------------------------------------------------------------

analysis_subset_list_bmp_edits <-
  analysis_subset_list %>% 
  map(
    ~ .x %>% 
      separate_longer_delim(bmp, ";") %>% 
      mutate(
        bmp = 
          case_when(
            str_detect(bmp, "[Rr]emove") ~ "remove_non-native_species",
            str_detect(bmp, "[Pp]rescribed") ~ "prescribed_fire",
            str_detect(bmp, "[Ss]hrub") ~ "edge_and_shrub_habitat",
            str_detect(bmp, "nwsg|[Nn]ative") ~ "plant_nwsg",
            str_detect(bmp, "[Pp]atches") ~ "manage_in_patches",
            str_detect(bmp, "[Ee]xclusion") ~ "stream_exclusion_and_buffers",
            str_detect(bmp, "[Bb]oxes") ~ "install_nest_boxes",
            str_detect(bmp, "[Dd]elay") ~ "delay_hay",
            str_detect(bmp, "[Oo]verw") ~ "provide_overwintering_habitat",
            str_detect(bmp, "[Gg]raz") ~ "reduce_grazing_intensity",
            str_detect(bmp, "[Ii]ndoors") ~ "keep_cats_indoors",
            str_detect(bmp, "[Uu]nmown") ~ "set_aside_adjacent_unmowed",
            .default = bmp
          )
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

# add species class variable ----------------------------------------------

analysis_subset_list_species_guilds <-
  analysis_subset_list_species_edits %>% 
  map(
    ~ .x %>% 
      left_join(species_guilds, by = "species")
  )

# write to file -----------------------------------------------------------

analysis_subset_list_species_guilds %>% 
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
