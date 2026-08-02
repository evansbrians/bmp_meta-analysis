
library(tidyverse)

analysis_subset_list <-
  readxl::excel_sheets("data/raw/bmp_review_analysis_subset.xlsx") %>% 
  set_names() %>% 
  map(
    ~ readxl::read_excel(
      "data/raw/bmp_review_analysis_subset.xlsx",
      sheet = .x
    )
  )

analysis_subset_list_bmp_edits <-
  analysis_subset_list %>% 
  map(
    ~ distinct(.x, bmp) %>% 
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

writexl::write_xlsx(
