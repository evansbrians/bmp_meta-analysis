library(tidyverse)

# avian conservation database ---------------------------------------------

avian_db <- 
  googlesheets4::read_sheet(
    file.path(
      "https://docs.google.com/spreadsheets/d",
      "1UhSvMqwGYMTpQFYyjrDI2xKxvZQVMSVRXHtaH10y_ss/edit?gid=955867080#gid=955867080"
    )
  ) %>% 
  janitor::clean_names() 

grass_shrub_subset <- 
  avian_db %>% 
  filter(
    if_any(
      contains("habitat"),
      ~ str_detect(.x, "[Gg]rass|Open Country|Scrub")
    )|
      !is.na(agriculture),
    group == "landbird"
  ) %>% 
  arrange(common_name) %>% 
  distinct(common_name)

danged_forest_birds <- 
  avian_db %>% 
  anti_join(
    grass_shrub_subset,
    by = "common_name"
  ) %>% 
  filter(
    group == "landbird",
    primary_breeding_habitat == "Forests:  Temperate Eastern") %>% 
  distinct(common_name) %>% 
  arrange(common_name) %>% 
  pull()

# all about birds because of the eastern forest biome ---------------------

all_about_birds_eastern_forest <- 
  danged_forest_birds %>% 
  map_dfr(
    \(.x) {
      Sys.sleep(1)
      tibble(common_name = .x) %>% 
        mutate(
          habitat = 
            .x %>%
            str_remove("'") %>% 
            str_replace_all(" ", "_") %>% 
            file.path(
              "https://www.allaboutbirds.org/guide",
              .,
              "lifehistory") %>%
            httr::GET() %>% 
            XML::htmlParse() %>% 
            as("character") %>% 
            str_extract("<li><a href=\"#habitat\"><span class=\"icon habitat\">.*</li>") %>% 
            str_extract("<span>Habitat</span>.*</span>") %>% 
            str_remove("<span>Habitat</span><span>") %>% 
            str_remove("</span></span>")
        )
    }
  )






