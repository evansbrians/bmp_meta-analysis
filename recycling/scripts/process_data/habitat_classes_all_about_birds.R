# Script for getting Cornell classification of species (aka hacking Cornell!)

# setup -------------------------------------------------------------------

library(rvest)
library(tidyverse)

temp <-
  read_html("https://eesc.usgs.gov/MBR/") %>% 
  html_elements("#aou") %>% 
  as.character()

# Get all species from https://www.mbr-pwrc.usgs.gov/ (click the button "full
# species list" then copy-and-paste):

bbs_spp <-
  clipr::read_clip() %>% 
  str_remove("[0-9]{1,}") %>% 
  str_remove("\\[.*") %>% 
  str_remove_all("'") %>% 
  str_trim() %>% 
  str_to_sentence() %>% 
  str_replace_all(" ", "_")

# Get All About Birds classifications for all North American species:

all_about_birds <- 
  bbs_spp %>% 
  map_dfr(
    \(.x) {
      Sys.sleep(1)
      
      response <-
        .x %>%
        str_remove("'") %>% 
        str_replace_all(" ", "_") %>% 
        file.path(
          "https://www.allaboutbirds.org/guide",
          .,
          "lifehistory") %>%
        read_html() %>% 
        html_elements("span") %>% 
        as.character() %>% 
        keep(
          ~ str_detect(
            .x, 
            "<span class=\"text-label\"><span>Habitat"
          )
        ) %>% 
        str_remove("^.*<span>") %>% 
        str_remove_all("</span>")
      
      if(length(response > 0)) {
        tibble(common_name = .x) %>% 
          mutate(habitat = response)
      }
    }
  )

all_about_birds %>% 
  mutate(
    common_name = 
      common_name %>% 
      str_replace_all("_", " ") %>% 
      tolower()
  ) %>% 
  write_rds("data/processed/habitat_classification_all_about_birds.rds")

