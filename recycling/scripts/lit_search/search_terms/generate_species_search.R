library(clipr)
library(rvest)
library(tidyverse)

bird_orders <-
  c(
    "Accipitriformes",
    "Apodiformes",
    "Caprimulgiformes",
    "Charadriiformes",
    "Columbiformes",
    "Cuculiformes",
    "Falconiformes",
    "Galliformes",
    "Gruiformes",
    "Otidiformes",
    "Passeriformes",
    "Piciformes",
    "Strigiformes"
  )

# north and south america grassland birds ---------------------------------

# From BBS:

grassland_birds_bbs <- 
  read_html("https://www.mbr-pwrc.usgs.gov/bbs/grass/actlist.htm") %>% 
  html_elements("a") %>% 
  as.character() %>% 
  str_remove("</a>.*$") %>% 
  str_remove("<a href=\"a[0-9]{4}.htm\">") %>% 
  str_replace("Chestnut-col.", "Chestnut-collared") %>% 
  str_replace("Common barn-owl", "Barn owl") %>% 
  str_trim() %>% 
  sort()

# From Partners in Flight (Avian Conservation Assessment Database):

avian_db <- 
  googlesheets4::read_sheet(
    file.path(
      "https://docs.google.com/spreadsheets/d",
      "1UhSvMqwGYMTpQFYyjrDI2xKxvZQVMSVRXHtaH10y_ss",
      "edit?gid=955867080#gid=955867080"
    )
  ) %>% 
  janitor::clean_names() 

# Grab a subset of grassland and shrubland birds:

grass_shrub_subset <- 
  avian_db %>% 
  filter(
    if_any(
      contains("habitat"),
      ~ str_detect(.x, "[Gg]rass|Open|Scrub|Chaparral")
    )|
      !is.na(agriculture),
    order %in% bird_orders
  ) %>% 
  arrange(common_name) %>% 
  distinct(common_name)

# Birds of our region are all classified as forest:

danged_forest_birds <- 
  avian_db %>% 
  anti_join(
    grass_shrub_subset,
    by = "common_name"
  ) %>% 
  filter(
    group == "landbird",
    if_any(
      primary_breeding_habitat:secondary_nonbreeding_habitat,
      ~ str_detect(.x, "Forests:  (Temperate|Mesoamerican) (Generalist|Eastern|Western|Pine-Oak|Highland)") 
    ),
    order %in% bird_orders
  ) %>%
  distinct(common_name) %>% 
  arrange(common_name) %>% 
  pull()

# For birds of eastern forests, use All About Birds for classification:

all_about_birds_eastern_forest <- 
  danged_forest_birds %>% 
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

# Additional open habitat species in Eastern forests, based on All About Birds:

aab_added_species <- 
  all_about_birds_eastern_forest %>% 
  filter(habitat != "Forests") %>% 
  pull(common_name)

# All of the open habitat species for North America:

north_america_grassland_spp <- 
  grass_shrub_subset %>% 
  pull() %>% 
  c(
    grassland_birds_bbs,
    aab_added_species,
    grass_shrub_subset %>% 
      pull() %>% 
      str_to_sentence()
  ) %>% 
  str_to_sentence() %>% 
  unique() %>% 
  sort()

# europe grassland birds --------------------------------------------------

europe_birds <-
  traitdata::eubirds %>% 
  as_tibble() %>% 
  janitor::clean_names() %>% 
  filter(
    if_any(
      woodland:mountain_meadows,
      ~ .x == 1
    ),
    order %in% bird_orders
  ) %>% 
  unite(
    genus:species, 
    col = "scientific_name",
    sep = " "
  ) %>% 
  select(scientific_name) %>% 
  left_join(
    read_csv("data/birdlife_international_all_species.csv") %>% 
      janitor::clean_names() %>% 
      select(scientific_name, common_name),
    by = "scientific_name"
  ) %>% 
  drop_na() %>% 
  pull(common_name) %>% 
  str_to_sentence()

# generating a search term ------------------------------------------------

str_c(
  "(TS = (",
  str_c(
    c(
      north_america_grassland_spp, 
      europe_birds
    ) %>% 
      unique() %>% 
      sort() %>% 
      str_c("\"", ., "\""),
    collapse = " OR "),
  "))"
) %>% 
  write_file("searches/species_search.txt")




