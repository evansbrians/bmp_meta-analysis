# setup -------------------------------------------------------------------

library(tidyverse)

source("scripts/functions.R")

# Get species classification table:

species_classification_start <- 
  read_rds("data/processed/species_classification.rds") %>% 
  mutate(
    across(
      c(classification, species = common_name),
      ~ tolower(.x) %>% 
        str_replace_all(" ", "_")
    )
  ) %>% 
  distinct(species, classification) %>% 
  arrange(species) %>% 
  
  # Keep only grassland or shrub classes:
  
  filter(
    !str_detect(classification, "develop|^town|river|lakes|shore|ocean")
  )

# Species tables from the Google spreadsheet:

species_table_start <- 
  read_effect_size_tables() %>% 
  map_df(
    ~ select(.x, species)
  ) %>% 
  distinct()

# species classification --------------------------------------------------

species_classified <- 
  species_classification_start %>% 
  
  # Classify into shrub and obligate or facultative grassland species:
  
  mutate(
    classification =
      case_when(
        str_detect(classification, "^fac|forag|overw|mosaic|woodl") ~
          "facultative_grassland",
        str_detect(
          "obligate|grasslands|marsh|tundra") ~
          "obligate_grassland",
        str_detect(classification, "s[ch]rub|chap") ~
          "shrub",
        .default = classification
      )
  ) %>% 
  distinct(species, classification) %>% 
  
  # Collapse classes:
  
  summarize(
    classification = str_c(classification, collapse = ";"),
    .by = species
  ) %>% 
  arrange(species)

# species lumping table ---------------------------------------------------

species_lumped <-
  species_table_start %>% 
  mutate(
    species_new = 
      species %>% 
      str_remove("'") %>% 
      str_replace_all("[- ]", "_") %>% 
      str_replace("florida_g", "g") %>% 
      str_replace("meadowlark_spp\\.", "eastern_meadowlark") %>% 
      str_replace("swainsonts", "swainsons") %>% 
      str_replace("thick_billed", "mccowns")
  ) %>% 
  arrange(species_new) %>% 
  left_join(
    species_classified,
    join_by(species_new == species)
  ) %>% 
  
  # Add species previously lumped:
  
  mutate(
    classification = 
      case_when(
        str_detect(
          species_new,
          str_c(
            "^grassland_(obli|spec)",
            "^obligate_g",
            "grasshopper_sparrow;_henslows_sparrow",
            "breeding_grassland",
            "skylark",
            sep = "|"
          )
        ) ~ "obligate_grassland",
        str_detect(
          species_new,
          str_c(
            "^facultative",
            "corn_bunting",
            "black_tailed_godw",
            "grassland_facultative",
            "lapwing",
            "meadow_pipit",
            "red_naped_sapsucker",
            "white_stork",
            "wilsons_phalarope",
            "wilsons_snipe",
            sep = "|"
          ),
        ) ~ "facultative_grassland",
        str_detect(
          species_new,
          str_c(
            "shrub",
            "edge_sp",
            "indigo_bunting;_blue_grosbeak",
            "chestnut_sided_warbler",
            "eurasian_blue_tit",
            "great_tit",
            "hooded_warbler",
            "plain_titmouse",
            "red_billed_leiothrix",
            "swamp_sparrow",
            sep = "|"
          )
        ) ~ "shrub",
        str_detect(
          species_new,
          str_c(
            "northern_bobw",
            "common_quail",
            "common_wood_pigeon",
            "eurasian_collared_dove",
            "eurasian_wryneck",
            "least_flycatcher",
            "ortolan_bunting",
            "red_backed_shrike",
            "rose_breasted_grosbeak",
            "ruby_crowned_kinglet",
            "ruffed_grouse",
            "spotted_nothura",
            "tree_pipit",
            "wagtail",
            "wheatear",
            "whinchat",
            "yellowhammer",
            sep = "|"
          )
        ) ~ "facultative_grassland;shrub",
        .default = classification
      )
  )

species_lumped %>% 
  write_rds("data/proc_2025-10-16/species_lumped.rds")

# old below here ----------------------------------------------------------

# Get pre-processed data:

species_classes_binned <- 
  read_rds("data/processed/species_classification.rds") %>% 
  filter(
    !str_detect(classification_source, "^A")
  ) %>% 
  bind_rows(
    read_rds("data/processed/habitat_classification_all_about_birds.rds") %>% 
      mutate(
        common_name,
        classification = habitat,
        classification_source = "All About Birds",
        .keep = "none"
      )
  ) %>% 
  mutate(
    common_name = 
      common_name %>% 
      str_replace_all("-", "_") %>% 
      str_replace_all(" ", "_"),
    classification = 
      classification %>% 
      tolower() %>% 
      str_replace_all("_", " "),
    combined_class = 
      case_when(
        str_detect(classification, "^fac|forag|overw") ~
          "facultative grassland",
        str_detect(classification, "obligate|grasslands|marsh|tundra") ~
          "obligate grassland",
        str_detect(classification, "s[ch]rub|chap|marsh|mosaic|woodl") ~
          "shrub",
        str_detect(classification, "develop|^town") ~
          "developed",
        str_detect(classification, "river|lakes|shore|ocean") ~
          "water",
        .default = classification
      ) %>% 
      str_replace_all(" ", "_")
  ) %>% 
  select(
    !c(
      classification_source,
      classification, 
      habitat_rank
    )
  ) %>% 
  arrange(common_name) %>% 
  filter(
    !str_detect(combined_class, "^des")
  ) %>% 
  distinct() %>% 
  nest(data = combined_class)

# Species column from effect size tables:

effect_size_species_var <- 
  read_rds("data/processed/effect_size_tables.rds") %>% 
  map_df(
    ~ .x %>% 
      distinct(species)
  ) %>% 
  distinct() %>% 
  arrange(species)

# Just birds classified to species:

effect_size_species <- 
  effect_size_species_var %>% 
  filter(
    !str_detect(
      species,
      str_c(
        "^pas|spec",
        "^ar",
        "^res",
        "omn|frug|insect|gran|carn",
        "obl|gen",
        sep = "|"
      )
    )
  ) %>% 
  mutate(
    species = str_split(species, "; ")
  ) %>% 
  unnest(species) %>% 
  mutate(
    species = 
      species %>% 
      str_replace("-", " ") %>% 
      str_remove("'") %>% 
      str_replace_all(" ", "_") %>% 
      str_replace("florida_g", "g") %>% 
      str_replace("meadowlark_spp\\.", "eastern_meadowlark") %>% 
      str_replace("swainsonts", "swainsons") %>% 
      str_replace("thick_billed", "mccowns")
  ) %>% 
  distinct(species) %>% 
  rename(common_name = species) %>% 
  filter(
    !str_detect(common_name, "tits|waders")
  ) 

# explore -----------------------------------------------------------------

# European species not covered in our classification system (looked up in 
# Birds of the World):

birds_not_north_america <- 
  effect_size_species %>% 
  anti_join(
    species_classes_binned,
    by = "common_name"
  ) %>% 
  mutate(
    combined_class = 
      case_when(
        str_detect(
          common_name,
          str_c(
            "common_q|pigeo|blue_tit|wryn|great_tit|plain_t|shrike|leio|tengm|",
            "pipit|yellowh"
          )
        ) ~ "shrub",
        str_detect(
          common_name,
          "godw|corn_|lapw|nothur"
        ) ~ "obligate_grassland",
        str_detect(
          common_name,
          "whinch|cornc|wagtail|white_st|wheat"
        ) ~ "facultative_grassland",
        .default = "other"
      )
  ) %>% 
  filter(common_name != "other")

# Final species lump list:

c(
  "shrub",
  "obligate_grassland",
  "facultative_grassland"
) %>% 
  set_names() %>% 
  map(
    ~ species_classes_binned %>% 
      semi_join(
        effect_size_species,
        by = "common_name"
      ) %>% 
      unnest(data) %>% 
      bind_rows(birds_not_north_america) %>% 
      filter(combined_class == .x) %>% 
      distinct()
  ) %>% 
  write_rds("data/processed/species_lump_list.rds")
