
# Purpose: This script:

# - Cleans the species lists in the two study results frames (mean_diff and
#   beta_categorical)
# - Defines facultative and grassland grassland species
# - Subsets the frames for analysis to just those associated with facultative 
#   and grassland species.

# setup -------------------------------------------------------------------

library(here)
library(tidyverse)

# Function to clean common names:

fix_common_names <-
  function(.common_name) {
    .common_name %>% 
      str_replace_all("-", " ") %>% 
      str_remove_all("'") %>% 
      str_to_snake()
  }

# Grab classification frames:

list(
  species_classes_start = "classification.csv",
  species_classes_pif = "partners_in_flight_classes.csv",
  species_classes_bb = "species_classification_birdbase.csv"
) %>% 
  map(
    ~ here::here("data/processed/species_classification", .x) %>% 
      read_csv()
  ) %>% 
  list2env(.GlobalEnv)

# Study results tables:

list(
  "mean_diff",
  "beta_categorical",
  "other_categorical"
) %>% 
  set_names() %>% 
  map(
    ~ readxl::read_xlsx(
      "data/raw/bmp_review_analysis_subset.xlsx",
      sheet = .x
    ) %>% 
      mutate(
        species = fix_common_names(species)
      )
  ) %>% 
  list2env(.GlobalEnv)

# pass 1: class combination -----------------------------------------------

# Remove hand-entered and pif, combine with pif and birdbase:

species_classes_combined <-
  species_classes_start %>% 
  filter(
    !source %in%
      c(
        "palearctic_extension",
        "hand_entered",
        "partners_in_flight"
      )
  ) %>% 
  select(!analysis) %>% 
  
  # Add partners in flight (subset to the species searched for):
  
  bind_rows(
    species_classes_pif %>% 
      semi_join(species_classes_start, by = "species") %>% 
      mutate(
        species,
        source = "partners_in_flight",
        classification = habitat,
        .keep = "none"
      )
  ) %>% 
  
  # Add birdbase (subset to the species searched for):
  
  bind_rows(
    species_classes_bb %>% 
      semi_join(species_classes_start, by = "species") %>% 
      mutate(
        species,
        source = "birdbase",
        classification = habitat,
        .keep = "none"
      )
  ) %>% 
  arrange(species) %>% 
  left_join(
    species_classes_start %>% 
      distinct(species, analysis),
    by = "species"
  )

# pass 2: hand classes ----------------------------------------------------

# Read in classification data across our sources; integrate trait data and one
# hand-entered double-species into the classification frame:

species_classified_hand_classes <- 
  species_classes_combined %>% 
  mutate(
    source = 
      if_else(
        str_detect(source, "vickery"),
        "vickery_1999",
        source
      )
  ) %>% 
  bind_rows(
    tribble(
      ~ species, ~ source, ~ classification, ~ analysis,
      
      # Classes we had to define by hand:
      
      "breeding_shrub_scrub_species", "hand_entered", "shrub", 1,
      "indigo_bunting_blue_grosbeak", "hand_entered", "facultative", 1,
      "meadowlark_spp", "hand_entered", "obligate", 1,
      
      # Classes defined in the articles themselves:
      
      "all_species", "article-classified", NA, 1,
      "artificial_nests", "article-classified", NA, 0,
      "artificial_nests_chestnut_sided_warbler", "article-classified", NA, 0,
      "artificial_nests_northern_bobwhite", "article-classified", NA, 0,
      "artificial_nests_ovenbird", "article-classified", NA, 0,
      "bird_and_mammal_species", "article-classified", NA, 1,
      "breeding_grassland_species", "article-classified", "obligate", 1,
      "breeding_species", "article-classified", NA, 1,
      "carnivores", "article-classified", NA, 1,
      "edge_species", "article-classified", "facultative", 1,
      "farmland_bird_indicator_species", "article-classified", "obligate", 1,
      "frugivores", "article-classified", NA, 1,
      "generalists", "article-classified", NA, 1,
      "granivores", "article-classified", NA, 1,
      "grassland_facultative_species", "article-classified", "facultative", 1,
      "grassland_obligates", "article-classified", "obligate", 1,
      "grassland_species", "article-classified", "facultative", 1,
      "ground_nesters", "article-classified", NA, 1,
      "insectivores", "article-classified", NA, 1,
      "obligate_grassland_species", "article-classified", "obligate", 1,
      "omnivores", "article-classified", NA, 1,
      "passerines", "article-classified", NA, 1,
      "resident_species", "article-classified", NA, 1,
      "residents", "article-classified", NA, 1,
      "shrub_species", "article-classified", "shrub", 1,
      "sparrows", "article-classified", NA, 1,
      "specialists", "article-classified", NA, 1,
      "tits", "article-classified", NA, 1,
      "wintering_shrub_scrub_species", "article-classified", "shrub", 1,
      "wintering_species", "article-classified", NA, 1,
      "woodland_species", "article-classified", "woodland", 1,
      
      # Classes defined in traitdata (Storchova and Horak 2018):
      
      "black_tailed_godwit", "traitdata", "grassland; swamp", 1,
      "corn_bunting", "traitdata", "grassland", 1,
      "eurasian_blue_tit", "traitdata", "woodland", 1,
      "eurasian_skylark", "traitdata", "grassland; mountain_meadow", 1,
      "eurasian_wryneck", "traitdata", "woodland", 1,
      "great_tit", "traitdata", "woodland", 1,
      "meadow_pipit", "traitdata", "grassland; mountain_meadow", 1,
      "northern_lapwing", "traitdata", "grassland", 1,
      "ortolan_bunting", "traitdata", "woodland; shrub", 1,
      "western_yellow_wagtail", "traitdata", "grassland; shrub; swamp", 1,
      "whinchat", "traitdata", "grassland", 1,
      "wild_turkey", "traitdata", "woodland; grassland", 1
      
      # Classes that we had to look up in Birds of the World:
      
      # "european_starling", "birds_of_the_world", "facultative", 1,
      # "eurasian_collared_dove", "birds_of_the_world", "facultative", 1,
    )
  )

# pass 3: analysis subset -------------------------------------------------

# Clean species names for the mean_diff table:

mean_diff_species_cleaned <-
  mean_diff %>% 
  mutate(
    species = 
      species %>% 
      str_replace("^lapwing", "northern_lapwing") %>% 
      str_replace("^skylark", "eurasian_skylark") %>% 
      str_replace("florida_grasshopper_sparrow", "grasshopper_sparrow") %>% 
      str_replace("yellow_wagtail", "western_yellow_wagtail") %>% 
      str_replace("mc_cowns", "mccowns") %>% 
      str_replace("thick_billed_longspur", "mccowns_longspur") %>% 
      str_replace("tengmalms", "boreal") %>% 
      str_replace("plain_titmouse", "oak_titmouse") %>% 
      str_replace("indigo_bunting_blue_grosbeak", "")
  ) %>% 
  filter(
    nchar(species) > 1
  )

# Other categorical (beta_categorical was fine):

other_categorical_cleaned <-
  other_categorical %>% 
  mutate(
    species =
      species %>% 
      str_replace("mc_cowns", "mccowns") %>% 
      str_replace("alpine_whinchat", "whinchat")
  )

# Combine species list from the two study results frames:

bmp_review_analysis_edit <-
  bmp_review_analysis_subset_name_fix %>% 
  select(species) %>% 
  bind_rows(
    beta_categorical_cleaned %>% 
      select(species),
    other_categorical_cleaned %>% 
      select(species)
  )

# Check the species list to ensure matching names:

bmp_review_analysis_edit %>% 
  select(species) %>% 
  distinct() %>% 
  anti_join(species_classified_hand_classes, by = "species") %>% 
  arrange(species) %>% 
  print(n = Inf)

# pass 4: lumping classes -------------------------------------------------

species_classified <- 
  species_classified_hand_classes %>% 
  arrange(source, species) %>% 
  pivot_wider(
    names_from = source, 
    values_from = classification
  ) %>% 
  mutate(
    
    # Define species as obligate or facultative for the analysis:
    
    analysis_class =
      case_when(
        
        # No artificial nests:
        
        str_detect(species, "artificial") ~ NA,
        
        # Shrub species if all includes a shrub class or NA:
        
        if_all(
          all_about_birds:vickery_1999,
          ~ str_detect(.x, "[sS](hrub|crub)|[Cc]hap") | 
            is.na(.x)
        ) ~ "shrub",
        
        # Obligate if any of the sources classify the species as such:
        
        if_any(
          all_about_birds:vickery_1999,
          ~ str_detect(.x, "[Oo]bligate")
        ) ~ "obligate",
        
        # Obligates defined by traitdata (Storchova and Horak 2018): 
        
        traitdata == "grassland" ~ "obligate",
        
        # Facultative if any of the sources classify the species as such:
        
        if_any(
          all_about_birds:vickery_1999,
          ~ str_detect(.x, "[Ff]acultative")
        ) ~ "facultative",
        
        # Facultative as defined by multiple traitdata and birdbase classes that
        # include grassland, savannah, or plains:
        
        if_any(
          c(birdbase, traitdata),
          ~ str_detect(.x, "[Gg]rassland|[Ss]avannah|[Pp]lains"),
        ) ~ "facultative",
        
        # Facultative if Partners in Flight class includes mosaic:
        
        str_detect(partners_in_flight, "[Mm]osaic") ~ "facultative",
        
        # All About Birds & Partners in Flight combination for shrub class:
        
        str_detect(partners_in_flight, "[sS](hrub|crub)|[Cc]hap") &
          str_detect(all_about_birds, "Desert|Woodland") ~ "shrub",
        
        .default = "other"
      ),
    include = 
      if_else(
        str_detect(analysis_class, "facultative|obligate") |
          species == "all_species",
        TRUE,
        FALSE
      )
  ) %>% 
  arrange(species)

# final edits and write data ----------------------------------------------

# Species-analysis frame:

species_classified %>% 
  filter(include) %>% 
  write_csv("data/processed/for_analysis/species_classified_analysis_frame.csv")

# Mean-diff frame for analysis:

mean_diff_species_cleaned %>% 
  semi_join(
    species_classified %>% 
      filter(include),
    by = "species"
  ) %>% 
  write_csv("data/processed/for_analysis/mean_diff.csv")

# Beta categorical table for analysis:

beta_categorical %>% 
  semi_join(
    species_classified %>% 
      filter(include),
    by = "species"
  ) %>% 
  write_csv("data/processed/for_analysis/beta_categorical.csv")

# Other categorical table for analysis:

beta_categorical %>% 
  semi_join(
    species_classified %>% 
      filter(include),
    by = "species"
  ) %>% 
  write_csv("data/processed/for_analysis/other_categorical.csv")
