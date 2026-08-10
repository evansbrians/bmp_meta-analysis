
# Purpose: This script:

# - Cleans the species lists in the three study results frames (mean_diff and
#   beta_categorical)
# - Defines facultative and grassland grassland species
# - Subsets the frames for analysis to just those associated with facultative 
#   and grassland species.

# setup -------------------------------------------------------------------

library(here)
library(tidyverse)

# Grab classification frames:

list(
  species_classes_start = "classification.csv",
  species_classes_pif = "partners_in_flight_classes.csv",
  species_classes_bb = "species_classification_birdbase.csv"
) %>% 
  map(
    ~ here("data/processed/species_classification", .x) %>% 
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
  select(!analysis) %>% 
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
      ~ species, ~ source, ~ classification,
      
      # Classes we had to define by hand:
      
      "breeding_shrub_scrub_species", "hand_entered", "shrub",
      "indigo_bunting_blue_grosbeak", "hand_entered", "facultative",
      "meadowlark_spp", "hand_entered", "obligate",
      
      # Classes defined in the articles themselves:
      
      "acadian_flycatcher_indigo_bunting", "article-classified", "shrub; forest", NA,
      "all_species", "article-classified", NA,
      "artificial_nests", "article-classified", NA,
      "artificial_nests_chestnut_sided_warbler", "article-classified", NA,
      "artificial_nests_northern_bobwhite", "article-classified", NA,
      "artificial_nests_ovenbird", "article-classified", NA,
      "bird_and_mammal_species", "article-classified", NA,
      "breeding_grassland_species", "article-classified", "obligate",
      "breeding_species", "article-classified", NA,
      "carnivores", "article-classified", NA,
      "edge_species", "article-classified", "facultative",
      "facultative_grassland_species", "article-classified", "facultative",
      "farmland_bird_indicator_species", "article-classified", "obligate",
      "farmland_specialists", "article-classified", "obligate",
      "frugivores", "article-classified", NA,
      "generalists", "article-classified", NA,
      "granivores", "article-classified", NA,
      "grasshopper_sparrow_henslows_sparrow", "article-classified", "facultative",
      "grassland_facultative_species", "article-classified", "facultative",
      "grassland_obligates", "article-classified", "obligate",
      "grassland_specialists", "article-classified", "obligate",
      "grassland_species", "article-classified", "facultative",
      "ground_nesters", "article-classified", NA,
      "insectivores", "article-classified", NA,
      "non_grassland_species", "article-classified", NA,
      "non_insectivores", "article-classified", NA,
      "obligate_grassland_species", "article-classified", "obligate",
      "omnivores", "article-classified", NA,
      "passerines", "article-classified", NA,
      "resident_species", "article-classified", NA,
      "residents", "article-classified", NA,
      "shrub_species", "article-classified", "shrub",
      "sparrows", "article-classified", NA,
      "specialists", "article-classified", NA,
      "tits", "article-classified", NA,
      "wintering_shrub_scrub_species", "article-classified", "shrub",
      "wintering_species", "article-classified", NA,
      "waders", "article-classified", NA,
      "woodland_species", "article-classified", "woodland",
      
      # Classes defined in traitdata (Storchova and Horak 2018):
      
      "black_tailed_godwit", "traitdata", "grassland; swamp",
      "common_wood_pigeon", "traitdata", "deciduous_forest; coniferous_forest; woodland; human_settlements",
      "corn_bunting", "traitdata", "grassland",
      "corncrake", "traitdata", "grassland",
      "eurasian_blue_tit", "traitdata", "woodland",
      "eurasian_skylark", "traitdata", "grassland; mountain_meadow",
      "eurasian_wryneck", "traitdata", "woodland",
      "great_tit", "traitdata", "woodland",
      "meadow_pipit", "traitdata", "grassland; mountain_meadow",
      "northern_lapwing", "traitdata", "grassland",
      "ortolan_bunting", "traitdata", "woodland; shrub",
      "red_backed_shrike", "traitdata", "shrub",
      "tree_pipit", "traitdata", "deciduous_forest; coniferous_forest; woodland; savanna",
      "western_yellow_wagtail", "traitdata", "grassland; shrub; swamp",
      "wheatear", "traitdata", "tundra; grassland; mountain_meadows; rocks",
      "whinchat", "traitdata", "grassland",
      "wild_turkey", "traitdata", "woodland; grassland",
      "yellowhammer", "traitdata", "woodland; shrub; grassland",
      
      # Classes that we had to look up in Birds of the World:
      
      "spotted_nothura", "birds_of_the_world", "obligate",
      "red_billed_leiothrix", "birds_of_the_world", "forest; scrub"
      # "eurasian_collared_dove", "birds_of_the_world", "facultative", 1,
    )
  )

species_classified_hand_classes %>% 
  arrange(source, species) %>% 
  pivot_wider(
    names_from = source,
    values_from = classification
  )

# pass 3: lumping classes -------------------------------------------------

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
  write_csv(
    here(
      "data/processed/for_analysis",
      "species_classified_analysis_frame.csv"
    )
  )
    