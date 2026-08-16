
# Purpose: This script:

# - Combines the habitat classifications from multiple sources
# - Defines facultative and grassland grassland species
# - Provides a reference to subset to facultative and obligate grassland species

# setup -------------------------------------------------------------------

library(here)
library(tidyverse)

# Grab classification frames:

species_classes_combined <- 
  list.files(
    here(
      "data/raw/for_species_classification",
      "species_classified_by_source"
    )
  ) %>% 
  set_names() %>% 
  imap(
    \ (.x, idx) {
      here(
        "data/raw/for_species_classification",
        "species_classified_by_source",
        .x
      ) %>% 
        read_csv() %>% 
        
        # Remove primary habitat (birdbase) because it is already listed in
        # habitat:
        
        select(
          !matches("^primary")
        ) %>% 
        
        # Rename species and habitat columns, if necessary:
        
        rename(
          species = matches("common_name"),
          classification = matches("habitat")
        ) %>% 
        
        # Add a source column, if necessary:
        
        mutate(
          source = 
            case_when(
              str_detect(idx, "aab\\.csv$") ~ "all_about_birds",
              str_detect(idx, "birdbase\\.csv$") ~ "birdbase",
              str_detect(idx, "eubirds\\.csv$") ~ "eubirds",
              str_detect(idx, "john_2006\\.csv$") ~ "peterjohn_2006",
              str_detect(idx, "pif\\.csv$") ~ "partners_in_flight",
              str_detect(idx, "vgbi\\.csv$") ~ "vgbi",
              str_detect(idx, "vickery") ~ "vickery_1999"
            ),
        
          # Repair names, if necessary:
          
          species = str_to_snake(species)
        )
    } 
  ) %>% 
  list_rbind() %>% 
  arrange(species)

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
      ~ species, ~ source, ~ classification,
      
      # Classes we had to define by hand:
      
      "breeding_shrub_scrub_species", "hand_entered", "shrub",
      "indigo_bunting_blue_grosbeak", "hand_entered", "facultative",
      "meadowlark_spp", "hand_entered", "obligate",
      "artificial_nests", "hand_entered", NA,
      "artificial_nests_northern_bobwhite", "hand_entered", NA,
      "artificial_nests_chestnut_sided_warbler", "hand_entered", NA,
      "artificial_nests_ovenbird", "hand_entered", NA,
      
      # Classes defined in the articles themselves:
      
      "acadian_flycatcher_indigo_bunting", "article_classified",
      "shrub; forest",
      "all_species", "article_classified", NA,
      "artificial_nests", "article_classified", NA,
      "artificial_nests_chestnut_sided_warbler", "article_classified", NA,
      "artificial_nests_northern_bobwhite", "article_classified", NA,
      "artificial_nests_ovenbird", "article_classified", NA,
      "bird_and_mammal_species", "article_classified", NA,
      "breeding_grassland_species", "article_classified", "obligate",
      "breeding_species", "article_classified", NA,
      "carnivores", "article_classified", NA,
      "edge_species", "article_classified", "facultative",
      "facultative_grassland_species", "article_classified", "facultative",
      "farmland_bird_indicator_species", "article_classified", "obligate",
      "farmland_specialists", "article_classified", "obligate",
      "frugivores", "article_classified", NA,
      "generalists", "article_classified", NA,
      "granivores", "article_classified", NA,
      "grasshopper_sparrow_henslows_sparrow", "article_classified",
      "facultative",
      "grassland_facultative_species", "article_classified", "facultative",
      "grassland_obligates", "article_classified", "obligate",
      "grassland_specialists", "article_classified", "obligate",
      "grassland_species", "article_classified", "facultative",
      "ground_nesters", "article_classified", NA,
      "insectivores", "article_classified", NA,
      "non_grassland_species", "article_classified", NA,
      "non_insectivores", "article_classified", NA,
      "obligate_grassland_species", "article_classified", "obligate",
      "omnivores", "article_classified", NA,
      "passerines", "article_classified", NA,
      "resident_species", "article_classified", NA,
      "residents", "article_classified", NA,
      "shrub_species", "article_classified", "shrub",
      "sparrows", "article_classified", NA,
      "specialists", "article_classified", NA,
      "tits", "article_classified", NA,
      "wintering_shrub_scrub_species", "article_classified", "shrub",
      "wintering_species", "article_classified", NA,
      "waders", "article_classified", NA,
      "woodland_species", "article_classified", "woodland",
      
      # Classes that we had to look up in Birds of the World:
      
      "spotted_nothura", "birds_of_the_world", "obligate",
      "red_billed_leiothrix", "birds_of_the_world", "forest; scrub"
    )
  )

# pass 3: lumping classes -------------------------------------------------

species_classified <- 
  species_classified_hand_classes %>% 
  arrange(source, species) %>% 
  pivot_wider(
    names_from = source, 
    values_from = classification
  ) %>% 
  
  # All About Birds and VGBI are not included within the classification system:
  
  select(
    !c(all_about_birds, vgbi)
  ) %>% 
  mutate(
    
    # Define species as obligate or facultative for the analysis:
    
    analysis_class =
      case_when(
        
        # No artificial nests:
        
        str_detect(species, "artificial") ~ NA,
        
        # All species:
        
        species == "all_species" ~ NA,
        
        
        # Shrub species if all includes a shrub class or NA:
        
        if_all(
          article_classified:vickery_1999,
          ~ str_detect(.x, "[sS](hrub|crub)|[Cc]hap") | 
            is.na(.x)
        ) ~ "shrub",
        
        # Obligate if any of the sources classify the species as such:
        
        if_any(
          article_classified:vickery_1999,
          ~ str_detect(.x, "[Oo]bligate")
        ) ~ "obligate",
        
        # Obligates defined by traitdata (Storchova and Horak 2018): 
        
        eubirds == "grassland" ~ "obligate",
        
        # Facultative if any of the sources classify the species as such:
        
        if_any(
          `article_classified`:vickery_1999,
          ~ str_detect(.x, "[Ff]acultative")
        ) ~ "facultative",
        
        # Facultative as defined by multiple traitdata and birdbase classes that
        # include grassland, savannah, or plains:
        
        if_any(
          c(birdbase, eubirds),
          ~ str_detect(.x, "[Gg]rassland|[Ss]avannah|[Pp]lains"),
        ) ~ "facultative",
        
        # Facultative if Partners in Flight class includes mosaic:
        
        str_detect(partners_in_flight, "[Mm]osaic") ~ "facultative",
        
        # Partners in Flight combination for shrub class:
        
        str_detect(partners_in_flight, "[sS](hrub|crub)|[Cc]hap") ~ "shrub",
        
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

# species group -----------------------------------------------------------

# Not every label in the extraction is one bird species. Anything the four
# tests below miss is taken to be a species.

species_classified_includes_grouping <-
  species_classified %>%
  mutate(
    species_group =
      case_when(
        str_detect(species, "^artificial_nests") ~ "artificial_nest",
        species == "bird_and_mammal_species" ~ "non_bird",
        species %in%
          c(
            "all_species",
            "breeding_species",
            "wintering_species"
          ) ~ "aggregate",
        species %in%
          c(
            "breeding_grassland_species",
            "breeding_shrub_scrub_species",
            "carnivores",
            "edge_species",
            "facultative_grassland_species",
            "farmland_bird_indicator_species",
            "farmland_specialists",
            "frugivores",
            "generalists",
            "granivores",
            "grassland_facultative_species",
            "grassland_obligates",
            "grassland_specialists",
            "grassland_species",
            "ground_nesters",
            "insectivores",
            "meadowlark_spp",
            "non_grassland_species",
            "non_insectivores",
            "obligate_grassland_species",
            "omnivores",
            "passerines",
            "resident_species",
            "residents",
            "shrub_species",
            "sparrows",
            "specialists",
            "tits",
            "waders",
            "wintering_shrub_scrub_species",
            "woodland_species"
          ) ~ "guild",
        .default = "species"
      ),
    .after = species
  )

# final edits and write data ----------------------------------------------

# Species-analysis frame:

species_classified %>%
  write_csv(
    here(
      "data/processed",
      "species_classified_analysis_frame.csv"
    )
  )
