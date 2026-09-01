
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
    \(.x, idx) {
      here(
        "data/raw/for_species_classification",
        "species_classified_by_source",
        .x
      ) %>%
        read_csv() %>%

        # Drop the duplicated primary habitat:

        select(
          !matches("^primary")
        ) %>%

        # Rename the columns:

        rename(
          species = matches("common_name"),
          classification = matches("habitat")
        ) %>%

        # Add a source column:

        mutate(
          source =
            case_when(
              str_detect(idx, "aab\\.csv$") ~ "all_about_birds",
              str_detect(idx, "birdbase\\.csv$") ~ "birdbase",
              str_detect(idx, "eubirds\\.csv$") ~ "eubirds",
              str_detect(idx, "pif\\.csv$") ~ "partners_in_flight",
              str_detect(idx, "vgbi\\.csv$") ~ "vgbi",
              str_detect(idx, "vickery") ~ "vickery_1999"
            ),

          # Repair the names:

          species =
            species %>%
            str_to_snake() %>%
            str_replace("le_conte", "leconte")
        )
    }
  ) %>%
  list_rbind() %>%
  arrange(species)

# pass 2: hand classes ----------------------------------------------------

# Combine the sources:

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

  # Add the classes no source covers:

  bind_rows(
    read_csv(
      here("src", "species_classes_by_hand.csv"),
      show_col_types = FALSE
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

  # Drop the sources outside the system:

  select(
    !c(all_about_birds, vgbi)
  ) %>%
  mutate(

    # Assign the analysis class:

    analysis_class =
      case_when(

        # No artificial nests:

        str_detect(species, "artificial") ~ NA,

        # All species:

        species == "all_species" ~ NA,

        # Obligate in any source:

        if_any(
          article_classified:vickery_1999,
          ~ str_detect(.x, "[Oo]bligate")
        ) ~ "obligate",

        # Obligate in traitdata:

        eubirds == "grassland" ~ "obligate",


        # Obligate in birdbase:

        birdbase == "grassland" ~ "obligate",

        # Facultative in any source:

        if_any(
          `article_classified`:vickery_1999,
          ~ str_detect(.x, "[Ff]acultative")
        ) ~ "facultative",

        # Facultative by open-habitat classes:

        if_any(
          c(birdbase, eubirds),
          ~ str_detect(.x, "[Gg]rassland|[Ss]avannah|[Pp]lains"),
        ) ~ "facultative",

        # Facultative by mosaic class:

        str_detect(partners_in_flight, "[Mm]osaic") ~ "facultative",

        # Shrub by Partners in Flight:

        str_detect(partners_in_flight, "[sS](hrub|crub)|[Cc]hap") ~ "shrub",

        # Shrub in any remaining source:

        if_any(
          article_classified:vickery_1999,
          ~ str_detect(.x, "[sS](hrub|crub)|[Cc]hap")
        ) ~ "shrub",

        # Woodland in any remaining source:

        if_any(
          article_classified:vickery_1999,
          ~ str_detect(.x, "[Ww]oodland")
        ) ~ "woodland",

        # Forest in any remaining source:

        if_any(
          article_classified:vickery_1999,
          ~ str_detect(.x, "[Ff]orest")
        ) ~ "forest",
        .default = "other"
      ),

    # Flag the grassland classes:

    include =
      analysis_class %in%
        c("obligate", "facultative", "other") |
        species == "all_species"
  ) %>%
  arrange(species)

# species group -----------------------------------------------------------

# Flag the multi-species labels:

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
            "ground_breeders",
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

species_classified_includes_grouping %>%
  write_csv(
    here(
      "data/processed",
      "species_classified_analysis_frame.csv"
    )
  )

# clear the environment ----------------------------------------------------

rm(
  list = ls()
)
