# Purpose: Grab habitat classifications from Storchova and Horak 2018 using the
# traitdata package and use the IUCN/Bird Life International classification to
# add common names.

# setup -------------------------------------------------------------------

library(here)
library(tidyverse)

# Bird Life common names:

bli_classes <-
  here(
    "data/raw/for_species_classification",
    "iucn_bli_classification.rds"
  ) %>%
  read_rds() %>%
  rename(common_name = name) %>%
  mutate(
    common_name =
      common_name %>%
      str_to_lower() %>%
      str_replace_all("-", "_") %>%
      str_remove_all("'") %>%
      str_replace_all(" ", "_")
  )

# Eubirds records (Storchova and Horak 2018):

eu_birds <-
  traitdata::eubirds %>%
  janitor::clean_names() %>%
  as_tibble() %>%
  select(
    genus:species,
    scientific_name_std,
    deciduous_forest:human_settlements
  ) %>%

  # Make longform:

  pivot_longer(
    cols = deciduous_forest:human_settlements,
    names_to = "classification"
  ) %>%

  # Subset to occupied classes:

  filter(value == 1) %>%

  # Non-matching genus/species and scientific names:

  mutate(
    genus =
      case_when(
        str_detect(
          scientific_name_std,
          str_c(
            "^Sylvia (cantillans|communis|conspicillata|curruca|",
            "hortensis|melanocephala|mystacea|nisoria|ruppeli|sarda|undata)"
          )
        ) ~ "Curruca",
        scientific_name_std == "Psittacula krameri" ~ "Alexandrinus",
        scientific_name_std == "Tetrastes bonasia" ~ "Tetrastes",
        scientific_name_std == "Phalacrocorax aristotelis" ~ "Gulosus",
        scientific_name_std == "Luscinia svecica" ~ "Luscinia",
        .default = genus
      ),
    species =
      case_when(
        str_detect(
          species,
          "nipalenis"
        ) ~ "nipalensis",
        genus == "Turnix" &
          species == "sylvatica" ~ "sylvaticus",
        species == "ruppelli" ~ "ruppeli",
        .default = species
      )
  ) %>%

  # Rebuild the scientific name:

  unite(
    "scientific_name",
    genus:species,
    sep = " "
  ) %>%

  # Flatten to combined classes:

  summarize(
    classification =
      str_flatten(classification, collapse = "; "),
    .by = scientific_name
  )

# Add common names:

eu_birds_matched <-
  eu_birds %>%
  left_join(
    bli_classes %>%
      distinct(scientific_name, common_name),
    by = "scientific_name",
    relationship = "many-to-many"
  )

# write! ------------------------------------------------------------------

eu_birds_matched %>%
  select(
    species = common_name,
    classification
  ) %>%
  write_csv(
    here(
      "data/raw/for_species_classification/species_classified_by_source",
      "species_classification_eubirds.csv"
    )
  )

