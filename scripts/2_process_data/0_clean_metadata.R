# This script:
# - Reads the paper metadata Google sheet
# - Cleans the screening flags and the notes column
# - Repairs the geography
# - Saves the result (one place per row) to data/processed

# setup -------------------------------------------------------------------

library(here)
library(tidyverse)

# Project functions:

source("src/functions.R")

# Sentinels that mean missing:

null_tokens <-
  c(
    "",
    "-",
    "na",
    "n/a",
    "none",
    "nan"
  )

# Google sheet url:

url <-
  str_c(
    "https://docs.google.com/spreadsheets/d/",
    "1Lf3v8fU0sCCAcJ6Wj1v8GwgogjI4ve3xcMlPzLW0hnU"
  )

# The sheet, with syntactic headers:

paper_metadata <-
  googlesheets4::read_sheet(url) %>%
  janitor::clean_names()

# Fold place names to snake_case:

fix_place_names <-
  function(.place) {
    .place %>%
      stringi::stri_trans_general("Latin-ASCII") %>%
      fix_common_names()
  }

# clean flags and notes ---------------------------------------------------

# Some basic cleaning:

paper_metadata_cleaned_fields <-
  paper_metadata %>%
  mutate(

    # Squish text and blank the sentinels:

    across(
      where(is.character),
      \(.column) {
        squished <- str_squish(.column)
        if_else(
          str_to_lower(squished) %in% null_tokens,
          NA_character_,
          squished
        )
      }
    ),

    # Lower and snake_case:

    across(
      any_of(
        c(
          "reviewed",
          "article_type",
          "effect_size",
          "useful",
          "problem"
        )
      ),
      \(.column) {
        .column %>%
          str_to_lower() %>%
          str_replace_all("[^a-z0-9]+", "_")
      }
    )
  )

# canonical bmps ----------------------------------------------------------

# Canonicalize the BMP names:

paper_metadata_canonical_bmps <-
  paper_metadata_cleaned_fields %>%
  mutate(
    bmp =
      case_when(
        bmp == "set_aside_adjacent_unmowed" ~ "manage_in_patches",
        bmp == "grazing_intensity" ~ "reduce_grazing_intensity",
        bmp == "summer_pasture_stockpiling" ~ "rotational_grazing",
        str_detect(bmp, "^remove_non[-_]native") ~ "remove_non_native_shrubs",
        .default = bmp
      ),
    multiple_bmps =
      multiple_bmps %>%
      str_replace_all(
        "set_aside_adjacent_unmowed",
        "manage_in_patches"
      ) %>%
      str_replace_all(
        "\\bgrazing_intensity\\b",
        "reduce_grazing_intensity"
      ) %>%
      str_replace_all(
        "summer_pasture_stockpiling",
        "rotational_grazing"
      ) %>%
      str_replace_all(
        "remove_non[-_]native_species",
        "remove_non_native_shrubs"
      )
  )

# reference geography -----------------------------------------------------

# List out the continents:

continent_reference <-
  read_csv(
    here("src", "continent_reference.csv"),
    show_col_types = FALSE
  )

# Country reference:

country_reference <-
  ISOcodes::ISO_3166_1 %>%
  pivot_longer(
    c(Name, Official_name, Common_name),
    values_to = "reference_name"
  ) %>%
  filter(
    !is.na(reference_name)
  ) %>%
  mutate(
    place = fix_place_names(reference_name),
    place_type = "country",
    country_code = Alpha_2,
    .keep = "none"
  )

# State and province reference:

state_province_reference <-
  ISOcodes::ISO_3166_2 %>%
  mutate(
    place = fix_place_names(Name),
    place_type = str_to_lower(Type),
    country_code =
      str_sub(Code, end = 2),
    .keep = "none"
  )

# Hand-entered geographies:

geography_by_hand <-
  read_csv(
    here("src", "geography_by_hand.csv"),
    show_col_types = FALSE
  ) %>%

  # Drop the note column:

  select(!note)

# Geography lookup:

geography_lookup <-
  geography_by_hand %>%
  bind_rows(
    list(
      continent_reference,
      country_reference,
      state_province_reference
    ) %>%
      map(
        ~ mutate(.x, recorded_place = place)
      )
  ) %>%
  distinct(
    recorded_place,
    .keep_all = TRUE
  )

# clean geographies -------------------------------------------------------

paper_metadata_cleaned_geographies <-
  paper_metadata_canonical_bmps %>%

  # Split multi-place records:

  separate_longer_delim(geography, ";") %>%
  mutate(
    geography = str_trim(geography),
    recorded_place = fix_place_names(geography)
  ) %>%

  # Match the lookup:

  left_join(
    geography_lookup,
    by = join_by(recorded_place)
  ) %>%

  # Add names and positions:

  mutate(
    geography = coalesce(place, recorded_place),
    geography_type = place_type,
    continent =
      country_code %>%
      countrycode::countrycode(
        origin = "iso2c",
        destination = "continent",
        warn = FALSE
      ) %>%
      fix_place_names(),
    .after = article_type
  ) %>%
  select(
    !c(
      recorded_place,
      place,
      place_type,
      country_code
    )
  )

# filtering pass ----------------------------------------------------------

# Keep only the final BMPs:

paper_metadata_bmp_subset <-
  paper_metadata_cleaned_geographies %>%
  select(!multiple_bmps) %>%
  filter(
    !bmp %in% c("keep_cats_indoors", "upgrade_to_darksky")
  )

# write to file -----------------------------------------------------------

paper_metadata_bmp_subset %>%
  write_csv(
    here("data/processed", "paper_metadata.csv")
  )

# clear the environment ----------------------------------------------------

rm(
  list = ls()
)
