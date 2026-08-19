# This script:
# - Reads the paper metadata Google sheet
# - Cleans the screening flags and the notes column
# - Repairs the geography
# - Saves the result (one place per row) to data/processed

# setup -------------------------------------------------------------------

library(here)
library(tidyverse)

source("scripts/functions.R")

# Sentinels that mean missing in the source workbook. "unknown" is not among
# them: in the screening columns it is a real category and is kept.

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

paper_metadata <-
  googlesheets4::read_sheet(url) %>%
  janitor::clean_names()

# Place names as this sheet records them: accents folded away, punctuation
# dropped, snake_case:

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
    
    # Character columns get squished, and the sentinels are blanked:

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
    
    # To lower and replace anything that's not a letter or number with "_":
    
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

# Ensuring that BMP names are equivalent across the data:

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
  tribble(
    ~ place, ~ place_type,
    "africa", "continent",
    "antarctica", "continent",
    "asia", "continent",
    "europe", "continent",
    "north_america", "continent",
    "oceania", "continent",
    "south_america", "continent",
    "global", "global"
  )

# Official country references:

country_reference <-
  ISOcodes::ISO_3166_1 %>%
  pivot_longer(
    c(Name, Official_name, Common_name),
    values_to = "reference_name"
  ) %>%
  filter(!is.na(reference_name)) %>%
  mutate(
    place = fix_place_names(reference_name),
    place_type = "country",
    country_code = Alpha_2,
    .keep = "none"
  )

# Official state/province reference:

state_province_reference <-
  ISOcodes::ISO_3166_2 %>%
  mutate(
    place = fix_place_names(Name),
    place_type = str_to_lower(Type),
    country_code =
      str_sub(Code, end = 2),
    .keep = "none"
  )

# Some geographies require hand-entry:

geography_by_hand <-
  tribble(
    ~ recorded_place, ~ place, ~ place_type, ~ country_code,

    # The lower 48, recorded as a region in its own right.

    "conterminous_united_states", "united_states", "country", "US",

    # ISO names this Saint Helena, Ascension and Tristan da Cunha.

    "saint_helena_island", "saint_helena", "territory", "SH",
    
    # Russia is Russian federation:
    
    "russia", "russian_federation", "country", "RU",

    # For names that are in more than one place:

    "florida", "florida", "state", "US",
    "maryland", "maryland", "state", "US",
    "montana", "montana", "state", "US",
    "georgia", "georgia", "state", "US",
    "mexico", "mexico", "country", "MX"
  )

# Make a geography look-up table using the above:

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
  
  # Long format for non-atomic geographies:
  
  separate_longer_delim(geography, ";") %>%
  mutate(
    geography = str_trim(geography),
    recorded_place = fix_place_names(geography)
  ) %>%
  
  # Match with the look-up table:
  
  left_join(
    geography_lookup,
    by = join_by(recorded_place)
  ) %>% 
  
  # Add names and define positions:
  
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

# write to file -----------------------------------------------------------

paper_metadata_cleaned_geographies %>%
  write_csv(
    here("data/processed", "paper_metadata.csv")
  )
