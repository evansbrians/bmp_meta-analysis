# This script:
# - Reads the screened effects from the audits and the study locations from
#   the database
# - Moves the northeastern states out of the "other" region label
# - Writes the paper and record counts by region, and by practice and region
# - Writes the study counts by geography, included against excluded
# - Draws static maps of the studies by continent, country and state

# setup --------------------------------------------------------------------

library(DBI)
library(duckdb)
library(rnaturalearth)
library(sf)
library(tmap)
library(tidyverse)

# Project functions:

source("src/functions.R")

# Output directories:

fs::dir_create("output/draft_output/tables")
fs::dir_create("output/supplementals/supplemental_figures")

# Screened effects:

screened_effects <-
  read_csv(
    "output/draft_output/audits/screened_effects.csv",
    show_col_types = FALSE
  )

# The records that reached a modeling pool:

papers_by_pool <-
  screened_effects %>%
  filter(
    is.na(excluded_by),
    !non_grassland_class,
    in_primary_pool
  ) %>%
  select(key, region, bmp)

# Connect to the database:

con <-
  dbConnect(
    duckdb(),
    dbdir = "data/raw/bmp_meta.duckdb",
    read_only = TRUE
  )

# Study locations:

study_locations <-
  tbl(con, "study_place") %>%
  collect()

# Disconnect:

dbDisconnect(con, shutdown = TRUE)

# piecing apart regions ----------------------------------------------------

# Studies labeled other:

other_region_studies <-
  study_locations %>%
  semi_join(
    papers_by_pool %>%
      filter(region == "other"),
    by = join_by(study_key == key)
  )

# Northeastern studies:

northeast <-
  study_locations %>%
  filter(
    geography_type == "state",
    geography %in%
      c("new_york", "vermont", "massachusetts")
  ) %>%
  distinct(study_key) %>%
  pull()

# Relabel them northeast:

primary_pool_regions <-
  papers_by_pool %>%
  mutate(
    region =
      case_when(
        key %in% northeast ~ "northeast_us",
        .default = region
      )
  )

# studies by geography -----------------------------------------------------

# The studies holding at least one modeled record:

included_studies <-
  papers_by_pool %>%
  distinct(key) %>%
  pull()

# Every located study, marked included or excluded:

studies_by_geography <-
  study_locations %>%
  mutate(
    screening = if_else(study_key %in% included_studies, "included", "excluded")
  ) %>%
  summarize(
    n_studies = n_distinct(study_key),
    .by = c(geography_type, geography, continent, screening)
  ) %>%
  pivot_wider(
    names_from = screening,
    values_from = n_studies,
    names_prefix = "n_studies_",
    values_fill = 0
  ) %>%
  mutate(
    n_studies_total = n_studies_included + n_studies_excluded
  ) %>%
  arrange(
    geography_type,
    desc(n_studies_total)
  )

# maps ---------------------------------------------------------------------

# Country outlines, less Antarctica:

country_outlines <-
  ne_countries(
    scale = "medium",
    returnclass = "sf"
  ) %>%
  filter(continent != "Antarctica") %>%
  select(
    place = admin,
    continent_name = continent
  )

# Continent outlines, dissolved from the countries:

continent_outlines <-
  country_outlines %>%
  group_by(continent_name) %>%
  summarize() %>%
  ungroup() %>%
  rename(place = continent_name)

# State and province outlines:

state_outlines <-
  ne_states(
    country = c("united states of america", "canada"),
    returnclass = "sf"
  ) %>%
  select(place = name)

# Names the outlines spell differently:

map_place_names <-
  read_csv(
    "src/map_place_names.csv",
    show_col_types = FALSE
  )

# Every located study at state, country and continent grain:

study_places <-
  study_locations %>%
  left_join(
    map_place_names,
    by = join_by(geography)
  ) %>%
  mutate(
    place = coalesce(map_place, restore_place_name(geography)),
    country =
      case_match(
        geography_type,
        "state" ~ "United States of America",
        "province" ~ "Canada",
        .default = place
      ),
    included = study_key %in% included_studies
  ) %>%
  left_join(
    country_outlines %>%
      st_drop_geometry() %>%
      select(
        country = place,
        continent_name
      ),
    by = join_by(country)
  )

# The included studies, at every grain each one carries:

study_grains <-
  study_places %>%
  filter(included) %>%
  mutate(
    state =
      if_else(
        geography_type %in% c("state", "province"),
        place,
        NA_character_
      ),
    continent = continent_name
  ) %>%
  select(
    study_key,
    state,
    country,
    continent
  ) %>%
  pivot_longer(
    cols = !study_key,
    names_to = "grain",
    values_to = "place",
    values_drop_na = TRUE
  )

# Included study counts at each grain:

study_counts <-
  study_grains %>%
  summarize(
    n_studies = n_distinct(study_key),
    .by = c(grain, place)
  )

# The outlines and title for each grain:

grain_outlines <-
  list(
    continent = continent_outlines,
    country = country_outlines,
    state = state_outlines
  )

grain_titles <-
  c(
    continent = "Studies by continent",
    country = "Studies by country",
    state = "Studies by state or province"
  )

# One map per grain:

study_maps <-
  grain_outlines %>%
  imap(
    \(.outlines, .grain) {
      draw_study_map(
        .outlines = .outlines,
        .counts =
          study_counts %>%
          filter(grain == .grain) %>%
          select(!grain),
        .title = grain_titles[[.grain]]
      )
    }
  )

# write --------------------------------------------------------------------

# Studies labeled other:

other_region_studies %>%
  write_output_table(
    file_name = "geography_other_region_studies.csv"
  )

# Papers and records by region:

primary_pool_regions %>%
  summarize(
    n_papers = n_distinct(key),
    n_records = n(),
    .by = region
  ) %>%
  write_output_table(
    file_name = "geography_by_region.csv"
  )

# By practice and region:

primary_pool_regions %>%
  summarize(
    n_papers = n_distinct(key),
    n_records = n(),
    .by = c(bmp, region)
  ) %>%
  write_output_table(
    file_name = "geography_by_bmp_region.csv"
  )

# Studies by geography, included against excluded:

studies_by_geography %>%
  write_output_table(
    file_name = "geography_studies_included_excluded.csv"
  )

# The maps:

study_maps %>%
  iwalk(
    \(.map, .grain) {
      tmap_save(
        .map,
        filename =
          fs::path(
            "output/supplementals/supplemental_figures",
            str_c("map_studies_by_", .grain),
            ext = "png"
          ),
        width = 9,
        height = 6,
        dpi = 400
      )
    }
  )

# clear the environment ----------------------------------------------------

rm(
  list = ls()
)
