# This script:
# - Reads the screened effects from the audits and the study locations from
#   the database
# - Moves the northeastern states out of the "other" region label

# setup --------------------------------------------------------------------

library(DBI)
library(duckdb)
library(flextable)
library(fs)
library(officer)
library(tidyverse)

# Project functions:

source("src/functions.R")

# Output directories:

dir_create("output/draft_output/tables")

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
  select(
    key,
    region,
    bmp
  )

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

# Papers and records by region:

region_counts <-
  primary_pool_regions %>%
  summarize(
    n_papers = n_distinct(key),
    n_records = n(),
    .by = region
  ) %>%
  arrange(
    desc(n_papers)
  )

# By practice and region:

bmp_region_counts <-
  primary_pool_regions %>%
  summarize(
    n_papers = n_distinct(key),
    n_records = n(),
    .by = c(bmp, region)
  ) %>%
  arrange(
    bmp,
    desc(n_papers)
  )

# studies by geography -----------------------------------------------------

# Studies holding a modeled record:

included_studies <-
  papers_by_pool %>%
  distinct(key) %>%
  pull()

# Every located study, included or excluded:

studies_by_geography <-
  study_locations %>%
  mutate(
    screening =
      if_else(
        study_key %in% included_studies,
        "included",
        "excluded"
      )
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

  # Both counts, and their sum:

  mutate(
    n_studies_total = n_studies_included + n_studies_excluded
  ) %>%
  arrange(
    geography_type,
    desc(n_studies_total)
  )
# clear the environment ----------------------------------------------------

rm(
  list = ls()
)
