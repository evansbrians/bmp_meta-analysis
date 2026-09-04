# This script:
# - Reads the screened effects from the audits and the study locations from
#   the database
# - Moves the northeastern states out of the "other" region label
# - Writes the paper and record counts by region, and by practice and region

# setup --------------------------------------------------------------------

library(DBI)
library(duckdb)
library(tidyverse)

# Project functions:

source("src/functions.R")

# Output directory:

fs::dir_create("output/tables")

# Screened effects:

papers_by_pool <-
  read_csv(
    "output/audits/screened_effects.csv",
    show_col_types = FALSE
  ) %>%
  select(key, region, bmp, in_primary_pool)

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
  filter(in_primary_pool) %>%
  select(!in_primary_pool) %>%
  mutate(
    region =
      case_when(
        key %in% northeast ~ "northeast_us",
        .default = region
      )
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
