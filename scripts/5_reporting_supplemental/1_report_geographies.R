# This script:
# - Reads the screened effects from the audits and the study locations from
#   the database
# - Moves the northeastern states out of the "other" region label
# - Writes the paper and record counts by region, and by practice and region
# - Writes the study counts by geography, included against excluded
# - Writes the geography supplement as a .docx

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

# supplement ---------------------------------------------------------------

# Flextable defaults:

set_flextable_defaults(
  font.family = "Times New Roman",
  table.layout = "autofit",
  split = FALSE,
  caption.style = "Table Caption",
  caption.align = "left"
)

# Page setup:

supplement_section <-
  prop_section(
    page_size = page_size(orient = "portrait"),
    page_margins =
      page_mar(
        top = 1,
        bottom = 1,
        left = 1,
        right = 1
      )
  )

# One supplemental table, captioned:

format_geography_table <-
  function(
    .data,
    .caption) {
    .data %>%
      flextable() %>%
      autofit() %>%
      fontsize(
        size = 10,
        part = "body"
      ) %>%
      align(
        align = "center",
        part = "all"
      ) %>%
      align(
        j = 1,
        align = "left",
        part = "all"
      ) %>%
      padding(
        padding.top = 2,
        padding.bottom = 2,
        part = "all"
      ) %>%
      set_caption(
        .caption,
        fp_p =
          fp_par(
            padding = 3,
            keep_with_next = TRUE
          ),
        align_with_table = FALSE
      )
  }

# Reader-facing region names:

region_labels <-
  read_csv(
    "src/region_labels.csv",
    show_col_types = FALSE
  )

# Papers and records by region:

region_flextable <-
  region_counts %>%
  left_join(
    region_labels,
    by = join_by(region)
  ) %>%
  select(
    Region = region_label,
    Papers = n_papers,
    Records = n_records
  ) %>%
  format_geography_table(
    .caption =
      as_paragraph(
        as_b("Table S5"),
        ". Papers and effect sizes in the primary analysis pool, by region. ",
        "Studies reporting a northeastern state are labeled Northeastern ",
        "U.S. rather than Other."
      )
  )

# By practice and region:

bmp_region_flextable <-
  bmp_region_counts %>%
  left_join(
    region_labels,
    by = join_by(region)
  ) %>%
  mutate(
    BMP = format_bmp(bmp)
  ) %>%
  select(
    BMP,
    Region = region_label,
    Papers = n_papers,
    Records = n_records
  ) %>%
  format_geography_table(
    .caption =
      as_paragraph(
        as_b("Table S6"),
        ". Papers and effect sizes in the primary analysis pool, by best ",
        "management practice and region."
      )
  )

# Studies by geography, included against excluded:

geography_flextable <-
  studies_by_geography %>%

  # Print the place names:

  mutate(
    across(
      c(geography_type, geography, continent),
      \(.place) {
        .place %>%
          str_replace_all("_", " ") %>%
          str_to_title()
      }
    )
  ) %>%
  select(
    Grain = geography_type,
    Place = geography,
    Continent = continent,
    Included = n_studies_included,
    Excluded = n_studies_excluded,
    Total = n_studies_total
  ) %>%
  format_geography_table(
    .caption =
      as_paragraph(
        as_b("Table S7"),
        ". Located studies by geography, those holding a modeled effect ",
        "size against those screened out."
      )
  )

# write --------------------------------------------------------------------

# Studies labeled other:

other_region_studies %>%
  write_output_table(
    file_name = "geography_other_region_studies.csv"
  )

# Papers and records by region:

region_counts %>%
  write_output_table(
    file_name = "geography_by_region.csv"
  )

# By practice and region:

bmp_region_counts %>%
  write_output_table(
    file_name = "geography_by_bmp_region.csv"
  )

# Studies by geography, included against excluded:

studies_by_geography %>%
  write_output_table(
    file_name = "geography_studies_included_excluded.csv"
  )

# The supplement:

geography_document <-
  read_docx() %>%
  body_add_flextable(region_flextable) %>%
  body_add_break() %>%
  body_add_flextable(bmp_region_flextable) %>%
  body_add_break() %>%
  body_add_flextable(geography_flextable) %>%
  body_set_default_section(
    value = supplement_section
  )

# Write to file:

print(
  geography_document,
  target = "output/supplementals/geography_supplement.docx"
)

# clear the environment ----------------------------------------------------

rm(
  list = ls()
)
