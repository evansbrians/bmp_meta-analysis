# This script:
# - Reads the database written by scripts/2_process_data/3_build_database.R
# - Restores the five extraction shapes and attaches the study and species
#   lookups every shape needs
# - Writes data/processed/for_analysis

# Nothing is screened or dropped here. The exclusion screen happens in
# 2_screen_effects.R.

# setup --------------------------------------------------------------------

library(tidyverse)

source("scripts/src/functions.R")

fs::dir_create("data/processed/for_analysis")

# read the database --------------------------------------------------------

# The database is the only input, and nothing here writes back to it:

database <-
  DBI::dbConnect(
    duckdb::duckdb(),
    dbdir = "data/raw/bmp_meta.duckdb",
    read_only = TRUE
  )

# The seven tables the sheets are rebuilt from, read in one pass:

bmp_tables <-
  c(
    "effect",
    "effect_bmp",
    "effect_arm",
    "effect_estimate",
    "study",
    "study_place",
    "species"
  ) %>%
  set_names() %>%
  map(
    \(.table_name) {
      DBI::dbReadTable(database, .table_name)
    }
  )

# Everything is in memory from here, so the connection closes:

DBI::dbDisconnect(database, shutdown = TRUE)

# restore the statistic shapes ---------------------------------------------

# The two estimate shapes are the same table split on which statistic was
# reported, each back under the names its own sheets used.

statistic_shapes <-
  list(
    arms =
      bmp_tables$effect_arm %>%
      pivot_wider(
        names_from = arm,
        values_from = c(xbar, n, sd, se, lcl, ucl, df)
      ) %>%
      rename_with(
        \(.name) {
          .name %>%
            str_replace("_treatment$", "_e") %>%
            str_replace("_control$", "_c")
        }
      ),
    coefficients =
      bmp_tables$effect_estimate %>%
      filter(statistic_type == "beta") %>%
      select(
        effect_id,
        beta = statistic_value,
        n,
        sd,
        se,
        lcl,
        ucl
      ),
    tests =
      bmp_tables$effect_estimate %>%
      filter(statistic_type != "beta") %>%
      select(
        effect_id,
        model,
        test_statistic = statistic_type,
        test_stat_value = statistic_value,
        df,
        global_n = n,
        global_sd = sd,
        global_se = se,
        global_lcl = lcl,
        global_ucl = ucl
      )
  )

# Which shapes each input type carries. A type joins only its own, which is
# what keeps every list item in the shape its sheet had.

sheet_statistics <-
  list(
    mean_diff = "arms",
    beta_categorical = c("arms", "coefficients"),
    other_categorical = c("arms", "tests")
  )

# assemble the records -----------------------------------------------------

# A record joins once per practice, which is how a dual-practice effect size
# reaches both cells.

records <-
  bmp_tables$effect %>%
  rename(key = study_key) %>%
  left_join(
    bmp_tables$effect_bmp,
    by = join_by(effect_id)
  ) %>%
  left_join(
    bmp_tables$study %>%
      select(
        key = study_key,
        paper
      ),
    by = join_by(key)
  ) %>%
  left_join(
    study_region_lookup(bmp_tables$study_place),
    by = join_by(key)
  ) %>%
  left_join(
    bmp_tables$species %>%
      select(
        species,
        species_group,
        analysis_class,
        species_include = include
      ),
    by = join_by(species)
  ) %>%

  # A study with no place recorded is its own region, so a region filter
  # never drops it silently.

  mutate(
    region = replace_na(region, "none_recorded")
  )

# One list item per statistic input type: its own records joined to its own
# statistic shapes, in the shape its sheet had.

for_analysis <-
  sheet_statistics %>%
  imap(
    \(.statistics, .source_sheet) {
      statistic_shapes[.statistics] %>%
        reduce(
          \(.records, .shape) {
            left_join(
              .records,
              .shape,
              by = join_by(effect_id)
            )
          },
          .init =
            records %>%
            filter(source_sheet == .source_sheet)
        )
    }
  )

# write --------------------------------------------------------------------

# One file per input type, under the name the sheet already had:

for_analysis %>%
  iwalk(
    \(.sheet_records, .source_sheet) {
      .sheet_records %>%
        write_csv(
          fs::path(
            "data/processed/for_analysis",
            .source_sheet,
            ext = "csv"
          ),
          na = ""
        )
    }
  )

# clean the environment ----------------------------------------------------

# Everything this script produces is written above; the next script
# reads it back from disk, so nothing is handed on in memory.

rm(list = ls())
