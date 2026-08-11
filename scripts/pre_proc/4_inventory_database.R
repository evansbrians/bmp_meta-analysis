# Purpose: Inventory the database 3_build_database.R writes: what each table
# holds, which candidate moderators the extraction can support, and the
# data-quality issues the build recorded. Run from the project root:
#
#   source("scripts/pre_proc/4_inventory_database.R")
#
# Reads   data/raw/bmp_meta.duckdb              written by 3_build_database.R
# Writes  data/processed/database_inventory/*.csv
#
# Nothing downstream depends on these tables. They are here to be read when
# deciding what the extraction can and cannot answer.

# libraries ----------------------------------------------------------------

library(DBI)
library(tidyverse)

# configuration ------------------------------------------------------------

config_inventory <-
  list(
    database = "data/raw/bmp_meta.duckdb",
    directory = "data/processed/database_inventory"
  )

fs::dir_create(config_inventory$directory)

# read the database --------------------------------------------------------

read_database_table <-
  function(.table_name) {
    connection <-
      dbConnect(
        duckdb::duckdb(),
        dbdir = config_inventory$database,
        read_only = TRUE
      )
    on.exit(
      dbDisconnect(
        connection,
        shutdown = TRUE
      )
    )
    connection %>%
      dbGetQuery(
        str_c("SELECT * FROM ", .table_name)
      ) %>%
      as_tibble()
  }

if (!fs::file_exists(config_inventory$database)) {
  cli::cli_abort(
    "No database at {config_inventory$database}. Build it by sourcing \\
     {.file scripts/pre_proc/3_build_database.R}."
  )
}

source_tables <-
  c(
    "v_effect_size_wide",
    "v_screening"
  ) %>%
  set_names() %>%
  map(
    \(.table_name) {
      read_database_table(.table_name)
    }
  )

# table inventory ----------------------------------------------------------

table_inventory <-
  source_tables %>%
  imap(
    \(.table_data, .table_name) {
      key_column <-
        intersect(
          c("study_key", "key"),
          names(.table_data)
        )
      tibble(
        table_name = .table_name,
        n_rows = nrow(.table_data),
        n_columns = ncol(.table_data),
        n_papers =
          if (length(key_column) > 0) {
            n_distinct(.table_data[[key_column[1]]])
          } else {
            NA_integer_
          },
        columns =
          .table_data %>%
          names() %>%
          str_flatten(collapse = "; ")
      )
    }
  ) %>%
  list_rbind()

# moderator inventory ------------------------------------------------------

# Moderators the analysis plan asks for, and the column names that would
# carry them. A moderator with no matching column cannot be modelled.

candidate_moderators <-
  tribble(
    ~ moderator, ~ plan_section, ~ column_pattern,
    "management_context", "4.4", "pasture|rangeland|context",
    "detection_correction", "4.5", "detect|survey_method|analysis_method",
    "stocking_rate", "4.2", "stocking|stock_rate",
    "grazing_system", "4.2", "rotation|deferred|continuous",
    "pesticide_compound_class", "4.3", "compound|chemical|pesticide_class",
    "study_year", "4.3", "^year$|study_year|sample_year"
  )

available_columns <-
  source_tables %>%
  map(names) %>%
  list_c() %>%
  unname() %>%
  unique()

moderator_inventory <-
  candidate_moderators %>%
  mutate(
    matching_columns =
      column_pattern %>%
      map_chr(
        \(.pattern) {
          available_columns %>%
            str_subset(.pattern) %>%
            str_flatten(collapse = "; ")
        }
      ),
    available = matching_columns != ""
  )

# write the inventory ------------------------------------------------------

# Keyed by output file name.

list(
  db_table_inventory.csv = table_inventory,
  moderator_inventory.csv = moderator_inventory,
  source_data_quality_issues.csv =
    read_database_table("data_quality_issue")
) %>%
  iwalk(
    \(.table_data, .file_name) {
      write_csv(
        .table_data,
        file.path(config_inventory$directory, .file_name),
        na = ""
      )
    }
  )
