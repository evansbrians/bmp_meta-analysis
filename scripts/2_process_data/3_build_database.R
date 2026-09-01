# This script:
# - Reads the cleaned inputs from data/processed
# - Normalises them into nine tables, one per level of observation, and loads
#   the practice vocabulary beside them
# - Writes data/raw/bmp_meta.duckdb

# setup --------------------------------------------------------------------

library(tidyverse)

# Project functions:

source("src/functions.R")

# Where the database is written:

database_path <- "data/raw/bmp_meta.duckdb"

# One row per extraction sheet:

extraction_sheets <-
  read_csv(
    "src/extraction_sheets.csv",
    show_col_types = FALSE
  )

# One arm naming convention:

arm_column_names <-
  c(
    n_e = "treatment_n",
    sd_e = "treatment_sd",
    se_e = "treatment_se",
    df_e = "treatment_df",
    n_c = "control_n",
    sd_c = "control_sd",
    se_c = "control_se",
    df_c = "control_df"
  )

# read the cleaned inputs --------------------------------------------------

paper_metadata <-
  read_csv("data/processed/paper_metadata.csv", show_col_types = FALSE)

# Recover one row per effect:

extraction <-
  extraction_sheets %>%
  pmap(
    \(source_sheet, prefix, extraction_type, design) {
      fs::path("data/processed/cleaned_data", source_sheet, ext = "csv") %>%
        read_csv(show_col_types = FALSE) %>%
        rename(
          any_of(arm_column_names)
        ) %>%
        mutate(
          source_row = row_number(),
          source_sheet = source_sheet,
          extraction_type = extraction_type,
          design = design
        ) %>%
        group_by(
          pick(
            !c(bmp, source_row)
          )
        ) %>%
        mutate(
          source_row = min(source_row)
        ) %>%
        ungroup() %>%
        mutate(
          effect_id =
            source_row %>%
            dense_rank() %>%
            str_pad(
              width = 4,
              pad = "0"
            ) %>%
            str_c(prefix, ., sep = "_")
        )
    }
  ) %>%
  list_rbind()

# Take the first row of each:

extraction_effects <-
  extraction %>%
  distinct(
    effect_id,
    .keep_all = TRUE
  )

# study --------------------------------------------------------------------

# Keys naming more than one paper:

colliding_keys <-
  paper_metadata %>%
  distinct(key, paper) %>%
  filter(n() > 1, .by = key) %>%
  summarize(
    papers = str_flatten_comma(paper),
    .by = key
  )

# Stop if any were found:

if (nrow(colliding_keys) > 0) {
  cli::cli_abort(
    c(
      "A study key names more than one paper:",
      set_names(
        str_c(colliding_keys$key, ": ", colliding_keys$papers),
        "x"
      )
    )
  )
}

# Union the metadata and extraction keys:

study <-
  paper_metadata %>%
  distinct(
    study_key = key,
    reviewed,
    paper,
    title,
    article_type
  ) %>%
  full_join(
    extraction_effects %>%
      distinct(study_key = key),
    by = join_by(study_key)
  ) %>%
  mutate(
    in_metadata = study_key %in% paper_metadata$key
  )

# One row per study and place:

study_place <-
  paper_metadata %>%
  distinct(
    study_key = key,
    geography,
    geography_type,
    continent
  ) %>%
  filter(
    !is.na(geography)
  )

# One row per study and practice:

study_bmp <-
  paper_metadata %>%
  distinct(
    study_key = key,
    bmp,
    effect_size,
    useful,
    problem,
    additional_notes
  ) %>%
  filter(
    !is.na(bmp)
  )

# One row per study, practice and response:

study_bmp_response <-
  paper_metadata %>%
  distinct(
    study_key = key,
    bmp,
    response
  ) %>%
  separate_longer_delim(response, ";") %>%
  mutate(
    response = str_squish(response)
  ) %>%
  filter(
    !is.na(bmp),
    !is.na(response),
    response != ""
  ) %>%
  distinct()

# species ------------------------------------------------------------------

# One row per species, classified or not:

species <-
  read_csv(
    "data/processed/species_classified_analysis_frame.csv",
    show_col_types = FALSE
  ) %>%
  select(
    species,
    species_group,
    analysis_class,
    include
  ) %>%
  full_join(
    extraction_effects %>%
      distinct(species) %>%
      filter(
        !is.na(species)
      ),
    by = join_by(species)
  )

# effect -------------------------------------------------------------------

effect <-
  extraction_effects %>%
  mutate(
    across(
      c(
        sign,
        response_flag,
        response_dir
      ),
      as.integer
    )
  ) %>%
  select(
    effect_id,
    study_key = key,
    source_sheet,
    source_row,
    extraction_type,
    design,
    response_class,
    response_var,
    survival_scale,
    link,
    baseline_survival,
    beta_is_derived,
    treatment,
    control,
    species,
    sign,
    treatment_control_flag,
    response_flag,
    response_dir,
    notes
  )

# One row per effect size and practice:

effect_bmp <-
  extraction %>%
  distinct(
    effect_id,
    bmp
  ) %>%
  filter(
    !is.na(bmp)
  )

# One row per effect size and arm:

effect_arm <-
  c(
    treatment = "_e",
    control = "_c"
  ) %>%
  imap(
    \(.suffix, .arm) {
      extraction_effects %>%
        select(
          effect_id,
          ends_with(.suffix)
        ) %>%
        rename_with(
          \(.name) {
            str_remove(
              .name, str_c(.suffix, "$")
            )
          }
        ) %>%
        mutate(
          arm = .arm
        )
    }
  ) %>%
  list_rbind() %>%
  filter(
    !if_all(
      c(
        xbar,
        n,
        sd,
        se,
        lcl,
        ucl,
        df
      ),
      is.na
    )
  ) %>%
  relocate(
    arm,
    .after = effect_id
  )

# One row per reported statistic:

effect_estimate <-
  bind_rows(
    extraction_effects %>%
      filter(
        !is.na(beta)
      ) %>%
      select(
        effect_id,
        statistic_value = beta,
        n,
        sd,
        se,
        lcl,
        ucl
      ) %>%
      mutate(
        statistic_type = "beta"
      ),
    extraction_effects %>%
      filter(
        !is.na(test_stat_value)
      ) %>%
      select(
        effect_id,
        statistic_type = test_statistic,
        statistic_value = test_stat_value,
        model,
        n = global_n,
        sd = global_sd,
        se = global_se,
        lcl = global_lcl,
        ucl = global_ucl,
        df
      )
  ) %>%
  relocate(
    statistic_type,
    .after = effect_id
  )

# write --------------------------------------------------------------------

# Shut down any open instance:

duckdb::duckdb_shutdown(
  duckdb::duckdb(dbdir = database_path)
)

# Remove the database and its log:

c(
  database_path,
  str_c(database_path, ".wal")
) %>%
  keep(fs::file_exists) %>%
  fs::file_delete()

# Create the database:

database <-
  DBI::dbConnect(
    duckdb::duckdb(dbdir = database_path)
  )

# Strip comments, then split on the semicolons:

"scripts/2_process_data/schema.sql" %>%
  read_lines() %>%
  str_remove("--.*$") %>%
  str_flatten("\n") %>%
  str_split_1(";") %>%
  keep(
    \(.statement) {
      str_detect(.statement, "\\S")
    }
  ) %>%
  walk(
    \(.statement) {
      DBI::dbExecute(database, .statement)
    }
  )

# Append in dependency order:

list(
  bmp = bmp_vocabulary,
  study = study,
  study_place = study_place,
  study_bmp = study_bmp,
  study_bmp_response = study_bmp_response,
  species = species,
  effect = effect,
  effect_bmp = effect_bmp,
  effect_arm = effect_arm,
  effect_estimate = effect_estimate
) %>%
  iwalk(
    \(.table, .name) {
      DBI::dbAppendTable(database, .name, .table)
    }
  )

# Close the database:

DBI::dbDisconnect(database, shutdown = TRUE)

# clear the environment ----------------------------------------------------

rm(
  list = ls()
)
