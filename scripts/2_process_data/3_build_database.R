# This script:
# - Reads the cleaned inputs from data/processed
# - Normalises them into nine tables, one per level of observation, and loads
#   the practice vocabulary beside them
# - Writes data/raw/bmp_meta.duckdb

# setup --------------------------------------------------------------------

library(tidyverse)

source("scripts/src/functions.R")

database_path <- "data/raw/bmp_meta.duckdb"

# One entry per extraction sheet. `prefix` fixes the effect_id namespace, so
# these codes are as load-bearing as the file names beside them.

# Eligibility requires a categorical treatment and response, so the two
# gradient sheets are out of scope and no longer extracted.

extraction_sheets <-
  tribble(
    ~ source_sheet, ~ prefix, ~ extraction_type, ~ design,
    "mean_diff", "md", "mean_difference", "categorical",
    "beta_categorical", "bc", "coefficient", "categorical",
    "other_categorical", "tc", "test_statistic", "categorical"
  )

# The two arm conventions in the sheets. Renaming to one of them here is what
# lets effect_arm be built by suffix rather than by sheet.

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

# The practice split in 2_clean_extraction_gsheet.R makes one effect several
# rows, so grouping on everything but the practice recovers the observation.

extraction <-
  extraction_sheets %>%
  pmap(
    \(source_sheet, prefix, extraction_type, design) {
      fs::path("data/processed/cleaned_data", source_sheet, ext = "csv") %>%
        read_csv(show_col_types = FALSE) %>%
        rename(any_of(arm_column_names)) %>%
        mutate(
          source_row = row_number(),
          source_sheet = source_sheet,
          extraction_type = extraction_type,
          design = design
        ) %>%
        group_by(
          pick(!c(bmp, source_row))
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

# Every column outside the practice is constant within an effect, so the first
# row of each is the whole observation.

extraction_effects <-
  extraction %>%
  distinct(
    effect_id,
    .keep_all = TRUE
  )

# study --------------------------------------------------------------------

# A key names one paper, so two papers sharing one would load as a single
# study. duckdb catches it, but not legibly, so it is named here first.

colliding_keys <-
  paper_metadata %>%
  distinct(key, paper) %>%
  filter(n() > 1, .by = key) %>%
  summarise(
    papers = str_flatten_comma(paper),
    .by = key
  )

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

# A study reaches the extraction without a metadata row, so the two sources are
# unioned and `in_metadata` records which side each key came from.

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

study_place <-
  paper_metadata %>%
  distinct(
    study_key = key,
    geography,
    geography_type,
    continent
  ) %>%
  filter(!is.na(geography))

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
  filter(!is.na(bmp))

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

# The union keeps the key: a species the extraction uses but the frame has yet
# to classify joins in here rather than breaking the effect table.

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
      filter(!is.na(species)),
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

effect_bmp <-
  extraction %>%
  distinct(
    effect_id,
    bmp
  ) %>%
  filter(!is.na(bmp))

# Arm is a variable, not a set of column suffixes. Stripping the suffix off the
# selected columns is the whole reshape.

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
            str_remove(.name, str_c(.suffix, "$"))
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

# Which statistic was reported is a value, so a coefficient and a test statistic
# are two rows of one table rather than two tables.

effect_estimate <-
  bind_rows(
    extraction_effects %>%
      filter(!is.na(beta)) %>%
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
      filter(!is.na(test_stat_value)) %>%
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

# A run that errors leaves the database open, and duckdb hands an already-open
# instance back to the next connection, so any instance is shut down first.

duckdb::duckdb_shutdown(
  duckdb::duckdb(dbdir = database_path)
)

# The build is not incremental, so the file and its write-ahead log are removed
# rather than opened: a rebuild cannot inherit a stale table.

c(
  database_path,
  str_c(database_path, ".wal")
) %>%
  keep(fs::file_exists) %>%
  fs::file_delete()

database <-
  DBI::dbConnect(
    duckdb::duckdb(dbdir = database_path)
  )

# Comments are stripped per line before the file is split, so a `--` cannot
# swallow the statement it sits above.

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

# Load order is dependency order: a table is appended only after every table it
# references.

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

DBI::dbDisconnect(database, shutdown = TRUE)

# clean the environment ----------------------------------------------------

# Everything this script produces is written above; the next script
# reads it back from disk, so nothing is handed on in memory.

rm(list = ls())
