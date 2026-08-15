# This script:
# - Reads the database written by scripts/process_data/3_build_database.R
# - Restores the five extraction shapes and attaches the analysis columns
# - Flags every record the analysis excludes, dropping none of them
# - Writes data/processed/for_analysis

# setup --------------------------------------------------------------------

library(tidyverse)

source("scripts/functions.R")

# The two directories this script writes into:

fs::dir_create(
  c(
    "data/processed/for_analysis",
    "output/audits"
  )
)

# read the database --------------------------------------------------------

# The database is the only input, and nothing here writes back to it:

database <-
  DBI::dbConnect(
    duckdb::duckdb(),
    dbdir = "data/raw/bmp_meta.duckdb",
    read_only = TRUE
  )

# The six tables the sheets are rebuilt from, read in one pass:

bmp_tables <-
  c(
    "effect",
    "effect_bmp",
    "effect_arm",
    "effect_estimate",
    "study",
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

# The database holds one row per arm; the sheets carry the arm as a column
# suffix, so the reshape is a pivot and a rename.

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
    other_categorical = c("arms", "tests"),
    beta_continuous = "coefficients",
    other_continuous = "tests"
  )

# assemble the records -----------------------------------------------------

# A record joins once per practice, which is how a dual-practice effect size
# reaches both cells. The study and species lookups are common to every type.

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
    bmp_tables$species %>%
      select(
        species,
        analysis_class,
        species_include = include
      ),
    by = join_by(species)
  )

# One list item per statistic input type: its own records, its own statistic
# shapes, then the analysis columns and the flag.

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
        ) %>%

        # `in_primary_pool` and the two flags above it are the levers a
        # sensitivity specification moves; they exclude nothing on their own.

        mutate(
          label_type =
            if_else(
              species == "all_species",
              "community",
              "species"
            ),
          guild =
            if_else(
              label_type == "species",
              str_c(analysis_class, "_grassland"),
              NA_character_
            ),
          is_diversity =
            response_var %>%
            str_to_lower() %>%
            str_detect("diversity|evenness|shannon|simpson"),
          fire_excluded_original =
            bmp == "prescribed_fire" &
            str_detect(
              replace_na(treatment, ""),
              str_c(
                "year of burn",
                "burned that year",
                "current year",
                "<1 growing season",
                "[678] years",
                sep = "|"
              )
            ),
          in_primary_pool = TRUE,

          # One line per reason, each the negation of a test the record has
          # to pass, and a missing value fails that test. Nothing is dropped.

          excluded_by =
            case_when(
              !replace_na(species_include, FALSE) ~ "species_include",

              # A continuous record has no arms, so the three contrast tests
              # are guarded rather than firing on every gradient row.

              design == "categorical" &
                !replace_na(treatment_control_flag != "oranges", FALSE) ~
                "treatment_control_flag",
              design == "categorical" &
                !replace_na(response_flag != 1, FALSE) ~ "response_flag",
              design == "categorical" &
                is.na(sign) ~ "sign",
              !response_class %in%
                c(
                  "species_richness",
                  "nest_success",
                  "abundance"
                ) ~ "response_class",
              !guild %in%
                c(
                  "obligate_grassland",
                  "facultative_grassland"
                ) &
                label_type != "community" ~ "analysis_class",

              # A contrast mismatch over a mixed-guild assemblage, held out
              # by a recorded team decision.

              key == "pytisvj6" ~ "study_decision"
            )
        )
    }
  )

# write --------------------------------------------------------------------

# The audit is every flagged record whole, so a reason can be checked against
# the values behind it:

for_analysis %>%
  list_rbind() %>%
  filter(!is.na(excluded_by)) %>%
  arrange(
    excluded_by,
    source_sheet,
    key
  ) %>%
  write_csv(
    "output/audits/flagged_records.csv",
    na = ""
  )

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
