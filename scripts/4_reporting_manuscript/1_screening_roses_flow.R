# This script:
# - Reads the screening record from the database and the screened effect sizes
# - Counts what each stage retained and lost, through the three-paper cutoff
# - Writes one row per stage, and the places the two tables disagree

# setup --------------------------------------------------------------------

library(DBI)
library(duckdb)
library(fs)
library(tidyverse)

# Project functions:

source("src/functions.R")

# Output directory:

flow_directory <- path("output/roses_diagram")

# Create it:

dir_create(flow_directory)

# the exclusion vocabulary -------------------------------------------------

# Exclusion reasons, in the order applied:

citation_problems <-
  read_csv(
    "src/citation_problems.csv",
    show_col_types = FALSE
  ) %>%

  # Rank the reasons:

  mutate(
    reason_rank =
      reason %>%
      fct_inorder() %>%
      as.integer()
  )

# Screen reasons from 2_screen_effects.R:

screen_reasons <-
  read_csv(
    "src/screen_reasons.csv",
    show_col_types = FALSE
  )

# read the tables ----------------------------------------------------------

# Connect to the database:

bmp_database <-
  dbConnect(
    duckdb(),
    dbdir = "data/raw/bmp_meta.duckdb",
    read_only = TRUE
  )

# Screening record:

citations <-
  dbReadTable(bmp_database, "study_bmp") %>%
  as_tibble() %>%
  rename(key = study_key) %>%

  # Standardize the keys:

  mutate(
    across(
      c(key, problem),
      \(.column) {
        .column %>%
          str_to_lower() %>%
          str_squish() %>%
          replace_na("-")
      }
    )
  ) %>%

  # Add the reason and its rank:

  left_join(
    citation_problems,
    by = join_by(problem)
  )

# Studies missing from the screening record:

studies_outside_metadata <-
  dbReadTable(bmp_database, "study") %>%
  as_tibble() %>%
  filter(!in_metadata) %>%
  pull(study_key) %>%
  str_to_lower()

# Disconnect:

dbDisconnect(bmp_database, shutdown = TRUE)

# Screened effect sizes:

screened_effects <-
  read_csv(
    "output/audits/screened_effects.csv",
    show_col_types = FALSE
  ) %>%

  # Standardize the keys:

  mutate(
    key =
      key %>%
      str_to_lower() %>%
      str_squish()
  )

# Effect sizes past the screen:

effect_size_pool <-
  screened_effects %>%
  filter(
    is.na(excluded_by) |
      excluded_by == "paper_count"
  )

# Effect sizes the screen removed:

excluded_effects <-
  screened_effects %>%
  drop_na(excluded_by) %>%
  filter(excluded_by != "paper_count")

# Grassland classes only:

primary_pool <-
  effect_size_pool %>%
  filter(in_primary_pool)

# stages over the citation table -------------------------------------------

# Records surviving each stage:

citation_flow <-
  citation_problems %>%

  # One row per stage:

  distinct(
    phase,
    reason,
    reason_rank
  ) %>%

  # Count survivors and losses:

  mutate(
    surviving =
      map(
        reason_rank,
        \(.rank) {
          citations %>%
            filter(
              is.na(reason_rank) |
                reason_rank > .rank
            ) %>%
            summarize(
              records = n(),
              papers = n_distinct(key)
            )
        }
      ),
    lost =
      map(
        reason_rank,
        \(.rank) {
          citations %>%
            filter(reason_rank == .rank) %>%
            summarize(
              records_lost = n(),
              keys_lost = n_distinct(key)
            )
        }
      )
  ) %>%

  # Unnest the counts:

  unnest(
    c(surviving, lost)
  ) %>%

  # Papers lost per stage:

  mutate(
    papers_lost =
      lag(
        papers,
        default = n_distinct(citations$key)
      ) - papers,
    stage = reason
  ) %>%

  # Drop the working columns:

  select(
    !c(
      reason,
      reason_rank,
      keys_lost
    )
  )

# Records reaching extraction:

eligible_records <-
  citations %>%
  filter(
    is.na(reason_rank)
  )

# stages over the effect sizes ---------------------------------------------

# Screen reasons, largest first:

screen_ranks <-
  excluded_effects %>%

  # Order by records removed:

  count(
    excluded_by,
    sort = TRUE,
    name = "records_lost"
  ) %>%

  # Add the printed wording:

  left_join(
    screen_reasons,
    by = join_by(excluded_by)
  ) %>%

  # Number them:

  mutate(
    screen_rank = row_number()
  )

# Add the rank to the excluded records:

excluded_effects <-
  excluded_effects %>%
  left_join(
    screen_ranks %>%
      select(
        excluded_by,
        screen_rank
      ),
    by = join_by(excluded_by)
  )

# Survivors of each screen reason:

screen_flow <-
  screen_ranks %>%
  mutate(
    surviving =
      map(
        screen_rank,
        \(.rank) {
          screened_effects %>%
            anti_join(
              excluded_effects %>%
                filter(screen_rank <= .rank),
              by = join_by(es_id)
            ) %>%
            summarize(
              records = n(),
              papers = n_distinct(key)
            )
        }
      )
  ) %>%

  # Unnest the counts:

  unnest(surviving)

# assemble -----------------------------------------------------------------

# Every stage, in reporting order:

roses_flow_stages <-
  bind_rows(

    # Records identified:

    tibble(
      phase = "identification",
      stage = "Records identified",
      records = nrow(citations),
      papers = n_distinct(citations$key),
      records_lost = 0,
      papers_lost = 0
    ),

    # Screening and eligibility:

    citation_flow,

    # Effect sizes extracted:

    tibble(
      phase = "extraction",
      stage = "Effect sizes extracted",
      records = nrow(screened_effects),
      papers = n_distinct(screened_effects$key),
      records_lost = 0,
      papers_lost = 0
    ),

    # Effect-size screen:

    screen_flow %>%
      rename(stage = reason) %>%
      mutate(
        phase = "screen",
        reason = excluded_by,
        papers_lost =
          lag(
            papers,
            default = n_distinct(screened_effects$key)
          ) - papers
      ) %>%
      select(
        !c(
          excluded_by,
          screen_rank
        )
      ),

    # Effect sizes retained:

    tibble(
      phase = "screen",
      stage = "Effect sizes retained",
      records = nrow(effect_size_pool),
      papers = n_distinct(effect_size_pool$key),
      records_lost = 0,
      papers_lost = 0
    ),

    # Grassland classes only:

    primary_pool %>%
      summarize(
        phase = "analysis",
        stage = "Grassland classes only",
        reason =
          str_c(
            "Artificial nests and shrubland, woodland, and forest ",
            "species removed"
          ),
        records = n(),
        papers = n_distinct(key),
        records_lost = nrow(effect_size_pool) - n(),
        papers_lost = n_distinct(effect_size_pool$key) - n_distinct(key)
      ),

    # Three-paper cutoff:

    primary_pool %>%
      filter(
        is.na(excluded_by)
      ) %>%
      summarize(
        phase = "cutoff",
        stage = "Cells of three or more papers",
        reason =
          str_c(
            "Fewer than three papers across the guilds for that ",
            "practice and response"
          ),
        records = n(),
        papers = n_distinct(key),
        records_lost = nrow(primary_pool) - n(),
        papers_lost = n_distinct(primary_pool$key) - n_distinct(key)
      ),

    # Modeling pools:

    read_csv(
      "output/audits/analysis_pool_summary.csv",
      show_col_types = FALSE
    ) %>%
      rename(
        stage = pool,
        records = n_effect_sizes,
        papers = n_studies
      ) %>%
      mutate(
        phase = "models",
        records_lost = 0,
        papers_lost = 0
      ) %>%
      select(
        phase,
        stage,
        records,
        papers,
        records_lost,
        papers_lost
      )
  ) %>%

  # Lead with the stage:

  relocate(
    phase,
    stage,
    reason
  )

# Eligible but never extracted:

records_never_extracted <-
  eligible_records %>%
  filter(!key %in% screened_effects$key)

# Extracted despite a problem:

papers_with_a_problem <-
  screened_effects$key %>%
  setdiff(eligible_records$key) %>%
  setdiff(studies_outside_metadata)

# Extracted without a record:

papers_outside_metadata <-
  intersect(
    screened_effects$key,
    studies_outside_metadata
  )

# Count each disagreement:

roses_flow_reconciliation <-
  tibble(
    eligible_papers = n_distinct(eligible_records$key),
    extracted_papers =
      length(
        intersect(
          eligible_records$key,
          screened_effects$key
        )
      ),
    not_in_analysis_table = n_distinct(records_never_extracted$key),
    not_in_analysis_records = nrow(records_never_extracted),
    extracted_despite_problem = length(papers_with_a_problem),
    extracted_despite_problem_records =
      extracted_records(
        .effects = screened_effects,
        .papers = papers_with_a_problem
      ),
    extracted_outside_metadata = length(papers_outside_metadata),
    extracted_outside_metadata_records =
      extracted_records(
        .effects = screened_effects,
        .papers = papers_outside_metadata
      )
  )

# write --------------------------------------------------------------------

# One row per stage:

roses_flow_stages %>%
  write_csv(
    path(
      flow_directory,
      "roses_flow_stages",
      ext = "csv"
    ),
    na = ""
  )

# Reconciliation:

roses_flow_reconciliation %>%
  write_csv(
    path(
      flow_directory,
      "roses_flow_reconciliation",
      ext = "csv"
    ),
    na = ""
  )

# clear the environment ----------------------------------------------------

rm(
  list = ls()
)
