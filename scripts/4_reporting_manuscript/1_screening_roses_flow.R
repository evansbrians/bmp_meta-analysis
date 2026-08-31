# This script:
# - Reads the screening record from the database, and the screened effect
#   sizes from the audit the analysis writes
# - Counts practice records, papers and effect sizes retained and lost at
#   every stage between the two, ending with the three-paper cutoff
# - Writes one row per stage, and the places the two tables disagree

# setup --------------------------------------------------------------------

library(fs)
library(tidyverse)

flow_directory <- fs::path("output/roses_diagram")

fs::dir_create(flow_directory)

# the exclusion vocabulary -------------------------------------------------

# `problem` carries one value per paper x practice record, so the stages are
# a single pass over it, in the order the review applied them.

citation_problems <-
  tribble(
    ~ phase, ~ problem, ~ reason,
    "screening", "review", "Review article, not a primary study",
    "screening", "no_access", "Full text not obtainable",
    "screening", "no_bmp", "No management practice evaluated",
    "screening", "author_reviewer", "Reviewer authored the paper",
    "eligibility", "continuous", "Continuous treatment",
    "eligibility", "continous_no_error", "Continuous treatment",
    "eligibility", "unusable_treatment", "Treatment contrast not comparable",
    "eligibility", "unusable_response",
    "Response not abundance, richness or nest success",
    "eligibility", "no_quantitative_results", "Missing quantitative results",
    "eligibility", "no_error", "No measure of dispersion reported",
    "eligibility", "unknown", "Reason not recorded (review)"
  ) %>%
  mutate(
    reason_rank =
      reason %>%
      fct_inorder() %>%
      as.integer()
  )

# The screen 2_screen_effects.R applies, under the names it records.

screen_reasons <-
  tribble(
    ~ excluded_by, ~ reason,
    "response_flag", "Response not the one the practice targets",
    "treatment_control_flag", "Treatment and control not comparable",
    "response_metric", "Response outside the three analysed",
    "diversity_index", "Diversity index, not a richness count",
    "conversion", "No route to an effect size",
    "duplicate_expression", "One result reported more than one way",
    "nest_hazard_scale",
    "Nest survival could not be transformed to the hazard scale",
    "artificial_nest", "Artificial nest, not a species' response",
    "unclassified_species", "Species does not fit the classification system"
  )

# read the tables ----------------------------------------------------------

# The screening record is in the database (primary key represents study_key x
# bmp).

bmp_database <-
  DBI::dbConnect(
    duckdb::duckdb(),
    dbdir = "data/raw/bmp_meta.duckdb",
    read_only = TRUE
  )

citations <-
  DBI::dbReadTable(bmp_database, "study_bmp") %>%
  as_tibble() %>%
  rename(key = study_key) %>%
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
  left_join(
    citation_problems,
    by = join_by(problem)
  )

# The studies the extraction holds that the screening record never had.

studies_outside_metadata <-
  DBI::dbReadTable(bmp_database, "study") %>%
  as_tibble() %>%
  filter(!in_metadata) %>%
  pull(study_key) %>%
  str_to_lower()

DBI::dbDisconnect(bmp_database, shutdown = TRUE)

# Every converted effect size, carrying the reason the screen held it out and
# whether it reaches the primary pool -- one frame for three stages.

screened_effects <-
  read_csv(
    "output/audits/screened_effects.csv",
    show_col_types = FALSE
  ) %>%
  mutate(
    key =
      key %>%
      str_to_lower() %>%
      str_squish()
  )

# The screen and the three-paper cutoff are separate stages: the cutoff is
# applied to the primary pool, after the screen, so it reads last.

effect_size_pool <-
  screened_effects %>%
  filter(
    is.na(excluded_by) |
      excluded_by == "paper_count"
  )

excluded_effects <-
  screened_effects %>%
  drop_na(excluded_by) %>%
  filter(excluded_by != "paper_count")

primary_pool <-
  effect_size_pool %>%
  filter(in_primary_pool)

# stages over the citation table -------------------------------------------

# A record survives a stage when its reason has not yet been applied. Papers
# are the distinct keys among the records that survive.

citation_flow <-
  citation_problems %>%
  distinct(
    phase,
    reason,
    reason_rank
  ) %>%
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
            summarise(
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
            summarise(
              records_lost = n(),
              keys_lost = n_distinct(key)
            )
        }
      )
  ) %>%
  unnest(c(surviving, lost)) %>%
  mutate(
    papers_lost =
      lag(papers, default = n_distinct(citations$key)) - papers,
    stage = reason
  ) %>%
  select(
    !c(
      reason,
      reason_rank,
      keys_lost
    )
  )

# The records that reach extraction, and the papers behind them.

eligible_records <-
  citations %>%
  filter(is.na(reason_rank))

# stages over the effect sizes ---------------------------------------------

# The screen is applied in descending order of what it holds out (widest cut
# reads first).

screen_ranks <-
  excluded_effects %>%
  count(
    excluded_by,
    sort = TRUE,
    name = "records_lost"
  ) %>%
  left_join(
    screen_reasons,
    by = join_by(excluded_by)
  ) %>%
  mutate(
    screen_rank = row_number()
  )

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
            summarise(
              records = n(),
              papers = n_distinct(key)
            )
        }
      )
  ) %>%
  unnest(surviving)

# assemble -----------------------------------------------------------------

# One row per stage, in reporting order, with the phase each belongs to.

roses_flow_stages <-
  bind_rows(
    tibble(
      phase = "identification",
      stage = "Records identified",
      records = nrow(citations),
      papers = n_distinct(citations$key),
      records_lost = 0,
      papers_lost = 0
    ),
    citation_flow,
    tibble(
      phase = "extraction",
      stage = "Effect sizes extracted",
      records = nrow(screened_effects),
      papers = n_distinct(screened_effects$key),
      records_lost = 0,
      papers_lost = 0
    ),
    screen_flow %>%
      rename(stage = reason) %>%
      mutate(
        phase = "screen",
        reason = excluded_by,
        papers_lost =
          lag(papers, default = n_distinct(screened_effects$key)) - papers
      ) %>%
      select(
        !c(
          excluded_by,
          screen_rank
        )
      ),
    tibble(
      phase = "screen",
      stage = "Effect sizes retained",
      records = nrow(effect_size_pool),
      papers = n_distinct(effect_size_pool$key),
      records_lost = 0,
      papers_lost = 0
    ),
    primary_pool %>%
      summarise(
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
    primary_pool %>%
      filter(is.na(excluded_by)) %>%
      summarise(
        phase = "cutoff",
        stage = "Cells of three or more papers",
        reason =
          str_c(
            "Fewer than three papers across the guilds for that practice ",
            "and response, or in the guild cell with no pooled cell to ",
            "carry the record"
          ),
        records = n(),
        papers = n_distinct(key),
        records_lost = nrow(primary_pool) - n(),
        papers_lost = n_distinct(primary_pool$key) - n_distinct(key)
      ),
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
  relocate(
    phase,
    stage,
    reason
  )

# Where the citation table and the extraction disagree. A group's practice
# records come from whichever of the two tables documents it.

records_never_extracted <-
  eligible_records %>%
  filter(!key %in% screened_effects$key)

papers_with_a_problem <-
  screened_effects$key %>%
  setdiff(eligible_records$key) %>%
  setdiff(studies_outside_metadata)

papers_outside_metadata <-
  intersect(
    screened_effects$key,
    studies_outside_metadata
  )

extracted_records <-
  function(.papers) {
    screened_effects %>%
      filter(key %in% .papers) %>%
      distinct(key, bmp) %>%
      nrow()
  }

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
      extracted_records(papers_with_a_problem),
    extracted_outside_metadata = length(papers_outside_metadata),
    extracted_outside_metadata_records =
      extracted_records(papers_outside_metadata)
  )

# write --------------------------------------------------------------------

roses_flow_stages %>%
  write_csv(
    fs::path(
      flow_directory,
      "roses_flow_stages",
      ext = "csv"
    ),
    na = ""
  )

roses_flow_reconciliation %>%
  write_csv(
    fs::path(
      flow_directory,
      "roses_flow_reconciliation",
      ext = "csv"
    ),
    na = ""
  )

# clean the environment ----------------------------------------------------

# Everything this script produces is written above; the next script
# reads it back from disk, so nothing is handed on in memory.

rm(list = ls())
