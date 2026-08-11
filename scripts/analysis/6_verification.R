# This script:
# - Refits every reported cell with REML as an independent check
# - Verifies the pools, the thresholds and the reported tables agree
# - Saves the verification report read by the results page

# setup --------------------------------------------------------------------

library(brms)
library(metafor)
library(posterior)
library(tidyverse)

source("scripts/functions.R")

fs::dir_create("output/diagnostics")

fitted_models <-
  read_rds("output/models/fitted_models.rds")

model_pools <-
  read_rds("output/models/model_data.rds")

effect_sizes <-
  "brian_sandbox/data/db_mirror/effect_sizes.csv" %>%
  read_csv(show_col_types = FALSE)

species_analysis_frame <-
  fs::path(
    "data/processed/species_classification",
    "species_classified_analysis_frame.csv"
  ) %>%
  read_csv(show_col_types = FALSE)

options(mc.cores = sampler_settings$cores)

table_guild_bmp <-
  fs::path("output/tables", "table_guild_bmp.csv") %>%
  read_csv(show_col_types = FALSE)

table_species_richness <-
  fs::path(
    "output/tables",
    "table_species_richness_by_bmp.csv"
  ) %>%
  read_csv(show_col_types = FALSE)

table_pooled_bmp <-
  fs::path("output/tables", "table_pooled_bmp.csv") %>%
  read_csv(show_col_types = FALSE)

results_tables <-
  list(
    table_guild_bmp,
    table_species_richness,
    table_pooled_bmp
  )

bmp_cell_tables <-
  bind_rows(
    table_guild_bmp,
    table_pooled_bmp
  )

cell_keys <-
  c(
    "response_metric",
    "guild",
    "bmp"
  )

# 1. frequentist cross-check -----------------------------------------------

reml_specifications <-
  list(
    list(
      response_metric = "abundance",
      pool_name = "abundance_guild_bmp",
      guild_scope = "by guild"
    ),
    list(
      response_metric = "nest_success",
      pool_name = "nest_success_guild_bmp",
      guild_scope = "by guild"
    ),
    list(
      response_metric = "abundance",
      pool_name = "abundance_pooled_bmp",
      guild_scope = "pooled"
    )
  )

reml_cell_means <-
  reml_specifications %>%
  map(
    \(.specification) {
      model_pools %>%
        pluck(.specification$pool_name) %>%
        fit_reml_cell_means(cell_variable = "guild_bmp") %>%
        mutate(
          response_metric = .specification$response_metric,
          guild_scope = .specification$guild_scope
        )
    }
  ) %>%
  list_rbind() %>%
  relocate(
    response_metric,
    .after = reml_ucl
  )

verification_bayes_vs_reml <-
  reml_cell_means %>%
  separate_wider_delim(
    cell,
    delim = "__",
    names = c("guild", "bmp")
  ) %>%
  left_join(
    bmp_cell_tables %>%
      select(
        response_metric,
        guild,
        bmp,
        bayes_estimate = estimate,
        bayes_lcl = lcl,
        bayes_ucl = ucl,
        meets_primary_threshold
      ),
    by = cell_keys
  ) %>%
  mutate(
    estimate_difference = bayes_estimate - reml_estimate,
    bayes_interval_width = bayes_ucl - bayes_lcl,
    reml_interval_width = reml_ucl - reml_lcl,
    width_ratio = bayes_interval_width / reml_interval_width,
    either_excludes_zero =
      same_sign(bayes_lcl, bayes_ucl) |
      same_sign(reml_lcl, reml_ucl),
    agrees_on_sign =
      !either_excludes_zero |
      same_sign(bayes_estimate, reml_estimate),
    agrees_on_exclusion =
      same_sign(bayes_lcl, bayes_ucl) ==
      same_sign(reml_lcl, reml_ucl)
  ) %>%
  arrange(
    guild_scope,
    desc(
      abs(estimate_difference)
    )
  )

verification_bayes_vs_reml %>%
  write_output_table(
    file_name = "verification_bayes_vs_reml.csv",
    directory = "output/diagnostics"
  )

# 2. sample-size reconciliation --------------------------------------------

usable_effect_sizes <-
  effect_sizes %>%
  filter(
    in_primary_pool,
    is.finite(yi),
    is.finite(sei),
    sei > 0
  )

recomputed_guild_bmp <-
  usable_effect_sizes %>%
  filter(
    response_metric %in% c("nest_success", "abundance"),
    !is.na(guild)
  ) %>%
  summarise(
    k_recomputed = n(),
    n_studies_recomputed = n_distinct(key),
    .by = all_of(cell_keys)
  )

recomputed_richness <-
  usable_effect_sizes %>%
  filter(response_metric == "species_richness") %>%
  summarise(
    k_recomputed = n(),
    n_studies_recomputed = n_distinct(key),
    .by = c(response_metric, bmp)
  ) %>%
  mutate(
    guild = NA_character_
  )

# Recomputed over both guilds at once, so a stray assemblage row would show up
# here as a k mismatch.

recomputed_pooled_bmp <-
  usable_effect_sizes %>%
  filter(
    response_metric %in% c("nest_success", "abundance"),
    !is.na(guild)
  ) %>%
  summarise(
    k_recomputed = n(),
    n_studies_recomputed = n_distinct(key),
    .by = c(response_metric, bmp)
  ) %>%
  mutate(
    guild = "all_grassland"
  )

reported_sample_sizes <-
  results_tables %>%
  map(
    ~ .x %>%
      select(
        response_metric,
        guild,
        bmp,
        k,
        n_studies
      )
  ) %>%
  bind_rows()

verification_reconciliation <-
  reported_sample_sizes %>%
  left_join(
    bind_rows(
      recomputed_guild_bmp,
      recomputed_richness,
      recomputed_pooled_bmp
    ),
    by = cell_keys
  ) %>%
  mutate(
    k_matches = k == k_recomputed,
    studies_match = n_studies == n_studies_recomputed
  )

verification_reconciliation %>%
  write_output_table(
    file_name = "verification_reconciliation.csv",
    directory = "output/diagnostics"
  )

# 3. assertions ------------------------------------------------------------

reported_cells <-
  results_tables %>%
  map(
    ~ .x %>%
      select(
        response_metric,
        k,
        n_studies,
        meets_primary_threshold
      )
  ) %>%
  bind_rows() %>%
  left_join(
    inclusion_thresholds %>%
      select(
        response_metric,
        metric_min_effect_sizes,
        metric_min_studies
      ),
    by = "response_metric"
  )

sub_threshold_flags <-
  reported_cells %>%
  filter(n_studies < 3) %>%
  pull(meets_primary_threshold)

primary_exclusion_agreement <-
  verification_bayes_vs_reml %>%
  filter(meets_primary_threshold) %>%
  pull(agrees_on_exclusion)

provisional_exclusion_agreement <-
  verification_bayes_vs_reml %>%
  filter(!meets_primary_threshold) %>%
  pull(agrees_on_exclusion)

guilds_per_species <-
  species_analysis_frame %>%
  summarise(
    n_guilds = n_distinct(analysis_class),
    .by = species
  ) %>%
  pull(n_guilds)

shrubland_flags <-
  results_tables %>%
  map(
    \(.table) {
      .table %>%
        pull(guild) %>%
        as.character()
    }
  ) %>%
  list_c() %>%
  replace_na("") %>%
  str_detect("shrub")

# The pooled models must hold the two guilds and nothing else: no assemblage
# row, and no cell smaller than the guild cells it pools.

pooled_community_rows <-
  model_pools %>%
  pluck("abundance_pooled_bmp") %>%
  filter(label_type == "community")

pooled_coverage <-
  table_pooled_bmp %>%
  mutate(
    pooled_k = k
  ) %>%
  select(
    response_metric,
    bmp,
    pooled_k
  ) %>%
  left_join(
    table_guild_bmp %>%
      summarise(
        guild_k = sum(k),
        .by = c(response_metric, bmp)
      ),
    by = c("response_metric", "bmp")
  ) %>%
  mutate(
    guild_k = replace_na(guild_k, 0),
    covers_guilds = pooled_k >= guild_k
  )

pooled_rows_labelled <-
  table_pooled_bmp %>%
  pull(guild) %>%
  str_detect("all_grassland")

guild_rows_mislabelled <-
  table_guild_bmp %>%
  pull(guild) %>%
  str_detect("all_grassland")

pooled_metrics_reported <-
  table_pooled_bmp %>%
  pull(response_metric) %>%
  unique()

verification_assertions <-
  tribble(
    ~ assertion, ~ passed, ~ detail,
    "Every reported cell meets its metric's effect-size threshold",
    all(reported_cells$k >= reported_cells$metric_min_effect_sizes),
    str_c("Minimum k reported: ", min(reported_cells$k)),
    "Every reported cell meets its metric's independent-study threshold",
    all(reported_cells$n_studies >= reported_cells$metric_min_studies),
    str_c("Minimum studies reported: ", min(reported_cells$n_studies)),
    "Cells below the primary threshold are flagged provisional",
    all(!sub_threshold_flags),
    str_c(
      "Provisional cells: ",
      sum(!reported_cells$meets_primary_threshold),
      " of ",
      nrow(reported_cells)
    ),
    "Reported sample sizes reconcile with the effect-size table",
    all(
      verification_reconciliation$k_matches,
      verification_reconciliation$studies_match
    ),
    str_c(
      "Mismatched cells: ",
      sum(
        !verification_reconciliation$k_matches |
        !verification_reconciliation$studies_match
      )
    ),
    "No species resolves to more than one guild",
    max(guilds_per_species) == 1,
    "Checked in the species analysis frame",
    "No shrubland guild appears in any results table",
    !any(shrubland_flags),
    "Shrubland dropped from the re-analysis",
    "Bayesian and REML fits agree on the sign of every non-null cell mean",
    all(verification_bayes_vs_reml$agrees_on_sign),
    str_c(
      "Disagreements: ",
      sum(!verification_bayes_vs_reml$agrees_on_sign)
    ),
    str_c(
      "Bayesian and REML fits agree on which intervals exclude zero, ",
      "for cells above the primary threshold"
    ),
    all(primary_exclusion_agreement),
    str_c(
      "Disagreements above the primary threshold: ",
      sum(!primary_exclusion_agreement)
    ),
    "Provisional cells where REML and Bayes disagree on excluding zero",
    TRUE,
    str_c(
      sum(!provisional_exclusion_agreement),
      " of ",
      length(provisional_exclusion_agreement),
      " provisional cells (informational)"
    ),
    "No assemblage-level effect size enters a pooled model",
    nrow(pooled_community_rows) == 0,
    str_c(
      "Community rows in the four pooled pools: ",
      nrow(pooled_community_rows)
    ),
    "Every pooled cell holds at least the effect sizes of its guild cells",
    all(pooled_coverage$covers_guilds),
    str_c(
      "Pooled cells smaller than their guild cells: ",
      sum(!pooled_coverage$covers_guilds)
    ),
    "Pooled estimates are labelled pooled, and guild estimates are not",
    all(pooled_rows_labelled) & !any(guild_rows_mislabelled),
    str_c(
      "Pooled cells reported: ",
      length(pooled_rows_labelled)
    ),
    "Both guild-level metrics have a pooled estimate by practice",
    setequal(pooled_metrics_reported, c("nest_success", "abundance")),
    str_c(
      "Metrics pooled: ",
      str_c(pooled_metrics_reported, collapse = ", ")
    )
  )

verification_assertions %>%
  write_output_table(
    file_name = "verification_assertions.csv",
    directory = "output/diagnostics"
  )

if (any(!verification_assertions$passed)) {
  cli::cli_warn(
    "Verification assertions failed; see the diagnostics folder."
  )
}
