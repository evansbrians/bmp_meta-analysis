# This script:
# - Refits every reported cell with REML as an independent check
# - Verifies the pools, the thresholds, the response scales and the reported
#   tables agree
# - Saves the verification report read by the results page

# setup --------------------------------------------------------------------

library(brms)
library(metafor)
library(posterior)
library(tidyverse)

# Project functions:

source("src/functions.R")

# The pools behind the primary fits:

model_pools <-
  read_rds("output/draft_output/models/model_data.rds")

# Every effect size in the pool:

effect_sizes <- read_effect_size_pool()

# The species classification frame:

species_analysis_frame <-
  fs::path(
    "data/processed",
    "species_classified_analysis_frame.csv"
  ) %>%
  read_csv(show_col_types = FALSE)

# Sample the chains in parallel:

options(mc.cores = sampler_settings$cores)

# The reported guild cell means:

table_guild_bmp <-
  fs::path("output/draft_output/tables", "table_guild_bmp.csv") %>%
  read_csv(show_col_types = FALSE)

# The reported richness cell means:

table_species_richness <-
  fs::path(
    "output/draft_output/tables",
    "table_species_richness_by_bmp.csv"
  ) %>%
  read_csv(show_col_types = FALSE)

# The reported pooled cell means:

table_pooled_bmp <-
  fs::path("output/draft_output/tables", "table_pooled_bmp.csv") %>%
  read_csv(show_col_types = FALSE)

# The three reported tables:

results_tables <-
  list(
    table_guild_bmp,
    table_species_richness,
    table_pooled_bmp
  )

# The guild and pooled tables together:

bmp_cell_tables <-
  bind_rows(
    table_guild_bmp,
    table_pooled_bmp
  )

# Columns identifying one cell:

cell_keys <-
  c(
    "response_metric",
    "guild",
    "bmp"
  )

# 1. frequentist cross-check -----------------------------------------------

# The pools refit with REML:

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
    ),
    list(
      response_metric = "nest_success",
      pool_name = "nest_success_pooled_bmp",
      guild_scope = "pooled"
    )
  ) %>%

  # Keep the pools that were modeled:

  keep(
    \(.specification) {
      .specification$pool_name %in% names(model_pools)
    }
  )

# REML cell means for each pool:

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
  ) %>%
  separate_wider_delim(
    cell,
    delim = "__",
    names = c("guild", "bmp")
  )

# Richness, with its own random terms:

reml_richness <-
  model_pools %>%
  pluck("richness_bmp") %>%
  fit_reml_cell_means(
    cell_variable = "bmp",
    random_terms =
      list(
        ~ 1 | key,
        ~ 1 | effect_id
      )
  ) %>%
  mutate(
    guild = NA_character_,
    bmp = cell,
    response_metric = "species_richness",
    guild_scope = "by practice",
    .keep = "unused"
  )

# Each cell under both frameworks:

verification_bayes_vs_reml <-
  bind_rows(
    reml_cell_means,
    reml_richness
  ) %>%
  left_join(
    bind_rows(
      bmp_cell_tables,
      table_species_richness
    ) %>%
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

# Write it:

verification_bayes_vs_reml %>%
  write_output_table(
    file_name = "verification_bayes_vs_reml.csv",
    directory = "output/draft_output/diagnostics"
  )

# 2. sample-size reconciliation --------------------------------------------

# The effect sizes the primary models fit:

usable_effect_sizes <-
  effect_sizes %>%
  filter(
    in_primary_pool,
    is.finite(yi),
    is.finite(sei),
    sei > 0
  )

# Guild cell sizes, recounted:

recomputed_guild_bmp <-
  usable_effect_sizes %>%
  filter(
    response_metric %in% c("nest_success", "abundance"),
    !is.na(guild)
  ) %>%
  summarize(
    k_recomputed = n(),
    n_studies_recomputed = n_distinct(key),
    .by = all_of(cell_keys)
  )

# Richness cell sizes, recounted:

recomputed_richness <-
  usable_effect_sizes %>%
  filter(response_metric == "species_richness") %>%
  summarize(
    k_recomputed = n(),
    n_studies_recomputed = n_distinct(key),
    .by = c(response_metric, bmp)
  ) %>%
  mutate(
    guild = NA_character_
  )

# Pooled cell sizes, one metric at a time:

recomputed_pooled_bmp <-
  c("nest_success", "abundance") %>%
  map(
    \(.metric) {
      usable_effect_sizes %>%
        filter(response_metric == .metric) %>%
        keep_pooled_rows()
    }
  ) %>%
  list_rbind() %>%
  summarize(
    k_recomputed = n(),
    n_studies_recomputed = n_distinct(key),
    .by = c(response_metric, bmp)
  ) %>%
  mutate(
    guild = "all_grassland"
  )

# The sizes the tables print:

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

# Reported against recounted:

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

# Write it:

verification_reconciliation %>%
  write_output_table(
    file_name = "verification_reconciliation.csv",
    directory = "output/draft_output/diagnostics"
  )

# 3. assertions ------------------------------------------------------------

# Every reported cell and its thresholds:

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

# Cells that did not reconcile:

reconciliation_mismatches <-
  verification_reconciliation %>%
  filter(
    !replace_na(k_matches, FALSE) |
      !replace_na(studies_match, FALSE)
  ) %>%
  nrow()

# Provisional flags on thin cells:

sub_threshold_flags <-
  reported_cells %>%
  filter(n_studies < 3) %>%
  pull(meets_primary_threshold)

# Zero-exclusion agreement above the threshold:

primary_exclusion_agreement <-
  verification_bayes_vs_reml %>%
  filter(meets_primary_threshold) %>%
  pull(agrees_on_exclusion)

# The same for provisional cells:

provisional_exclusion_agreement <-
  verification_bayes_vs_reml %>%
  filter(!meets_primary_threshold) %>%
  pull(agrees_on_exclusion)

# Guilds each species resolves to:

guilds_per_species <-
  species_analysis_frame %>%
  summarize(
    n_guilds = n_distinct(analysis_class),
    .by = species
  ) %>%
  pull(n_guilds)

# Shrubland labels in the tables:

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

# Non-grassland rows reaching a pool:

non_grassland_pool_rows <-
  model_pools %>%
  map_int(
    \(.pool) {
      .pool %>%
        filter(non_grassland_class) %>%
        nrow()
    }
  ) %>%
  sum()

# Assemblage rows reaching the pooled pools:

pooled_community_rows <-
  model_pools %>%
  keep_at(
    c(
      "abundance_pooled_bmp",
      "nest_success_pooled_bmp"
    )
  ) %>%
  map_int(
    \(.pool) {
      .pool %>%
        filter(label_type == "community") %>%
        nrow()
    }
  ) %>%
  sum()

# Pooled cells against their guild cells:

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
      summarize(
        guild_k = sum(k),
        .by = c(response_metric, bmp)
      ),
    by = c("response_metric", "bmp")
  ) %>%
  mutate(
    guild_k = replace_na(guild_k, 0),
    covers_guilds = pooled_k >= guild_k
  )

# Pooled rows carrying the pooled label:

pooled_rows_labeled <-
  table_pooled_bmp %>%
  pull(guild) %>%
  str_detect("all_grassland")

# Guild rows carrying it wrongly:

guild_rows_mislabelled <-
  table_guild_bmp %>%
  pull(guild) %>%
  str_detect("all_grassland")

# The metrics reported pooled:

pooled_metrics_reported <-
  table_pooled_bmp %>%
  pull(response_metric) %>%
  unique()

# Practices eligible for pooling, by metric:

pooled_practices_expected <-
  c(
    abundance = "abundance_guild_bmp",
    nest_success = "nest_success_guild_bmp"
  ) %>%
  map(
    \(.pool_name) {
      model_pools %>%
        pluck(.pool_name) %>%
        keep_practices_covering_both_guilds() %>%
        distinct(bmp) %>%
        pull(bmp) %>%
        as.character()
    }
  ) %>%
  keep(
    \(.practices) {
      length(.practices) > 1
    }
  )

# The practices reported pooled:

pooled_practices_reported <-
  table_pooled_bmp %>%
  summarize(
    practices =
      bmp %>%
      as.character() %>%
      list(),
    .by = response_metric
  ) %>%
  deframe()

# Nest effects on the hazard scale:

nest_rows_on_hazard_scale <-
  usable_effect_sizes %>%
  filter(response_metric == "nest_success") %>%
  pull(effect_metric) %>%
  str_equal("log_hazard_ratio")

# The other effects, on Hedges' g:

other_rows_on_g_scale <-
  usable_effect_sizes %>%
  filter(response_metric != "nest_success") %>%
  pull(effect_metric) %>%
  str_equal("hedges_g")

# Derived coefficients and their notes:

derived_coefficients <-
  usable_effect_sizes %>%
  filter(
    replace_na(beta_is_derived, FALSE)
  ) %>%
  mutate(
    has_note =
      !replace_na(
        str_squish(notes) == "",
        TRUE
      )
  )

# Every assertion, with its detail:

verification_assertions <-
  tribble(
    ~ assertion, ~ passed, ~ detail,
    "Every reported cell meets its metric's effect-size threshold",
    all(reported_cells$k >= reported_cells$metric_min_effect_sizes),
    glue::glue("Minimum k reported: {min(reported_cells$k)}"),
    "Every reported cell meets its metric's independent-study threshold",
    all(reported_cells$n_studies >= reported_cells$metric_min_studies),
    glue::glue("Minimum studies reported: {min(reported_cells$n_studies)}"),
    "Cells below the primary threshold are flagged provisional",
    all(!sub_threshold_flags),
    glue::glue(
      "Provisional cells: ",
      "{sum(!reported_cells$meets_primary_threshold)} ",
      "of {nrow(reported_cells)}"
    ),
    "Reported sample sizes reconcile with the effect-size table",
    reconciliation_mismatches == 0,
    glue::glue("Mismatched cells: {reconciliation_mismatches}"),
    "No species resolves to more than one guild",
    max(guilds_per_species) == 1,
    glue::glue("Species checked: {length(guilds_per_species)}"),
    "No shrubland guild appears in any results table",
    !any(shrubland_flags),
    glue::glue(
      "Shrubland rows in the results tables: {sum(shrubland_flags)}"
    ),
    "No shrubland, woodland or forest species reaches a primary pool",
    non_grassland_pool_rows == 0,
    glue::glue("Vegetation-class rows in the pools: {non_grassland_pool_rows}"),
    "Bayesian and REML fits agree on the sign of every non-null cell mean",
    all(verification_bayes_vs_reml$agrees_on_sign),
    glue::glue(
      "Disagreements: {sum(!verification_bayes_vs_reml$agrees_on_sign)}"
    ),
    str_c(
      "Bayesian and REML fits agree on which intervals exclude zero, ",
      "for cells above the primary threshold"
    ),
    all(primary_exclusion_agreement),
    glue::glue(
      "Disagreements above the primary threshold: ",
      "{sum(!primary_exclusion_agreement)}"
    ),
    "Provisional cells where REML and Bayes disagree on excluding zero",
    TRUE,
    glue::glue(
      "{sum(!provisional_exclusion_agreement)} of ",
      "{length(provisional_exclusion_agreement)} provisional cells ",
      "(informational)"
    ),
    "No assemblage-level effect size enters a pooled model",
    pooled_community_rows == 0,
    glue::glue(
      "Community rows in the pooled pools: {pooled_community_rows}"
    ),
    "Every pooled cell holds at least the effect sizes of its guild cells",
    all(pooled_coverage$covers_guilds),
    glue::glue(
      "Pooled cells smaller than their guild cells: ",
      "{sum(!pooled_coverage$covers_guilds)}"
    ),
    "Pooled estimates are labeled pooled, and guild estimates are not",
    all(pooled_rows_labeled) &&
      !any(guild_rows_mislabelled),
    glue::glue("Pooled cells reported: {length(pooled_rows_labeled)}"),
    str_c(
      "Every pooled metric pools only the practices found in both of its ",
      "guilds, and no metric is pooled that should not be"
    ),
    setequal(
      pooled_metrics_reported,
      names(pooled_practices_expected)
    ) &&
      all(
        imap_lgl(
          pooled_practices_expected,
          \(.practices, .metric) {
            setequal(
              .practices,
              pluck(pooled_practices_reported, .metric)
            )
          }
        )
      ),
    glue::glue(
      "Metrics pooled: {str_flatten_comma(pooled_metrics_reported)}; ",
      "eligible practices: ",
      "{str_flatten_comma(lengths(pooled_practices_expected))}"
    ),
    str_c(
      "Every nest-survival effect size is a log hazard ratio, and every ",
      "other effect size is Hedges' g"
    ),
    all(nest_rows_on_hazard_scale) &&
      all(other_rows_on_g_scale),
    glue::glue(
      "Scales verified from the effect_metric column across ",
      "{nrow(usable_effect_sizes)} effect sizes"
    ),
    str_c(
      "Every coefficient derived from a paper's own estimates, rather than ",
      "read off it, records how it was derived"
    ),
    all(derived_coefficients$has_note),
    glue::glue(
      "Derived coefficients in the pool: {nrow(derived_coefficients)}"
    )
  )

# Write it:

verification_assertions %>%
  write_output_table(
    file_name = "verification_assertions.csv",
    directory = "output/draft_output/diagnostics"
  )

# Any that did not pass:

failed_assertions <-
  verification_assertions %>%
  filter(
    !replace_na(passed, FALSE)
  ) %>%
  pull(assertion)

# Warn on them:

if (length(failed_assertions) > 0) {
  cli::cli_warn(
    "Verification assertions failed: {failed_assertions}."
  )
}

# clear the environment ----------------------------------------------------

rm(
  list = ls()
)
