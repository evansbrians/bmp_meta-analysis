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

source("scripts/functions.R")

fs::dir_create("output/diagnostics")

model_pools <-
  read_rds("output/models/model_data.rds")

effect_sizes <- read_effect_size_pool()

species_analysis_frame <-
  fs::path(
    "data/processed",
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
    ),
    list(
      response_metric = "nest_success",
      pool_name = "nest_success_pooled_bmp",
      guild_scope = "pooled"
    )
  ) %>%

  # A pool 2_models.R did not model has no cell means to check against.

  keep(
    \(.specification) {
      .specification$pool_name %in% names(model_pools)
    }
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
  ) %>%
  separate_wider_delim(
    cell,
    delim = "__",
    names = c("guild", "bmp")
  )

# The richness model is cross-checked too, with the random terms and the
# diversity offset its own formula carries.

reml_richness <-
  model_pools %>%
  pluck("richness_bmp") %>%
  fit_reml_cell_means(
    cell_variable = "bmp",
    random_terms =
      list(
        ~ 1 | key,
        ~ 1 | es_id
      ),
    with_index_type = TRUE
  ) %>%
  mutate(
    guild = NA_character_,
    bmp = cell,
    response_metric = "species_richness",
    guild_scope = "by practice",
    .keep = "unused"
  )

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

# Recomputed over both guilds at once, from the effect-size table rather than
# the pool, so a cell reported over rows the rule does not admit mismatches.

# One metric at a time: the assemblage rule reads a study's other records for
# the same practice, and the two metrics must not see each other's.

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

# A reported cell with no recomputed counterpart joins to NA, which is a
# failure to reconcile, not a pass.

reconciliation_mismatches <-
  verification_reconciliation %>%
  filter(
    !replace_na(k_matches, FALSE) |
      !replace_na(studies_match, FALSE)
  ) %>%
  nrow()

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

# The vegetation classes are held out of the primary analysis, so no record
# carrying one can reach a pool a primary model is fitted to.

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

# The pooled models must hold the two guilds and nothing else: no assemblage
# row, and no cell smaller than the guild cells it pools.

pooled_community_rows <-
  model_pools %>%
  keep_at(c("abundance_pooled_bmp", "nest_success_pooled_bmp")) %>%
  map_int(
    \(.pool) {
      .pool %>%
        filter(label_type == "community") %>%
        nrow()
    }
  ) %>%
  sum()

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

# The pooled model set is a decision, not an accident: a metric is pooled over
# the practices clearing the thresholds in both of its guilds, and only where
# two or more of them do -- one cell is the guild estimate under another name.

pooled_practices_expected <-
  c(
    abundance = "abundance_guild_bmp",
    nest_success = "nest_success_guild_bmp"
  ) %>%
  map(
    \(.pool_name) {
      model_pools %>%
        pluck(.pool_name) %>%
        practices_in_both_guilds()
    }
  ) %>%
  keep(
    \(.practices) {
      length(.practices) > 1
    }
  )

pooled_practices_reported <-
  table_pooled_bmp %>%
  summarise(
    practices = list(as.character(bmp)),
    .by = response_metric
  ) %>%
  deframe()

# Response scales must not mix: every nest-survival effect on the log hazard
# scale, every other effect on Hedges' g.

nest_rows_on_hazard_scale <-
  usable_effect_sizes %>%
  filter(response_metric == "nest_success") %>%
  pull(effect_metric) %>%
  str_equal("log_hazard_ratio")

other_rows_on_g_scale <-
  usable_effect_sizes %>%
  filter(response_metric != "nest_success") %>%
  pull(effect_metric) %>%
  str_equal("hedges_g")

# A derived coefficient is arithmetic on the paper's estimates, so the note
# is the only record of what was done.

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
    "Pooled estimates are labelled pooled, and guild estimates are not",
    all(pooled_rows_labelled) &&
      !any(guild_rows_mislabelled),
    glue::glue("Pooled cells reported: {length(pooled_rows_labelled)}"),
    str_c(
      "Every pooled metric pools only the practices found in both of its ",
      "guilds, and no metric is pooled that should not be"
    ),
    setequal(pooled_metrics_reported, names(pooled_practices_expected)) &&
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

verification_assertions %>%
  write_output_table(
    file_name = "verification_assertions.csv",
    directory = "output/diagnostics"
  )

failed_assertions <-
  verification_assertions %>%
  filter(
    !replace_na(passed, FALSE)
  ) %>%
  pull(assertion)

if (length(failed_assertions) > 0) {
  cli::cli_warn(
    "Verification assertions failed: {failed_assertions}."
  )
}
