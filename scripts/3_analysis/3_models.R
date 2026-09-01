# This script:
# - Reads the analysis pool written by 2_screen_effects.R
# - Builds one modeling pool per response, guild and practice cell
# - Fits the Bayesian multilevel meta-analysis models with four chains
# - Saves the fits, their pools, and the cell and convergence tables

# setup --------------------------------------------------------------------

library(brms)
library(posterior)
library(tidyverse)

# Project functions:

source("src/functions.R")

# Output directories:

fs::dir_create(
  c(
    "output/audits",
    "output/models",
    "output/diagnostics"
  )
)

# Sample the chains in parallel:

options(mc.cores = sampler_settings$cores)

# Weakly informative priors:

model_priors <-
  c(
    prior(
      normal(0, 1),
      class = "b"
    ),
    prior(
      student_t(
        3,
        0,
        0.5
      ),
      class = "sd"
    )
  )

# assemble the analysis pool -----------------------------------------------

# The primary pool, with its grouping variables:

analysis_pool <-
  read_effect_size_pool() %>%
  filter(in_primary_pool) %>%
  mutate(
    across(
      c(
        key,
        effect_id,
        species_key,
        bmp,
        guild
      ),
      \(.column) {
        as.factor(.column)
      }
    )
  )

# modeling pools------------------------------------------------------------

richness_pool <-
  analysis_pool %>%
  filter(response_metric == "species_richness") %>%
  apply_inclusion_thresholds(grouping_vars = "bmp") %>%
  mutate(
    bmp = fct_drop(bmp)
  )

# Abundance, by guild:

abundance_guild_pool <-
  analysis_pool %>%
  build_guild_pool(metric = "abundance")

# The same, by guild x practice cell:

abundance_guild_bmp_pool <-
  abundance_guild_pool %>%
  build_guild_bmp_pool()

# Nest success, by guild x practice cell:

nest_success_guild_bmp_pool <-
  analysis_pool %>%
  build_guild_pool(metric = "nest_success") %>%
  build_guild_bmp_pool()

# Abundance with the guilds pooled:

abundance_pooled_bmp_pool <-
  analysis_pool %>%
  build_pooled_pool(metric = "abundance") %>%
  build_guild_bmp_pool()

# Nest success with the guilds pooled:

nest_success_pooled_bmp_pool <-
  analysis_pool %>%
  build_pooled_pool(metric = "nest_success") %>%
  build_guild_bmp_pool()

# Drop the pools with one cell:

model_pools <-
  list(
    richness_bmp = richness_pool,
    abundance_guild = abundance_guild_pool,
    abundance_guild_bmp = abundance_guild_bmp_pool,
    nest_success_guild_bmp = nest_success_guild_bmp_pool,
    abundance_pooled_bmp = abundance_pooled_bmp_pool,
    nest_success_pooled_bmp = nest_success_pooled_bmp_pool
  ) %>%
  keep(
    \(.pool) {
      !"guild_bmp" %in% names(.pool) ||
        n_distinct(.pool$guild_bmp) > 1
    }
  )

# pool summaries -----------------------------------------------------------

model_pools %>%
  map(
    \(.pool) {
      summarize(
        .pool,
        n_effect_sizes = n(),
        n_studies = n_distinct(key),
        n_species = n_distinct(species_key),
        n_bmps = n_distinct(bmp),
        median_sei = median(sei)
      )
    }
  ) %>%
  list_rbind(names_to = "pool") %>%
  write_csv(
    "output/audits/analysis_pool_summary.csv",
    na = ""
  )

# The metric each modeled pool belongs to:

cell_pool_metrics <-
  c(
    abundance_guild_bmp = "abundance",
    nest_success_guild_bmp = "nest_success",
    abundance_pooled_bmp = "abundance",
    nest_success_pooled_bmp = "nest_success"
  ) %>%
  keep_at(
    names(model_pools)
  )

# Effect sizes and studies per cell:

cell_sample_sizes <-
  bind_rows(
    richness_pool %>%
      count_cells(grouping_vars = "bmp") %>%
      mutate(
        response_metric = "species_richness",
        guild = NA_character_
      ),
    cell_pool_metrics %>%
      imap(
        \(.metric, .pool_name) {
          model_pools %>%
            pluck(.pool_name) %>%
            count_cells(
              grouping_vars = c("guild", "bmp")
            ) %>%
            mutate(response_metric = .metric)
        }
      ) %>%
      list_rbind()
  ) %>%
  mutate(
    across(
      c(bmp, guild),
      \(.column) {
        as.character(.column)
      }
    )
  ) %>%
  select(
    response_metric,
    guild,
    bmp,
    k,
    n_studies
  ) %>%
  flag_primary_threshold()

# The reporting scripts read these back:

cell_sample_sizes %>%
  write_csv(
    "output/audits/cell_sample_sizes.csv",
    na = ""
  )

# model formulas -----------------------------------------------------------

formula_richness_bmp <-
  bf(
    yi | se(sei) ~ 0 + bmp +
      (1 | key) +
      (1 | effect_id)
  )

# Guild intercept, for the species estimates:

formula_guild <-
  bf(
    yi | se(sei) ~ 0 + guild +
      (1 | key) +
      (1 | effect_id) +
      (1 | species_key) +
      (1 | bmp)
  )

# A cell mean per guild x practice:

formula_guild_bmp <-
  bf(
    yi | se(sei) ~ 0 + guild_bmp +
      (1 | key) +
      (1 | effect_id) +
      (1 | species_key)
  )

# fit models ---------------------------------------------------------------

# One row per model:

model_specs <-
  tribble(
    ~ model, ~ pool, ~ model_formula, ~ refit_from,
    "richness_bmp", "richness_bmp",
    formula_richness_bmp, NA,
    "abundance_guild", "abundance_guild",
    formula_guild, NA,
    "abundance_guild_bmp", "abundance_guild_bmp",
    formula_guild_bmp, NA,
    "nest_success_guild_bmp", "nest_success_guild_bmp",
    formula_guild_bmp, "abundance_guild_bmp",
    "abundance_pooled_bmp", "abundance_pooled_bmp",
    formula_guild_bmp, "abundance_guild_bmp",
    "nest_success_pooled_bmp", "nest_success_pooled_bmp",
    formula_guild_bmp, "abundance_guild_bmp"
  ) %>%
  filter(
    pool %in% names(model_pools)
  )

# Fit one compile group at a time:

grouped_fits <-
  model_specs %>%
  mutate(
    program = coalesce(refit_from, model)
  ) %>%
  group_split(program) %>%
  map(
    \(.specs) {
      fit_model_group(
        .specs,
        pools = model_pools,
        priors = model_priors
      )
    },
    .progress = TRUE
  ) %>%
  list_flatten()

# In the order the specifications name them:

fitted_models <- grouped_fits[model_specs$model]

# save ---------------------------------------------------------------------

fitted_models %>%
  write_rds("output/models/fitted_models.rds")

# The pools behind them:

model_pools %>%
  write_rds("output/models/model_data.rds")

# The cell means, as thinned draws:

fitted_models %>%
  write_cell_draws()

# diagnostics --------------------------------------------------------------

convergence <-
  fitted_models %>%
  imap(
    \(.fit, .model) {
      summarize_convergence(.fit, model_name = .model)
    }
  ) %>%
  list_rbind() %>%
  mutate(
    converged =
      max_rhat < 1.01 &
      min_bulk_ess > 400 &
      n_divergent == 0
  )

# The diagnostics, whether or not they pass:

convergence %>%
  write_csv(
    "output/diagnostics/convergence.csv",
    na = ""
  )

# Warn on any that did not converge:

if (any(!convergence$converged)) {
  unconverged <-
    convergence %>%
    filter(!converged) %>%
    pull(model) %>%
    str_flatten(collapse = ", ")
  cli::cli_warn("Convergence problems in: {unconverged}.")
}

# clear the environment ----------------------------------------------------

rm(
  list = ls()
)
