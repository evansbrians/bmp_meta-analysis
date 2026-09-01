# This script:
# - Refits every model family under each alternate specification, prior,
#   aggregation, conversion route and inclusion threshold among them
# - Tests for publication bias (Egger, PET, PEESE) and flags influential
#   effect sizes and studies
# - Saves the sensitivity tables read by the results page

# setup --------------------------------------------------------------------

library(brms)
library(metafor)
library(posterior)
library(tidyverse)

# Project functions:

source("src/functions.R")

# Output directory:

fs::dir_create("output/tables")

# The primary fits:

fitted_models <-
  read_rds("output/models/fitted_models.rds")

# The pools behind them:

model_pools <-
  read_rds("output/models/model_data.rds")

# Every effect size in the pool:

effect_sizes <- read_effect_size_pool()

# Sample the chains in parallel:

options(mc.cores = sampler_settings$cores)

# The primary prior, doubled and halved:

sensitivity_priors <-
  list(
    wider_priors =
      c(
        prior(
          normal(0, 2),
          class = "b"
        ),
        prior(
          student_t(
            3,
            0,
            1
          ),
          class = "sd"
        )
      ),
    tighter_priors =
      c(
        prior(
          normal(0, 0.5),
          class = "b"
        ),
        prior(
          student_t(
            3,
            0,
            0.25
          ),
          class = "sd"
        )
      )
  )

# refit helpers ------------------------------------------------------------

# The families refitted under each specification:

model_families <-
  list(
    list(
      response_metric = "abundance",
      template = "abundance_guild_bmp",
      cell_variable = "guild_bmp",
      grouping_vars = c("guild", "bmp"),
      pooled = FALSE
    ),
    list(
      response_metric = "nest_success",
      template = "nest_success_guild_bmp",
      cell_variable = "guild_bmp",
      grouping_vars = c("guild", "bmp"),
      pooled = FALSE
    ),
    list(
      response_metric = "species_richness",
      template = "richness_bmp",
      cell_variable = "bmp",
      grouping_vars = "bmp",
      pooled = FALSE
    ),

    # The guilds pooled:

    list(
      response_metric = "abundance",
      template = "abundance_pooled_bmp",
      cell_variable = "guild_bmp",
      grouping_vars = c("guild", "bmp"),
      pooled = TRUE
    ),
    list(
      response_metric = "nest_success",
      template = "nest_success_pooled_bmp",
      cell_variable = "guild_bmp",
      grouping_vars = c("guild", "bmp"),
      pooled = TRUE
    )
  ) %>%

  # Keep the families that were fitted:

  keep(
    \(.family) {
      .family$template %in% names(fitted_models)
    }
  )

# specifications -----------------------------------------------------------

# One entry per specification:

specifications <-
  list(
    list(
      specification = "primary",
      arguments = list()
    ),

    # Return the non-grassland species:

    list(
      specification = "non_grassland_classes_retained",
      arguments =
        list(
          retain_non_grassland = TRUE
        )
    ),

    # Hold out the flagged records:

    list(
      specification = "flagged_effects_removed",
      arguments =
        list(
          drop_flagged = TRUE
        )
    ),
    list(
      specification = "wider_priors",
      arguments = list(),
      priors = sensitivity_priors$wider_priors
    ),
    list(
      specification = "tighter_priors",
      arguments = list(),
      priors = sensitivity_priors$tighter_priors
    ),

    # One effect size per study and cell:

    list(
      specification = "one_effect_per_study_cell",
      arguments =
        list(
          one_per_study_cell = TRUE
        )
    ),

    # Arm summaries only:

    list(
      specification = "group_means_only",
      arguments =
        list(
          group_means_only = TRUE
        )
    ),

    # One region at a time:

    list(
      specification = "great_plains_only",
      arguments =
        list(
          only_region = "great_plains"
        )
    ),
    list(
      specification = "great_plains_dropped",
      arguments =
        list(
          drop_region = "great_plains"
        )
    ),
    list(
      specification = "southeast_us_dropped",
      arguments =
        list(
          drop_region = "southeast_us"
        )
    ),
    list(
      specification = "europe_dropped",
      arguments =
        list(
          drop_region = "europe"
        )
    ),

    # The floor, one paper either side:

    list(
      specification = "paper_floor_two",
      arguments =
        list(
          min_papers = 2
        )
    ),
    list(
      specification = "paper_floor_four",
      arguments =
        list(
          min_papers = 4
        )
    ),

    # Restrict the pool to guild records:

    list(
      specification = "guild_assigned_only",
      arguments =
        list(
          guild_assigned_only = TRUE
        )
    )
  )

# run the suite ------------------------------------------------------------

partial_estimates_file <-
  "output/tables/sensitivity_estimates_partial.csv"

# Clear any partial file:

if (fs::file_exists(partial_estimates_file)) {
  fs::file_delete(partial_estimates_file)
}

# Every specification refitted over every family:

sensitivity_estimates <-
  model_families %>%
  map(
    \(.family) {
      specifications %>%
        map(
          \(.specification) {
            pool_arguments <-
              c(
                list(
                  .effect_sizes = effect_sizes,
                  response_metric = .family$response_metric,
                  by_guild = .family$cell_variable != "bmp",
                  pooled = .family$pooled
                ),
                .specification$arguments
              )
            pool <- exec(build_pool, !!!pool_arguments)
            refit_family(
              .pool = pool,
              .family = .family,
              specification = .specification$specification,
              models = fitted_models,
              priors = .specification$priors,
              thresholds = .specification$thresholds
            )
          }
        ) %>%
        list_rbind() %>%
        write_partial_estimates(file_path = partial_estimates_file)
    }
  ) %>%
  list_rbind()

# outlier sensitivity ------------------------------------------------------

guild_bmp_columns <- c("guild", "bmp")

# Families estimating a cell mean:

outlier_families <-
  model_families %>%
  keep(
    ~ .x$cell_variable %in% c("guild_bmp", "bmp")
  )

# Refit each family without its outliers:

outlier_results <-
  outlier_families %>%
  map(
    \(.family) {
      join_vars <-
        switch(
          .family$cell_variable,
          guild_bmp = guild_bmp_columns,
          guild = "guild",
          bmp = "bmp"
        )
      pool <-
        build_pool(
          .effect_sizes = effect_sizes,
          response_metric = .family$response_metric,
          by_guild = .family$cell_variable != "bmp",
          pooled = .family$pooled
        ) %>%
        apply_inclusion_thresholds(
          grouping_vars = .family$grouping_vars
        )
      flagged <-
        flag_outliers(
          .pool = pool,
          join_vars = join_vars,
          cell_columns = guild_bmp_columns
        ) %>%
        mutate(
          response_metric = .family$response_metric,
          model_family = .family$template,
          .before = 1
        )
      per_cell <-
        flagged %>%
        summarize(
          k = n(),
          n_outliers = sum(is_outlier),
          n_untestable = sum(is.na(studentized_residual)),
          percent_removed = mean(is_outlier) * 100,
          .by = all_of(join_vars)
        ) %>%
        mutate(
          response_metric = .family$response_metric,
          model_family = .family$template,
          .before = 1
        )

      # One row per flagged effect:

      per_effect <-
        flagged %>%
        select(
          response_metric,
          model_family,
          all_of(join_vars),
          any_of(
            c(
              "key",
              "es_id",
              "species_key",
              "response_var"
            )
          ),
          yi,
          sei,
          studentized_residual,
          is_outlier
        )
      estimates <-
        flagged %>%
        filter(!is_outlier) %>%
        refit_family(
          .family = .family,
          specification = "outliers_removed",
          models = fitted_models
        )
      lst(per_cell, per_effect, estimates)
    }
  )

# What each cell lost, worst first:

outliers_removed_per_cell <-
  outlier_results %>%
  map("per_cell") %>%
  list_rbind() %>%
  arrange(
    desc(percent_removed)
  )

# Write it:

outliers_removed_per_cell %>%
  write_output_table(
    file_name = "sensitivity_outliers_per_cell.csv"
  )

# And which effect sizes those were:

outliers_removed_per_effect <-
  outlier_results %>%
  map("per_effect") %>%
  list_rbind() %>%
  arrange(
    desc(
      abs(studentized_residual)
    )
  )

# Write it:

outliers_removed_per_effect %>%
  write_output_table(
    file_name = "sensitivity_outliers_per_effect.csv"
  )

# Carry them in beside the rest:

sensitivity_estimates <-
  bind_rows(
    sensitivity_estimates,
    outlier_results %>%
      map("estimates") %>%
      list_rbind()
  )

# influential studies ------------------------------------------------------

# Leave one study out of each clear cell:

primary_clear_cells <-
  sensitivity_estimates %>%
  filter(
    specification == "primary",
    excludes_zero
  )

# Refit each clear cell:

influence_estimates <-
  model_families %>%
  map(
    \(.family) {
      family_cells <-
        primary_clear_cells %>%
        filter(model_family == .family$template)
      if (nrow(family_cells) == 0) {
        return(NULL)
      }
      pool <-
        build_pool(
          .effect_sizes = effect_sizes,
          response_metric = .family$response_metric,
          by_guild = .family$cell_variable != "bmp",
          pooled = .family$pooled
        ) %>%
        apply_inclusion_thresholds(
          grouping_vars = .family$grouping_vars
        )
      family_cells %>%
        pmap(
          \(guild, bmp, ...) {
            in_cell <-
              pool$bmp == bmp &
              (is.na(guild) | pool$guild == guild)
            cell_pool <- pool[in_cell, ]
            if (n_distinct(cell_pool$key) < 2) {
              return(NULL)
            }
            most_influential <-
              cell_pool %>%
              mutate(
                studentized_residual =
                  cell_deleted_residuals(
                    yi = yi,
                    sei = sei
                  )
              ) %>%
              summarize(
                influence =
                  max(
                    abs(studentized_residual),
                    na.rm = TRUE
                  ),
                .by = key
              ) %>%
              slice_max(
                influence,
                n = 1,
                with_ties = FALSE
              ) %>%
              pull(key)
            refit_family(
              .pool =
                pool %>%
                filter(
                  !(in_cell & key == most_influential)
                ),
              .family = .family,
              specification = "most_influential_study_removed",
              models = fitted_models
            ) %>%
              filter(
                bmp == {{ bmp }},
                is.na(guild) | guild == {{ guild }}
              ) %>%
              mutate(
                removed_study = most_influential
              )
          }
        ) %>%
        list_rbind()
    }
  ) %>%
  list_rbind()

# Write it:

influence_estimates %>%
  write_output_table(
    file_name = "sensitivity_influence.csv"
  )

# Carry them in beside the rest:

sensitivity_estimates <-
  bind_rows(
    sensitivity_estimates,
    influence_estimates %>%
      select(!removed_study)
  )

# Every specification, in one table:

sensitivity_estimates %>%
  write_output_table(
    file_name = "sensitivity_estimates.csv"
  )

# summary ------------------------------------------------------------------

# Columns identifying one cell:

cell_keys <-
  c(
    "response_metric",
    "model_family",
    "guild",
    "bmp"
  )

# The primary estimate to compare against:

primary_estimates <-
  sensitivity_estimates %>%
  filter(specification == "primary") %>%
  select(
    all_of(cell_keys),
    primary_estimate = estimate,
    primary_excludes_zero = excludes_zero
  )

# One row per cell and specification:

sensitivity_summary <-
  sensitivity_estimates %>%
  filter(specification != "primary") %>%
  left_join(
    primary_estimates,
    by = cell_keys
  ) %>%
  mutate(
    is_new_cell = is.na(primary_estimate),
    estimate_shift = estimate - primary_estimate,
    conclusion_changed =
      !is.na(primary_excludes_zero) &
      excludes_zero != primary_excludes_zero
  ) %>%
  arrange(
    response_metric,
    desc(
      abs(estimate_shift)
    )
  )

# Write it:

sensitivity_summary %>%
  write_output_table(
    file_name = "sensitivity_summary.csv"
  )

# The cells whose conclusion changed:

conclusion_changes <-
  sensitivity_summary %>%
  filter(conclusion_changed) %>%
  mutate(
    change_type =
      case_when(
        excludes_zero &
          sign(estimate) !=
          sign(primary_estimate) ~
          "Reversed",
        excludes_zero ~ "Gained",
        .default = "Lost"
      )
  ) %>%
  arrange(
    factor(
      change_type,
      levels =
        c(
          "Reversed",
          "Lost",
          "Gained"
        )
    ),
    desc(
      abs(estimate_shift)
    )
  )

# Write it:

conclusion_changes %>%
  write_output_table(
    file_name = "sensitivity_conclusion_changes.csv"
  )

# The records the data-quality screen holds out:

flagged_effects_listed <-
  if (fs::file_exists("data/flagged_effects.csv")) {
    read_csv(
      "data/flagged_effects.csv",
      show_col_types = FALSE
    ) %>%
      filter(status == "open")
  } else {
    tibble()
  }

# Write it:

flagged_effects_listed %>%
  write_output_table(
    file_name = "sensitivity_flagged_effects.csv"
  )

# The counts the results page quotes:

sensitivity_digest <-
  sensitivity_summary %>%
  summarize(
    n_cells = n(),
    n_new_cells = sum(is_new_cell),
    n_conclusions_changed = sum(conclusion_changed),
    median_absolute_shift =
      median(
        abs(estimate_shift),
        na.rm = TRUE
      ),
    .by =
      c(
        response_metric,
        model_family,
        specification
      )
  ) %>%
  arrange(
    response_metric,
    model_family,
    desc(n_conclusions_changed)
  )

# Write it:

sensitivity_digest %>%
  write_output_table(
    file_name = "sensitivity_digest.csv"
  )

# publication bias ---------------------------------------------------------

# One test per guild, and one for richness:

publication_bias <-
  list(
    abundance =
      model_pools %>%
      pluck("abundance_guild_bmp"),
    "nest success" =
      model_pools %>%
      pluck("nest_success_guild_bmp"),
    "species richness" =
      model_pools %>%
      pluck("richness_bmp")
  ) %>%
  imap(
    \(.pool, .label) {
      .pool %>%
        group_split(guild) %>%
        map(
          \(.guild_pool) {
            .guild_pool %>%
              run_egger_test(label = .label) %>%
              mutate(
                guild =
                  .guild_pool$guild %>%
                  first() %>%
                  as.character(),
                .after = analysis
              )
          }
        ) %>%
        list_rbind()
    }
  ) %>%
  list_rbind()

# Write it:

publication_bias %>%
  write_output_table(
    file_name = "sensitivity_publication_bias.csv"
  )

# clear the environment ----------------------------------------------------

rm(
  list = ls()
)
