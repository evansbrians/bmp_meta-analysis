# This script:
# - Refits every model family under each alternate specification
# - Tests for publication bias and flags influential effect sizes
# - Saves the sensitivity tables read by the results page

# setup --------------------------------------------------------------------

library(brms)
library(metafor)
library(posterior)
library(tidyverse)

source("scripts/functions.R")

fs::dir_create("output/tables")

fitted_models <-
  read_rds("output/models/fitted_models.rds")

model_pools <-
  read_rds("output/models/model_data.rds")

effect_sizes <-
  "brian_sandbox/data/db_mirror/effect_sizes.csv" %>%
  read_csv(show_col_types = FALSE)

options(mc.cores = sampler_settings$cores)

# Every fire interval is retained in the primary pool, as build_pool() has it.

retain_all_fire <- TRUE

# refit helpers ------------------------------------------------------------

factor_columns <-
  c(
    "key",
    "es_id",
    "species_key",
    "bmp",
    "guild",
    "guild_bmp"
  )

# The model families refitted under each specification. Guild-level and fully
# pooled models are no longer reported, so they are not refitted either;
# abundance_guild is still fitted in 2_models.R for the species estimates but
# carries no reported cell means to test.

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

    # Abundance only, and only for practices estimable in both guilds.

    list(
      response_metric = "abundance",
      template = "abundance_pooled_bmp",
      cell_variable = "guild_bmp",
      grouping_vars = c("guild", "bmp"),
      pooled = TRUE
    )
  )

# specifications -----------------------------------------------------------

alternate_fire_label <-
  if (retain_all_fire) {
    "original_fire_exclusion_applied"
  } else {
    "all_fire_intervals_retained"
  }

specifications <-
  list(
    list(
      specification = "primary",
      arguments = list()
    ),
    list(
      specification = alternate_fire_label,
      arguments =
        list(
          retain_fire = !retain_all_fire
        )
    ),
    # Every extraction record carrying an unresolved data-quality flag is held
    # out, so the tables say how much of the result rests on records that have
    # not yet been checked against their source paper.

    list(
      specification = "flagged_effects_removed",
      arguments =
        list(
          drop_flagged = TRUE
        )
    )
  )

# run the suite ------------------------------------------------------------

partial_estimates_file <-
  "output/tables/sensitivity_estimates_partial.csv"

if (fs::file_exists(partial_estimates_file)) {
  fs::file_delete(partial_estimates_file)
}

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
              specification = .specification$specification
            )
          }
        ) %>%
        list_rbind() %>%
        write_partial_estimates()
    }
  ) %>%
  list_rbind()

# outlier sensitivity ------------------------------------------------------

guild_bmp_columns <- c("guild", "bmp")

outlier_families <-
  model_families %>%
  keep(
    ~ .x$cell_variable %in% c("guild_bmp", "bmp")
  )

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
          join_vars = join_vars
        ) %>%
        mutate(
          response_metric = .family$response_metric,
          model_family = .family$template,
          .before = 1
        )
      per_cell <-
        flagged %>%
        summarise(
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

      # One row per effect, so a flagged effect can be traced back to its
      # paper and checked rather than only counted.

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
          specification = "outliers_removed"
        )
      lst(per_cell, per_effect, estimates)
    }
  )

outliers_removed_per_cell <-
  outlier_results %>%
  map("per_cell") %>%
  list_rbind() %>%
  arrange(
    desc(percent_removed)
  )

outliers_removed_per_cell %>%
  write_output_table(
    file_name = "sensitivity_outliers_per_cell.csv"
  )

outliers_removed_per_effect <-
  outlier_results %>%
  map("per_effect") %>%
  list_rbind() %>%
  arrange(
    desc(
      abs(studentized_residual)
    )
  )

outliers_removed_per_effect %>%
  write_output_table(
    file_name = "sensitivity_outliers_per_effect.csv"
  )

sensitivity_estimates <-
  bind_rows(
    sensitivity_estimates,
    outlier_results %>%
      map("estimates") %>%
      list_rbind()
  )

sensitivity_estimates %>%
  write_output_table(
    file_name = "sensitivity_estimates.csv"
  )

# summary ------------------------------------------------------------------

# Columns identifying one cell across specifications.

cell_keys <-
  c(
    "response_metric",
    "model_family",
    "guild",
    "bmp"
  )

primary_estimates <-
  sensitivity_estimates %>%
  filter(specification == "primary") %>%
  select(
    all_of(cell_keys),
    primary_estimate = estimate,
    primary_excludes_zero = excludes_zero
  )

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

sensitivity_summary %>%
  write_output_table(
    file_name = "sensitivity_summary.csv"
  )

# The cells whose conclusion changed, so the results page can name them rather
# than only count them. The three kinds of change are not equivalent: a cell
# that gains precision leaves the primary reading intact and merely
# conservative, whereas one whose sign flips contradicts it.

conclusion_changes <-
  sensitivity_summary %>%
  filter(conclusion_changed) %>%
  mutate(
    change_type =
      case_when(
        excludes_zero & sign(estimate) != sign(primary_estimate) ~ "Reversed",
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

conclusion_changes %>%
  write_output_table(
    file_name = "sensitivity_conclusion_changes.csv"
  )

# The records the data-quality specification holds out, copied beside the other
# sensitivity tables so the page can describe the exclusion without reaching
# outside the output directory.

excluded_effects_listed <-
  if (fs::file_exists("brian_sandbox/data/excluded_effects.csv")) {
    read_csv(
      "brian_sandbox/data/excluded_effects.csv",
      show_col_types = FALSE
    ) %>%
      filter(status == "open")
  } else {
    tibble()
  }

excluded_effects_listed %>%
  write_output_table(
    file_name = "sensitivity_excluded_effects.csv"
  )

sensitivity_digest <-
  sensitivity_summary %>%
  summarise(
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

sensitivity_digest %>%
  write_output_table(
    file_name = "sensitivity_digest.csv"
  )

# publication bias ---------------------------------------------------------

abundance_pool <-
  model_pools %>%
  pluck("abundance_guild_bmp")

publication_bias <-
  list(
    "abundance (all guilds, aggregated)" = abundance_pool,
    "abundance (obligate grassland)" =
      abundance_pool %>%
      filter(guild == "obligate_grassland"),
    "abundance (facultative grassland)" =
      abundance_pool %>%
      filter(guild == "facultative_grassland"),
    "nest success (all guilds, aggregated)" =
      model_pools %>%
      pluck("nest_success_guild_bmp"),
    "species richness" =
      model_pools %>%
      pluck("richness_bmp"),
    "abundance (both guilds pooled)" =
      model_pools %>%
      pluck("abundance_pooled_bmp")
  ) %>%
  imap(
    \(.pool, .label) {
      .pool %>%
        run_egger_test(label = .label)
    }
  ) %>%
  list_rbind()

publication_bias %>%
  write_output_table(
    file_name = "sensitivity_publication_bias.csv"
  )
