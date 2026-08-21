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

source("scripts/functions.R")

fs::dir_create("output/tables")

fitted_models <-
  read_rds("output/models/fitted_models.rds")

model_pools <-
  read_rds("output/models/model_data.rds")

effect_sizes <- read_effect_size_pool()

options(mc.cores = sampler_settings$cores)

# Every fire interval is retained in the primary pool, as build_pool() has it.

retain_all_fire <- TRUE

# The prior-sensitivity settings: the primary prior, doubled and halved on
# both the effect scale and the variance components. A conclusion that moves
# between these is a conclusion the prior was carrying.

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

    # The guilds pooled, for the practices estimable in both of them.

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

  # A family whose template 2_models.R did not fit has nothing to refit from.

  keep(
    \(.family) {
      .family$template %in% names(fitted_models)
    }
  )

# specifications -----------------------------------------------------------

alternate_fire_label <-
  if (retain_all_fire) {
    "original_fire_exclusion_applied"
  } else {
    "all_fire_intervals_retained"
  }

# Each specification is a set of build_pool() arguments, plus an optional
# prior set handed to the refit.

# In order: the fire rule, the vegetation classes, the unresolved
# data-quality records, the prior, the independence of a study's effect sizes,
# the conversion routes, and the geography.

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

    # The shrubland, woodland and forest species the primary analysis holds
    # out, returned to it, so the tables say what the grassland-only
    # restriction is worth.

    list(
      specification = "non_grassland_classes_retained",
      arguments =
        list(
          retain_non_grassland = TRUE
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

    # One inverse-variance weighted effect size per study and cell: the
    # bound on what treating a study's sampling errors as independent
    # could cost.

    list(
      specification = "one_effect_per_study_cell",
      arguments =
        list(
          one_per_study_cell = TRUE
        )
    ),

    # Only effects computed from reported arm summaries, dropping the
    # coefficient and test-statistic conversion routes and the assumptions
    # they carry.

    list(
      specification = "group_means_only",
      arguments =
        list(
          group_means_only = TRUE
        )
    ),

    # Geography. Only the Great Plains holds enough of the pool to be refitted
    # on its own -- 6 of the 17 abundance cells survive there, against 1 in
    # the Southeast and 1 in Europe -- so the other regions are read by
    # dropping them instead. South America carries two studies and no
    # surviving cell, so dropping it would reproduce the primary exactly and
    # it has no specification.

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
              specification = .specification$specification,
              priors = .specification$priors,
              thresholds = .specification$thresholds
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

# influential studies ------------------------------------------------------

# For every primary cell whose interval excludes zero: drop the study with
# the largest absolute studentized deleted residual and refit.

# A headline finding that cannot survive the loss of one study is reported
# as such.

primary_clear_cells <-
  sensitivity_estimates %>%
  filter(
    specification == "primary",
    excludes_zero
  )

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
              summarise(
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
              specification = "most_influential_study_removed"
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

influence_estimates %>%
  write_output_table(
    file_name = "sensitivity_influence.csv"
  )

sensitivity_estimates <-
  bind_rows(
    sensitivity_estimates,
    influence_estimates %>%
      select(!removed_study)
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
  if (fs::file_exists("data/excluded_effects.csv")) {
    read_csv(
      "data/excluded_effects.csv",
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
