# This script:
# - Reads the three categorical sheets from data/processed/for_analysis
# - Converts each to Hedges' g by the pathway its columns support
# - Screens the combined table once, at the end
# - Saves the analysis pool, and the records it holds out, to db_mirror
#   and output/audits

# setup --------------------------------------------------------------------

library(tidyverse)

source("scripts/functions.R")

fs::dir_create(
  c(
    "brian_sandbox/data/db_mirror",
    "output/audits"
  )
)

# read the extraction ------------------------------------------------------

# One sheet per pathway to an effect size, each with the prefix its
# identifiers carry. Identifiers are assigned before any screening.

extraction <-
  c(
    mean_diff = "md_",
    beta_categorical = "bc_",
    other_categorical = "tc_"
  ) %>%
  imap(
    \(.prefix, .sheet) {
      str_c("data/processed/for_analysis/", .sheet, ".csv") %>%
        read_csv(show_col_types = FALSE) %>%
        mutate(
          source_sheet = .sheet,
          species_key = species,
          response_metric = response_class,

          # A diversity index is not a species count.

          response_scale =
            response_var %>%
            str_to_lower() %>%
            str_detect("diversity|evenness|shannon|simpson") %>%
            if_else("diversity", "count")
        ) %>%
        add_row_id(prefix = .prefix)
    }
  )

# mean differences ---------------------------------------------------------

# Hedges' g from two group means and their pooled SD. Each arm's SD is the
# reported one, else the SE, else the confidence interval.

mean_difference_effects <-
  extraction %>%
  pluck("mean_diff") %>%
  add_group_sd(arm = "e") %>%
  add_group_sd(arm = "c") %>%
  bind_cols(
    g_from_group_means(
      xbar_e = .$xbar_e,
      sd_e = .$sd_e_used,
      n_e = .$n_e,
      xbar_c = .$xbar_c,
      sd_c = .$sd_c_used,
      n_c = .$n_c
    )
  ) %>%
  mutate(conversion = "two group means")

# categorical coefficients -------------------------------------------------

# t = beta / SE, then Hedges' g. Exact group sizes where both are reported,
# balanced groups assumed where only a total is.

beta_categorical_effects <-
  extraction %>%
  pluck("beta_categorical") %>%
  bind_cols(
    derive_beta_se(
      se_reported = .$se,
      sd_reported = .$sd,
      lower_cl = .$lcl,
      upper_cl = .$ucl,
      lower_cl_e = .$lcl_e,
      upper_cl_e = .$ucl_e,
      n = .$n
    )
  ) %>%
  bind_cols(
    g_from_categorical_beta(
      beta = .$beta,
      se = .$se_used,
      n_total = .$n,
      n_e = .$n_e,
      n_c = .$n_c
    )
  ) %>%
  mutate(conversion = "regression coefficient")

# test statistics ----------------------------------------------------------

# `needs_one_df` is whether a two-group contrast has to be established first.
# A statistic with no rule here -- "D" -- has no route to an effect size.

test_statistic_effects <-
  extraction %>%
  pluck("other_categorical") %>%
  left_join(
    tribble(
      ~ test_statistic, ~ needs_one_df,
      "t", FALSE,
      "F", TRUE,
      "chi-square", TRUE,
      "Z", FALSE,
      "Odds ratio", FALSE
    ),
    by = "test_statistic"
  ) %>%
  mutate(

    # Free text is lowered and blanked so the patterns below read off it.

    across(
      c(model, notes),
      \(.text) {
        .text %>%
          str_to_lower() %>%
          replace_na("")
      }
    ),

    # Group sizes fall back to halving the global sample size.

    groups_reported =
      !is.na(treatment_n) &
      !is.na(control_n),
    n_e =
      if_else(
        groups_reported,
        treatment_n,
        global_n / 2
      ),
    n_c =
      if_else(
        groups_reported,
        control_n,
        global_n / 2
      ),
    n_total = n_e + n_c,
    contrast_se =
      sqrt(1 / n_e + 1 / n_c),

    # The direction comes from `response_dir`, so the statistic's own sign is
    # discarded and every conversion below is defined on every row.

    magnitude = abs(test_stat_value),

    # Phi from chi-square, rank-biserial r from Z.

    correlation =
      case_match(
        test_statistic,
        "chi-square" ~ sqrt(magnitude / n_total),
        "Z" ~ magnitude / sqrt(n_total)
      ),

    # A contrast converts on one degree of freedom, independent arms, and a
    # correlation below one.

    one_contrast =
      replace_na(df == 1, FALSE) |
      str_detect(notes, "comparison of two groups"),
    convertible =
      !is.na(needs_one_df) &
      !is.na(response_dir) &
      !str_detect(model, "signed rank|matched.pair|paired") &
      !str_detect(notes, "> 2 groups") &
      replace_na(n_total >= 4, FALSE) &
      (!needs_one_df | one_contrast) &
      !replace_na(correlation >= 1, FALSE)
  ) %>%
  mutate(

    # Only a convertible contrast reaches the arithmetic; the rest keep a
    # missing effect size for the screen.

    across(
      c(magnitude, correlation),
      \(.value) {
        value_when(convertible, .value)
      }
    ),
    cohens_d =
      case_match(
        test_statistic,
        "t" ~ magnitude * contrast_se,
        "F" ~ sqrt(magnitude) * contrast_se,
        c("chi-square", "Z") ~ d_from_r(correlation),
        "Odds ratio" ~ d_from_odds_ratio(magnitude)
      ) %>%
      abs() %>%
      magrittr::multiply_by(response_dir),
    correction =
      hedges_correction(n_total - 2),
    yi = correction * cohens_d,
    sei =
      sqrt(
        correction^2 *
          (n_total / (n_e * n_c) + cohens_d^2 / (2 * n_total))
      ),
    conversion = "test statistic"
  )

# one table ----------------------------------------------------------------

all_effects <-
  bind_rows(
    mean_difference_effects,
    beta_categorical_effects,
    test_statistic_effects
  ) %>%
  select(
    es_id = row_id,
    key,
    source_sheet,
    bmp,
    treatment,
    control,
    response_var,
    species,
    species_key,
    analysis_class,
    response_metric,
    response_scale,
    species_include,
    treatment_control_flag,
    response_flag,
    sign,
    conversion,
    yi,
    sei,
    n_total
  ) %>%
  mutate(

    # `sign` was assessed against the papers during extraction, and is the
    # only source of the direction a benefit runs in.

    yi = yi * sign,
    label_type =
      if_else(
        species_key == "all_species",
        "community",
        "species"
      ),
    guild =
      if_else(
        label_type == "species",
        str_c(analysis_class, "_grassland"),
        NA_character_
      ),
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

    # Every fire interval is retained in the primary pool. Set this to
    # `!fire_excluded_original` to apply the original post-fire rule instead.

    in_primary_pool = TRUE
  )

# screen the table ---------------------------------------------------------

# One line per reason, each the negation of a test the record has to pass,
# and a missing value fails that test. Comment one out to run without it.

screened_effects <-
  all_effects %>%
  mutate(
    excluded_by =
      case_when(
        !replace_na(species_include, FALSE) ~ "species_include",
        !replace_na(treatment_control_flag != "oranges", FALSE) ~
          "treatment_control_flag",
        !replace_na(response_flag != 1, FALSE) ~ "response_flag",
        is.na(sign) ~ "sign",
        !response_metric %in%
          c("species_richness", "nest_success", "abundance") ~
          "response_metric",
        !guild %in%
          c("obligate_grassland", "facultative_grassland") &
          label_type != "community" ~ "analysis_class",

        # Contrast mismatch over a mixed-guild assemblage, held out by a
        # recorded team decision.

        key == "pytisvj6" ~ "study_decision",
        !replace_na(is.finite(yi) & is.finite(sei) & sei > 0, FALSE) ~
          "conversion",
        abs(yi) > 20 ~ "implausible_effect"
      )
  )

# One result reported more than one way, say as both nest success and daily
# survival. The preferred expression is kept and the rest held out.

response_expression_preference <-
  tribble(
    ~ response_metric, ~ pattern, ~ preference_rank,
    "nest_success", "^(percent )?nest.?(success|surv)|probability of nesting",
    1,
    "nest_success", "dsr|daily (nest )?surv|mayfield", 2,
    "nest_success", "fledg|successful nests", 3,
    "nest_success", "depredat|predation", 4,
    "abundance", "abundance", 1,
    "abundance", "densit", 2,
    "abundance", "territor|singing males|crowing", 3,
    "abundance", "nest", 4,
    "species_richness", "^(total )?species.?richness", 1,
    "species_richness", "species per field", 2
  )

duplicate_expressions <-
  screened_effects %>%

  # Resolved among the records that passed everything above, so that a
  # held-out expression cannot displace a kept one.

  filter(is.na(excluded_by)) %>%
  bind_cols(
    split_response_var(.$response_var)
  ) %>%
  mutate(
    group_id =
      str_c(
        key,
        bmp,
        species_key,
        replace_na(treatment, ""),
        replace_na(control, ""),
        response_metric,
        response_token,
        sep = " | "
      )
  ) %>%
  filter(
    n_distinct(response_base) > 1,
    .by = group_id
  ) %>%
  mutate(
    preference_rank =
      map2_int(
        response_metric,
        response_var,
        rank_response_expression
      )
  ) %>%

  # Best-ranked row, ties broken by the smaller standard error. A group whose
  # expressions disagree in sign is a reading to check against the paper.

  mutate(
    keep_row =
      n_distinct(yi > 0) == 1 &
      preference_rank == min(preference_rank) &
      sei == min(sei[preference_rank == min(preference_rank)]),
    .by = group_id
  ) %>%
  filter(!keep_row) %>%
  pull(es_id)

screened_effects <-
  screened_effects %>%
  mutate(
    excluded_by =
      if_else(
        es_id %in% duplicate_expressions,
        "duplicate_expression",
        excluded_by
      )
  )

# write --------------------------------------------------------------------

# The pool, without the screening columns, which are constant across it.

screened_effects %>%
  filter(is.na(excluded_by)) %>%
  select(
    !c(
      species_include,
      treatment_control_flag,
      response_flag,
      sign,
      excluded_by
    )
  ) %>%
  write_csv(
    "brian_sandbox/data/db_mirror/effect_sizes.csv",
    na = ""
  )

# Everything held out, whole, with the reason and the values behind it.

screened_effects %>%
  filter(!is.na(excluded_by)) %>%
  arrange(
    excluded_by,
    source_sheet,
    key
  ) %>%
  write_csv(
    "output/audits/excluded_effects.csv",
    na = ""
  )
