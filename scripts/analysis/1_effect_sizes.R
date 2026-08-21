# This script:
# - Reads the three categorical sheets from data/processed/for_analysis
# - Converts abundance and richness records to Hedges' g by the pathway
#   their columns support
# - Converts nest-survival records to log hazard ratios
# - Writes the converted table, unscreened, to db_mirror

# Note: Screening is 1b_screen_effects.R, so a record that fails a conversion is
# still here with an empty yi.

# setup --------------------------------------------------------------------

library(tidyverse)

source("scripts/functions.R")

fs::dir_create("data/db_mirror")

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
      fs::path(
        "data/processed/for_analysis",
        .sheet,
        ext = "csv"
      ) %>%
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

# Hedges' g from two group means and their pooled SD:

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
  mutate(
    conversion = "two group means",
    effect_metric = "hedges_g"
  )

## nest survival to the log hazard scale in mean differences --------------

# A daily rate and a period probability are two reporting scales of one
# quantity -- we combine them with the log hazard ratio:

# Note: `yi` is negated to run benefit-positive, so it is the negative of a
# conventional log hazard ratio, and `sign` is not applied again below.

nest_survival_effects <-
  mean_difference_effects %>%
  filter(response_metric == "nest_success")

nest_hazard <-
  log_hazard_contrast(
    treatment =
      arm_log_hazard(
        xbar = nest_survival_effects$xbar_e,
        group_sd = nest_survival_effects$sd_e_used,
        n = nest_survival_effects$n_e,
        sign = nest_survival_effects$sign
      ),
    control =
      arm_log_hazard(
        xbar = nest_survival_effects$xbar_c,
        group_sd = nest_survival_effects$sd_c_used,
        n = nest_survival_effects$n_c,
        sign = nest_survival_effects$sign
      )
  )

# Inform the mean difference effects with the log-hazard:

mean_difference_effects <-
  mean_difference_effects %>%
  rows_update(
    nest_survival_effects %>%
      mutate(
        yi = nest_hazard$yi,
        sei = nest_hazard$sei,
        conversion = "log hazard ratio from arm survival",
        effect_metric = "log_hazard_ratio"
      ),
    by = "row_id"
  )

# categorical coefficients -------------------------------------------------

# t = beta / SE, then Hedges' g

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
  mutate(
    conversion = "regression coefficient",
    effect_metric = "hedges_g"
  )

## nest survival from a coefficient in beta categorical -------------------

# `link` names the scale the coefficient sits on -- logit of a survival,
# its logarithm, or the survival itself -- and each maps to the hazard scale
# once the baseline it maps from is known.

# It is the only route: a nest coefficient with no link, or none with a
# recorded baseline where one is needed, keeps its Hedges' g, which is
# withdrawn below and held out as `nest_hazard_scale`.

nest_coefficient_effects <-
  beta_categorical_effects %>%
  filter(
    response_metric == "nest_success",
    link %in% c("logistic_exposure", "logit", "log", "identity")
  )

nest_coefficient_hazard <-
  log_hazard_from_coefficient(
    beta = nest_coefficient_effects$beta,
    se = nest_coefficient_effects$se_used,
    sign = nest_coefficient_effects$sign,
    link = nest_coefficient_effects$link,
    baseline_survival = nest_coefficient_effects$baseline_survival
  )

beta_categorical_effects <-
  beta_categorical_effects %>%
  rows_update(
    nest_coefficient_effects %>%
      mutate(
        yi = nest_coefficient_hazard$yi,
        sei = nest_coefficient_hazard$sei,
        conversion =
          str_c(
            "log hazard ratio from ",
            link,
            " coefficient"
          ),
        effect_metric = "log_hazard_ratio"
      ),
    by = "row_id"
  )

# test statistics ----------------------------------------------------------

# `needs_one_df` is whether a two-group contrast has to be established first.
# A statistic with no rule here -- "d" -- has no route to an effect size.

test_statistic_effects <-
  extraction %>%
  pluck("other_categorical") %>%
  left_join(
    tribble(
      ~ test_statistic, ~ needs_one_df,
      "t", FALSE,
      "f", TRUE,
      "chi_square", TRUE,
      "z", FALSE,
      "odds_ratio", FALSE
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

    # Group sizes fall back to halving the global sample size. The arm
    # counts carry the _e and _c suffixes 0_prep_data.R restores.

    groups_reported =
      !is.na(n_e) &
      !is.na(n_c),
    n_e_used =
      if_else(
        groups_reported,
        n_e,
        global_n / 2
      ),
    n_c_used =
      if_else(
        groups_reported,
        n_c,
        global_n / 2
      ),
    n_total = n_e_used + n_c_used,
    contrast_se =
      sqrt(1 / n_e_used + 1 / n_c_used),

    # The direction comes from `response_dir`, so the statistic's own sign is
    # discarded and every conversion below is defined on every row.

    magnitude = abs(test_stat_value),

    # Phi from chi-square, rank-biserial r from Z.

    correlation =
      case_match(
        test_statistic,
        "chi_square" ~ sqrt(magnitude / n_total),
        "z" ~ magnitude / sqrt(n_total)
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
        "f" ~ sqrt(magnitude) * contrast_se,
        c("chi_square", "z") ~ d_from_r(correlation),
        "odds_ratio" ~ d_from_odds_ratio(magnitude)
      ) %>%
      abs() %>%
      magrittr::multiply_by(response_dir),
    correction =
      hedges_correction(n_total - 2),
    yi = correction * cohens_d,
    sei =
      sqrt(
        correction^2 *
          (n_total / (n_e_used * n_c_used) + cohens_d^2 / (2 * n_total))
      ),
    conversion = "test statistic",
    effect_metric = "hedges_g"
  )

## nest survival from an odds ratio ---------------------------------------

# A reported odds ratio of daily survival is the coefficient route wearing a
# different hat: log(OR) is the coefficient, and its interval the error.

nest_odds_ratio_effects <-
  test_statistic_effects %>%
  filter(
    response_metric == "nest_success",
    test_statistic == "odds_ratio",
    link %in% c("logistic_exposure", "logit")
  )

nest_odds_ratio_hazard <-
  log_hazard_from_odds_ratio(
    odds_ratio = nest_odds_ratio_effects$test_stat_value,
    lower_cl = nest_odds_ratio_effects$global_lcl,
    upper_cl = nest_odds_ratio_effects$global_ucl,
    sign = nest_odds_ratio_effects$sign,
    link = nest_odds_ratio_effects$link,
    baseline_survival = nest_odds_ratio_effects$baseline_survival
  )

test_statistic_effects <-
  test_statistic_effects %>%
  rows_update(
    nest_odds_ratio_effects %>%
      mutate(
        yi = nest_odds_ratio_hazard$yi,
        sei = nest_odds_ratio_hazard$sei,
        conversion = "log hazard ratio from odds ratio",
        effect_metric = "log_hazard_ratio"
      ),
    by = "row_id"
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
    effect_id,
    key,
    paper,
    region,
    source_sheet,
    bmp,
    treatment,
    control,
    response_var,
    species,
    species_key,
    species_group,
    analysis_class,
    response_metric,
    response_scale,
    survival_scale,
    link,
    baseline_survival,
    beta_is_derived,
    notes,
    species_include,
    treatment_control_flag,
    response_flag,
    sign,
    conversion,
    effect_metric,
    xbar_e,
    xbar_c,
    yi,
    sei,
    n_total
  ) %>%
  mutate(

    # Nest-survival records reported as a coefficient or a test statistic
    # have no route to the hazard scale, and a Hedges' g cannot be pooled
    # with a log hazard ratio, so their converted values are withdrawn here
    # and 1b_screen_effects.R records the reason.

    across(
      c(yi, sei),
      \(.value) {
        value_when(
          !(response_metric == "nest_success" &
              effect_metric == "hedges_g"),
          .value
        )
      }
    ),

    # `sign` was assessed against the papers during extraction, and is the
    # only source of the direction a benefit runs in -- except on the hazard
    # scale, where the conversion has already resolved it.

    yi =
      if_else(
        effect_metric == "log_hazard_ratio",
        yi,
        yi * sign
      )
  )

# write --------------------------------------------------------------------

all_effects %>%
  bmp_write_table("converted_effects")
