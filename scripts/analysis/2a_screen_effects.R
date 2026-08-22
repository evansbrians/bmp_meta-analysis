# This script:
# - Reads the converted table written by 1_effect_sizes.R
# - Derives the guild, fire and pool columns the models group on
# - Applies the exclusion screen once, in one place
# - Holds out the cells resting on fewer than three papers
# - Writes the analysis pool to db_mirror and every audit to output/audits

# The screen used to be written twice, here and in 0_prep_data.R, and the two
# copies drifted. This is the only copy.

# setup --------------------------------------------------------------------

library(tidyverse)

source("scripts/functions.R")

fs::dir_create(
  c(
    "data/db_mirror",
    "output/audits"
  )
)

# screen the table ---------------------------------------------------------

# One line per reason, each the negation of a test the record has to pass,
# and a missing value fails that test. Comment one out to run without it.

screened_effects <-
  read_converted_effects() %>%
  mutate(

    # A richness value is a property of an assemblage, whatever label that
    # assemblage carries, so every richness record is community level.

    label_type =
      if_else(
        species_key == "all_species" |
          response_metric == "species_richness",
        "community",
        "species"
      ),

    # Only the two grassland classes name a guild, so a species outside them
    # is species level without one and reaches the pooled model alone.

    guild =
      if_else(
        label_type == "species" &
          analysis_class %in% c("obligate", "facultative"),
        str_c(analysis_class, "_grassland"),
        NA_character_
      ),

    # A treatment burned in the year of the count, or long enough ago that
    # the response has recovered, was held out of the original analysis.

    # The digit is anchored so a two-digit interval cannot match it:
    # "16 years" is not "6 years".

    fire_excluded_original =
      bmp == "prescribed_fire" &
      str_detect(
        replace_na(treatment, ""),
        str_c(
          "year of burn",
          "burned that year",
          "current year",
          "<1 growing season",
          "\\b[6-9] years",
          sep = "|"
        )
      ),

    # A shrubland, woodland or forest species responds to a grassland
    # practice in its own right, so the primary analysis is without them.

    non_grassland_class =
      analysis_class %in% c("shrub", "woodland", "forest"),

    # Every fire interval is retained. Add `!fire_excluded_original` here to
    # apply the original post-fire rule as well.

    in_primary_pool = !non_grassland_class,
    excluded_by =
      case_when(

        # An artificial nest measures predation pressure at a place, not a
        # species' response to the practice, so it is out of scope by design.

        species_group == "artificial_nest" ~ "artificial_nest",

        # A label the frame gives no class at all -- a name the species table
        # has yet to reach -- supports no species-level estimate.

        label_type != "community" &
          is.na(analysis_class) ~ "unclassified_species",
        !replace_na(treatment_control_flag != "oranges", FALSE) ~
          "treatment_control_flag",
        !replace_na(response_flag != 1, FALSE) ~ "response_flag",
        is.na(sign) ~ "sign",
        !response_metric %in%
          c("species_richness", "nest_success", "abundance") ~
          "response_metric",

        # A diversity index weights a count by evenness, so it does not pool
        # with species richness.

        response_scale == "diversity" ~ "diversity_index",

        # Contrast mismatch over a mixed-guild assemblage, held out by a
        # recorded team decision.

        key == "pytisvj6" ~ "study_decision",

        # A nest-survival record that could not reach the hazard scale: no
        # arm probabilities, an unusable error, or a survival outside (0, 1).

        response_metric == "nest_success" &
          !replace_na(is.finite(yi) & is.finite(sei) & sei > 0, FALSE) ~
          "nest_hazard_scale",
        !replace_na(is.finite(yi) & is.finite(sei) & sei > 0, FALSE) ~
          "conversion"
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
      sei == min(
        sei[preference_rank == min(preference_rank)]
        ),
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

# the paper cutoff ---------------------------------------------------------

# A cell resting on fewer than three papers is out of scope, not merely
# unmodelled: the guild cell needs three of its own, and the practice and
# response need three across the guilds.

# Counted over the records that passed everything above and reach the primary
# pool, since that is the pool the primary analysis fits. Records outside it
# keep their own reason and stay available to the sensitivity specifications.

below_floor_effects <-
  screened_effects %>%
  filter(
    is.na(excluded_by),
    in_primary_pool
  ) %>%
  below_paper_floor()

screened_effects <-
  screened_effects %>%
  mutate(
    excluded_by =
      if_else(
        es_id %in% below_floor_effects,
        "paper_count",
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
  bmp_write_table("effect_sizes")

# audits -------------------------------------------------------------------

# Every converted record with the screening columns beside it, which the
# ROSES flow counts its stages from.

screened_effects %>%
  write_output_table(
    file_name = "screened_effects.csv",
    directory = "output/audits"
  )

# Everything held out, whole, with the reason and the values behind it.

screened_effects %>%
  filter(!is.na(excluded_by)) %>%
  arrange(
    excluded_by,
    source_sheet,
    key
  ) %>%
  write_output_table(
    file_name = "excluded_effects.csv",
    directory = "output/audits"
  )

# Every nest-survival record, its input scale, and whether it reached the
# hazard scale.

screened_effects %>%
  filter(response_metric == "nest_success") %>%
  select(
    es_id,
    key,
    paper,
    bmp,
    response_var,
    survival_scale,
    sign,
    xbar_e,
    xbar_c,
    effect_metric,
    yi,
    sei,
    excluded_by
  ) %>%
  write_output_table(
    file_name = "nest_hazard_conversion.csv",
    directory = "output/audits"
  )

# Every fire treatment the original post-fire rule holds out, so the interval
# the pattern matched can be read off the treatment text.

screened_effects %>%
  filter(fire_excluded_original) %>%
  distinct(
    key,
    treatment,
    control,
    response_metric,
    excluded_by
  ) %>%
  arrange(treatment) %>%
  write_output_table(
    file_name = "fire_excluded_treatments.csv",
    directory = "output/audits"
  )
