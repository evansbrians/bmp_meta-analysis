# This script:
# - Reads the converted table written by 1_effect_sizes.R
# - Derives the guild, fire and pool columns the models group on
# - Applies the exclusion screen once, in one place
# - Holds out the cells resting on fewer than three papers, less those the
#   pooled model still carries
# - Writes the analysis pool to db_mirror and every audit to output/audits

# setup --------------------------------------------------------------------

library(tidyverse)

# Project functions:

source("src/functions.R")

# Output directories:

fs::dir_create(
  c(
    "data/db_mirror",
    "output/audits"
  )
)

# screen the table ---------------------------------------------------------

# One line per exclusion reason:

screened_effects <-
  read_converted_effects() %>%
  mutate(

    # Richness is always community level:

    label_type =
      if_else(
        species_key == "all_species" |
          response_metric == "species_richness",
        "community",
        "species"
      ),

    # Only grassland classes name a guild:

    guild =
      if_else(
        label_type == "species" &
          analysis_class %in% c("obligate", "facultative"),
        str_c(analysis_class, "_grassland"),
        NA_character_
      ),

    # Flag the post-fire treatments:

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

    # Flag the non-grassland classes:

    non_grassland_class =
      analysis_class %in% c("shrub", "woodland", "forest"),

    # Every fire interval is retained:

    in_primary_pool = !non_grassland_class,
    excluded_by =
      case_when(

        # Artificial nests are out of scope:

        species_group == "artificial_nest" ~ "artificial_nest",

        # Unclassified labels:

        label_type != "community" &
          is.na(analysis_class) ~ "unclassified_species",
        !replace_na(treatment_control_flag != "oranges", FALSE) ~
          "treatment_control_flag",
        !replace_na(response_flag != 1, FALSE) ~ "response_flag",
        is.na(sign) ~ "sign",
        !response_metric %in%
          c("species_richness", "nest_success", "abundance") ~
          "response_metric",

        # A diversity index is not richness:

        response_scale == "diversity" ~ "diversity_index",

        # Held out by team decision:

        key == "pytisvj6" ~ "study_decision",

        # Nest records off the hazard scale:

        response_metric == "nest_success" &
          !replace_na(
            is.finite(yi) &
              is.finite(sei) &
              sei > 0,
            FALSE
          ) ~
          "nest_hazard_scale",
        !replace_na(
          is.finite(yi) &
            is.finite(sei) &
            sei > 0,
          FALSE
        ) ~
          "conversion"
      )
  )

# Preferred expression of each response:

response_expression_preference <-
  read_csv(
    "src/response_expression_preference.csv",
    show_col_types = FALSE
  )

# Keep one expression per result:

duplicate_expressions <-
  screened_effects %>%

  # Resolved among the records that passed:

  filter(
    is.na(excluded_by)
  ) %>%
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
        \(.metric, .expression) {
          rank_response_expression(
            .response_metric = .metric,
            .response_var = .expression,
            .preferences = response_expression_preference
          )
        }
      )
  ) %>%

  # Best rank, then smallest standard error:

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

# Hold out the rest:

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

# Papers behind each cell, against the cutoff:

paper_floor <-
  screened_effects %>%
  filter(
    is.na(excluded_by),
    in_primary_pool
  ) %>%
  paper_floor_status() %>%
  select(
    es_id,
    floor_status
  )

# Hold out the cells below the cutoff:

screened_effects <-
  screened_effects %>%
  left_join(
    paper_floor,
    by = "es_id"
  ) %>%
  mutate(
    excluded_by =
      if_else(
        replace_na(floor_status == "excluded", FALSE),
        "paper_count",
        excluded_by
      ),
    pooled_only = replace_na(floor_status == "pooled_only", FALSE),
    .keep = "unused"
  )

# write --------------------------------------------------------------------

# The pool, without the screening columns:

screened_effects %>%
  filter(
    is.na(excluded_by)
  ) %>%
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

# Every record, with its screening columns:

screened_effects %>%
  write_output_table(
    file_name = "screened_effects.csv",
    directory = "output/audits"
  )

# Everything held out, with its reason:

screened_effects %>%
  filter(
    !is.na(excluded_by)
  ) %>%
  arrange(
    excluded_by,
    source_sheet,
    key
  ) %>%
  write_output_table(
    file_name = "excluded_effects.csv",
    directory = "output/audits"
  )

# Nest records and their input scale:

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

# Fire treatments the original rule held out:

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

# clear the environment ----------------------------------------------------

rm(
  list = ls()
)
