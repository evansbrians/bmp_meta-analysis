# This script:
# - Reads the fits, their pools and the cell table written by 2_models.R
# - Turns them into the results tables and the manuscript tables

# setup --------------------------------------------------------------------

library(brms)
library(posterior)
library(tidyverse)

source("scripts/functions.R")

fs::dir_create("output/tables")

fitted_models <-
  read_rds("output/models/fitted_models.rds")

model_pools <-
  read_rds("output/models/model_data.rds")

cell_sample_sizes <-
  "output/audits/cell_sample_sizes.csv" %>%
  read_csv(show_col_types = FALSE)

# species richness by BMP --------------------------------------------------

table_species_richness <-
  fitted_models %>%
  pluck("richness_bmp") %>%
  summarise_cell_means(term_prefix = "bmp") %>%
  mutate(
    response_metric = "species_richness",
    guild = NA_character_,
    bmp = cell
  ) %>%
  select(
    response_metric,
    guild,
    bmp,
    estimate,
    lcl,
    ucl,
    prob_positive,
    excludes_zero
  ) %>%
  left_join(
    cell_sample_sizes %>%
      filter(response_metric == "species_richness") %>%
      select(
        bmp,
        k,
        n_studies,
        meets_primary_threshold
      ),
    by = "bmp"
  ) %>%
  arrange(
    desc(estimate)
  )

# abundance and nest success by guild --------------------------------------

# Named in reporting order.

guild_bmp_models <-
  c(
    nest_success = "nest_success_guild_bmp",
    abundance = "abundance_guild_bmp"
  )

# The pooled x BMP models share the guild x BMP term names; the pooled overall
# models carry an indicator instead of a cell factor and need their own reader.

pooled_bmp_models <-
  c(
    nest_success = "nest_success_pooled_bmp",
    abundance = "abundance_pooled_bmp"
  )

# A pooled model with one cell is not fitted, so the names come from what
# 2_models.R produced rather than from this list alone.

bmp_cell_models <-
  c(
    guild_bmp_models,
    pooled_bmp_models
  ) %>%
  keep(\(.model_name) {
    .model_name %in% names(fitted_models)
  })

# guild x BMP and pooled x BMP cell means ----------------------------------

table_bmp_cells <-
  bmp_cell_models %>%
  imap(
    \(.model_name, .metric) {
      fitted_models %>%
        pluck(.model_name) %>%
        summarise_cell_means(term_prefix = "guild_bmp") %>%
        separate_wider_delim(
          cell,
          delim = "__",
          names = c("guild", "bmp")
        ) %>%
        mutate(
          response_metric = .metric
        ) %>%
        select(
          response_metric,
          guild,
          bmp,
          estimate,
          lcl,
          ucl,
          prob_positive,
          excludes_zero
        )
    }
  ) %>%
  list_rbind() %>%
  left_join(
    cell_sample_sizes,
    by =
      c(
        "response_metric",
        "guild",
        "bmp"
      )
  ) %>%
  arrange(
    response_metric,
    guild,
    desc(estimate)
  ) %>%
  add_guild_scope()

table_guild_bmp <-
  table_bmp_cells %>%
  filter(guild_scope == "by guild") %>%
  select(!guild_scope)

table_pooled_bmp <-
  table_bmp_cells %>%
  filter(guild_scope == "pooled")

# guild contrasts ----------------------------------------------------------

table_guild_contrasts <-
  guild_bmp_models %>%
  imap(contrast_guilds_within_bmp) %>%
  list_rbind()

# heterogeneity ------------------------------------------------------------

table_heterogeneity <-
  c(
    "richness_bmp",
    "nest_success_guild_bmp",
    "abundance_guild_bmp",
    "abundance_pooled_bmp",
    "nest_success_pooled_bmp"
  ) %>%
  keep(\(.model_name) {
    .model_name %in% names(fitted_models)
  }) %>%
  set_names() %>%
  imap(
    \(.model_name, .label) {
      fit <-
        fitted_models %>%
        pluck(.model_name)
      summarise_heterogeneity(
        fit = fit,
        sampling_se = fit$data$sei
      ) %>%
        mutate(
          model = .label,
          .before = level
        )
    }
  ) %>%
  list_rbind() %>%
  mutate(
    level =
      level %>%
      case_match(
        "key" ~ "between studies",
        "es_id" ~ "within study (between effect sizes)",
        "species_key" ~ "between species",
        "bmp" ~ "between practices",
        .default = level
      )
  )

# species-level estimates --------------------------------------------------

table_species_abundance <-
  extract_species_estimates(
    model_name = "abundance_guild",
    metric = "abundance"
  ) %>%
  left_join(
    model_pools %>%
      pluck("abundance_guild") %>%
      summarise(
        k = n(),
        n_studies = n_distinct(key),
        .by = species_key
      ) %>%
      mutate(
        species_key = as.character(species_key)
      ),
    by = "species_key"
  ) %>%
  arrange(
    guild,
    desc(estimate)
  )

# write tables -------------------------------------------------------------

list(
  table_species_richness_by_bmp = table_species_richness,
  table_guild_bmp = table_guild_bmp,
  table_pooled_bmp = table_pooled_bmp,
  table_guild_contrasts_by_bmp = table_guild_contrasts,
  table_heterogeneity = table_heterogeneity,
  table_species_abundance = table_species_abundance
) %>%
  iwalk(
    \(.table, .name) {
      .table %>%
        write_output_table(
          file_name = str_c(.name, ".csv")
        )
    }
  )

# formatted manuscript tables ----------------------------------------------

# Column blocks and caption text shared by every manuscript table.

sample_size_columns <-
  c(
    K = "k",
    Studies = "n_studies"
  )

posterior_columns <-
  c(
    "estimate",
    "lcl",
    "ucl",
    "prob_positive",
    "excludes_zero"
  )

bold_note <- "Bold indicates a 95% credible interval that excludes zero."

richness_flextable <-
  table_species_richness %>%
  mutate(
    BMP = format_bmp(bmp),
    .keep = "unused"
  ) %>%
  select(
    BMP,
    all_of(sample_size_columns),
    all_of(posterior_columns)
  ) %>%
  format_manuscript_table(
    caption =
      str_c(
        "Table 1. Pooled effect of each best management practice on species ",
        "richness, from a Bayesian multilevel meta-analysis. Effect sizes are ",
        "Hedges' g; positive values indicate a better conservation outcome ",
        "under the practice. K is the number of effect sizes and Studies the ",
        "number of independent studies contributing to each estimate. ",
        bold_note
      )
  )

guild_bmp_flextable <-
  table_bmp_cells %>%
  arrange(
    response_metric,
    guild_scope,
    guild,
    desc(estimate)
  ) %>%
  mutate(
    Response = format_response(response_metric),
    Guild = format_guild(guild),
    BMP = format_bmp(bmp),
    .keep = "unused"
  ) %>%
  select(
    Response,
    Guild,
    BMP,
    all_of(sample_size_columns),
    all_of(posterior_columns)
  ) %>%
  format_manuscript_table(
    caption =
      str_c(
        "Table 2. Effect of each best management practice on abundance and ",
        "nest success, estimated separately for obligate and facultative ",
        "grassland birds. Each is additionally estimated with the guilds ",
        "pooled, for the practices that clear the inclusion thresholds in ",
        "both guilds; those rows are a separate model over the same ",
        "species-level effect sizes, not a sum of the two guild rows, and ",
        "contain no assemblage-level record. Shrubland, woodland and forest ",
        "species are not analysed. Abundance effect ",
        "sizes are ",
        "Hedges' g and nest-survival effect sizes are log hazard ratios, so ",
        "the two metrics are not on a common scale; positive values indicate ",
        "a better conservation outcome under the practice in both cases. ",
        bold_note
      )
  )

contrasts_flextable <-
  table_guild_contrasts %>%
  mutate(
    Response = format_response(response_metric),
    BMP = format_bmp(bmp),
    prob_positive = prob_a_greater
  ) %>%
  select(
    Response,
    BMP,
    all_of(posterior_columns)
  ) %>%
  format_manuscript_table(
    caption =
      str_c(
        "Table 3. Direct posterior contrasts between obligate and facultative ",
        "grassland birds within each practice, on the scale of the metric ",
        "concerned: Hedges' g for abundance, log hazard ratio for nest ",
        "survival. A positive contrast means the practice benefits obligate ",
        "species more than facultative species; the final column is the ",
        "posterior probability of that. These are joint posterior ",
        "comparisons, not inferences from whether two intervals overlap. ",
        bold_note
      )
  ) %>%
  flextable::set_header_labels(
    `Effect size (95% CrI)` = "Obligate minus facultative (95% CrI)",
    `P(effect > 0)` = "P(obligate > facultative)"
  )

flextable::save_as_docx(
  `Species richness` = richness_flextable,
  `Abundance and nest success by guild and practice` = guild_bmp_flextable,
  `Guild contrasts` = contrasts_flextable,
  path = "output/tables/reanalysis_tables.docx"
)
