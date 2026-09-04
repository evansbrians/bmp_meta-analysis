# This script:
# - Reads the fits, their pools and the cell table written by 3_models.R
# - Turns them into the results tables and the manuscript tables

# setup --------------------------------------------------------------------

library(brms)
library(flextable)
library(officer)
library(posterior)
library(tidyverse)

# Project functions:

source("src/functions.R")

# Output directory:

fs::dir_create("output/tables")

# Fitted models:

fitted_models <-
  read_rds("output/models/fitted_models.rds")

# Pools they were fitted to:

model_pools <-
  read_rds("output/models/model_data.rds")

# Cell sample sizes:

cell_sample_sizes <-
  "output/audits/cell_sample_sizes.csv" %>%
  read_csv(show_col_types = FALSE)

# species richness by BMP --------------------------------------------------

table_species_richness <-
  fitted_models %>%
  
  # Cell means:
  
  pluck("richness_bmp") %>%
  summarize_cell_means(term_prefix = "bmp") %>%
  
  # Richness belongs to no guild:
  
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
  
  # Add sample sizes:
  
  left_join(
    cell_sample_sizes %>%
      filter(response_metric == "species_richness") %>%
      select(
        bmp,
        k,
        n_studies,
        meets_primary_threshold
      ),
    by = join_by(bmp)
  ) %>%
  
  # Strongest effect first:
  
  arrange(
    desc(estimate)
  )

# abundance and nest success by guild --------------------------------------

# Guild x BMP models:

guild_bmp_models <-
  c(
    nest_success = "nest_success_guild_bmp",
    abundance = "abundance_guild_bmp"
  )

# Pooled x BMP models:

pooled_bmp_models <-
  c(
    nest_success = "nest_success_pooled_bmp",
    abundance = "abundance_pooled_bmp"
  )

# Only the models that were fitted:

bmp_cell_models <-
  c(
    guild_bmp_models,
    pooled_bmp_models
  ) %>%
  keep(
    \(.model_name) {
      .model_name %in% names(fitted_models)
    }
  )

# guild x BMP and pooled x BMP cell means ----------------------------------

table_bmp_cells <-
  bmp_cell_models %>%
  
  # Cell means per model:
  
  imap(
    \(.model_name, .metric) {
      fitted_models %>%
        pluck(.model_name) %>%
        summarize_cell_means(term_prefix = "guild_bmp") %>%
        
        # Split the cell name:
        
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
  
  # Add sample sizes:
  
  left_join(
    cell_sample_sizes,
    by =
      join_by(
        response_metric,
        guild,
        bmp
      )
  ) %>%
  
  # Strongest effect first:
  
  arrange(
    response_metric,
    guild,
    desc(estimate)
  ) %>%
  
  # Mark guild rows and pooled rows:
  
  mutate(
    guild_scope =
      if_else(
        guild == "all_grassland",
        "pooled",
        "by guild"
      ),
    .after = guild
  )

# By-guild rows:

table_guild_bmp <-
  table_bmp_cells %>%
  filter(guild_scope == "by guild") %>%
  select(!guild_scope)

# Pooled rows:

table_pooled_bmp <-
  table_bmp_cells %>%
  filter(guild_scope == "pooled")

# guild contrasts ----------------------------------------------------------

table_guild_contrasts <-
  guild_bmp_models %>%
  
  # Obligate minus facultative:
  
  imap(
    \(.model_name, .metric) {
      
      # Model draws:
      
      draws <-
        fitted_models %>%
        pluck(.model_name) %>%
        draws_tibble()
      
      # Practices fitted in both guilds:
      
      shared_bmps <-
        draws %>%
        names() %>%
        keep(
          \(.term) {
            str_starts(.term, "b_guild_bmp")
          }
        ) %>%
        str_remove("^b_guild_bmp") %>%
        tibble(cell = .) %>%
        separate_wider_delim(
          cell,
          delim = "__",
          names = c("guild", "bmp")
        ) %>%
        filter(
          n_distinct(guild) == 2,
          .by = bmp
        ) %>%
        distinct(bmp) %>%
        pull(bmp)
      
      # Difference of the two guild columns:
      
      shared_bmps %>%
        map(
          \(.bmp) {
            obligate_column <-
              str_c("b_guild_bmpobligate_grassland__", .bmp)
            
            facultative_column <-
              str_c("b_guild_bmpfacultative_grassland__", .bmp)
            
            (draws[[obligate_column]] - draws[[facultative_column]]) %>%
              summarize_draws_vector() %>%
              rename(
                prob_a_greater = prob_positive
              ) %>%
              mutate(
                response_metric = .metric,
                bmp = .bmp,
                contrast = "obligate minus facultative",
                cell_a = str_c("obligate_grassland__", .bmp),
                cell_b = str_c("facultative_grassland__", .bmp),
                .before = estimate
              )
          }
        ) %>%
        list_rbind()
    }
  ) %>%
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
  
  # Only the models that were fitted:
  
  keep(
    \(.model_name) {
      .model_name %in% names(fitted_models)
    }
  ) %>%
  set_names() %>%
  
  # Variance shares by level:
  
  imap(
    \(.model_name, .label) {
      
      # The fit:
      
      fit <-
        fitted_models %>%
        pluck(.model_name)
      
      # Typical sampling variance:
      
      sampling_weights <- 1 / fit$data$sei^2
      
      typical_variance <-
        (length(sampling_weights) - 1) * sum(sampling_weights) /
        (sum(sampling_weights)^2 - sum(sampling_weights^2))
      
      # Variance draws per level:
      
      draws <- draws_tibble(fit)
      
      variance_draws <-
        draws %>%
        names() %>%
        keep(
          \(.term) {
            str_starts(.term, "sd_")
          }
        ) %>%
        set_names() %>%
        map(
          \(.term) {
            draws[[.term]]^2
          }
        )
      
      # Total variance:
      
      total_variance <-
        variance_draws %>%
        reduce(`+`) +
        typical_variance
      
      # Tau and its share of the total:
      
      variance_draws %>%
        imap(
          \(.variance, .level) {
            bind_cols(
              draws %>%
                pull(.level) %>%
                summarize_draws_vector() %>%
                select(
                  tau = estimate,
                  tau_lcl = lcl,
                  tau_ucl = ucl
                ),
              (.variance / total_variance * 100) %>%
                summarize_draws_vector() %>%
                select(
                  i2 = estimate,
                  i2_lcl = lcl,
                  i2_ucl = ucl
                )
            )
          }
        ) %>%
        list_rbind(names_to = "level") %>%
        mutate(
          level =
            level %>%
            str_remove("^sd_") %>%
            str_remove("__Intercept$"),
          typical_sampling_variance = typical_variance,
          .before = tau
        ) %>%
        mutate(
          model = .label,
          .before = level
        )
    }
  ) %>%
  list_rbind() %>%
  
  # Name the levels:
  
  mutate(
    level =
      level %>%
      case_match(
        "key" ~ "between studies",
        "effect_id" ~ "within study (between effect sizes)",
        "species_key" ~ "between species",
        "bmp" ~ "between practices",
        .default = level
      )
  )

# species-level estimates --------------------------------------------------

# Species-level model:

species_fit <-
  fitted_models %>%
  pluck("abundance_guild")

# Its draws:

species_draws <- draws_tibble(species_fit)

# Species guilds:

species_guilds <-
  species_fit$data %>%
  as_tibble() %>%
  distinct(species_key, guild) %>%
  mutate(
    across(
      everything(),
      as.character
    )
  )

# Guild mean plus species offset:

table_species_abundance <-
  species_draws %>%
  names() %>%
  
  # Offset columns:
  
  keep(
    \(.term) {
      str_starts(.term, "r_species_key\\[")
    }
  ) %>%
  set_names() %>%
  
  # One row per species:
  
  map(
    \(.column) {
      species_name <-
        .column %>%
        str_extract("(?<=\\[).+(?=,)")
      
      species_row <-
        species_guilds %>%
        filter(species_key == species_name)
      
      # Skip unplaceable columns:
      
      if (nrow(species_row) != 1) {
        return(NULL)
      }
      
      guild_column <- str_c("b_guild", species_row$guild)
      
      (species_draws[[guild_column]] + species_draws[[.column]]) %>%
        summarize_draws_vector() %>%
        mutate(
          species_key = species_name,
          guild = species_row$guild,
          .before = estimate
        )
    }
  ) %>%
  list_rbind() %>%
  mutate(
    response_metric = "abundance",
    .before = species_key
  ) %>%
  
  # Add sample sizes:
  
  left_join(
    model_pools %>%
      pluck("abundance_guild") %>%
      summarize(
        k = n(),
        n_studies = n_distinct(key),
        .by = species_key
      ) %>%
      mutate(
        species_key = as.character(species_key)
      ),
    by = join_by(species_key)
  ) %>%
  
  # Strongest effect first:
  
  arrange(
    guild,
    desc(estimate)
  )

# write tables -------------------------------------------------------------

# Write one csv per table:

list(
  table_species_richness_by_bmp = table_species_richness,
  table_bmp_cells = table_bmp_cells,
  table_guild_bmp = table_guild_bmp,
  table_pooled_bmp = table_pooled_bmp,
  table_guild_contrasts = table_guild_contrasts,
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
