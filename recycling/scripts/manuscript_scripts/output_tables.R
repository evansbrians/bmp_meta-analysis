# Script for generating output tables for the manuscript:

# Created: 2025-10-27
# Last modified: 2025-10-28

# Based results from scripts/manuscript_scripts/analysis.R

# setup -------------------------------------------------------------------

library(flextable)
library(tidyverse)

source("scripts/functions.R")

# Effect size table:

effect_sizes_categorical <-
  file.path(
    "manuscript",
    "manuscript_processed_data",
    "categorical_effect_sizes_fire_and_n_papers_subset.rds"
  ) %>% 
  read_rds()

# BMP models across species classes and species:

list.files(
  "manuscript/manuscript_processed_data/model_output",
  full.names = TRUE
) %>% 
  set_names(
    str_extract(., "[^/]*rds") %>% 
      str_remove(".rds")
  ) %>% 
  map(
    ~ read_rds(.x)
  ) %>% 
  list2env(.GlobalEnv)

# Lit review table from Google Sheets:

papers <- 
  file.path(
    "https://docs.google.com/spreadsheets/d",
    "1Lf3v8fU0sCCAcJ6Wj1v8GwgogjI4ve3xcMlPzLW0hnU"
  ) %>% 
  googlesheets4::read_sheet()

# paper counts table ------------------------------------------------------

# Papers (used and unused) from Google Sheets:

reviewed_papers <- 
  papers %>% 
  filter(article_type == "research") %>% 
  select(
    key, 
    multiple_bmps, 
    response
  ) %>% 
  separate_longer_delim(
    multiple_bmps,
    delim = ";"
  ) %>% 
  separate_longer_delim(
    response,
    delim = ";"
  ) %>% 
  mutate(
    across(
      multiple_bmps:response,
      ~ str_trim(.x)
    ),
    bmp = 
      multiple_bmps %>% 
      str_replace_all("_", " ") %>% 
      fix_bmp() %>% 
      str_replace("Summer Pasture Stockpiling", "Rotational Grazing") %>% 
      str_replace("Bufferss", "Buffers") %>% 
      str_replace("Nwsg", "NWSG"),
    response_var = 
      case_when(
        str_detect(response, "abundance|density|detections") ~ "Abundance",
        str_detect(response, "^fake|nest_(pred|succ)") ~ "Nest Success",
        str_detect(response, "species_rich") ~ "Species Richness",
        .default = "other"
      ),
    .keep = "unused"
  ) %>% 
  filter(
    !str_detect(bmp, "Cats|^No |Darksky|Unmown"),
    !(response_var == "Nest Success" & bmp == "Provide Overwintering Habitat"),
    response_var != "other"
  ) %>% 
  count(response_var, bmp)

analyzed_papers <- 
  effect_sizes_categorical %>% 
  summarize(
    n_papers = length_unique(key), 
    n_estimates = n(), 
    .by = c(response_var, bmp)
  ) %>% 
  arrange(response_var, bmp) %>% 
  mutate(
    bmp = fix_bmp(bmp),
    response_var = fix_response(response_var)
  ) 

reviewed_papers %>% 
  full_join(
    analyzed_papers,
    by = c("response_var", "bmp")
  ) %>% 
  mutate(
    across(
      n:n_estimates,
      ~ replace_na(.x, 0)
    )
  ) %>% 
  flextable::flextable() %>% 
  merge_v(
    j = "response_var"
  ) %>% 
  align(
    j = 
      c(
        "response_var",
        "n",
        "n_papers",
        "n_estimates"
      ),
    align = "center"
  ) %>% 
  labelizor(
    labels = 
      c(
        "response_var" = "Response metric",
        "bmp" = "Best Management Practice",
        "n" = "Studies retrieved",
        "n_papers" = "Studies included",
        "n_estimates" = "Estimates included"
      )
  ) %>% 
  hline() %>% 
  vline(j = "bmp") %>% 
  fontsize(size = 10) %>% 
  font(part = "all", fontname = "Times") %>% 
  autofit() %>% 
  save_as_image(
    path = "manuscript/manuscript_tables/paper_counts.png"
  )

# random effects models by bmp --------------------------------------------

# BMP is the subgroup

# Overall effects:

bmp_models %>% 
  names() %>% 
  map_df(
    \(.response_metric) {
      meta_out <-
        bmp_models %>% 
        pluck(.response_metric)
      
      tibble(
        response = .response_metric,
        n_studies = length_unique(meta_out$studlab),
        q = meta_out$Q.b.random,
        df = meta_out$df.Q,
        pval = meta_out$pval.Q.b.random
      )
    }
  )

# Effects by subgroup (BMP):

bmp_models %>% 
  names() %>% 
  map_df(
    \(.response_metric) {
      bmp_models %>% 
        pluck(.response_metric) %>% 
        make_effects_table() %>% 
        mutate(
          response_metric = .response_metric,
          .before = bmp
        )
    }
  ) %>% 
  
  # Indicate whether an effect is significant:
  
  mutate(
    bmp = 
      if_else(
        pval <= 0.05,
        str_c(bmp, "*"),
        bmp
      ) 
  ) %>% 
  filter(response_metric == "species_richness") %>% 
  select(bmp, effect_size, se, pval)

# Format output:

format_output_table() %>% 
  arrange(response_metric, bmp) %>% 
  clipr::write_clip()

# random effects models by bmp and species class --------------------------

# Species class is the subgroup, with separate models for each BMP

# Overall support for subgroup heterogeneity:

model_output_species_class %>% 
  names() %>% 
  map_df(
    \(.response_metric) {
      response_list <-
        model_output_species_class %>% 
        pluck(.response_metric)
      
      response_list %>% 
        names() %>% 
        map_df(
          \(.bmp) {
            meta_out <-
              response_list %>% 
              pluck(.bmp) %>% 
              pluck("subgroup_output")
            
            tibble(
              response = .response_metric,
              bmp = .bmp,
              n_studies = length_unique(meta_out$studlab),
              q = meta_out$Q.b.random,
              df = meta_out$df.Q,
              pval = meta_out$pval.Q.b.random
            )
          }
        )
    }
  )

# Output table for manuscript:

model_output_species_class %>% 
  names() %>% 
  map_df(
    \(.response_metric) {
      response_list <-
        model_output_species_class %>% 
        pluck(.response_metric)
      
      response_list %>% 
        names() %>% 
        map_df(
          \(.bmp) {
            response_list %>% 
              pluck(.bmp) %>% 
              pluck("effects_table") %>% 
              mutate(
                response_metric = .response_metric,
                bmp = .bmp,
                .before = species_class
              ) 
          }
        )
    }
  ) %>% 
  
  # Indicate whether an effect is significant:
  
  mutate(
    species_class = 
      if_else(
        pval <= 0.05,
        str_c(species_class, "*"),
        species_class
      ) %>% 
      str_to_title() %>% 
      str_replace("Facultative_", "F") %>% 
      str_replace("Obligate_", "O") %>% 
      str_replace("grassland", "G") %>% 
      str_replace("Shrub", "SH")
  ) %>% 
  
  # Format output:
  
  format_output_table() %>%
  arrange(
    response_metric,
    bmp, 
    species_class
  ) #%>% 

# Write for importing in into a spreadsheet:

clipr::write_clip()

# random effects models by bmp and species --------------------------------

# Species is the subgroup, with separate models run for each BMP

# Overall support for subgroup heterogeneity:

model_output_species %>% 
  names() %>% 
  map_df(
    \(.response_metric) {
      response_list <-
        model_output_species_class %>% 
        pluck(.response_metric)
      
      response_list %>% 
        names() %>% 
        map_df(
          \(.bmp) {
            meta_out <-
              response_list %>% 
              pluck(.bmp) %>% 
              pluck("subgroup_output")
            
            tibble(
              response = .response_metric,
              bmp = .bmp,
              n_studies = length_unique(meta_out$studlab),
              q = meta_out$Q.b.random,
              df = meta_out$df.Q,
              pval = meta_out$pval.Q.b.random
            )
          }
        )
    }
  )

# Output table for manuscript:

output_table_start <-
  model_output_species %>% 
  names() %>% 
  map_df(
    \(.response_metric) {
      response_list <-
        model_output_species %>% 
        pluck(.response_metric)
      
      response_list %>% 
        names() %>% 
        map_df(
          \(.bmp) {
            response_list %>% 
              pluck(.bmp) %>% 
              pluck("effects_table") %>% 
              mutate(
                response_metric = .response_metric,
                bmp = .bmp,
                .before = species_class
              ) 
          }
        )
    }
  ) %>% 
  
  # Indicate whether an effect is significant:
  
  mutate(
    species = 
      if_else(
        pval <= 0.05,
        str_c(species_class, "*"),
        species_class
      ) %>% 
      fix_species(),
    .after = bmp
  ) %>% 
  select(!species_class) %>% 
  
  # Format output:
  
  format_output_table() %>%
  arrange(
    response_metric,
    bmp, 
    species
  )

# Add scientific names:

output_table_start %>% 
  mutate(
    sci_name = 
      species %>% 
      str_remove("\\*") %>% 
      taxize::comm2sci(db = "itis")
  ) %>% 
  unnest(sci_name) %>% 
  filter(
    str_count(sci_name, " ") == 1
  ) %>% 
  mutate(
    sci_name =
      case_when(
        str_detect(species, "^America") ~ "Spinus tristis",
        .default = sci_name
      )
  ) %>% 
  distinct() %>% 
  clipr::write_clip()

# Add rite for importing in into a spreadsheet:

output_tables_with_sci_names %>%
  unnest(sci_name) %>% 
  filter(
    str_count(sci_name, " ") == 1
  ) %>% 
  mutate(
    sci_name =
      case_when(
        str_detect(species, "^America") ~ "Spinus tristis",
        .default = sci_name
      )
  ) %>% 
  distinct() %>% 
  clipr::write_clip()


