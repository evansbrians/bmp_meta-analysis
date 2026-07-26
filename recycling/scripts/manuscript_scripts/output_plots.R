# Script for generating output plots for the manuscript:

# Created: 2025-10-27
# Last modified: 2025-10-27

# Based results from scripts/manuscript_scripts/analysis.R

# setup -------------------------------------------------------------------

library(tidyverse)

source("scripts/functions.R")

# effects plots by bmp ----------------------------------------------------

# Count papers:

paper_count_effect_size_by_bmp <- 
  effect_sizes_categorical %>%
  summarize(
    n_studies = length_unique(key),
    .by = c(response_var, bmp)
  ) %>% 
  arrange(response_var)

# Format data for plotting:

plot_data_by_bmp <- 
  bmp_models %>% 
  names() %>% 
  map_df(
    \(.response_metric) {
      model_subset <-
        bmp_models %>% 
        pluck(.response_metric) %>% 
        make_effects_table() %>% 
        mutate(
          response_metric = .response_metric,
          .before = bmp
        ) %>% 
        left_join(
          paper_count_effect_size_by_bmp,
          by = 
            join_by(
              response_metric == response_var,
              bmp == bmp
            )
        ) %>% 
        relocate(
          n_studies,
          .before = n_estimates
        ) %>% 
        select(
          response_metric:effect_size,
          lcl:ucl
        ) %>% 
        mutate(
          bmp = fix_bmp(bmp),
          response_metric = fix_response(response_metric)
        )
    }
  )

# Plot the data:

plots_by_bmp <- 
  plot_data_by_bmp %>% 
  pull(response_metric) %>% 
  unique() %>% 
  set_names() %>% 
  map(
    \(.response_metric) {
      plot_data_by_bmp %>% 
        filter(response_metric == .response_metric) %>% 
        mutate(
          bmp = fct_reorder(bmp, effect_size)
        ) %>% 
        ggplot() +
        aes(
          x = effect_size,
          y = bmp,
          label = n_studies
        ) +
        geom_point(
          size = 1.5
        ) +
        geom_segment(
          aes(
            x = lcl,
            xend = ucl
          ),
          lineend = "round",
          linewidth = 0.60
        ) +
        geom_vline(
          xintercept = 0,
          linetype = "dashed",
          linewidth = 0.25
        ) +
        geom_text(
          aes(x = ucl),
          nudge_x = 
            if(.response_metric == "Species Richness") {
              .1
            } else {
              .05
            },
          size = 3
        ) +
        labs(
          title = 
            str_c("Effects of Best Management Practices on ", .response_metric),
          x = "Pooled Standardized Effect Size",
          y = "Best Management Practice"
        ) +
        my_manuscript_theme()
    }
  )

# Write to file:

plots_by_bmp %>% 
  names() %>% 
  map(
    \(.response_var) {
      ggsave(
        filename = 
          str_c(
            "manuscript_plots/",
            .response_var %>% 
              str_to_lower() %>% 
              str_replace(" ", "_") %>% 
              str_c("_by_bmp"),
            ".png"
          ),
        plot = plots_by_bmp[[.response_var]],
        height = 5,
        width = 7,
        units = "in"
      )
    }
  )

# effects plots by bmp and species class ----------------------------------

# Count papers:

paper_count_effect_size_by_bmp_spp_class <- 
  effect_sizes_species_class %>%
  summarize(
    n_studies = length_unique(key),
    .by = 
      c(
        response_var, 
        bmp, 
        species_class
      )
  ) %>% 
  arrange(
    response_var,
    bmp,
    species_class
  )

# Format data for plotting:

plot_data_by_bmp_spp_class <- 
  model_output_species_class %>% 
  names() %>% 
  map_df(
    \(.response_metric) {
      model_subset_response <-
        model_output_species_class %>% 
        pluck(.response_metric)
      
      model_subset_response %>% 
        names() %>% 
        map_df(
          \(.bmp) {
            model_subset_response %>% 
              pluck(.bmp) %>% 
              pluck("effects_table") %>% 
              mutate(
                bmp = .bmp,
                .before = species_class
              )
          }
        ) %>% 
        mutate(
          response_metric = .response_metric,
          .before = bmp
        ) %>% 
        left_join(
          paper_count_effect_size_by_bmp_spp_class,
          by = 
            join_by(
              response_metric == response_var,
              bmp == bmp,
              species_class == species_class
            )
        ) %>% 
        relocate(
          n_studies,
          .before = n_estimates
        ) %>% 
        select(
          response_metric:effect_size,
          lcl:ucl
        ) %>% 
        mutate(
          bmp = fix_bmp(bmp),
          response_metric = fix_response(response_metric),
          species_class = 
            species_class %>% 
            str_replace("_", " ") %>% 
            str_to_title()
        )
    }
  )

# Plot the data:

abundance_plot_by_bmp_spp_class <- 
  plot_data_by_bmp_spp_class %>% 
  filter(response_metric == "Abundance") %>% 
  mutate(
    bmp = fct_rev(bmp),
    species_class = 
      species_class %>% 
      fct_relevel(
        "Obligate Grassland",
        "Facultative Grassland",
        "Shrub"
      )
  ) %>% 
  ggplot() +
  aes(
    x = effect_size,
    y = bmp,
    label = n_studies
  ) +
  geom_point(
    size = 1.5
  ) +
  geom_segment(
    aes(
      x = lcl,
      xend = ucl
    ),
    lineend = "round",
    linewidth = 0.60
  ) +
  geom_vline(
    xintercept = 0,
    linetype = "dashed",
    linewidth = 0.25
  ) +
  geom_text(
    aes(x = ucl),
    nudge_x = .05,
    size = 3
  ) +
  facet_wrap(
    ~ species_class,
    scales = "free_x"
  ) +
  labs(
    title = "Effects of Best Management Practices on Abundance by Habitat Guild",
    x = "Pooled Standardized Effect Size",
    y = "Best Management Practice"
  ) +
  my_theme_today() +
  my_manuscript_theme()

# Write to file:

ggsave(
  filename = "manuscript_plots/abundance_plot_by_bmp_spp_class.png",
  plot = abundance_plot_by_bmp_spp_class,
  height = 4.5,
  width = 10,
  units = "in"
)

# effects plots by bmp and species ----------------------------------------

# Count papers:

paper_count_effect_size_by_bmp_spp <- 
  effect_sizes_species %>%
  summarize(
    n_studies = length_unique(key),
    .by = 
      c(
        response_var, 
        bmp, 
        species
      )
  ) %>% 
  arrange(
    response_var,
    bmp,
    species
  )

# Format data for plotting:

plot_data_by_bmp_spp <- 
  model_output_species %>% 
  pluck("abundance") %>%
  names() %>% 
  map_df(
    \(.bmp) {
      model_output_species %>% 
        pluck("abundance") %>% 
        pluck(.bmp) %>% 
        pluck("effects_table") %>% 
        mutate(
          bmp = .bmp,
          .before = species_class
        ) %>% 
        rename(species = species_class)
    }
  ) %>% 
  left_join(
    paper_count_effect_size_by_bmp_spp,
    by = 
      join_by(
        bmp == bmp,
        species == species
      )
  ) %>% 
  relocate(
    n_studies,
    .before = n_estimates
  ) %>% 
  select(
    bmp:effect_size,
    lcl:ucl
  ) %>% 
  mutate(
    bmp = fix_bmp(bmp),
    response_metric = fix_response(response_metric),
    species = fix_species(species)
  )

# Plot the data:

abundance_plot_by_bmp_spp <- 
  plot_data_by_bmp_spp %>% 
  filter(
    !str_detect(species, "^Field|chat$|flicker")
  ) %>% 
  mutate(
    species = fct_rev(species)
  ) %>% 
  ggplot() +
  aes(
    x = effect_size,
    y = species,
    label = n_studies
  ) +
  geom_point(
    size = 1.5
  ) +
  geom_segment(
    aes(
      x = lcl,
      xend = ucl
    ),
    lineend = "round",
    linewidth = 0.60
  ) +
  geom_vline(
    xintercept = 0,
    linetype = "dashed",
    linewidth = 0.25
  ) +
  geom_text(
    aes(x = ucl),
    nudge_x = .05,
    size = 3
  ) +
  facet_wrap(
    ~ bmp,
    scales = "free_x"
  ) +
  labs(
    title = "Effects of Best Management Practices on Abundance by Species",
    x = "Pooled Standardized Effect Size",
    y = "Species"
  ) +
  my_theme_today() +
  my_manuscript_theme()

# Write to file:

ggsave(
  filename = "manuscript_plots/abundance_plot_by_bmp_spp.png",
  plot = abundance_plot_by_bmp_spp,
  height = 8,
  width = 8,
  units = "in"
)

