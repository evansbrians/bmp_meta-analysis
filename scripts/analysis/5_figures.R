# This script:
# - Reads the results tables written by 3_contrasts_tables.R
# - Builds the manuscript figures from them and the posterior draws

# setup --------------------------------------------------------------------

library(brms)
library(tidybayes)
library(tidyverse)

source("scripts/functions.R")

fs::dir_create("output/figures")

results <-
  c(
    species_richness = "table_species_richness_by_bmp",
    guild_bmp = "table_guild_bmp",
    guild_contrasts = "table_guild_contrasts_by_bmp",
    pooled_bmp = "table_pooled_bmp",
    heterogeneity = "table_heterogeneity",
    species_abundance = "table_species_abundance"
  ) %>%
  map(read_table_output)

# By-guild and pooled cell means, stacked for the panelled figures.

bmp_cells <-
  bind_rows(
    results$guild_bmp,
    results$pooled_bmp
  )

# shared figure elements ---------------------------------------------------

# Panels stack one per row throughout, so each panel keeps the full figure
# width; the heights below scale with the rows a panel carries.

guild_display_levels <-
  c(
    c("obligate_grassland", "facultative_grassland"),
    "all_grassland"
  ) %>%
  format_guild()

guild_colours <-
  c(
    "#1B5E3C",
    "#B07A2A",
    "#3B4A6B"
  ) %>%
  set_names(guild_display_levels)

effect_axis_label <- "Pooled effect size (Hedges' g, 95% credible interval)"

# Wrapped, because a one-line version overruns a stacked figure's width.

mixed_scale_axis_label <-
  str_c(
    "Effect size (Hedges' g for abundance and richness, log hazard ratio ",
    "for nest survival; 95% credible interval)"
  ) %>%
  wrap_label(width = 70)

filled_point_note <- "Filled points mark intervals excluding zero."

bmp_estimate_aes <-
  aes(
    x = estimate,
    y = bmp_label
  )

# A star marks a cell that meets only the reduced threshold.

sample_size_label <-
  aes(
    x = ucl,
    label =
      str_c(
        "k=",
        k,
        "; n=",
        n_studies,
        if_else(
          meets_primary_threshold,
          "",
          "*",
          missing = ""
        )
      )
  )

# figure 1: species richness -----------------------------------------------

figure_species_richness <-
  results$species_richness %>%
  add_bmp_label() %>%
  ggplot(mapping = bmp_estimate_aes) %>%
  add_forest_layers() +
  scale_x_continuous(
    expand =
      expansion(
        mult = c(0.05, 0.28)
      )
  ) +
  labs(
    title = "Species richness",
    subtitle =
      wrap_label(
        str_c(
          "Community-level metric; not partitioned by guild. ",
          filled_point_note
        )
      ),
    x = effect_axis_label,
    y = NULL
  ) +
  theme_bmp(base_size = 12)

figure_species_richness %>%
  write_output_figure(
    file_name = "figure_1_species_richness.png",
    width = 9,
    height = 5
  )

# figure 2: abundance by guild ---------------------------------------------

figure_abundance <-
  bmp_cells %>%
  build_guild_figure(
    metric = "abundance",
    plot_title = "Abundance, by vegetation-type guild and pooled"
  )

figure_abundance %>%
  write_output_figure(
    file_name = "figure_2_abundance_by_guild.png",
    width = 9,
    height = 11.5
  )

figure_nest_success <-
  bmp_cells %>%
  build_guild_figure(
    metric = "nest_success",
    plot_title = "Nest success, by vegetation-type guild and pooled"
  )

figure_nest_success %>%
  write_output_figure(
    file_name = "figure_3_nest_success_by_guild.png",
    width = 9,
    height = 7.5
  )

# figure 4: guild contrasts ------------------------------------------------

contrast_probability_label <-
  aes(
    x = ucl,
    label =
      str_c(
        "P=",
        format_number(prob_a_greater)
      )
  )

if (nrow(results$guild_contrasts) > 0) {
  figure_guild_contrasts <-
    results$guild_contrasts %>%
    mutate(
      panel_label = format_response(response_metric)
    ) %>%
    add_bmp_label() %>%
    ggplot(mapping = bmp_estimate_aes) %>%
    add_forest_layers(
      label_mapping = contrast_probability_label,
      label_nudge = 0.08
    ) +
    facet_wrap(
      facets = vars(panel_label),
      ncol = 1,
      scales = "free_y"
    ) +
    scale_x_continuous(
      expand =
        expansion(
          mult = c(0.08, 0.22)
        )
    ) +
    labs(
      title = "Do practices affect the two guilds differently?",
      subtitle =
        wrap_label(
          str_c(
            "Obligate minus facultative, from the joint posterior. P is the ",
            "probability that the practice benefits obligate species more."
          )
        ),
      x = str_c("Difference in ", mixed_scale_axis_label),
      y = NULL
    ) +
    theme_bmp(base_size = 12)

  figure_guild_contrasts %>%
    write_output_figure(
      file_name = "figure_4_guild_contrasts.png",
      width = 9.5,
      height = 6.5
    )
}

# figure 5: heterogeneity --------------------------------------------------

figure_heterogeneity <-
  results$heterogeneity %>%
  mutate(
    model_label =
      model %>%
      case_match(
        "richness_bmp" ~ "Species richness, by practice",
        "abundance_guild_bmp" ~ "Abundance, guild x practice",
        "nest_success_guild_bmp" ~ "Nest success, guild x practice",
        "abundance_pooled_bmp" ~ "Abundance, pooled x practice",
        "nest_success_pooled_bmp" ~ "Nest success, pooled x practice",
        .default = model
      ),
    level_label =
      level %>%
      case_match(
        "between studies" ~ "Between studies",
        "within study (between effect sizes)" ~ "Within study",
        "between species" ~ "Between species",
        "between practices" ~ "Between practices",
        .default = level
      ) %>%
      fct_reorder(i2)
  ) %>%
  ggplot(
    mapping =
      aes(
        x = i2,
        y = level_label
      )
  ) %>%
  add_interval_layers(
    interval_mapping =
      aes(
        xmin = i2_lcl,
        xmax = i2_ucl
      ),
    point_size = 2.4
  ) +
  facet_wrap(
    facets = vars(model_label),
    ncol = 1,
    scales = "free_y"
  ) +
  scale_x_continuous(
    limits = c(0, 100)
  ) +
  labs(
    title = "Where does the heterogeneity live?",
    subtitle =
      wrap_label(
        str_c(
          "Share of total variance by level, a multilevel generalisation of ",
          "I-squared. A single-level model attributes all of this to one term."
        )
      ),
    x = "Percent of total variance (95% credible interval)",
    y = NULL
  ) +
  theme_bmp(base_size = 11)

figure_heterogeneity %>%
  write_output_figure(
    file_name = "figure_5_heterogeneity.png",
    width = 9.5,
    height = 14
  )


# figure 7: species-level abundance ----------------------------------------

figure_species <-
  results$species_abundance %>%
  add_guild_label() %>%
  mutate(
    species_label =
      species_key %>%
      format_species() %>%
      fct_reorder(estimate)
  ) %>%
  filter(k >= 3) %>%
  ggplot(
    mapping =
      aes(
        x = estimate,
        y = species_label,
        colour = guild_label
      )
  ) %>%
  add_zero_line() %>%
  add_interval_layers(
    point_size = 1.9,
    line_width = 0.6
  ) +
  facet_grid(
    rows = vars(guild_label),
    scales = "free_y",
    space = "free_y"
  ) +
  scale_colour_manual(
    values = guild_colours,
    guide = "none"
  ) +
  labs(
    title = "Species-level responses in abundance",
    subtitle =
      wrap_label(
        str_c(
          "Partially pooled estimates: species with few effect sizes are ",
          "shrunk towards their guild mean rather than estimated in isolation."
        )
      ),
    x = effect_axis_label,
    y = NULL
  ) +
  theme_bmp(base_size = 10)

figure_species %>%
  write_output_figure(
    file_name = "figure_7_species_abundance.png",
    width = 9.5,
    height = 14
  )

# posterior distribution figures -------------------------------------------

# Figures 9 to 12 redraw the cell means of figures 1, 2, 3 and 8 as full
# posteriors, read from the fitted models rather than from the tables.

fitted_models <-
  read_rds("output/models/fitted_models.rds")

posterior_slab_note <-
  str_c(
    "The slab is the posterior density of the cell mean, scaled to a common ",
    "height; the point and bar are the median and 95% credible interval the ",
    "forest plots show. P>0 is the posterior mass above zero."
  )

posterior_subtitle_note <-
  str_c(
    "Each guild estimated separately, and the two guilds pooled in their ",
    "own panel. ",
    posterior_slab_note
  )

posterior_cell_aes <-
  aes(
    x = .value,
    y = bmp_label,
    fill = guild_label,
    colour = guild_label
  )

overall_posterior_aes <-
  aes(
    x = .value,
    y = guild_label,
    fill = guild_label,
    colour = guild_label
  )

# Pinned to the panel edge rather than to the interval, because a slab is
# wider than the interval it carries.

probability_label_mapping <-
  aes(
    x = Inf,
    label = probability_label
  )

# Richness is assemblage-level and belongs to no guild, so it takes a neutral
# slab rather than a palette colour.

richness_slab_colour <- "#7A8595"

# figure 9: species richness posterior -------------------------------------

richness_draws <-
  fitted_models %>%
  pluck("richness_bmp") %>%
  gather_cell_draws(term_prefix = "bmp") %>%
  rename(bmp = cell) %>%
  add_posterior_bmp_label()

richness_probability_labels <-
  richness_draws %>%
  summarise_posterior_probability(
    grouping_vars = "bmp_label"
  )

figure_richness_posterior <-
  richness_draws %>%
  ggplot(
    mapping =
      aes(
        x = .value,
        y = bmp_label
      )
  ) %>%
  add_posterior_layers(
    fill = richness_slab_colour,
    colour = "grey15"
  ) %>%
  add_probability_labels(
    label_data = richness_probability_labels
  ) +
  scale_x_continuous(
    expand =
      expansion(
        mult = c(0.06, 0.22)
      )
  ) +
  labs(
    title = "Species richness: posterior distribution by practice",
    subtitle =
      wrap_label(
        str_c(
          "Community-level metric; not partitioned by guild. ",
          posterior_slab_note
        )
      ),
    x = effect_axis_label,
    y = NULL
  ) +
  theme_bmp(base_size = 12)

figure_richness_posterior %>%
  write_output_figure(
    file_name = "figure_9_species_richness_posterior.png",
    width = 9,
    height = 6.5
  )

# figures 10 and 11: abundance and nest success posteriors -----------------

# The guild x BMP and pooled x BMP draws stacked, as figures 2 and 3 stack
# the corresponding tables.

bmp_cell_draw_models <-
  list(
    abundance =
      c("abundance_guild_bmp", "abundance_pooled_bmp"),
    nest_success =
      c("nest_success_guild_bmp", "nest_success_pooled_bmp")
  ) %>%
  map(
    \(.model_names) {
      keep(.model_names, \(.model_name) {
        .model_name %in% names(fitted_models)
      })
    }
  )

bmp_cell_draws <-
  bmp_cell_draw_models %>%
  map(
    \(.model_names) {
      .model_names %>%
        map(
          \(.model_name) {
            fitted_models %>%
              pluck(.model_name) %>%
              gather_guild_bmp_draws()
          }
        ) %>%
        list_rbind()
    }
  )

figure_abundance_posterior <-
  bmp_cell_draws %>%
  pluck("abundance") %>%
  build_posterior_guild_figure(
    metric = "abundance",
    plot_title = "Abundance: posterior distribution by practice"
  )

figure_abundance_posterior %>%
  write_output_figure(
    file_name = "figure_10_abundance_posterior.png",
    width = 10.5,
    height = 16
  )

figure_nest_success_posterior <-
  bmp_cell_draws %>%
  pluck("nest_success") %>%
  build_posterior_guild_figure(
    metric = "nest_success",
    plot_title = "Nest success: posterior distribution by practice"
  )

figure_nest_success_posterior %>%
  write_output_figure(
    file_name = "figure_11_nest_success_posterior.png",
    width = 9,
    height = 11
  )
