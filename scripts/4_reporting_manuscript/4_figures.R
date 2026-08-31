# This script:
# - Reads the results tables written by 3_contrasts_tables.R
# - Builds the manuscript figures from them and the posterior draws

# Every figure is one chain: the data, the aesthetic mapping, the geometries,
# the scales, the facets, the labels, the theme.

# The manuscript figures come first, numbered as the manuscript numbers them;
# Figure 1 is the ROSES diagram. The supplemental figures follow, S1 upward.

# setup --------------------------------------------------------------------

library(brms)
library(tidybayes)
library(tidyverse)

source("scripts/src/functions.R")

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

# The palette, the axis labels, the filled-point note and the sample-size
# mapping are in functions.R, beside the labels they are built from.

# Wrapped, because a one-line version overruns a stacked figure's width.

mixed_scale_axis_label <-
  str_c(
    "Effect size (Hedges' g for abundance and richness, log hazard ratio ",
    "for nest survival; 95% credible interval)"
  ) %>%
  wrap_label(width = 70)

# posterior draws ----------------------------------------------------------

# Each figure redraws a cell mean as its full posterior, read from the fitted
# model rather than from the table.

fitted_models <-
  read_rds("output/models/fitted_models.rds")

# The slab note, the subtitle it builds and the right-edge label mapping are
# in functions.R, beside the notes they are built from.

# Richness is assemblage-level and belongs to no guild, so it takes a neutral
# slab rather than a palette color.

richness_slab_color <- "#7A8595"

richness_draws <-
  fitted_models %>%
  pluck("richness_bmp") %>%
  gather_cell_draws(term_prefix = "bmp") %>%
  rename(bmp = cell) %>%
  add_posterior_bmp_label()

richness_edge_labels <-
  richness_draws %>%
  posterior_edge_labels(
    .cells = results$species_richness,
    grouping_vars = c("bmp", "bmp_label"),
    join_vars = "bmp"
  )

# The guild x BMP and pooled x BMP draws are read separately, because the
# pooled model is its own figure and the two guilds share one.

cell_draws <-
  c(
    abundance_guild = "abundance_guild_bmp",
    abundance_pooled = "abundance_pooled_bmp",
    nest_success_guild = "nest_success_guild_bmp",
    nest_success_pooled = "nest_success_pooled_bmp"
  ) %>%
  keep(
    \(.model_name) {
      .model_name %in% names(fitted_models)
    }
  ) %>%
  map(
    \(.model_name) {
      fitted_models %>%
        pluck(.model_name) %>%
        gather_guild_bmp_draws() %>%
        add_guild_label() %>%
        add_posterior_bmp_label()
    }
  )

# `bmp_cells` carries both responses, so the counts are cut to the pool's own
# response before the join; the pool's name says which that is.

edge_labels <-
  cell_draws %>%
  imap(
    \(.draws, .pool) {
      .draws %>%
        posterior_edge_labels(
          .cells =
            bmp_cells %>%
            filter(
              response_metric ==
                str_remove(.pool, "_(guild|pooled)$")
            ),
          grouping_vars =
            c("guild", "bmp", "guild_label", "bmp_label"),
          join_vars = c("guild", "bmp")
        )
    }
  )

# figure 2: species richness -----------------------------------------------

figure_richness_posterior <-
  richness_draws %>%

  # Initialize the plot with the data:

  ggplot() +

  # Map data to visual elements:

  aes(
    x = .value,
    y = bmp_label
  ) +

  # Add geometries:

  geom_vline(
    xintercept = 0,
    linetype = "dashed",
    linewidth = 0.3,
    color = "grey40"
  ) +
  stat_halfeye(
    .width = 0.95,
    point_interval = "median_qi",
    normalize = "xy",
    slab_alpha = 0.55,
    slab_linewidth = 0.3,
    point_size = 1.6,
    fill = richness_slab_color,
    color = "grey15"
  ) +
  geom_text(
    probability_label_mapping,
    data = richness_edge_labels,
    hjust = 1.06,
    size = 2.8,
    color = "grey25"
  ) +

  # Define scale elements:

  scale_x_continuous(
    expand =
      expansion(
        mult = c(0.06, 0.46)
      )
  ) +

  # Add labels:

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

  # Modify the theme:

  theme_bmp(base_size = 12)

figure_richness_posterior %>%
  write_output_figure(
    file_name = "figure_2_species_richness.png",
    width = 9.5,
    height = 6.5
  )

# figure 3: abundance, guilds pooled ---------------------------------------

figure_abundance_pooled <-
  cell_draws %>%
  pluck("abundance_pooled") %>%

  # Initialize the plot with the data:

  ggplot() +

  # Map data to visual elements:

  aes(
    x = .value,
    y = bmp_label,
    fill = guild_label,
    color = guild_label
  ) +

  # Add geometries:

  geom_vline(
    xintercept = 0,
    linetype = "dashed",
    linewidth = 0.3,
    color = "grey40"
  ) +
  stat_halfeye(
    .width = 0.95,
    point_interval = "median_qi",
    normalize = "xy",
    slab_alpha = 0.55,
    slab_linewidth = 0.3,
    point_size = 1.6
  ) +
  geom_text(
    probability_label_mapping,
    data = edge_labels$abundance_pooled,
    hjust = 1.06,
    size = 2.8,
    color = "grey25"
  ) +

  # Define scale elements:

  scale_fill_manual(
    values = guild_colors,
    guide = "none"
  ) +
  scale_color_manual(
    values = guild_colors,
    guide = "none"
  ) +
  scale_x_continuous(
    expand =
      expansion(
        mult = c(0.06, 0.46)
      )
  ) +

  # Add labels:

  labs(
    title = "Abundance: posterior distribution by practice",
    subtitle =
      wrap_label(
        str_c(
          "Obligate and facultative grassland birds pooled. ",
          posterior_slab_note
        )
      ),
    x = effect_axis_label,
    y = NULL
  ) +

  # Modify the theme:

  theme_bmp(base_size = 12)

figure_abundance_pooled %>%
  write_output_figure(
    file_name = "figure_3_abundance_pooled.png",
    width = 9.5,
    height = 8
  )

# figure 4: abundance by guild ---------------------------------------------

# The two guilds read as columns of a single row, so a practice is compared
# across guilds by looking left and right rather than up and down.

figure_abundance_by_guild <-
  cell_draws %>%
  pluck("abundance_guild") %>%

  # Initialize the plot with the data:

  ggplot() +

  # Map data to visual elements:

  aes(
    x = .value,
    y = bmp_label,
    fill = guild_label,
    color = guild_label
  ) +

  # Add geometries:

  geom_vline(
    xintercept = 0,
    linetype = "dashed",
    linewidth = 0.3,
    color = "grey40"
  ) +
  stat_halfeye(
    .width = 0.95,
    point_interval = "median_qi",
    normalize = "xy",
    slab_alpha = 0.55,
    slab_linewidth = 0.3,
    point_size = 1.6
  ) +
  geom_text(
    probability_label_mapping,
    data = edge_labels$abundance_guild,
    hjust = 1.06,
    size = 2.6,
    color = "grey25"
  ) +

  # Define scale elements:

  scale_fill_manual(
    values = guild_colors,
    guide = "none"
  ) +
  scale_color_manual(
    values = guild_colors,
    guide = "none"
  ) +
  scale_x_continuous(
    expand =
      expansion(
        mult = c(0.06, 0.62)
      )
  ) +

  # Divide the plot into facets:

  facet_wrap(
    ~ guild_label,
    nrow = 1,
    scales = "free_y"
  ) +

  # Add labels:

  labs(
    title = "Abundance: posterior distribution by practice and guild",
    subtitle =
      wrap_label(
        str_c(
          "Each guild estimated separately. ",
          posterior_slab_note
        )
      ),
    x = effect_axis_label,
    y = NULL
  ) +

  # Modify the theme:

  theme_bmp(base_size = 11)

figure_abundance_by_guild %>%
  write_output_figure(
    file_name = "figure_4_abundance_by_guild.png",
    width = 15,
    height = 7.5
  )

# figure S1: species richness, intervals -----------------------------------

figure_species_richness <-
  results$species_richness %>%

  # Order the practice axis by effect size:

  add_bmp_label() %>%

  # Initialize the plot with the data:

  ggplot() +

  # Map data to visual elements:

  aes(
    x = estimate,
    y = bmp_label
  ) +

  # Add geometries:

  geom_vline(
    xintercept = 0,
    linetype = "dashed",
    linewidth = 0.3,
    color = "grey40"
  ) +
  geom_linerange(
    aes(
      xmin = lcl,
      xmax = ucl
    ),
    linewidth = 0.7
  ) +
  geom_point(
    aes(shape = excludes_zero),
    size = 2.6
  ) +
  geom_text(
    sample_size_label,
    hjust = 0,
    nudge_x = 0.12,
    size = 2.9,
    color = "grey25"
  ) +

  # Define scale elements:

  scale_shape_manual(
    values =
      c(
        `TRUE` = 16,
        `FALSE` = 1
      ),
    guide = "none"
  ) +
  scale_x_continuous(
    expand =
      expansion(
        mult = c(0.05, 0.28)
      )
  ) +

  # Add labels:

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

  # Modify the theme:

  theme_bmp(base_size = 12)

figure_species_richness %>%
  write_output_figure(
    file_name = "figure_S1_species_richness_intervals.png",
    width = 9,
    height = 5
  )

# figure S2: abundance by guild, intervals ---------------------------------

figure_abundance <-
  bmp_cells %>%

  # Subset to the abundance cells:

  filter(response_metric == "abundance") %>%

  # Order the guild panels and the practice axis:

  add_guild_label() %>%
  add_bmp_label() %>%

  # Initialize the plot with the data:

  ggplot() +

  # Map data to visual elements:

  aes(
    x = estimate,
    y = bmp_label,
    color = guild_label
  ) +

  # Add geometries:

  geom_vline(
    xintercept = 0,
    linetype = "dashed",
    linewidth = 0.3,
    color = "grey40"
  ) +
  geom_linerange(
    aes(
      xmin = lcl,
      xmax = ucl
    ),
    linewidth = 0.7
  ) +
  geom_point(
    aes(shape = excludes_zero),
    size = 2.6
  ) +
  geom_text(
    sample_size_label,
    hjust = 0,
    nudge_x = 0.12,
    size = 2.9,
    color = "grey25"
  ) +

  # Define scale elements:

  scale_shape_manual(
    values =
      c(
        `TRUE` = 16,
        `FALSE` = 1
      ),
    guide = "none"
  ) +
  scale_color_manual(
    values = guild_colors,
    guide = "none"
  ) +
  scale_x_continuous(
    expand =
      expansion(
        mult = c(0.06, 0.28)
      )
  ) +

  # Divide the plot into facets:

  facet_wrap(
    ~ guild_label,
    ncol = 1,
    scales = "free_y"
  ) +

  # Add labels:

  labs(
    title = "Abundance, by vegetation-type guild and pooled",
    subtitle =
      wrap_label(
        str_c(
          "Each guild estimated separately, and the two guilds pooled ",
          "in their own panel. ",
          filled_point_note
        )
      ),
    x = effect_axis_label,
    y = NULL
  ) +

  # Modify the theme:

  theme_bmp(base_size = 12)

figure_abundance %>%
  write_output_figure(
    file_name = "figure_S2_abundance_by_guild_intervals.png",
    width = 9,
    height = 11.5
  )

# figure S3: nest success by guild -----------------------------------------

figure_nest_success_posterior <-
  cell_draws %>%
  pluck("nest_success_guild") %>%

  # Initialize the plot with the data:

  ggplot() +

  # Map data to visual elements:

  aes(
    x = .value,
    y = bmp_label,
    fill = guild_label,
    color = guild_label
  ) +

  # Add geometries:

  geom_vline(
    xintercept = 0,
    linetype = "dashed",
    linewidth = 0.3,
    color = "grey40"
  ) +
  stat_halfeye(
    .width = 0.95,
    point_interval = "median_qi",
    normalize = "xy",
    slab_alpha = 0.55,
    slab_linewidth = 0.3,
    point_size = 1.6
  ) +
  geom_text(
    probability_label_mapping,
    data = edge_labels$nest_success_guild,
    hjust = 1.06,
    size = 2.6,
    color = "grey25"
  ) +

  # Define scale elements:

  scale_fill_manual(
    values = guild_colors,
    guide = "none"
  ) +
  scale_color_manual(
    values = guild_colors,
    guide = "none"
  ) +
  scale_x_continuous(
    expand =
      expansion(
        mult = c(0.06, 0.62)
      )
  ) +

  # Divide the plot into facets:

  facet_wrap(
    ~ guild_label,
    nrow = 1,
    scales = "free_y"
  ) +

  # Add labels:

  labs(
    title = "Nest success: posterior distribution by practice and guild",
    subtitle =
      wrap_label(
        str_c(
          "Each guild estimated separately. ",
          posterior_slab_note
        )
      ),
    x = hazard_axis_label,
    y = NULL
  ) +

  # Modify the theme:

  theme_bmp(base_size = 11)

figure_nest_success_posterior %>%
  write_output_figure(
    file_name = "figure_S3_nest_success_by_guild.png",
    width = 13,
    height = 6
  )

# figure S4: nest success by guild, intervals ------------------------------

figure_nest_success <-
  bmp_cells %>%

  # Subset to the nest success cells:

  filter(response_metric == "nest_success") %>%

  # Order the guild panels and the practice axis:

  add_guild_label() %>%
  add_bmp_label() %>%

  # Initialize the plot with the data:

  ggplot() +

  # Map data to visual elements:

  aes(
    x = estimate,
    y = bmp_label,
    color = guild_label
  ) +

  # Add geometries:

  geom_vline(
    xintercept = 0,
    linetype = "dashed",
    linewidth = 0.3,
    color = "grey40"
  ) +
  geom_linerange(
    aes(
      xmin = lcl,
      xmax = ucl
    ),
    linewidth = 0.7
  ) +
  geom_point(
    aes(shape = excludes_zero),
    size = 2.6
  ) +
  geom_text(
    sample_size_label,
    hjust = 0,
    nudge_x = 0.12,
    size = 2.9,
    color = "grey25"
  ) +

  # Define scale elements:

  scale_shape_manual(
    values =
      c(
        `TRUE` = 16,
        `FALSE` = 1
      ),
    guide = "none"
  ) +
  scale_color_manual(
    values = guild_colors,
    guide = "none"
  ) +
  scale_x_continuous(
    expand =
      expansion(
        mult = c(0.06, 0.28)
      )
  ) +

  # Divide the plot into facets:

  facet_wrap(
    ~ guild_label,
    ncol = 1,
    scales = "free_y"
  ) +

  # Add labels:

  labs(
    title = "Nest success, by vegetation-type guild and pooled",
    subtitle =
      wrap_label(
        str_c(
          "Each guild estimated separately, and the two guilds pooled ",
          "in their own panel. ",
          filled_point_note
        )
      ),
    x = hazard_axis_label,
    y = NULL
  ) +

  # Modify the theme:

  theme_bmp(base_size = 12)

figure_nest_success %>%
  write_output_figure(
    file_name = "figure_S4_nest_success_by_guild_intervals.png",
    width = 9,
    height = 7.5
  )

# figure S5: guild contrasts in abundance ----------------------------------

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

    # Name the response each panel carries:

    mutate(
      panel_label = format_response(response_metric)
    ) %>%

    # Order the practice axis by effect size:

    add_bmp_label() %>%

    # Initialize the plot with the data:

    ggplot() +

    # Map data to visual elements:

    aes(
      x = estimate,
      y = bmp_label
    ) +

    # Add geometries:

    geom_vline(
      xintercept = 0,
      linetype = "dashed",
      linewidth = 0.3,
      color = "grey40"
    ) +
    geom_linerange(
      aes(
        xmin = lcl,
        xmax = ucl
      ),
      linewidth = 0.7
    ) +
    geom_point(
      aes(shape = excludes_zero),
      size = 2.6
    ) +
    geom_text(
      contrast_probability_label,
      hjust = 0,
      nudge_x = 0.08,
      size = 2.9,
      color = "grey25"
    ) +

    # Define scale elements:

    scale_shape_manual(
      values =
        c(
          `TRUE` = 16,
          `FALSE` = 1
        ),
      guide = "none"
    ) +
    scale_x_continuous(
      expand =
        expansion(
          mult = c(0.08, 0.22)
        )
    ) +

    # Divide the plot into facets:

    facet_wrap(
      ~ panel_label,
      ncol = 1,
      scales = "free_y"
    ) +

    # Add labels:

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

    # Modify the theme:

    theme_bmp(base_size = 12)

  figure_guild_contrasts %>%
    write_output_figure(
      file_name = "figure_S5_abundance_guild_contrasts.png",
      width = 9.5,
      height = 6.5
    )
}

# figure S6: heterogeneity -------------------------------------------------

figure_heterogeneity <-
  results$heterogeneity %>%

  # Name each model panel and order the variance levels:

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

  # Initialize the plot with the data:

  ggplot() +

  # Map data to visual elements:

  aes(
    x = i2,
    y = level_label
  ) +

  # Add geometries:

  geom_linerange(
    aes(
      xmin = i2_lcl,
      xmax = i2_ucl
    ),
    linewidth = 0.7
  ) +
  geom_point(size = 2.4) +

  # Define scale elements:

  scale_x_continuous(
    limits = c(0, 100)
  ) +

  # Divide the plot into facets:

  facet_wrap(
    ~ model_label,
    ncol = 1,
    scales = "free_y"
  ) +

  # Add labels:

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

  # Modify the theme:

  theme_bmp(base_size = 11)

figure_heterogeneity %>%
  write_output_figure(
    file_name = "figure_S6_heterogeneity.png",
    width = 9.5,
    height = 14
  )

# figure S7: species-level abundance ---------------------------------------

figure_species <-
  results$species_abundance %>%

  # Order the guild panels and the species axis:

  add_guild_label() %>%
  mutate(
    species_label =
      species_key %>%
      format_species() %>%
      fct_reorder(estimate)
  ) %>%

  # A species resting on fewer than three effect sizes is not drawn:

  filter(k >= 3) %>%

  # Initialize the plot with the data:

  ggplot() +

  # Map data to visual elements:

  aes(
    x = estimate,
    y = species_label,
    color = guild_label
  ) +

  # Add geometries:

  geom_vline(
    xintercept = 0,
    linetype = "dashed",
    linewidth = 0.3,
    color = "grey40"
  ) +
  geom_linerange(
    aes(
      xmin = lcl,
      xmax = ucl
    ),
    linewidth = 0.6
  ) +
  geom_point(size = 1.9) +

  # Define scale elements:

  scale_color_manual(
    values = guild_colors,
    guide = "none"
  ) +

  # Divide the plot into facets:

  facet_grid(
    guild_label ~ .,
    scales = "free_y",
    space = "free_y"
  ) +

  # Add labels:

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

  # Modify the theme:

  theme_bmp(base_size = 10)

figure_species %>%
  write_output_figure(
    file_name = "figure_S7_species_abundance.png",
    width = 9.5,
    height = 14
  )

# clean the environment ----------------------------------------------------

# Everything this script produces is written above; the next script
# reads it back from disk, so nothing is handed on in memory.

rm(list = ls())
