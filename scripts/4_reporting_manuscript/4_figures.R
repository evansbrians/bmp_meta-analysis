
# This script:
# - Reads the results tables written by 3_contrasts_tables.R
# - Builds the manuscript figures from them and the posterior draws

# Figure 1 is the ROSES diagram.

# setup --------------------------------------------------------------------

library(tidybayes)
library(tidyverse)
library(patchwork)

# Project functions:

source("src/functions.R")

# Output directory:

fs::dir_create("output/figures")

# figure labels ------------------------------------------------------------

# Practice labels for the y-axis:

practice_labels <-
  read_csv(
    "src/practice_labels.csv",
    show_col_types = FALSE
  ) %>%
  
  # Add line breaks:
  
  mutate(
    bmp_label =
      str_wrap(bmp_label, width = 25) %>% 
      str_replace(" grasses and\nforbs", "\ngrasses and forbs") %>%
      str_replace(" and shrub\nhabitat", "\nand shrub habitat") %>% 
      str_replace(" between\npastures", "\nbetween pastures"),
    bmp_label = 
      if_else(
        str_detect(bmp_label, "\\\n"),
        str_c(bmp_label, "\n "),
        bmp_label
      )
  )

# Hedges' g axis:

effect_axis_label <- 
  bquote(
    "Pooled effect size (Hedges'"
    ~ italic("g") * ", 95% credible interval)")

# Probability labels, at the panel edge:

probability_label_mapping <-
  aes(
    x = Inf,
    label = edge_label
  )

# results tables ----------------------------------------------------------

# Results tables:

results <-
  c(
    species_richness = "table_species_richness_by_bmp",
    guild_bmp = "table_guild_bmp",
    guild_contrasts = "table_guild_contrasts_by_bmp",
    pooled_bmp = "table_pooled_bmp",
    heterogeneity = "table_heterogeneity",
    species_abundance = "table_species_abundance"
  ) %>%
  map(
    \(.table_name) {
      fs::path(
        "output/draft_output/tables", 
        .table_name, 
        ext = "csv"
      ) %>%
        read_csv(show_col_types = FALSE)
    }
  )

# Guild and pooled cell means:

bmp_cells <-
  bind_rows(
    results$guild_bmp,
    results$pooled_bmp
  )

# BMP results:

bmp_results <-
  results %>% 
  keep_at(
    ~ !str_detect(.x, "hetero|species_abu")
  ) %>% 
  names() %>% 
  set_names() %>% 
  map(
    ~ results %>% 
      pluck(.x) %>% 
      left_join(
        practice_labels,
        by = "bmp"
      )
  )

# posterior draws ----------------------------------------------------------

# Posterior draws:

posterior_draws <- read_cell_draws()

# Neutral slab color for richness:

richness_slab_color <- "#7A8595"

# Richness draws:

richness_draws <-
  posterior_draws %>%
  
  # Subset to richness:
  
  filter(model == "richness_bmp") %>%
  rename(bmp = cell) %>%
  
  # Label and order the practice axis:
  
  left_join(
    practice_labels,
    by = join_by(bmp)
  ) %>%
  mutate(
    bmp_label =
      fct_reorder(bmp_label, .value)
  )

# Edge labels:

richness_edge_labels <-
  richness_draws %>%
  posterior_edge_labels(
    .cells = results$species_richness,
    grouping_vars = c("bmp", "bmp_label"),
    join_vars = "bmp"
  )

# Guild and pooled cell draws:

cell_draws <-
  c(
    abundance_guild = "abundance_guild_bmp",
    abundance_pooled = "abundance_pooled_bmp",
    nest_success_guild = "nest_success_guild_bmp"
  ) %>%
  map(
    \(.model_name) {
      posterior_draws %>%
        
        # Subset to the model:
        
        filter(model == .model_name) %>%
        rename(bmp = cell) %>%
        
        # Name the guild panels:
        
        add_guild_label() %>%
        
        # Label and order the practice axis:
        
        left_join(
          practice_labels,
          by = join_by(bmp)
        ) %>%
        mutate(
          bmp_label =
            fct_reorder(bmp_label, .value)
        )
    }
  )

# Edge labels, per pool:

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
            c(
              "guild",
              "bmp",
              "guild_label",
              "bmp_label"
            ),
          join_vars = c("guild", "bmp")
        )
    }
  )

# pre-processing ----------------------------------------------------------

# Assign factor levels for guild-specific abundance:

guild_abundance_start <- 
  cell_draws %>%
  pluck("abundance_guild") %>%
  left_join(
    bmp_results %>% 
      pluck("guild_bmp") %>% 
      filter(response_metric == "abundance") %>% 
      select(
        guild:bmp, 
        estimate,
        excludes_zero
      ),
    by = c("guild", "bmp")
  ) %>%
  mutate(
    guild_label = as.character(guild_label),
    bmp_label = 
      fct_reorder2(
        bmp_label, 
        guild_label,
        estimate,
        .desc = FALSE
      ),
    guild_label =
      fct(
        guild_label,
        levels = 
          c(
            "Obligate grassland",
            "Facultative grassland"
          )
      )
  )

# figure 2: species richness ----------------------------------------------

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
    linewidth = 0.4,
    color = "grey40"
  ) +
  stat_halfeye(
    .width = 0.95,
    normalize = "xy",
    slab_alpha = 0.22,
    slab_linewidth = 0.8,
    point_size = 0.5,
    slab_fill = richness_slab_color,
    slab_color = "grey25",
    linewidth = 2.3,
    scale = 0.8
  ) +
  geom_point(
    data = 
      bmp_results %>%
      pluck("species_richness"),
    aes(
      x = estimate,
      y = bmp_label,
      fill = excludes_zero
    ),
    size = 3.4,
    shape = 21
  ) +
  geom_text(
    aes(label = sample_note),
    data = 
      richness_edge_labels,
    x = 1.95,
    hjust = 0,
    vjust = -5,
    size = 2.7,
    color = "grey25",
  ) +
  geom_text(
    aes(label = probability_label),
    data = 
      richness_edge_labels,
    x = 1.95,
    hjust = 0,
    vjust = -3,
    size = 2.7,
    color = "grey25"
  ) +
  
  # Define scale elements:
  
  scale_fill_manual(
    values = c("white", "black")
  ) +
  scale_x_continuous(
    limits = c(-2, 2.5),
    breaks = 
      seq(-2, 2.5, by = 0.5),
    expand =
      expansion(
        mult = c(0.001, 0.001)
      )
  ) +
  scale_y_discrete(
    expand =
      expansion(
        mult = c(0.04, 0.05)
      )
  ) +
  
  # Add labels:
  
  labs(
    x = effect_axis_label,
    y = NULL
  ) +
  
  # Modify the theme:
  
  theme_bmp(base_size = 12) +
  theme(
    axis.title.x = element_text(vjust = 0.2),
    plot.margin = 
      margin(10, 10, 7, 2),
  )

# Write figure 2:

figure_richness_posterior %>%
  write_output_figure(
    file_name = "figure_2_species_richness.png",
    width = 9.5,
    height = 6
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
    y = bmp_label
  ) +
  
  # Add geometries:
  
  geom_vline(
    xintercept = 0,
    linetype = "dashed",
    linewidth = 0.4,
    color = "grey40"
  ) +
  stat_halfeye(
    .width = 0.95,
    normalize = "xy",
    slab_alpha = 0.22,
    slab_linewidth = 0.8,
    point_size = 0.5,
    slab_fill = richness_slab_color,
    slab_color = "grey25",
    linewidth = 2.3,
    scale = 0.8
  ) +
  geom_point(
    data = 
      bmp_results %>%
      pluck("pooled_bmp"),
    aes(
      x = estimate,
      y = bmp_label,
      fill = excludes_zero
    ),
    size = 3.4,
    shape = 21
  ) +
  geom_text(
    aes(label = sample_note),
    data = 
      edge_labels$abundance_pooled,
    hjust = 0,
    x = 1.6,
    vjust = -3.5,
    size = 2.7,
    color = "grey25",
  ) +
  geom_text(
    aes(label = probability_label),
    data = 
      edge_labels$abundance_pooled,
    x = 1.6,
    hjust = 0,
    vjust = -1.5,
    size = 2.7,
    color = "grey25"
  ) +
  
  # Define scale elements:
  
  scale_fill_manual(
    values = c("white", "black")
  ) +
  scale_x_continuous(
    limits = c(-1, 2),
    breaks = 
      seq(-1, 2, by = 0.5),
    expand =
      expansion(
        mult = c(0.001, 0.001)
      )
  ) +
  scale_y_discrete(
    expand =
      expansion(
        mult = c(0.03, 0.05)
      )
  ) +
  
  # Add labels:
  
  labs(
    x = effect_axis_label,
    y = NULL
  ) +
  
  # Modify the theme:
  
  theme_bmp(base_size = 12) +
  theme(
    axis.title.x = element_text(vjust = 0.2),
    plot.margin = 
      margin(10, 10, 7, 2),
  )

# Write figure 3:

figure_abundance_pooled %>%
  write_output_figure(
    file_name = "figure_3_abundance_pooled.png",
    width = 9.5,
    height = 8
  )

# figure 4: abundance by guild ---------------------------------------------

## obligate abundance -----------------------------------------------------

obligate_abundance <-
  guild_abundance_start %>% 
  filter(
    guild_label == "Obligate grassland"
  ) %>% 
  full_join(
    guild_abundance_start %>% 
      distinct(bmp_label),
    by = "bmp_label"
  ) %>% 
  mutate(
    guild_label = 
      replace_na(
        guild_label, 
        "Obligate grassland"
      )
  ) %>% 
  
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
    color = "grey25"
  ) +
  stat_halfeye(
    .width = 0.95,
    point_interval = "median_qi",
    normalize = "xy",
    slab_alpha = 0.22,
    slab_linewidth = 0.8,
    point_color = NA,
    scale = 0.8,
    slab_fill = "#1B5E3C",
    slab_color = "#1B5E3C",
    fill = "#1B5E3C",
    color = "#1B5E3C"
  ) +
  geom_point(
    aes(
      x = estimate,
      fill = excludes_zero
    ),
    size = 3.4,
    shape = 21,
    color = "#1B5E3C"
  ) +
  geom_text(
    aes(label = sample_note),
    data = 
      edge_labels$abundance_guild %>% 
      filter(guild == "obligate_grassland"),
    hjust = 0,
    x = 1.13,
    vjust = -4,
    size = 2.7,
    color = "grey25",
  ) +
  geom_text(
    aes(label = probability_label),
    data = 
      edge_labels$abundance_guild %>% 
      filter(guild == "obligate_grassland"),
    x = 1.13,
    hjust = 0,
    vjust = -2,
    size = 2.7,
    color = "grey25"
  ) +
  
  # Define scale elements:
  
  scale_fill_manual(
    values = c("#ffffff", "#1B5E3C")
  ) +
  scale_x_continuous(
    limits = c(-1, 1.5),
    expand = 
      expansion(
        mult = c(0.001, 0.001)
      )
  ) +
  scale_y_discrete(
    expand =
      expansion(
        mult = c(0.03, NA)
      )
  ) +
  
  # Add facet:
  
  facet_wrap(
    ~ guild_label,
    nrow = 1
  ) +
  
  # Add labels:
  
  labs(
    x = effect_axis_label,
    y = NULL
  ) +
  
  # Modify the theme:
  
  theme_bmp(base_size = 11) +
  theme(
    axis.title.x = element_text(size = 14, vjust = 0.2),
    strip.text = element_text(size = 14),
    plot.margin = 
      margin(l = 1, r = 10),
  )

## facultative abundance --------------------------------------------------

facultative_abundance <-
  guild_abundance_start %>% 
  filter(
    guild_label == "Facultative grassland"
  ) %>% 
  full_join(
    guild_abundance_start %>% 
      distinct(bmp_label),
    by = "bmp_label"
  ) %>% 
  mutate(
    guild_label = 
      replace_na(
        guild_label, 
        "Facultative grassland"
      )
  ) %>% 
  
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
    color = "grey25"
  ) +
  stat_halfeye(
    .width = 0.95,
    point_interval = "median_qi",
    normalize = "xy",
    slab_alpha = 0.22,
    slab_linewidth = 0.8,
    point_color = NA,
    scale = 0.8,
    slab_fill = "#B07A2A",
    slab_color = "#B07A2A",
    fill = "#B07A2A",
    color = "#B07A2A"
  ) +
  geom_point(
    aes(
      x = estimate,
      fill = excludes_zero
    ),
    size = 3.4,
    shape = 21,
    color = "#B07A2A"
  ) +
  geom_text(
    aes(label = sample_note),
    data = 
      edge_labels$abundance_guild %>% 
      filter(guild == "facultative_grassland"),
    hjust = 0,
    x = 1.13,
    vjust = -4,
    size = 2.7,
    color = "grey25",
  ) +
  geom_text(
    aes(label = probability_label),
    data = 
      edge_labels$abundance_guild %>% 
      filter(guild == "facultative_grassland"),
    x = 1.13,
    hjust = 0,
    vjust = -2,
    size = 2.7,
    color = "grey25"
  ) +
  
  # Define scale elements:
  
  scale_fill_manual(
    values = c("#ffffff", "#B07A2A")
  ) +
  scale_x_continuous(
    limits = c(-1, 1.5),
    expand = 
      expansion(
        mult = c(0.001, 0.001)
      )
  ) +
  scale_y_discrete(
    expand =
      expansion(
        mult = c(0.03, 0.111)
      )
  ) +
  
  # Add facet:
  
  facet_wrap(
    ~ guild_label,
    nrow = 1
  ) +
  
  # Add labels:
  
  labs(
    x = effect_axis_label,
    y = NULL
  ) +
  
  # Modify the theme:
  
  theme_bmp(base_size = 11) +
  theme(
    axis.title.x = element_text(size = 14, vjust = 0.2),
    axis.text.y = element_blank(),
    axis.ticks.y = element_blank(),
    strip.text = element_text(size = 14),
    plot.margin = 
      margin(l = 10, r = 10),
  )

## combine and write ------------------------------------------------------

# Combine plots:

figure_abundance_by_guild <- 
  obligate_abundance + 
  facultative_abundance +
  plot_layout(axis_titles = "collect")

# Write figure 4:

figure_abundance_by_guild %>%
  write_output_figure(
    file_name = "figure_4_abundance_by_guild.png",
    width = 15,
    height = 7.5
  )

# clear the environment ----------------------------------------------------

rm(
  list = ls()
)
