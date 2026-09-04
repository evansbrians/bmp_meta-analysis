# This script:
# - Reads the results tables written by 3_contrasts_tables.R
# - Builds the manuscript figures from them and the posterior draws

# Figure 1 is the ROSES diagram; supplementals run S1 upward.

# setup --------------------------------------------------------------------

library(tidybayes)
library(tidyverse)

# Project functions:

source("src/functions.R")

# Output directory:

fs::dir_create("output/figures")

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
      fs::path("output/tables", .table_name, ext = "csv") %>%
        read_csv(show_col_types = FALSE)
    }
  )

# Guild and pooled cell means:

bmp_cells <-
  bind_rows(
    results$guild_bmp,
    results$pooled_bmp
  )

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
      str_wrap(bmp_label, width = 25),
    bmp_label = 
      bmp_label %>% 
      # str_replace("Grasses and", "Grasses\nand") %>% 
      str_replace("Grasses and\nForbs", "Grasses\nand Forbs") %>% 
      str_replace("Livestock Between\nPastures", "Livestock\nBetween Pastures") %>% 
      str_replace("Promote Edge and Shrub\nHabitat", "Promote Edge and\nShrub Habitat")
  )

# Hedges' g axis:

effect_axis_label <- "Pooled effect size (Hedges' g, 95% credible interval)"

# Log hazard ratio axis:

hazard_axis_label <-
  "Pooled effect size (log hazard ratio, 95% credible interval)"

# Mixed-scale axis, for the contrast figure:

mixed_scale_axis_label <-
  str_c(
    "Effect size (Hedges' g for abundance and richness, -log hazard ratio ",
    "for nest survival; 95% credible interval)"
  ) %>%
  str_wrap(width = 70)

# Sample size labels, star if provisional:

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

# Probability labels, at the panel edge:

probability_label_mapping <-
  aes(
    x = Inf,
    label = edge_label
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
  ) %>% 
  mutate(
    edge_label = 
      edge_label %>% 
      str_replace("P>0", "P > 0")
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
      richness_draws %>% 
      group_by(bmp_label) %>%  
      median_qi(.value, .width = 0.95) %>% 
      mutate(
        cross_zero =
          if_else(
            .lower < 0 & .upper > 0,
            TRUE,
            FALSE
          )
      ),
    aes(
      x = .value,
      y = bmp_label,
      fill = cross_zero
    ),
    size = 3.4,
    shape = 21
  ) +
  geom_text(
    probability_label_mapping,
    data = richness_edge_labels,
    hjust = 1.06,
    size = 2.8,
    color = "grey25"
  ) +
  
  # Define scale elements:
  
  scale_fill_manual(
    values = c("black", "white")
  ) +
  scale_x_continuous(
    limits = c(-2, NA),
    expand =
      expansion(
        mult = c(0.001, 0.3)
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
    title = "Species richness: Posterior distribution by practice",
    x = effect_axis_label,
    y = NULL
  ) +
  
  # Modify the theme:
  
  theme_bmp(base_size = 12) +
  theme(
    # text = element_text(family = "sans"),
    plot.title = element_text(size = 15, hjust = 0.4),
    axis.text = element_text(color = "gray5"),
    axis.ticks = element_line(color = "gray55"),
    legend.position = "none"
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
    slab_alpha = 0.22,
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
    x = effect_axis_label,
    y = NULL
  ) +
  
  # Modify the theme:
  
  theme_bmp(base_size = 12)

# Write figure 3:

figure_abundance_pooled %>%
  write_output_figure(
    file_name = "figure_3_abundance_pooled.png",
    width = 9.5,
    height = 8
  )

# figure 4: abundance by guild ---------------------------------------------

# Guilds as side-by-side panels:

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
    slab_alpha = 0.22,
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
    x = effect_axis_label,
    y = NULL
  ) +
  
  # Modify the theme:
  
  theme_bmp(base_size = 11)

# Write figure 4:

figure_abundance_by_guild %>%
  write_output_figure(
    file_name = "figure_4_abundance_by_guild.png",
    width = 15,
    height = 7.5
  )

# figure S1: species richness, intervals -----------------------------------

figure_species_richness <-
  results$species_richness %>%
  
  # Label and order the practice axis:
  
  left_join(
    practice_labels,
    by = join_by(bmp)
  ) %>%
  mutate(
    bmp_label =
      fct_reorder(bmp_label, estimate)
  ) %>%
  
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
    x = effect_axis_label,
    y = NULL
  ) +
  
  # Modify the theme:
  
  theme_bmp(base_size = 12)

# Write figure S1:

figure_species_richness %>%
  write_output_figure(
    file_name = "figure_S1_species_richness_intervals.png",
    width = 9,
    height = 5
  )

# figure S2: abundance by guild, intervals ---------------------------------

figure_abundance <-
  bmp_cells %>%
  
  # Subset to abundance:
  
  filter(response_metric == "abundance") %>%
  
  # Name the guild panels:
  
  add_guild_label() %>%
  
  # Label and order the practice axis:
  
  left_join(
    practice_labels,
    by = join_by(bmp)
  ) %>%
  mutate(
    bmp_label =
      fct_reorder(bmp_label, estimate)
  ) %>%
  
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
    x = effect_axis_label,
    y = NULL
  ) +
  
  # Modify the theme:
  
  theme_bmp(base_size = 12)

# Write figure S2:

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
    slab_alpha = 0.22,
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
    x = hazard_axis_label,
    y = NULL
  ) +
  
  # Modify the theme:
  
  theme_bmp(base_size = 11)

# Write figure S3:

figure_nest_success_posterior %>%
  write_output_figure(
    file_name = "figure_S3_nest_success_by_guild.png",
    width = 13,
    height = 6
  )

# figure S4: nest success by guild, intervals ------------------------------

figure_nest_success <-
  bmp_cells %>%
  
  # Subset to nest success:
  
  filter(response_metric == "nest_success") %>%
  
  # Name the guild panels:
  
  add_guild_label() %>%
  
  # Label and order the practice axis:
  
  left_join(
    practice_labels,
    by = join_by(bmp)
  ) %>%
  mutate(
    bmp_label =
      fct_reorder(bmp_label, estimate)
  ) %>%
  
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
    x = hazard_axis_label,
    y = NULL
  ) +
  
  # Modify the theme:
  
  theme_bmp(base_size = 12)

# Write figure S4:

figure_nest_success %>%
  write_output_figure(
    file_name = "figure_S4_nest_success_by_guild_intervals.png",
    width = 9,
    height = 7.5
  )

# figure S5: guild contrasts in abundance ----------------------------------

# Contrast probability labels:

contrast_probability_label <-
  aes(
    x = ucl,
    label =
      str_c(
        "P=",
        format_number(prob_a_greater)
      )
  )

# Drawn only when contrasts exist:

if (nrow(results$guild_contrasts) > 0) {
  figure_guild_contrasts <-
    results$guild_contrasts %>%
    
    # Name the response panels:
    
    mutate(
      panel_label = format_response(response_metric)
    ) %>%
    
    # Label and order the practice axis:
    
    left_join(
      practice_labels,
      by = join_by(bmp)
    ) %>%
    mutate(
      bmp_label =
        fct_reorder(bmp_label, estimate)
    ) %>%
    
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
      x = str_c("Difference in ", mixed_scale_axis_label),
      y = NULL
    ) +
    
    # Modify the theme:
    
    theme_bmp(base_size = 12)
  
  # Write figure S5:
  
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
  
  # Name the panels, order the levels:
  
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
    x = "Percent of total variance (95% credible interval)",
    y = NULL
  ) +
  
  # Modify the theme:
  
  theme_bmp(base_size = 11)

# Write figure S6:

figure_heterogeneity %>%
  write_output_figure(
    file_name = "figure_S6_heterogeneity.png",
    width = 9.5,
    height = 14
  )

# figure S7: species-level abundance ---------------------------------------

figure_species <-
  results$species_abundance %>%
  
  # Order the panels and species axis:
  
  add_guild_label() %>%
  mutate(
    species_label =
      species_key %>%
      format_species() %>%
      fct_reorder(estimate)
  ) %>%
  
  # Drop species with fewer than three:
  
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
    x = effect_axis_label,
    y = NULL
  ) +
  
  # Modify the theme:
  
  theme_bmp(base_size = 10)

# Write figure S7:

figure_species %>%
  write_output_figure(
    file_name = "figure_S7_species_abundance.png",
    width = 9.5,
    height = 14
  )

# clear the environment ----------------------------------------------------

rm(
  list = ls()
)
