

# setup -------------------------------------------------------------------

library(meta)
library(tidyverse)

effect_model_output <- 
  read_rds("data/processed/effect_sizes_mean_diff.rds")

# Plot theme:

my_theme <-
  function() {
    theme(
      panel.background = element_blank(),
      axis.line = element_line(color = "#999999"),
      panel.grid.major.x = element_line(color = "#efefef"),
      panel.grid.minor.x = 
        element_line(
          color = "#efefef",
          linetype = "dashed",
          linewidth = 1
        ),
      text = element_text(family = "Times"),
      plot.title = element_text(size = 16),
      axis.title = element_text(size = 15),
      axis.text = element_text(size = 12)
    )
  }

# nests are boring -------------------------------------------------------

bmp_%>% 
  mutate(
    predictor_response =
      fct_reorder(
        predictor_response,
        overall_effect
      )
  ) %>% 
  ggplot() +
  aes(
    x = overall_effect,
    y = predictor_response,
    label = n_studies
  ) +
  geom_point(
    size = 2
  ) +
  geom_segment(
    aes(
      x = lower_ci,
      xend = upper_ci
    ),
    linewidth = 1
  ) +
  geom_text(
    aes(x = upper_ci),
    nudge_x = 0.05,
    size = 5
  ) +
  geom_vline(
    xintercept = 0,
    linetype = "dashed",
    color = "#666666") +
  labs(
    title = "Effect sizes: Nest success by Best Management Practice",
    x = "Mean effect size",
    y = "Best Management Practice"
  ) +
  my_theme()

# species richness --------------------------------------------------------

effect_model_output %>% 
  filter(
    str_detect(key, "richness")
  ) %>% 
  mutate(
    predictor_response =
      fct_reorder(
        predictor_response,
        overall_effect
      )
  ) %>% 
  ggplot() +
  aes(
    x = overall_effect,
    y = predictor_response,
    label = n_studies
  ) +
  geom_point(
    size = 2
  ) +
  geom_segment(
    aes(
      x = lower_ci,
      xend = upper_ci
    ),
    linewidth = 1
  ) +
  geom_text(
    aes(x = upper_ci),
    nudge_x = 0.15,
    size = 5
  ) +
  geom_vline(
    xintercept = 0,
    linetype = "dashed",
    color = "#666666") +
  labs(
    title = "Effect sizes: Species richness by Best Management Practice",
    x = "Mean effect size",
    y = "Best Management Practice"
  ) +
  my_theme()

# abundance ---------------------------------------------------------------

# "All species" and taxa groups only only:

effect_model_output %>% 
  filter(
    str_detect(key, "abund"),
    str_detect(predictor_response, "All|Grassl")
  ) %>% 
  mutate(
    predictor_response =
      fct_reorder(
        predictor_response,
        overall_effect
      )
  ) %>% 
  ggplot() +
  aes(
    x = overall_effect,
    y = predictor_response,
    label = n_studies
  ) +
  geom_point(
    size = 2
  ) +
  geom_segment(
    aes(
      x = lower_ci,
      xend = upper_ci
    ),
    linewidth = 1
  ) +
  geom_text(
    aes(x = upper_ci),
    nudge_x = 0.07,
    size = 5
  ) +
  geom_vline(
    xintercept = 0,
    linetype = "dashed",
    color = "#666666") +
  labs(
    title = "Effect sizes: Abundance by Best Management Practice",
    x = "Mean effect size",
    y = "Best Management Practice"
  ) +
  my_theme()

# Species-specific results:

effect_model_output %>% 
  filter(
    str_detect(key, "abund"),
    !str_detect(predictor_response, "All|Grassl|Shru")
  ) %>% 
  mutate(
    predictor_response =
      fct_reorder(
        predictor_response,
        overall_effect
      )
  ) %>% 
  ggplot() +
  aes(
    x = overall_effect,
    y = predictor_response,
    label = n_studies
  ) +
  geom_point(
    size = 2
  ) +
  geom_segment(
    aes(
      x = lower_ci,
      xend = upper_ci
    ),
    linewidth = 1
  ) +
  geom_text(
    aes(x = upper_ci),
    nudge_x = 0.08,
    size = 5
  ) +
  geom_vline(
    xintercept = 0,
    linetype = "dashed",
    color = "#666666") +
  labs(
    title = "Effect sizes: Abundance by Best Management Practice",
    x = "Mean effect size",
    y = "Best Management Practice"
  ) +
  my_theme()
