
# setup -------------------------------------------------------------------

library(meta)
library(tidyverse)

source("functions.R")

sheets_url <-
  file.path(
    "https://docs.google.com/spreadsheets/d",
    "14SWR7TXIKNvrYGr2_vwx9xBp5LDrDNaYcqZt6pldSoA"
  )

# species richness --------------------------------------------------------

# Get the species richness spreadsheet:

species_richness <-
  googlesheets4::read_sheet(
    ss = sheets_url,
    sheet = "species_richness")

# Make the standardized effects size table with standard deviations:

effects_species_richness <-
  species_richness %>% 
  
  # Add standard deviations:
  
  add_sd_to_table() %>% 
  
  # Make standardized effect size table:
  
  purrr::transpose() %>%
  map(
    \(x) {
      get_effects(
        paper = x$paper,
        mean_treatment = x$xbar_e,
        mean_control = x$xbar_c,
        n_treatment = x$n_e,
        n_control = x$n_c,
        sd_treatment = x$sd_e,
        sd_control = x$sd_c
      ) %>%
        mutate(
          bmp = x$bmp,
          predictor_var = x$predictor_var,
          response_var = x$response_var,
          .before = study) %>% 
        select(
          bmp,
          study,
          everything()
        )
    }
  ) %>% 
  list_rbind()

# abundance ---------------------------------------------------------------

abundance <-
  googlesheets4::read_sheet(
    ss = sheets_url,
    sheet = "abundance")

# Make the standardized effects size table with standard deviations:

effects_abundance <-
  abundance %>% 
  
  # Add standard deviations:
  
  add_sd_to_table() %>% 
  
  # Make standardized effect size table:
  
  purrr::transpose() %>%
  map(
    \(x) {
      get_effects(
        paper = x$paper,
        mean_treatment = x$xbar_e,
        mean_control = x$xbar_c,
        n_treatment = x$n_e,
        n_control = x$n_c,
        sd_treatment = x$sd_e,
        sd_control = x$sd_c
      ) %>%
        mutate(
          bmp = x$bmp,
          predictor_var = x$predictor_var,
          response_var = x$response_var,
          .before = study) %>% 
        select(
          bmp,
          study,
          everything()
        )
    }
  ) %>% 
  list_rbind()

# just a look  ------------------------------------------------------------

effects_species_richness %>% 
  print(n = Inf)

effects_abundance  %>% 
  print(n = Inf)

# diving in ---------------------------------------------------------------

effects_species_richness_formatted <-
  effects_species_richness %>% 
  separate(
    bmp, 
    into = c("bmp1", "bmp2"),
    sep = ";",
  ) %>% 
  pivot_longer(
    bmp1:bmp2
  ) %>% 
  select(
    bmp = value,
    everything()
  ) %>% 
  select(!name) %>% 
  mutate(bmp = str_trim(bmp)) %>% 
  drop_na(bmp)

effects_abundance_formatted <-
  effects_abundance %>% 
  separate(
    bmp, 
    into = c("bmp1", "bmp2"),
    sep = ";",
  ) %>% 
  pivot_longer(
    bmp1:bmp2
  ) %>% 
  select(
    bmp = value,
    everything()
  ) %>% 
  select(!name) %>% 
  mutate(bmp = str_trim(bmp)) %>% 
  drop_na(bmp)

effects_species_richness_formatted %>% 
  count(bmp)

effects_abundance_formatted %>% 
  count(bmp)

# overview plots ----------------------------------------------------------

# Species richness:

effects_species_richness_formatted %>% 
  ggplot() +
  aes(
    x = 
      fct_reorder(
        .f = bmp, 
        .x = effect_size, 
        .fun = median),
    y = effect_size) +
  geom_hline(
    yintercept = 0,
    linetype = "dashed") +
  geom_boxplot(fill = "#dcdcdc") + 
  geom_text(
    data = 
      effects_species_richness_formatted %>% 
      count(bmp),
    aes(
      x = bmp,
      y = -1,
      label = n)
  ) +
  coord_flip() +
  labs(
    title = "Effects of BMPs on Species Richness",
    x = "BMP", 
    y = "Effect") +
  theme_classic()

# Abundance:

effects_abundance_formatted %>% 
  ggplot() +
  aes(
    x = 
      fct_reorder(
        .f = bmp, 
        .x = effect_size, 
        .fun = median),
    y = effect_size) +
  geom_hline(
    yintercept = 0,
    linetype = "dashed") +
  geom_boxplot(fill = "#dcdcdc") + 
  geom_text(
    data = 
      effects_abundance_formatted %>% 
      count(bmp),
    aes(
      x = bmp,
      y = -1.5,
      label = n)
  ) +
  coord_flip() +
  labs(
    title = "Effects of BMPs on Abundance",
    x = "BMP", 
    y = "Effect") +
  theme_classic()


# meta tree ---------------------------------------------------------------

# Species richness, warm season grasses:

species_richness %>% 
  
  filter(
    str_detect(bmp, "warm")
  ) %>% 
  
  # Add standard deviations:
  
  add_sd_to_table() %>% 
  
  # Make meta object:
  
  metacont(
    n.e = n_e,
    mean.e = xbar_e,
    sd.e = sd_e,
    n.c = n_c,
    mean.c = xbar_c,
    sd.c = sd_c,
    sm = "SMD",
    method.smd = "Cohen",
    data = .
  ) %>% 
  
  # Make forest plot:
  
  meta::forest(
    common = FALSE,
    comb.fixed = FALSE,
    comb.random = FALSE)

# Abundance, warm season grasses:

abundance %>% 
  
  filter(
    str_detect(bmp, "warm")
  ) %>% 
  
  # Add standard deviations:
  
  add_sd_to_table() %>% 
  
  # Make meta object:
  
  metacont(
    n.e = n_e,
    mean.e = xbar_e,
    sd.e = sd_e,
    n.c = n_c,
    mean.c = xbar_c,
    sd.c = sd_c,
    sm = "SMD",
    method.smd = "Cohen",
    data = .
  ) %>% 
  
  # Make forest plot:
  
  meta::forest(
    # comb.fixed = FALSE,
    comb.random = FALSE
  )


