# Exploring species classifications

# setup -------------------------------------------------------------------

library(tidyverse)

# List for species lumping:

species_classes <- 
  read_rds("data/processed/species_lump_list.rds") %>% 
  bind_rows() %>% 
  summarize(
    species_class = 
      str_c(combined_class, collapse = ";"),
    .by = common_name
  )

# Species from effect size tables (to double-check classes):

species_from_effect_sizes <-
  read_rds("data/processed/effect_size_tables_2025-10-16.rds") %>% 
  map_df(
    ~ select(.x, species)
  ) %>% 
  semi_join(
    species_classes,
    by = join_by(species == common_name)
  ) %>% 
  count(species)

# summaries ---------------------------------------------------------------

species_class_count <- 
  species_classes %>% 
  count(species_class)
  
# Proportion (%) of species in which the species was lumped into more than
# one class:

species_classes %>% 
  mutate(
    multiple_classes = 
      if_else(
        str_count(species_class, ";") > 0,
        "more than one",
        "one"
      )
  ) %>% 
  count(multiple_classes) %>% 
  mutate(
    total_n = sum(n),
    percent_classes = n / sum(n) * 100
  )

# Number and proportion (%) of species in which the species was lumped into more
# than one class, by class:

prop_of_classes <- 
  species_classes %>% 
  filter(
    !str_detect(species_class, ";")
  ) %>% 
  pull(species_class) %>% 
  unique() %>% 
  set_names() %>% 
  map_df(
    ~ species_classes %>% 
      filter(
        str_detect(species_class, .x)
      ) %>% 
      mutate(
        multiple_classes = 
          if_else(
            str_count(species_class, ";") > 0,
            "more than one",
            "one"
          )
      ) %>% 
      count(multiple_classes) %>% 
      mutate(
        species_class = .x,
        n_class_total = sum(n),
        percent_classes = n / sum(n) * 100
      ) %>% 
      select(
        species_class, everything()
      )
  )

prop_of_classes %>% 
  ggplot() +
  aes(
    x = species_class,
    y = percent_classes,
    fill = multiple_classes
  ) +
  geom_bar(stat = "identity") +
  scale_y_continuous(
    limits = c(0, 100),
    expand = c(0, 0)
  ) +
  my_theme_today()

prop_of_classes %>% 
  ggplot() +
  aes(
    x = species_class,
    y = n,
    fill = multiple_classes
  ) +
  geom_bar(stat = "identity") +
  scale_y_continuous(
    limits = c(0, 125),
    expand = c(0, 0),
    breaks = 
      seq(
        0, 
        125, 
        by = 25
      )
  ) +
  my_theme_today()

species_classes %>% 
  filter(str_detect(species_class, "shrub")) %>% 
  summarize(
    n = n(), 
    .by = species_class
  ) %>% 
  mutate(
    total_n = sum(n),
    percentage = n/sum(n) * 100
  )
