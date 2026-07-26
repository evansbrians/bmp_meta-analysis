# 

# setup -------------------------------------------------------------------

library(tidyverse)

# subset to species from effect size tables -------------------------------

read_rds("data/processed/effect_size_tables.rds") %>% 
  map_df(
    ~ .x %>% 
      distinct(species)
  ) %>% 
  distinct() %>% 
  arrange(species) %>% 
  filter(
    !str_detect(
      species,
      str_c(
        "^pas|spec",
        "^ar",
        "^res",
        "omn|frug|insect|gran|carn",
        "obl|gen",
        sep = "|"
      )
    )
  ) %>% 
  mutate(
    species = str_split(species, "; ")
  ) %>% 
  unnest(species) %>% 
  mutate(
    species = 
      species %>% 
      str_replace("-", " ") %>% 
      str_remove("'")
  ) %>% 
  distinct(species) %>% 
  print(n = Inf)

